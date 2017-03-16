#lang 2d racket/base
(provide lindenmayer-lexer)
(require racket/match syntax-color/racket-lexer racket/bool racket/list
         syntax-color/default-lexer)
(module+ test (require rackunit))

(define (paren-info mode data)
  (cond
    [(assoc data '(((#"[") . |[|) ((#"]") . |]|)))
     => (λ (info) (values mode 'symbol (cdr info)))]
    [else mode]))

(struct errstate (mode data) #:transparent)

(define errlabel 'errlabel)
(define errnobrk 'errnobrk)
(define errresum 'errresum)
(define errnewln 'errnewln)

(define sec-regexp
  #rx"^(#+)[ \t]*([a-zA-Z]+)[ \t]*(#+)[ \t]*($|\n)")

(define (sec-next state data)
  (define transit '(("axiom" . axiom-new) ("rules" . rules-lhs) ("variables" . vars-lhs)))
  (cond
    [(and (>= (length data) 3)
          (equal? (list-ref data 0) (list-ref data 2))
          (assoc (bytes->string/utf-8 (list-ref data 1)) transit))
     => cdr]
    [else (values state 'error)]))

(struct rule (match output to-state reset) #:transparent)

(define (make-lexer-table 2d)
  (define cells (drop 2d 3))
  (define cell-table (make-hash))
  (define rule-count
    (for/fold ([h-max 0])
              ([cells cells])
      (for ([cell (first cells)])
        (hash-set! cell-table cell (cdr cells)))
      (apply max h-max (map second (car cells)))))
  (define rule-table (make-hash))
  (for ([i (in-range rule-count)])
    (for ([from-state (hash-ref cell-table (list 0 i))])
      (hash-set! rule-table from-state '())))


  (define (pick j i)
    (define eles (hash-ref cell-table (list j i)))
    (unless (pair? eles)
      (error 'make-lexer-table "expected something in cell (~a,~a) but found nothing"
             i j))
    (car eles))
  (for ([i+1 (in-range rule-count 0 -1)])
    (define i (- i+1 1))
    (for ([from-state (hash-ref cell-table (list 0 i))])
      (hash-set! rule-table from-state
                 (cons
                  (rule (pick 1 i)
                        (pick 2 i)
                        (pick 3 i)
                        (pick 4 i))
                  (hash-ref rule-table from-state)))))
  rule-table)

(define (state-reset state)
  (define reset-state (map rule-reset (hash-ref lexer-fsm state)))
  (first (filter (λ (x) x) reset-state)))

;; FSM transition table of the lexer. The state of the FSM is stored in the
;; mode. The error state is handled specially; it must be able to make a
;; transition for arbitrary input string.
;;
;; The current error recovery strategy tries to re-lex with the previous state
;; after every spaces (and falls back to the 'rule-reset' state of the original state
;; when hitting a new line). Thus, when designing states, it's better not to
;; have a token that spans across spaces.
(define lexer-fsm
  (make-lexer-table
   ;;  state     transition regular expression       output symbol  next state    error recovery
   `#2d
   ╔═══════════╦════════════════════════════════════╦═════════════╦═════════════╦═══════════╗
   ║ ,errlabel ║ #rx"^[^ \t\n]+"                    ║ error       ║ ,errlabel   ║           ║
   ╠═══════════╬════════════════════════════════════╬═════════════╬═════════════╣           ║
   ║ ,errlabel ║ #rx"^[ \t]+"                       ║             ║ ,errresum   ║           ║
   ╠═══════════╬════════════════════════════════════╣ white-space ╠═════════════╣           ║
   ║ ,errlabel ║ #rx"^\n[ \t]*"                     ║             ║ ,errnewln   ║ #f        ║
   ╠═══════════╬════════════════════════════════════╬═════════════╬═════════════╣           ║
   ║ ,errnobrk ║ #rx"^[^\n]+"                       ║ error       ║ ,errlabel   ║           ║
   ╠═══════════╬════════════════════════════════════╬═════════════╬═════════════╣           ║
   ║ ,errnobrk ║ #px"^\n\\s*"                       ║ white-space ║ ,errnewln   ║           ║
   ╠═══════════╬════════════════════════════════════╬═════════════╬═════════════╬═══════════╣
   ║ any-new   ║                                    ║             ║             ║           ║
   ║ axiom-new ║ ,sec-regexp                        ║ comment     ║ ,sec-next   ║ #f        ║
   ║ rules-lhs ║                                    ║             ║             ║           ║
   ║ vars-lhs  ║                                    ║             ║             ║           ║
   ╠═══════════╬════════════════════════════════════╬═════════════╬═════════════╣           ║
   ║ axiom-new ║                                    ║             ║             ║           ║
   ║ axiom-nta ║                                    ║             ║             ║           ║
   ║ axiom-axm ║ #rx"^(\\[|\\])"                    ║ symbol      ║ ,paren-info ║           ║
   ║ rules-rhs ║                                    ║             ║             ║           ║
   ║ rules-ntr ║                                    ║             ║             ║           ║
   ╠═══════════╬════════════════════════════════════╬═════════════╬═════════════╣           ║
   ║ any-new   ║                                    ║             ║ any-new     ║           ║
   ╠═══════════╣                                    ║             ╠═════════════╣           ║
   ║ axiom-new ║                                    ║             ║ axiom-new   ║           ║
   ╠═══════════╣ #px"^\\s+"                         ║ white-space ╠═════════════╣           ║
   ║ rules-lhs ║                                    ║             ║ rules-lhs   ║           ║
   ╠═══════════╣                                    ║             ╠═════════════╣           ║
   ║ vars-lhs  ║                                    ║             ║ vars-lhs    ║           ║
   ╠═══════════╬════════════════════════════════════╬═════════════╬═════════════╬═══════════╣
   ║ any-new   ║ #rx"^#lang[^\n]*(\n|$)"            ║ other       ║ any-new     ║ any-new   ║
   ╠═══════════╬════════════════════════════════════╬═════════════╬═════════════╬═══════════╣
   ║ axiom-new ║ #px"^[^][\\s#(]+"                  ║ symbol      ║ axiom-nta   ║           ║
   ╠═══════════╬════════════════════════════════════╬═════════════╬═════════════╣           ║
   ║ axiom-nta ║ #rx"^[ \t]+"                       ║ white-space ║             ║           ║
   ╠═══════════╬════════════════════════════════════╬═════════════╣ axiom-nta   ║           ║
   ║ axiom-nta ║ #px"^[^][\\s#()]+"                 ║ symbol      ║             ║           ║
   ╠═══════════╬════════════════════════════════════╬═════════════╬═════════════╣           ║
   ║ axiom-nta ║ #px"^\n\\s*"                       ║ white-space ║ axiom-new   ║ axiom-new ║
   ╠═══════════╬════════════════════════════════════╬═════════════╬═════════════╣           ║
   ║ axiom-axm ║ #rx"^[ \t]+"                       ║ white-space ║ axiom-axm   ║           ║
   ╠═══════════╬════════════════════════════════════╬═════════════╬═════════════╣           ║
   ║ axiom-axm ║ #px"^[^][\\s#()]+"                 ║ symbol      ║ axiom-nta   ║           ║
   ╠═══════════╬════════════════════════════════════╬═════════════╬═════════════╣           ║
   ║ axiom-axm ║ #px"^\n\\s*"                       ║ white-space ║ axiom-new   ║           ║
   ╠═══════════╬════════════════════════════════════╬═════════════╬═════════════╬═══════════╣
   ║ rules-lhs ║ #rx"^((?!->|→)[^ \t\n])"           ║ symbol      ║ rules-ntl   ║           ║
   ╠═══════════╬════════════════════════════════════╬═════════════╬═════════════╣           ║
   ║ rules-ntl ║ #rx"^[ \t]+"                       ║ white-space ║ rules-ntl   ║           ║
   ╠═══════════╬════════════════════════════════════╬═════════════╬═════════════╣           ║
   ║ rules-ntl ║ #rx"^(->|→)"                       ║ parenthesis ║ rules-rhs   ║           ║
   ╠═══════════╬════════════════════════════════════╬═════════════╬═════════════╣           ║
   ║ rules-rhs ║ #rx"^[ \t]+"                       ║ white-space ║ rules-rhs   ║ rules-lhs ║
   ╠═══════════╬════════════════════════════════════╬═════════════╬═════════════╣           ║
   ║ rules-rhs ║ #rx"^((?!->|→)[^ \t\n])+"          ║ symbol      ║ rules-rhs   ║           ║
   ╠═══════════╬════════════════════════════════════╬═════════════╬═════════════╣           ║
   ║ rules-rhs ║ #rx"^\n[ \t]"                      ║             ║ rules-rhs   ║           ║
   ╠═══════════╬════════════════════════════════════╣             ╠═════════════╣           ║
   ║ rules-rhs ║ #rx"^\n(?=[^ \t])"                 ║ white-space ║ rules-lhs   ║           ║
   ╠═══════════╬════════════════════════════════════╬═════════════╬═════════════╬═══════════╣
   ║ vars-lhs  ║ #rx"^[^ \t\n=()#]+"                ║ symbol      ║ vars-equ    ║           ║
   ╠═══════════╬════════════════════════════════════╬═════════════╬═════════════╣           ║
   ║ vars-equ  ║ #rx"^[ \t]+"                       ║ white-space ║ vars-equ    ║           ║
   ╠═══════════╬════════════════════════════════════╬═════════════╬═════════════╣           ║
   ║ vars-equ  ║ #rx"^="                            ║ parenthesis ║ vars-rhs    ║           ║
   ╠═══════════╬════════════════════════════════════╬═════════════╬═════════════╣ vars-lhs  ║
   ║ vars-rhs  ║ #rx"^[ \t]+"                       ║ white-space ║             ║           ║
   ╠═══════════╬════════════════════════════════════╬═════════════╣ vars-rhs    ║           ║
   ║ vars-rhs  ║ #rx"^[^ \t\n=#()]+"                ║ constant    ║             ║           ║
   ╠═══════════╬════════════════════════════════════╬═════════════╬═════════════╣           ║
   ║ vars-rhs  ║ #rx"^\n[ \t]*"                     ║ white-space ║ vars-lhs    ║           ║
   ╚═══════════╩════════════════════════════════════╩═════════════╩═════════════╩═══════════╝))

(define (error-can-resume? port state)
  (or (not (member state '(any-new axiom-new rules-lhs vars-lhs)))
      (not (regexp-match-peek #rx"^(->|#)" port))))
(define (lindenmayer-lexer port offset mode)
  (define-values (line col pos) (port-next-location port))
  ;; (or/c bytes syntax) natural mode -> result for the lexer
  (define (make-token-values token type mode paren)
    (define-values (line2 col2 pos2) (port-next-location port))
    (values token type paren pos pos2 0 mode))
  (define state (or mode 'any-new))
  #;(printf "lexer:    ~a ~s\n" state (peek-string 20 0 port))
  (cond
    [(eof-object? (peek-char port))
     (values (read-char port) 'eof #f 0 0 0 state)]
    [(errstate? state)
     (define-values (lexeme type data new-token-start new-token-end backup-delta new-mode)
       (lindenmayer-lexer port offset
                          (if (errstate-data state)
                              errlabel
                              errnobrk)))
     #;(printf "errstate: was: ~a, matched: ~s; new mode: ~a\n" state lexeme new-mode)
     (define old-state (errstate-mode state))
     (define wrapped-mode
       (cond [(equal? new-mode errlabel) state]
             [(equal? new-mode errresum) old-state]
             [(equal? new-mode errnewln) (state-reset old-state)]
             [else (raise-result-error 'lindenmayer-lexer
                                       "(or/c 'errlabel 'errresum 'errnewln)"
                                       new-mode)]))
     (values lexeme type data new-token-start new-token-end backup-delta wrapped-mode)]
    [(for/or ([rule (hash-ref lexer-fsm state)])
       (define match-result
         (regexp-match-peek (rule-match rule) port))
       #;(printf "lexer:                 ~s => ~s / ~s\n"
                 (rule-match rule) match-result (peek-string 20 0 port))
       (and match-result (cons rule match-result)))
     =>
     (match-lambda
       [(list rule matched-str substrs ...)
        (read-bytes (bytes-length matched-str) port)
        (define to-state (rule-to-state rule))
        #;(printf "lexer:                 matched ~s; to-state: ~a\n" matched-str to-state)
        (define-values (new-output new-state new-paren)
          (cond
            [(procedure? to-state)
             (call-with-values
              (λ () (to-state state substrs))
              (case-lambda
                [(new-state) (values (rule-output rule) new-state #f)]
                [(new-state new-output) (values new-output new-state #f)]
                [(new-state new-output new-info) (values new-output new-state new-info)]))]
            [else (values (rule-output rule) to-state #f)]))
        (make-token-values matched-str new-output new-state new-paren)])]
    [else (lindenmayer-lexer port offset (errstate state (error-can-resume? port state)))]))

(module+ test
  (require racket/port)

  (define (test-lexer mode0 input)
    (define port (open-input-string input))
    (define (run* limit mode)
      (define-values (lexeme type data new-token-start new-token-end backup-delta new-mode)
        (lindenmayer-lexer port 0 mode))
      (cond
        [(or (eof-object? lexeme) (equal? limit 1)) (list mode)]
        [else (cons (list mode type (bytes->string/utf-8 lexeme))
                    (run* (if (number? limit) (sub1 limit) #f) new-mode))]))
    (run* #f mode0))
  (check-equal? (test-lexer 'any-new "# axiom #")
                `((any-new   comment "# axiom #") axiom-new))
  (check-equal? (test-lexer 'axiom-new "## variables ##\n")
                `((axiom-new comment "## variables ##\n") vars-lhs))
  (check-equal? (test-lexer 'rules-lhs "# rules #")
                `((rules-lhs comment "# rules #") rules-lhs))
  (check-equal? (test-lexer 'vars-lhs "# axiom #\t")
                `((vars-lhs  comment "# axiom #\t") axiom-new))

  (check-equal?
   (test-lexer 'any-new "#lang lindenmayer racket\n \t \n")
   `((any-new      other            "#lang lindenmayer racket\n")
     (any-new      white-space      " \t \n")
     any-new))

  (check-equal?
   (test-lexer 'axiom-new " F X  \t\n")
   `((axiom-new    white-space      " ")
     (axiom-new    symbol           "F")
     (axiom-nta    white-space      " ")
     (axiom-nta    symbol           "X")
     (axiom-nta    white-space      "  \t")
     (axiom-nta    white-space      "\n")
     axiom-new))

  (check-equal?
   (test-lexer 'rules-lhs " A ->AB\n")
   `((rules-lhs    white-space      " ")
     (rules-lhs    symbol           "A")
     (rules-ntl    white-space      " ")
     (rules-ntl    parenthesis      "->")
     (rules-rhs    symbol           "AB")
     (rules-rhs    white-space      "\n")
     rules-lhs))

  (check-equal?
   (test-lexer 'rules-lhs "X→Y\n ")
   `((rules-lhs    symbol           "X")
     (rules-ntl    parenthesis      "→")
     (rules-rhs    symbol           "Y")
     (rules-rhs    white-space      "\n ")
     rules-rhs))

  (check-equal?
   (test-lexer 'rules-lhs "X→Y\n Z W\n")
   `((rules-lhs    symbol           "X")
     (rules-ntl    parenthesis      "→")
     (rules-rhs    symbol           "Y")
     (rules-rhs    white-space      "\n ")
     (rules-rhs    symbol           "Z")
     (rules-rhs    white-space      " ")
     (rules-rhs    symbol           "W")
     (rules-rhs    white-space      "\n")
     rules-lhs))

  (check-equal?
   (test-lexer 'vars-lhs " \t n = 8\t\n ")
   `((vars-lhs     white-space      " \t ")
     (vars-lhs     symbol           "n")
     (vars-equ     white-space      " ")
     (vars-equ     parenthesis      "=")
     (vars-rhs     white-space      " ")
     (vars-rhs     constant         "8")
     (vars-rhs     white-space      "\t")
     (vars-rhs     white-space      "\n ")
     vars-lhs))

  (check-equal?
   (test-lexer
    #f
    (string-append
     "## axiom ##\n"
     "\n"
     "A\n"
     "\n"
     "## rules ##\n"
     "\n"
     "A -> AB\n"
     "B -> A\n"))
   '((#f        comment     "## axiom ##\n")
     (axiom-new white-space "\n")
     (axiom-new symbol      "A")
     (axiom-nta white-space "\n\n")
     (axiom-new comment     "## rules ##\n")
     (rules-lhs white-space "\n")
     (rules-lhs symbol      "A")
     (rules-ntl white-space " ")
     (rules-ntl parenthesis "->")
     (rules-rhs white-space " ")
     (rules-rhs symbol     "AB")
     (rules-rhs white-space "\n")
     (rules-lhs symbol      "B")
     (rules-ntl white-space " ")
     (rules-ntl parenthesis "->")
     (rules-rhs white-space " ")
     (rules-rhs symbol      "A")
     (rules-rhs white-space "\n")
     rules-lhs))

  (check-equal?
   (test-lexer
    #f
    (string-append
     "## axiom ##\n"
     "\n"
     "A\n"
     "\n"
     "## rules ##\n"
     "\n"
     "A -> AB\n"
     "\n"
     "B -> A\n"))
   '((#f        comment     "## axiom ##\n")
     (axiom-new white-space "\n")
     (axiom-new symbol      "A")
     (axiom-nta white-space "\n\n")
     (axiom-new comment     "## rules ##\n")
     (rules-lhs white-space "\n")
     (rules-lhs symbol      "A")
     (rules-ntl white-space " ")
     (rules-ntl parenthesis "->")
     (rules-rhs white-space " ")
     (rules-rhs symbol     "AB")
     (rules-rhs white-space "\n")
     (rules-lhs white-space "\n")
     (rules-lhs symbol      "B")
     (rules-ntl white-space " ")
     (rules-ntl parenthesis "->")
     (rules-rhs white-space " ")
     (rules-rhs symbol      "A")
     (rules-rhs white-space "\n")
     rules-lhs))

  (check-equal?
   (test-lexer
    #f
    (string-append
     "## axiom ##\n"
     "\n"
     "A\n"
     "\n"
     "## rules ##\n"
     "\n"
     "A 1 -> AB\n"))
   `((#f        comment     "## axiom ##\n")
     (axiom-new white-space "\n")
     (axiom-new symbol      "A")
     (axiom-nta white-space "\n\n")
     (axiom-new comment     "## rules ##\n")
     (rules-lhs white-space "\n")
     (rules-lhs symbol      "A")
     (rules-ntl white-space " ")
     (rules-ntl error "1")
     (,(errstate 'rules-ntl #t) white-space " ")
     (rules-ntl parenthesis "->")
     (rules-rhs white-space " ")
     (rules-rhs symbol     "AB")
     (rules-rhs white-space "\n")
     rules-lhs))

  )
