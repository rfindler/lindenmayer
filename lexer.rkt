#lang 2d racket/base
(provide wrap-lexer)
(require racket/match syntax-color/racket-lexer racket/bool racket/list)
(module+ test (require rackunit))

(define (wrap-lexer inner-lexer*)
  (define inner-lexer (or inner-lexer* racket-lexer))
  (define inner
    (if (procedure-arity-includes? inner-lexer 3)
        inner-lexer
        (lambda (port offset mode)
          (define-values (text type paren start end) (inner-lexer port))
          (values text type paren start end 0 mode))))
  (make-lexer inner))

(define (post-hyph0 mode)
  (post-hyph #f))

(struct post-hyph (mode) #:transparent)
(struct errstate (mode) #:transparent)
(struct in-param (mode) #:transparent)

(define errlabel 'errlabel)
(define errresum 'errresum)
(define errnewln 'errnewln)
(define parlabel 'parlabel)
(define paramend 'paramend)

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
  (for ([i (in-range (- rule-count 1) -1 -1)])
    (for ([from-state (hash-ref cell-table (list 0 i))])
      (hash-set! rule-table from-state
                 (cons
                  (rule (first (hash-ref cell-table (list 1 i)))
                        (first (hash-ref cell-table (list 2 i)))
                        (first (hash-ref cell-table (list 3 i)))
                        (first (hash-ref cell-table (list 4 i))))
                  (hash-ref rule-table from-state)))))
  rule-table)

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
   ║ ,errlabel ║ #rx"^[^\n \t]+"                    ║ error       ║ ,errlabel   ║           ║
   ╠═══════════╬════════════════════════════════════╬═════════════╬═════════════╣           ║
   ║ ,errlabel ║ #rx"^[ \t]+"                       ║             ║ ,errresum   ║ #f        ║
   ╠═══════════╬════════════════════════════════════╣ white-space ╠═════════════╣           ║
   ║ ,errlabel ║ #px"^\n\\s*"                       ║             ║ ,errnewln   ║           ║
   ╠═══════════╬════════════════════════════════════╬═════════════╬═════════════╬═══════════╣
   ║ ,parlabel ║ #px"^\\s+"                         ║ white-space ║ ,parlabel   ║           ║
   ╠═══════════╬════════════════════════════════════╬═════════════╬═════════════╣           ║
   ║ ,parlabel ║ #rx"^,"                            ║             ║ ,parlabel   ║           ║
   ╠═══════════╬════════════════════════════════════╣ parenthesis ╠═════════════╣           ║
   ║ ,parlabel ║ #rx"^\\)"                          ║             ║ ,paramend   ║ ,parlabel ║
   ╠═══════════╬════════════════════════════════════╬═════════════╬═════════════╣           ║
   ║ ,parlabel ║ #px"^\\d+"                         ║ constant    ║ ,parlabel   ║           ║
   ╠═══════════╬════════════════════════════════════╬═════════════╬═════════════╣           ║
   ║ ,parlabel ║ #px"^[^,)\\d \t\n]+"               ║ symbol      ║ ,parlabel   ║           ║
   ╠═══════════╬════════════════════════════════════╬═════════════╬═════════════╬═══════════╣
   ║ any-new   ║                                    ║             ║             ║           ║
   ║ axiom-new ║ #rx"^[ \t]*===+[ \t]*(\n|$)"       ║             ║ ,post-hyph0 ║           ║
   ║ rules-lhs ║                                    ║             ║             ║           ║
   ║ vars-lhs  ║                                    ║             ║             ║           ║
   ╠═══════════╬════════════════════════════════════╣             ╠═════════════╣           ║
   ║ any-new   ║                                    ║             ║             ║           ║
   ║ axiom-new ║ #rx"^[ \t]*---+[ \t]*(\n|$)"       ║ comment     ║ any-new     ║           ║
   ║ rules-lhs ║                                    ║             ║             ║           ║
   ║ vars-lhs  ║                                    ║             ║             ║           ║
   ╠═══════════╬════════════════════════════════════╣             ╠═════════════╣           ║
   ║ axiom-new ║                                    ║             ║             ║           ║
   ║ rules-lhs ║ #rx"^#+[ \t]*"                     ║             ║ start       ║ #f        ║
   ║ vars-lhs  ║                                    ║             ║             ║           ║
   ╠═══════════╬════════════════════════════════════╬═════════════╬═════════════╣           ║
   ║ axiom-axm ║                                    ║             ║             ║           ║
   ║ rules-arr ║ #rx"^\\("                          ║ parenthesis ║ ,in-param   ║           ║
   ║ rules-rhs ║                                    ║             ║             ║           ║
   ║ vars-equ  ║                                    ║             ║             ║           ║
   ╠═══════════╬════════════════════════════════════╬═════════════╬═════════════╣           ║
   ║ axiom-new ║                                    ║             ║ axiom-new   ║           ║
   ╠═══════════╣                                    ║             ╠═════════════╣           ║
   ║ rules-lhs ║ #px"^\\s+"                         ║ white-space ║ rules-lhs   ║           ║
   ╠═══════════╣                                    ║             ╠═════════════╣           ║
   ║ vars-lhs  ║                                    ║             ║ vars-lhs    ║           ║
   ╠═══════════╬════════════════════════════════════╬═════════════╬═════════════╬═══════════╣
   ║ any-new   ║ #rx"^(?!#lang)#+[ \t]*"            ║ comment     ║ start       ║           ║
   ╠═══════════╬════════════════════════════════════╬═════════════╬═════════════╣           ║
   ║ any-new   ║ #rx"^#lang[^\n]*(\n|$)"            ║ other       ║ any-new     ║ any-new   ║
   ╠═══════════╬════════════════════════════════════╬═════════════╬═════════════╣           ║
   ║ any-new   ║ #px"^\\s+"                         ║ white-space ║ any-new     ║           ║
   ╠═══════════╬════════════════════════════════════╬═════════════╬═════════════╬═══════════╣
   ║ start     ║ #rx"^axiom[ \t]*"                  ║             ║ start-axm   ║           ║
   ╠═══════════╬════════════════════════════════════╣             ╠═════════════╣           ║
   ║ start     ║ #rx"^rules[ \t]*"                  ║             ║ start-rul   ║ any-new   ║
   ╠═══════════╬════════════════════════════════════╣             ╠═════════════╣           ║
   ║ start     ║ #rx"^variables[ \t]*"              ║             ║ start-var   ║           ║
   ╠═══════════╬════════════════════════════════════╣ comment     ╠═════════════╬═══════════╣
   ║ start-axm ║ #rx"^#+[ \t]*(\n|$)"               ║             ║ axiom-new   ║ axiom-new ║
   ╠═══════════╬════════════════════════════════════╣             ╠═════════════╬═══════════╣
   ║ start-rul ║ #rx"^#+[ \t]*(\n|$)"               ║             ║ rules-lhs   ║ rules-lhs ║
   ╠═══════════╬════════════════════════════════════╣             ╠═════════════╬═══════════╣
   ║ start-var ║ #rx"^#+[ \t]*(\n|$)"               ║             ║ vars-lhs    ║ vars-lhs  ║
   ╠═══════════╬════════════════════════════════════╬═════════════╬═════════════╬═══════════╣
   ║ axiom-new ║ #px"^[^\\s#(]+"                    ║ symbol      ║ axiom-axm   ║           ║
   ╠═══════════╬════════════════════════════════════╬═════════════╬═════════════╣           ║
   ║ axiom-axm ║ #rx"^[ \t]+"                       ║ white-space ║             ║           ║
   ╠═══════════╬════════════════════════════════════╬═════════════╣ axiom-axm   ║ axiom-new ║
   ║ axiom-axm ║ #px"^[^\\s#(]+"                    ║ symbol      ║             ║           ║
   ╠═══════════╬════════════════════════════════════╬═════════════╬═════════════╣           ║
   ║ axiom-axm ║ #px"^\n\\s*"                       ║ white-space ║ axiom-new   ║           ║
   ╠═══════════╬════════════════════════════════════╬═════════════╬═════════════╬═══════════╣
   ║ rules-lhs ║ #rx"^((?!->|→)[^ \t\n(])+"         ║ symbol      ║ rules-arr   ║           ║
   ╠═══════════╬════════════════════════════════════╬═════════════╬═════════════╣           ║
   ║ rules-arr ║ #rx"^[ \t]+"                       ║ white-space ║ rules-arr   ║           ║
   ╠═══════════╬════════════════════════════════════╬═════════════╬═════════════╣           ║
   ║ rules-arr ║ #rx"^(->|→)"                       ║ parenthesis ║ rules-rhs   ║           ║
   ╠═══════════╬════════════════════════════════════╬═════════════╬═════════════╣ rules-lhs ║
   ║ rules-rhs ║ #rx"^[ \t]+"                       ║ white-space ║             ║           ║
   ╠═══════════╬════════════════════════════════════╬═════════════╣ rules-rhs   ║           ║
   ║ rules-rhs ║ #rx"^((?!->|→)[^ \t\n(#])+"        ║ symbol      ║             ║           ║
   ╠═══════════╬════════════════════════════════════╬═════════════╬═════════════╣           ║
   ║ rules-rhs ║ #px"^\n\\s*"                       ║ white-space ║ rules-lhs   ║           ║
   ╠═══════════╬════════════════════════════════════╬═════════════╬═════════════╬═══════════╣
   ║ vars-lhs  ║ #rx"^[^ \t\n=(]+"                  ║ symbol      ║ vars-equ    ║           ║
   ╠═══════════╬════════════════════════════════════╬═════════════╬═════════════╣           ║
   ║ vars-equ  ║ #rx"^[ \t]+"                       ║ white-space ║ vars-equ    ║           ║
   ╠═══════════╬════════════════════════════════════╬═════════════╬═════════════╣           ║
   ║ vars-equ  ║ #rx"^="                            ║ parenthesis ║ vars-rhs    ║           ║
   ╠═══════════╬════════════════════════════════════╬═════════════╬═════════════╣ vars-lhs  ║
   ║ vars-rhs  ║ #rx"^[ \t]+"                       ║ white-space ║             ║           ║
   ╠═══════════╬════════════════════════════════════╬═════════════╣ vars-rhs    ║           ║
   ║ vars-rhs  ║ #rx"^[^ \t\n=#]+"                  ║ constant    ║             ║           ║
   ╠═══════════╬════════════════════════════════════╬═════════════╬═════════════╣           ║
   ║ vars-rhs  ║ #px"^\n\\s*"                       ║ white-space ║ vars-lhs    ║           ║
   ╚═══════════╩════════════════════════════════════╩═════════════╩═════════════╩═══════════╝))

(define (make-lexer inner)
  (define  (lex port offset mode)
    (define-values (line col pos) (port-next-location port))
    ;; (or/c bytes syntax) natural mode -> result for the lexer
    (define (make-token-values token type mode)
      (define-values (line2 col2 pos2) (port-next-location port))
      (values token
              type
              #f
              pos
              pos2
              0
              mode))
    (define state (or mode 'any-new))
    #;(printf "lexer:    ~a ~s\n" state (peek-string 20 0 port))
    (cond
      [(eof-object? (peek-char port))
       (values (read-char port) 'eof #f 0 0 0 state)]
      [(post-hyph? state)
       (call-with-values
        (λ () (inner port offset (post-hyph-mode state)))
        (λ (lexeme type data new-token-start new-token-end backup-delta new-mode)
          (values lexeme type data new-token-start new-token-end backup-delta (post-hyph new-mode))))]
      [(in-param? state)
       (call-with-values
        (λ () (lex port offset parlabel))
        (λ (lexeme type data new-token-start new-token-end backup-delta new-mode)
          #;(printf "parlabel: was: ~a, matched: ~s; new mode: ~a\n" state lexeme new-mode)
          (define wrapped-mode
            (cond [(equal? new-mode parlabel) state]
                  [(equal? new-mode paramend) (in-param-mode state)]
                  [else (raise-result-error 'lindenmayer-lexer "(or/c 'parlabel 'paramend)" new-mode)]))
          (values lexeme type data new-token-start new-token-end backup-delta wrapped-mode)))]
      [(errstate? state)
       (call-with-values
        (λ () (lex port offset errlabel))
        (λ (lexeme type data new-token-start new-token-end backup-delta new-mode)
          #;(printf "errstate: was: ~a, matched: ~s; new mode: ~a\n" state lexeme new-mode)
          (define old-state (errstate-mode state))
          (define wrapped-mode
            (cond [(equal? new-mode errlabel) state]
                  [(equal? new-mode errresum) old-state]
                  [(equal? new-mode errnewln)
                   (define reset-state (map rule-reset (hash-ref lexer-fsm old-state)))
                   (first (filter (λ (x) x) reset-state))]
                  [else (raise-result-error 'lindenmayer-lexer "(or/c 'errlabel 'errresum 'errnewln)" new-mode)]))
          (values lexeme type data new-token-start new-token-end backup-delta wrapped-mode)))]
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
          (define new-state
            (cond
              [(procedure? to-state)
               (to-state state)]
              [else to-state]))
          (make-token-values matched-str (rule-output rule) new-state)])]
      [else (lex port offset (errstate state))]))
  lex)

(module+ test
  (require racket/port)
  (define lex (wrap-lexer #f))

  (define (test-lexer mode0 input)
    (define port (open-input-string input))
    (define (run* limit mode)
      (define-values (lexeme type data new-token-start new-token-end backup-delta new-mode)
        (lex port 0 mode))
      (cond
        [(or (eof-object? lexeme) (equal? limit 1)) (list mode)]
        [else (cons (list mode type (bytes->string/utf-8 lexeme))
                    (run* (if (number? limit) (sub1 limit) #f) new-mode))]))
    (run* #f mode0))
  (check-equal? (test-lexer 'any-new "#")     `((any-new   comment "#") start))
  (check-equal? (test-lexer 'axiom-new "## ") `((axiom-new comment "## ") start))
  (check-equal? (test-lexer 'rules-lhs "# ")  `((rules-lhs comment "# ") start))
  (check-equal? (test-lexer 'vars-lhs "#\t")  `((vars-lhs  comment "#\t") start))

  (check-equal? (test-lexer 'any-new "---\n")     `((any-new   comment "---\n") any-new))
  (check-equal? (test-lexer 'axiom-new "----\n")  `((axiom-new comment "----\n") any-new))
  (check-equal? (test-lexer 'rules-lhs " ---\n")  `((rules-lhs comment " ---\n") any-new))
  (check-equal? (test-lexer 'vars-lhs "--- \t\n") `((vars-lhs  comment "--- \t\n") any-new))

  (check-equal? (test-lexer 'any-new "===\n")     `((any-new   comment "===\n") ,(post-hyph #f)))
  (check-equal? (test-lexer 'axiom-new "====\n")  `((axiom-new comment "====\n") ,(post-hyph #f)))
  (check-equal? (test-lexer 'rules-lhs " ===\n")  `((rules-lhs comment " ===\n") ,(post-hyph #f)))
  (check-equal? (test-lexer 'vars-lhs "=== \t\n") `((vars-lhs  comment "=== \t\n") ,(post-hyph #f)))

  (check-equal?
   (test-lexer 'any-new "#lang lindenmayer racket\n \t \n")
   `((any-new      other            "#lang lindenmayer racket\n")
     (any-new      white-space      " \t \n")
     any-new))

  (check-equal?
   (test-lexer 'axiom-new " F X  \t\n")
   `((axiom-new    white-space      " ")
     (axiom-new    symbol           "F")
     (axiom-axm    white-space      " ")
     (axiom-axm    symbol           "X")
     (axiom-axm    white-space      "  \t")
     (axiom-axm    white-space      "\n")
     axiom-new))

  (check-equal?
   (test-lexer 'rules-lhs " A ->AB\n")
   `((rules-lhs    white-space      " ")
     (rules-lhs    symbol           "A")
     (rules-arr    white-space      " ")
     (rules-arr    parenthesis      "->")
     (rules-rhs    symbol           "AB")
     (rules-rhs    white-space      "\n")
     rules-lhs))

  (check-equal?
   (test-lexer 'rules-lhs "X→Y\n ")
   `((rules-lhs    symbol           "X")
     (rules-arr    parenthesis      "→")
     (rules-rhs    symbol           "Y")
     (rules-rhs    white-space      "\n ")
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
   (test-lexer 'any-new "# axiom\nA\n")
   `((any-new                comment          "# ")
     (start                  comment          "axiom")
     (start-axm              white-space      "\n")
     (axiom-new              symbol           "A")
     (axiom-axm              white-space      "\n")
     axiom-new))

  (check-equal?
   (test-lexer (errstate 'start-axm) ". !\nA")
   `((,(errstate 'start-axm) error            ".")
     (,(errstate 'start-axm) white-space      " ")
     (start-axm              error            "!")
     (,(errstate 'start-axm) white-space      "\n")
     (axiom-new              symbol           "A")
     axiom-axm))

  (for ([state (in-list '(axiom-new rules-lhs rules-rhs vars-lhs))])
    (define next-state
      (match state
        ['axiom-new 'axiom-axm]
        ['rules-lhs 'rules-arr]
        ['rules-rhs 'rules-rhs]
        ['vars-lhs  'vars-equ]
        ['vars-rhs  'vars-rhs]))
    (check-equal?
     (test-lexer state "R(3+x *5,)")
     `((,state                 symbol           "R")
       (,next-state            parenthesis      "(")
       (,(in-param next-state) constant         "3")
       (,(in-param next-state) symbol           "+x")
       (,(in-param next-state) white-space      " ")
       (,(in-param next-state) symbol           "*")
       (,(in-param next-state) constant         "5")
       (,(in-param next-state) parenthesis      ",")
       (,(in-param next-state) parenthesis      ")")
       ,next-state))))
