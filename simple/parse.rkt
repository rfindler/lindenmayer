#lang racket/base

(require racket/list
         syntax/readerr
         racket/match
         "structs.rkt")

(provide parse-module)

(define mandatory-sections '(axiom rules))
(define valid-sections (append mandatory-sections '(variables)))
(define (valid-section? s) (member s valid-sections))
(define section-names-str
  (apply string-append (add-between (map (λ (x) (format "~a" x)) valid-sections) " ")))

(define (parse-module port name)
  (define non-terminals (make-hash))
  (define sections (parse-sections port name))

  (define (add-sym-nt sym) (hash-set! non-terminals (syntax-e (sym-id sym)) #t))
  (for ([a-rule (in-list (hash-ref sections 'rules))])
    (add-sym-nt (rule-nt a-rule))
    (for ([sym (in-list (rule-rhs a-rule))])
      (add-sym-nt sym)))
  (for ([sym (in-list (hash-ref sections 'axiom))])
    (add-sym-nt sym))

  (define nts 
    (sort (hash-map non-terminals (λ (x y) x))
          symbol<?))

  (define iterations-pr (assoc 'n (hash-ref sections 'variables '())))
  (define iterations
    (if iterations-pr
        (list-ref iterations-pr 1)
        4))
  
  (datum->syntax
   #f
   `(module name racket/base
      (require lindenmayer/simple/compile)
      (define (finish val) (newline))
      ,@(for/list ([nt (in-list nts)])
          `(define (,nt value) (display ',nt)))
      (lindenmayer-system
       (void) finish ,iterations
       ,(for/list ([c (in-list (hash-ref sections 'axiom))])
          (sym-id c))
       ,@(for/list ([pr (in-list (hash-ref sections 'rules))])
           `(,(sym-id (rule-nt pr))
             ->
             ,@(for/list ([c (in-list (rule-rhs pr))])
                 (sym-id c))))))))

(define (parse-sections port name)
  (define-values (start-line start-col start-pos) (port-next-location port))
  (define sections (make-hash))
  (let loop ([current-section #f] [pending-rule #f])
    (define (update-section nv) (hash-set! sections current-section nv))
    (define (section-value default) (hash-ref sections current-section default))
    (define-values (line col pos) (port-next-location port))
    (define l (read-line port))
    (define-values (after-line after-col after-pos) (port-next-location port))
    (define (failed msg) (raise-read-error msg name line col pos (- after-pos pos)))
    (define (handle-pending-rule)
      (when pending-rule
        (update-section (cons pending-rule (section-value '())))))
    (cond
      [(eof-object? l)
       (handle-pending-rule)
       (for ([sec (in-list mandatory-sections)])
         (unless (hash-ref sections sec #f)
           (raise-read-error
            (format "did not find the `## ~a ##' section" sec)
            name start-line start-col start-pos (- after-pos start-pos))))
       (hash-set! sections 'rules (reverse (hash-ref sections 'rules)))
       sections]
      [(blank-line? l)
       (handle-pending-rule)
       (loop current-section #f)]
      [(regexp-match #rx"#" l)
       (handle-pending-rule)
       (define m (regexp-match #rx"^ *(#+) +([a-zA-Z][a-zA-Z ]*[a-zA-Z]) +(#*) *$" l))
       (unless m (failed "found a `#' but could not parse a section line"))
       (define first-hashes (list-ref m 1))
       (define section-name (string->symbol (list-ref m 2)))
       (define last-hashes (list-ref m 3))
       (unless (equal? first-hashes last-hashes)
         (failed "expected equal numbers of `#' at the start and end of the line"))
       (unless (valid-section? section-name)
         (failed (format "unknown section name ~a; expected one of ~a"
                         section-name
                         section-names-str)))
       (loop section-name #f)]
      [(equal? current-section 'axiom)
       (handle-pending-rule)
       (when (section-value #f)
         (failed "found a second axiom"))
       (update-section (parse-axiom l name line col pos))
       (loop current-section #f)]
      [(equal? current-section 'rules)
       (define arr1? (regexp-match? #rx"->" l))
       (define arr2? (regexp-match #rx"→" l))
       (define pending? (and pending-rule (regexp-match? #px"^\\s+" l)))
       (unless (or arr1? arr2? pending?)
         (failed
          (string-append
           "expected either `->' or `→' to start a new rule or"
           "leading spaces to indicate the continuation of a previous rule")))
       (cond
         [(or arr1? arr2?) ;; this is a new rule
          (handle-pending-rule)
          (define split (if arr1?
                            (regexp-split #rx"->" (remove-whitespace l))
                            (regexp-split #rx"→" (remove-whitespace l))))
          (unless (= 2 (length split))
            (failed (format "expected only one `~a'" (if arr1? "->" "→"))))
          (define the-rule (parse-rule l name line col pos failed))
          (loop current-section the-rule)]
         [else ;; continuation of a pending rule
          (match-define (rule rule-nt rule-current-body) pending-rule)
          (define rule-continued-body (parse-axiom l name line col pos))
          (loop current-section (rule rule-nt
                                      (append rule-current-body rule-continued-body)))])]
      [(equal? current-section 'variables)
       (handle-pending-rule)
       (unless (regexp-match? #rx"=" l)
         (failed "expected `=' in the variable assignment"))
       (define split (regexp-split #rx"=" (remove-whitespace l)))
       (unless (= 2 (length split)) (failed "expected only one `='"))
       (define key (string->symbol (list-ref split 0)))
       (define val (read (open-input-string (list-ref split 1)))) ;; TODO: better error checking
       (update-section (cons (list key val) (section-value '())))
       (loop current-section #f)]
      [else (failed (format "internal error.1 ~s ~s"
                            current-section
                            l))])))

(define (parse-axiom str name line col pos)
  (define the-port (open-input-string str))
  (port-count-lines! the-port)
  (set-port-next-location! the-port line col pos)
  (define-values (axiom last-axiom-char) (process-sequence-of-symbols the-port name))
  axiom)

(define (parse-rule str name line col pos failed)
  (define arr1? (regexp-match? #rx"->" str))
  (define (end? c p)
    (if arr1?
        (and (equal? c #\-)
             (equal? (peek-char p) #\>)
             (read-char p) ;; consume the `>` character
             #t)
        (equal? c #\→)))
  (define the-port (open-input-string str))
  (port-count-lines! the-port)
  (set-port-next-location! the-port line col pos)
  (define-values (left last-left-char) (process-sequence-of-symbols the-port name end?))
  (define-values (right last-right-char) (process-sequence-of-symbols the-port name))
  (when (empty? left)
    (failed "expected the name of a non-terminal"))
  (when (> (length left) 1)
    (failed "expected exactly 1 non-terminal name"))
  (rule (first left) right))

(define (process-sequence-of-symbols sp name [end? (λ (c p) (eof-object? c))])
  (let loop ([stxs '()])
    (define-values (line col pos) (port-next-location sp))
    (define c (read-char sp))
    (cond
      [(end? c sp) (values (reverse stxs) c)]
      [(char-whitespace? c)
       ;; skip whitespace, can assume no newlines
       (loop stxs)]
      [else
       (define id (to-identifier (string->symbol (string c)) name line col pos))
       (define non-terminal (sym id '()))
       (loop (cons non-terminal stxs))])))

(define (remove-whitespace l) (regexp-replace* #rx"[\u00A0 \t]" l ""))
(define (blank-line? l) (regexp-match? #rx"^[\u00A0 \t]*$" l))

(define (to-identifier sym name line col pos)
  (define stx-port (open-input-string (format "~s" sym)))
  (port-count-lines! stx-port)
  (set-port-next-location! stx-port line col pos)
  (read-syntax name stx-port))

(module+ test
  (require rackunit)
  (define (try parse-something str)
    (define port (open-input-string str))
    (port-count-lines! port)
    (let loop ([obj (parse-something port #f)])
      (cond
        [(syntax? obj) (loop (syntax-e obj))]
        [(pair? obj) (cons (loop (car obj)) (loop (cdr obj)))]
        [(hash? obj) (for/hash ([(k v) (in-hash obj)]) (values (loop k) (loop v)))]
        [(struct? obj) (loop (struct->vector obj))]
        [(vector? obj) (for/vector ([e (in-vector obj)]) (loop e))]
        [else obj])))

  (check-equal? (try parse-module "# axiom #\nA\n# rules #\nA->AA")
                '(module name racket/base
                   (require lindenmayer/simple/compile)
                   (define (finish val) (newline))
                   (define (A value) (display 'A))
                   (lindenmayer-system (void) finish 4 (A) (A -> A A))))
  
  (check-equal? (try parse-sections "# axiom #\n\n\nA\n\n# rules #\nA->AA\nB->BA")
                (hash 'rules '(#(struct:rule #(struct:sym A ())
                                             (#(struct:sym A ()) #(struct:sym A ())))
                               #(struct:rule #(struct:sym B ())
                                             (#(struct:sym B ()) #(struct:sym A ()))))
                      'axiom '(#(struct:sym A ()))))
  
  (check-equal? (try parse-sections
                     (string-append
                      "# axiom #\n"
                      "A\n"
                      "\n"
                      "### variables ###\n"
                      "n=20\n"
                      "w=54\n"
                      "## rules ##\n"
                      "A → A A\n"
                      "B → B A\n"))
                (hash 'rules
                      '(#(struct:rule #(struct:sym A ()) (#(struct:sym A ()) #(struct:sym A ())))
                        #(struct:rule #(struct:sym B ()) (#(struct:sym B ()) #(struct:sym A ()))))
                      'variables '((w 54) (n 20))
                      'axiom '(#(struct:sym A ()))))

  (check-equal? (try parse-sections
                     (string-append
                      "## axiom ##\n"
                      "A\n"
                      "\n"
                      "## rules ##\n"
                      "A -> A\n"
                      "          B\n"
                      "B ->\n"
                      " A\n"
                      "\n"
                      "## variables ##\n"
                      "n=4"))
                (hash 'rules
                      '(#(struct:rule #(struct:sym A ()) (#(struct:sym A ()) #(struct:sym B ())))
                        #(struct:rule #(struct:sym B ()) (#(struct:sym A ()))))
                      'variables '((n 4))
                      'axiom '(#(struct:sym A ()))))
                
  (check-not-exn
   (λ ()
     (with-handlers ([exn:fail?
                      (λ (x)
                        (for ([x (in-list (continuation-mark-set->context
                                           (exn-continuation-marks x)))])
                          (printf "  ~s\n" x))
                        (raise x))])
       (parameterize ([read-accept-reader #t])
         (read (open-input-string "#lang lindenmayer\n# axiom #\nA\n# rules #\nA -> A\n"))))))

  (check-not-exn
   (λ ()
     (with-handlers ([exn:fail?
                      (λ (x)
                        (for ([x (in-list (continuation-mark-set->context
                                           (exn-continuation-marks x)))])
                          (printf "  ~s\n" x))
                        (raise x))])
       (parameterize ([read-accept-reader #t])
         (read (open-input-string "#lang lindenmayer\n# axiom #\nαλ\n# rules #\nα -> α\n"))))))

  (check-not-exn
   (λ ()
     (parameterize ([read-accept-reader #t])
       (read-syntax
        #f
        (open-input-string
         (string-append
          "#lang lindenmayer racket\n"
          "# axiom #\nA\n# rules #\nA -> A\n"
          "=========\n(+ 1 2)"))))))

  (define ns (make-base-namespace))

  (check-exn
   (λ (x) (and (exn:fail:syntax? x)
               (regexp-match #rx"expected only one rule for.* for A" (exn-message x))))
   (λ ()
     (parameterize ([read-accept-reader #t]
                    [current-namespace ns])
       (expand
        (read-syntax
         #f
         (open-input-string
          (string-append
           "#lang lindenmayer\n"
           "# axiom #\nA\n# rules #\nA -> A\nA -> A")))))))

  (check-not-exn
   (λ ()
     (parameterize ([read-accept-reader #t]
                    [current-namespace ns])
       (expand
        (read-syntax
         #f
         (open-input-string
          (string-append
           "#lang lindenmayer\n"
           "# axiom #\n"
           "A\n"
           "# rules #\n"
           "B -> B\n"))))))))
