#lang racket/base
(provide wrap-lexer)
(require racket/match syntax-color/racket-lexer racket/bool)
(module+ test (require rackunit))

(define (wrap-lexer inner-lexer*)
  (define inner-lexer (or inner-lexer* racket-lexer))
  (define inner
    (if (procedure-arity-includes? inner-lexer 3)
        inner-lexer
        (lambda (port offset mode)
          (define-values (text type paren start end) (inner-lexer port))
          (values text type paren start end offset mode))))
  (make-lexer inner))

(define line-start-mode #f)
(define inner-line-mode 'inner-line)
(define below-double-hyphens-mode 'below-double-hyphens)

(define (reading-regexp-match m p)
  (match (regexp-match-peek m p)
    [(list v r ...)
     (define len (bytes-length v))
     (read-bytes len p)
     (cons v r)]
    [#f #f]))


(define o (current-output-port))
(define (make-lexer inner)
  (define  (lex port offset mode)
    (define-values (a b c) (port-next-location port))
    ;; (or/c bytes syntax) natural mode -> result for the lexer
    (define (ret token type mode)
      (define start 0)
      (define end
        (if (bytes? token)
            (bytes-length token)
            (syntax-span token)))
      (values token
              type
              #f
              (+ c start)
              (+ c end)
              offset
              mode))
    (cond
      [(eof-object? (peek-char port))
       (values (read-char port) 'eof #f 0 0 offset mode)]
      [(eq? mode line-start-mode)
       (cond
         [(reading-regexp-match #rx"^#lang.*?\n" port)
          =>
          (match-lambda
            [(list result r ...)
             (ret result 'other line-start-mode)])]
         [(reading-regexp-match #px"^[\\s]+" port)
          =>
          (match-lambda
            [(list result r ...)
             (ret result 'white-space line-start-mode)])]
         [(reading-regexp-match #rx"^=+\n" port) =>
          (match-lambda
            [(list result r ...)
             (ret result 'other below-double-hyphens-mode)])]
         [(reading-regexp-match #rx"^-+\n" port) =>
          (match-lambda
            [(list result r ...)
             (ret result 'other line-start-mode)])]
         [(reading-regexp-match #rx"^ *(#+) +([a-zA-Z][a-zA-Z ]*[a-zA-Z]) +?(#+) *?\n" port) =>
          (match-lambda
            [(list result r ...)
             (ret result 'comment line-start-mode)])]
         [(regexp-match-peek #px"^ *[\\S]* *(\n|[(->|→|=)].*?\n)" port #;"do we have a valid line?")
          (lex port offset inner-line-mode)]
         [(read-bytes-line port) =>
          (lambda (result)
            (ret result 'error line-start-mode))])]
      [(eq? mode inner-line-mode)
       (cond [(equal? (peek-char port) #\newline)
              (ret (read-bytes 1 port) 'white-space line-start-mode)]
             [(with-handlers ([exn:fail:read? (lambda (e) #f)])
                (read-syntax (object-name port) port))
              =>
              (lambda (v)
                (if (memq (syntax-e v) (list '= '-> '→))
                    (ret v 'symbol inner-line-mode)
                    (ret v 'constant inner-line-mode)))]
             [(reading-regexp-match #px"^ +" port)
              =>
              (match-lambda
                [(list result r ...)
                 (ret result 'white-space inner-line-mode)])]
             [(reading-regexp-match #rx"^[^\n\r]*\n" port) =>
              (match-lambda
                [(list result r ...)
                 (ret result 'error line-start-mode)])])]
      ;; we use else here because the inner lexer may have its own modes
      [else (inner port offset mode)]))
  lex)

(module+ test
  (require racket/port)
  (define lex (wrap-lexer #f))
  (define (test-type string mode type mode2 result [token #f])
    (define in (open-input-string string))
    (define-values (outtoken outtype _1 _2 _3 _4 outmode)
      (lex in 0 mode))
    (define rstring (port->string in))
    (or (and (equal? outtype type)
             (equal? outmode mode2)
             (equal? rstring result)
             (implies token (equal? token outtoken)))
        `(expected/got
          (,type ,outtype)
          (,mode2 ,outmode)
          (,result ,rstring)
          (,token ,outtoken))))

  (check-true
   (test-type
    "### axiom ###\nA"
    line-start-mode
    'comment
    line-start-mode
    "A"
    #"### axiom ###\n"))
  (check-true
   (test-type
    "F -> F A\n"
    line-start-mode
    'constant
    inner-line-mode
    " -> F A\n")))
