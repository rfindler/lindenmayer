#lang racket/base
(require parser-tools/yacc
         parser-tools/lex
         (prefix-in : parser-tools/lex-sre))
(provide to-identifier parse-arguments)

(define-tokens value-tokens (NUM VAR))
(define-empty-tokens op-tokens (newline = OP CP + - * / ^ EOF NEG COMMA))
  
(define-lex-abbrevs
  (lower-letter (:/ "a" "z"))
    
  (upper-letter (:/ #\A #\Z))
    
  ;; (:/ 0 9) would not work because the lexer does not understand numbers.  (:/ #\0 #\9) is ok too.
  (digit (:/ "0" "9")))
  
(define expression-lex
  (lexer-src-pos
   [(eof) 'EOF]
   [(:or #\tab #\space) (expression-lex input-port)]
   [#\newline (token-newline)]
   [(:or "+" "-" "*" "/" "^") (string->symbol lexeme)]
   ["(" 'OP]
   [")" 'CP]
   ["," 'COMMA]
   [(:+ (:or lower-letter upper-letter)) (token-VAR (string->symbol lexeme))]
   [(:+ digit) (token-NUM (string->number lexeme))]
   [(:: (:+ digit) #\. (:* digit)) (token-NUM (string->number lexeme))]))

(define the-name (make-parameter #f))
(define expression-parse
  (parser

   (start start)
   (end EOF)
   (tokens value-tokens op-tokens)
   (error (lambda args (void)))

   (precs (left - +)
          (left * /)
          (left NEG)
          (right ^))
   (src-pos)
   (grammar
    (start [() #f]
           ;; If there is an error, ignore everything before the error
           ;; and try to start over right after the error
           [(error start) $2]
           [(OP exp-sequence CP) $2])

    (exp-sequence [(exp) (list $1)]
                  [(exp COMMA exp-sequence) (cons $1 $3)])
    
    (exp [(NUM) $1]
         [(VAR) (to-identifier $1 (the-name)
                               (position-line $1-start-pos)
                               (position-col $1-start-pos)
                               (position-offset $1-start-pos))]
         [(exp + exp) `(+ ,$1 ,$3)]
         [(exp - exp) `(- ,$1 ,$3)]
         [(exp * exp) `(* ,$1 ,$3)]
         [(exp / exp) `(/ ,$1 ,$3)]
         [(- exp) (prec NEG) `(- ,$2)]
         [(exp ^ exp) `(expt ,$1 ,$3)]
         [(OP exp CP) $2]))))

(define (parse-arguments name sp)
  (parameterize ([the-name name])
    (expression-parse (λ () (expression-lex sp)))))
  
(define (to-identifier sym name line col pos)
  (define stx-port (open-input-string (format "~s" sym)))
  (port-count-lines! stx-port)
  (set-port-next-location! stx-port line col pos)
  (read-syntax name stx-port))

(module+ test
  (require rackunit racket/port)
  (define (try str)
    (with-syntax ([s (parse-arguments #f (open-input-string str))])
      (syntax->datum #'s)))
  (check-equal? (try "(x)") (list 'x))
  (check-equal? (try "(x+1)") (list '(+ x 1)))
  (check-equal? (try "(x+1,y^z)") (list '(+ x 1) '(expt y z)))
  (check-equal? (try "(x,y)") (list 'x 'y))
  (check-equal? (try "(-x,y-z-w-p*23)") (list '(- x) '(- (- (- y z) w) (* p 23)))))

