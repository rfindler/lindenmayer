#lang racket/base
(require parser-tools/yacc
         parser-tools/lex
         (prefix-in : parser-tools/lex-sre)
         syntax/readerr)
(provide to-identifier parse-arguments parse-expression-and-arrow)

(define-tokens value-tokens (NUM VAR))
(define-empty-tokens op-tokens (newline = OP CP + - * / ^ < > ≤ ≥ EOF NEG COMMA ARROW))
  
(define-lex-abbrevs
  (lower-letter (:/ "a" "z" "α" "ω"))

  ;; the Α is uppercase α, not uppercase a
  (upper-letter (:/ #\A #\Z #\Α #\Ω))
    
  ;; (:/ 0 9) would not work because the lexer does not understand numbers.  (:/ #\0 #\9) is ok too.
  (digit (:/ "0" "9")))
  
(define expression-lex
  (lexer-src-pos
   [(eof) 'EOF]
   [(:or #\tab #\space) (return-without-pos (expression-lex input-port))]
   [#\newline (token-newline)]
   [(:or "+" "-" "*" "/" "^" "≤" "≥" "<" ">" "=") (string->symbol lexeme)]
   ["(" 'OP]
   [")" 'CP]
   ["," 'COMMA]
   ["->" 'ARROW]
   ["→" 'ARROW]
   [(:+ (:or lower-letter upper-letter)) (token-VAR (string->symbol lexeme))]
   [(:+ digit) (token-NUM (string->number lexeme))]
   [(:: (:+ digit) #\. (:* digit)) (token-NUM (string->number lexeme))]
   [any-char (raise-a-read-error (format "unrecognized character `~a'" lexeme)
                                 start-pos end-pos)]))
                               

(define the-name (make-parameter #f))
(define comma-sequence-expression-parse+expression-and-arrow-parse
  (parser

   (start comma-sequence expression-and-arrow)
   (end EOF)
   (tokens value-tokens op-tokens)
   (error (λ (tok-ok? tok-name tok-value start-pos end-pos)
            (raise-a-read-error (format "unexpected ~a ~a" tok-name tok-value)
                                start-pos end-pos)))

   (precs (left < > ≤ ≥ =)
          (left - +)
          (left * /)
          (left NEG)
          (right ^))
   (src-pos)
   (grammar
    (comma-sequence [(OP exp-sequence CP) $2])

    (expression-and-arrow [(exp ARROW) (add-srcloc $1 $1-start-pos $1-end-pos)])

    (exp-sequence [(exp) (list (add-srcloc $1 $1-start-pos $1-end-pos))]
                  [(exp COMMA exp-sequence)
                   (cons (add-srcloc $1 $1-start-pos $1-end-pos)
                         $3)])
    
    (exp [(NUM) $1]
         [(VAR) (to-identifier $1 (the-name)
                               (position-line $1-start-pos)
                               (position-col $1-start-pos)
                               (position-offset $1-start-pos))]
         [(exp + exp) `(+ ,$1 ,$3)]
         [(exp - exp) `(- ,$1 ,$3)]
         [(exp * exp) `(* ,$1 ,$3)]
         [(exp / exp) `(/ ,$1 ,$3)]
         [(exp < exp) `(< ,$1 ,$3)]
         [(exp > exp) `(> ,$1 ,$3)]
         [(exp ≥ exp) `(>= ,$1 ,$3)]
         [(exp ≤ exp) `(<= ,$1 ,$3)]
         [(exp = exp) `(= ,$1 ,$3)]
         [(- exp) (prec NEG) `(- ,$2)]
         [(exp ^ exp) `(expt ,$1 ,$3)]
         [(OP exp CP) $2]))))

(define (raise-a-read-error str start-pos end-pos)
  (raise-read-error str
                    (the-name)
                    (position-line start-pos)
                    (position-col start-pos)
                    (position-offset start-pos)
                    (- (position-offset end-pos)
                       (position-offset start-pos))))

(define-values (comma-sequence-expression-parse expression-and-arrow-parse)
  (apply values comma-sequence-expression-parse+expression-and-arrow-parse))

(define (add-srcloc sexp start-pos end-pos)
  (datum->syntax
   #f
   sexp
   (vector (the-name)
           (position-line start-pos)
           (position-col start-pos)
           (position-offset start-pos)
           (- (position-offset end-pos)
              (position-offset start-pos)))))

(require racket/port)
(define (parse-something name port something)
  (parameterize ([the-name name])
    (something (λ () (expression-lex port)))))

(define (parse-arguments name port)
  (parse-something name port comma-sequence-expression-parse))
(define (parse-expression-and-arrow name port)
  (parse-something name port expression-and-arrow-parse))
  
(define (to-identifier sym name line col pos)
  (define stx-port (open-input-string (format "~s" sym)))
  (port-count-lines! stx-port)
  (set-port-next-location! stx-port line col pos)
  (fixup-span (read-syntax name stx-port) sym))

(define (fixup-span stx sym)
  (define str (symbol->string sym))
  (cond
    ;; work around what appears to be a bug in read-syntax or maybe ports
    [(and (exact-nonnegative-integer? (syntax-column stx))
          (exact-nonnegative-integer? (syntax-line stx)))
     (datum->syntax
      stx
      (syntax->datum stx)
      (list (syntax-source stx) (syntax-line stx)
            (syntax-column stx) (syntax-position stx)
            (string-length str))
      stx)]
    [else stx]))

(module+ test
  (require rackunit racket/port)
  (define (try-expressions str)
    (with-syntax ([s (parse-arguments #f (open-input-string str))])
      (syntax->datum #'s)))
  (define (try-expression-and-arrow str)
    (with-syntax ([s (parse-expression-and-arrow #f (open-input-string str))])
      (syntax->datum #'s)))

  (check-equal? (try-expressions "(x)") (list 'x))
  (check-equal? (try-expressions "(x+1)") (list '(+ x 1)))
  (check-equal? (try-expressions "( x + 1 )") (list '(+ x 1)))
  (check-equal? (try-expressions "(x+1,y^z)") (list '(+ x 1) '(expt y z)))
  (check-equal? (try-expressions "(x,y)") (list 'x 'y))
  (check-equal? (try-expressions "(-x,y-z-w-p*23)") (list '(- x) '(- (- (- y z) w) (* p 23))))
  (check-equal? (try-expression-and-arrow " x > 10 ->")
                '(> x 10)))
