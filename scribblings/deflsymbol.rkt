#lang racket
(require scribble/struct
         scribble/scheme
         scribble/basic
         scribble/manual-struct
         scribble/manual
         scribble/decode
         scribble/html-properties
         (only-in scribble/core make-style make-table-columns nested-flow)
         
         ;; maybe this needs to be made part of the public api so
         ;; others can make new kinds of blue boxes?
         (only-in scribble/private/manual-vars add-background-label)
         (only-in scribble/private/manual-bind
                  id-to-target-maker with-exporting-libraries
                  definition-site)
         
         (for-syntax racket/base syntax/parse)
         (for-label racket/base
                    racket/contract
                    racket/class))

(provide deflsymbol)

(begin-for-syntax
 (define-splicing-syntax-class kind-kw
   #:description "#:kind keyword"
   (pattern (~optional (~seq #:kind kind)
                       #:defaults ([kind #'#f]))))

 (define-splicing-syntax-class value-kw
   #:description "#:value keyword"
   (pattern (~optional (~seq #:value value)
                       #:defaults ([value #'no-value]))))
 
 (define-splicing-syntax-class link-target?-kw
   #:description "#:link-target? keyword"
   (pattern (~seq #:link-target? expr))
   (pattern (~seq)
            #:with expr #'#t))

 (define-syntax-class id-or-false
   (pattern i:id)
   (pattern #f #:with i #'#f))
   
 (define-splicing-syntax-class id-kw
   #:description "#:id keyword"
   (pattern (~optional (~seq #:id [key:id-or-false expr])
                       #:defaults ([key #'#f]
                                   [expr #'#f]))))
 )

(define-syntax (deflsymbol stx)
  (syntax-parse stx
    [(_ kind:kind-kw 
        lt:link-target?-kw 
        (~optional (~seq #:id id-expr)
                   #:defaults ([id-expr #'#f]))
        (id (args ...) ...)
        ((arg-for-contract contract) ...)
        value:value-kw
        desc ...)
     #'(let-syntax ([id (make-element-id-transformer
                         (λ (stx) #`(racketidfont #,(format "~a" (syntax-e stx)))))]
                    [arg-for-contract (make-variable-id 'arg-for-contract)] ...)
         (*defls kind.kind
                 lt.expr
                 (or id-expr (quote-syntax id))
                 'id
                 (quote-syntax ((args ...) ...))
                 (list (quote-syntax arg-for-contract) ...)
                 (list (racketblock0 contract) ...)
                 (lambda () (list desc ...))))]))

;; copied from scribble/private/manual-vars.rkt
(define boxed-style 
  (make-style 'boxed (list (make-attributes (list (cons 'class "RBoxed"))))))

(define (*defls kind link? stx-id name arg-namess args-for-contracts contracts content-thunk)
  (make-splice
   (cons
    (make-blockquote
     (make-style 'vertical-inset null)
     (list
      (make-table
       boxed-style
       (append
        (for/list ([stx-arg-names (in-list (syntax->list arg-namess))]
                   [i (in-naturals)])
          (define arg-names (syntax->list stx-arg-names))
          (define-values (content ref-content) 
            (if (and (= i 0) link?)
                (definition-site name stx-id #f)
                (let ([s (make-just-context name stx-id)])
                  (values (to-element #:defn? #t s)
                          (to-element s)))))
          
          (define target-maker
            (and (= i 0)
                 link?
                 (id-to-target-maker stx-id #t)))
          (define thing-id
            (if target-maker
                (target-maker
                 content
                 (lambda (tag)
                   (make-toc-target2-element
                    #f
                    (make-index-element
                     #f
                     content
                     tag
                     (list (datum-intern-literal (symbol->string name)))
                     (list ref-content)
                     (with-exporting-libraries
                      (lambda (libs) (make-thing-index-desc name libs))))
                    tag
                    ref-content)))
                content))
          (list
           ((if (zero? i) (add-background-label (or kind "lindenmayer symbol")) values)
            (top-align
             "argcontract"
             (list
              (list
               (list (make-omitable-paragraph
                      (cons
                       thing-id
                       (cond
                         [(null? arg-names)
                          '()]
                         [else
                          (define last-ellipsis? (equal? '...
                                                         (syntax-e (car (reverse arg-names)))))
                          (when last-ellipsis?
                            (set! arg-names (reverse (cdr (reverse arg-names)))))
                          (list (racketparenfont "(")
                                (to-element (car arg-names))
                                (for/list ([arg-name (in-list (cdr arg-names))])
                                  (if (equal? (syntax-e arg-name) '_)
                                      (list (racketparenfont ", _"))
                                      (list (racketparenfont ", ")
                                            (to-element arg-name))))
                                (if last-ellipsis?
                                    (racketparenfont " ...)")
                                    (racketparenfont ")")))]))))))))))

        (list
         (list
          (list
           (tabular
            (for/list ([arg-for-contract (in-list args-for-contracts)]
                       [contract (in-list contracts)])
              (list (hspace 2)
                    (to-flow (to-element (make-var-id arg-for-contract)))
                    (hspace 1)
                    (to-flow ":")
                    (hspace 1)
                    contract))))))))))
    (content-thunk))))

(define (to-flow e) (nested-flow (make-style #f '()) (list (make-omitable-paragraph (list e)))))

(define top-align-styles (make-hash))
(define (top-align style-name cols)
  (list
   (if (null? cols)
       (make-table style-name '())
       (let* ([n (length (car cols))]
              [k (cons style-name n)])
         (make-table
          (hash-ref top-align-styles
                    k
                    (lambda ()
                      (define s
                        (make-style style-name
                                    (list (make-table-columns (for/list ([i n])
                                                                (make-style #f '(top)))))))
                      (hash-set! top-align-styles k s)
                      s))
          cols)))))