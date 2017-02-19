#lang racket/base
(module reader racket/base
  (require (only-in syntax/module-reader
                    make-meta-reader
                    lang-reader-module-paths)
           racket/list
           syntax/readerr)

  (define (wrap-read _read) _read)
  (define (wrap-read-syntax _read-syntax)
    (define double-hyphen (bytes-ref #"=" 0))
    (define newline (bytes-ref #"\n" 0))
    (λ (name in src line col pos)
      (define-values (_in _out) (make-pipe))
      (define front-parse-chan (make-channel))
      (thread (λ ()
                (set-port-next-location! _out line col pos)
                (channel-put front-parse-chan (parse-front _in src))))
      (define (fetch-front-result)
        (close-output-port _out)
        (define val (channel-get front-parse-chan))
        (cond
          [(exn? val) (raise val)]
          [else val]))
      (define (read-and-put-byte) (write-byte (read-byte in) _out))
      (let loop ([n 0]
                 [line line]
                 [col col]
                 [pos pos])
        (define c (peek-byte in))
        (cond
          [(eof-object? c)
           ;; we expect this to be an error
           (fetch-front-result)]
          [(equal? c double-hyphen)
           (read-and-put-byte)
           (loop (+ n 1) line (+ col 1) (+ pos 1))]
          [(n . > . 3)
           (define front-result (fetch-front-result))
           ;; found the end of our double hyphens
           (define module-after-hyphens
             (cond
               [(procedure-arity-includes? _read-syntax 6)
                (_read-syntax name in src line col pos)]
               [else
                (_read-syntax name in)]))
           (cond
             [(syntax? module-after-hyphens)
              (syntax-case module-after-hyphens ()
                [(mod name . whatever)
                 (equal? 'module (syntax-e #'mod))
                 `(,#'module ,#'name racket/base
                             ,#'(mod post-hyphens . whatever)
                             (require (prefix-in ::: (submod "." post-hyphens))
                                      lindenmayer/lang)
                             ,front-result)]
                [_ module-after-hyphens])]
             [else module-after-hyphens])]
          [(equal? c newline)
           ;; found a non-hyphen and not at the end of a sequence of hyphens
           (read-and-put-byte)
           (loop 0 (+ line 1) 0 (+ pos 1))]
          [else
           (read-and-put-byte)
           (loop 0 line (+ col 1) (+ pos 1))]))))
  (define (wrap-get-info _get-info) _get-info)

  (define mandatory-sections '(axiom rules))
  (define valid-sections (append mandatory-sections '(variables)))
  (define (valid-section? s) (member s valid-sections))
  (define section-names-str
    (apply string-append (add-between (map (λ (x) (format "~a" x)) valid-sections) " ")))
  
  (define (parse-front port src)
    (define-values (start-line start-col start-pos) (port-next-location port))
    (define sections (make-hash))
    (with-handlers ([exn:fail:read? (λ (x) x)])
      (let loop ([current-section #f])
        (define (update-section nv) (hash-set! sections current-section nv))
        (define (section-value default) (hash-ref sections current-section default))
        (define-values (line col pos) (port-next-location port))
        (define l (read-line port))
        (define-values (after-line after-col after-pos) (port-next-location port))
        (define (failed msg) (raise-read-error msg src line col pos (- after-pos pos)))
        (cond
          [(eof-object? l)
           (for ([sec (in-list mandatory-sections)])
             (unless (hash-ref sections sec #f)
               (raise-read-error
                (format "did not find the `## ~a ##' section" sec)
                src start-line start-col start-pos (- after-pos start-pos))))
           sections]
          [(regexp-match? #rx"^ *===+ *$" l)
           (loop #f)]
          [(blank-line? l) (loop current-section)]
          [(regexp-match #rx"#" l)
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
           (loop section-name)]
          [(equal? current-section 'axiom)
           (when (section-value #f)
             (failed "found a second axiom"))
           (update-section (remove-whitespace l))
           (loop current-section)]
          [(equal? current-section 'rules)
           (define arr1? (regexp-match? #rx"->" l))
           (unless (or arr1? (regexp-match #rx"→" l))
             (failed "expected either `->' or `→' in the rule"))
           (define split (if arr1?
                             (regexp-split #rx"->" (remove-whitespace l))
                             (regexp-split #rx"→" (remove-whitespace l))))
           (unless (= 2 (length split))
             (failed (format "expected only one `~a'" (if arr1? "->" "→"))))
           (update-section (cons split (section-value '())))
           (loop current-section)]
          [(equal? current-section 'variables)
           (unless (regexp-match? #rx"=" l)
             (failed "expected either `=' in the variable assignment"))
           (define split (regexp-split #rx"=" (remove-whitespace l)))
           (unless (= 2 (length split)) (failed "expected only one `='"))
           (define key (string->symbol (list-ref split 0)))
           (define val (read (open-input-string (list-ref split 1)))) ;; TODO: better error checking
           (update-section (cons (list key val) (section-value '())))
           (loop current-section)]
          [else (failed "internal error.1")]))
      (hash-set! sections 'rules (reverse (hash-ref sections 'rules)))
      (datum->syntax
       #f
       `(l-system
         :::start :::finish
         (quote
          ,(for/hash ([pr (in-list (hash-ref sections 'variables '()))])
             (values (list-ref pr 0) (list-ref pr 1))))
         ,(for/list ([c (in-string (hash-ref sections 'axiom))])
            (string->symbol (string c)))
         ,@(for/list ([pr (in-list (hash-ref sections 'rules))])
             `(,(string->symbol (list-ref pr 0))
               ->
               ,@(for/list ([c (in-string (list-ref pr 1))])
                   (string->symbol (string c)))))))))

  (define (remove-whitespace l) (regexp-replace* #rx"[ \t]" l ""))
  (define (blank-line? l) (regexp-match? #rx"^[ \t]*$" l))
  
  (define-values (-read -read-syntax -get-info)
    (make-meta-reader
     'lindenmayer
     "language path"
     lang-reader-module-paths
     wrap-read
     wrap-read-syntax
     wrap-get-info))
  (provide
   parse-front ;; for tests
   (rename-out
    [-read read]
    [-read-syntax read-syntax]
    [-get-info get-info])))


(module+ test
  (require rackunit (submod ".." reader))
  (check-equal? (syntax->datum
                 (parse-front (open-input-string "# axiom #\nA\n# rules #\nA->AA") #f))
                '(l-system :::start :::finish '#hash() (A) (A -> A A)))
  (check-equal? (syntax->datum
                 (parse-front (open-input-string "# axiom #\n\n\nA\n\n# rules #\nA->AA\nB->BA") #f))
                '(l-system :::start :::finish  '#hash() (A) (A -> A A) (B -> B A)))
  (check-equal? (syntax->datum
                 (parse-front (open-input-string
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
                              #f))
                '(l-system :::start :::finish
                           '#hash((w . 54) (n . 20))
                           (A)
                           (A -> A A)
                           (B -> B A))))
