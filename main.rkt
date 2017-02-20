#lang racket/base
(module reader racket/base
  (require (only-in syntax/module-reader
                    make-meta-reader
                    lang-reader-module-paths)
           racket/list
           racket/port
           syntax/readerr
           racket/match
           "lexer.rkt")

  (define (wrap-read _read) _read)
  (define (wrap-read-syntax _read-syntax)
    (define double-hyphen (bytes-ref #"=" 0))
    (define newline (bytes-ref #"\n" 0))
    (λ (name in src line col pos)
      (define-values (_in _out) (make-pipe))
      (define front-parse-chan (make-channel))
      (thread (λ ()
                (set-port-next-location! _out line col pos)
                (define-values (fronts nts) (parse-fronts _in src))
                (channel-put front-parse-chan fronts)))
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
                             ,@front-result)]
                [_ module-after-hyphens])]
             [else module-after-hyphens])]
          [(equal? c newline)
           ;; found a non-hyphen and not at the end of a sequence of hyphens
           (read-and-put-byte)
           (loop 0 (+ line 1) 0 (+ pos 1))]
          [else
           (read-and-put-byte)
           (loop 0 line (+ col 1) (+ pos 1))]))))
  (define (wrap-get-info _get-info)
    (match-lambda**
     [('color-lexer default)
      (wrap-lexer (_get-info 'color-lexer default))]
     [(sym def) (_get-info sym def)]))


(define mandatory-sections '(axiom rules))
  (define valid-sections (append mandatory-sections '(variables)))
  (define (valid-section? s) (member s valid-sections))
  (define section-names-str
    (apply string-append (add-between (map (λ (x) (format "~a" x)) valid-sections) " ")))

  (define (parse-fronts port src)
    (define non-terminals (make-hash))
    (let loop ([n 0]
               [l-systems '()])
      (define-values (sections eof?)
        (parse-front port src))
      (cond
        [(exn? sections) (values sections '())]
        [else
         (define l-system
           (datum->syntax
            #f
            `(l-system
              ,n :::start :::finish
              (quote
               ,(for/hash ([pr (in-list (hash-ref sections 'variables '()))])
                  (values (list-ref pr 0) (list-ref pr 1))))
              ,(for/list ([c (in-string (hash-ref sections 'axiom))])
                 (string->symbol (string c)))
              ,@(for/list ([pr (in-list (hash-ref sections 'rules))])
                  `(,(string->symbol (list-ref pr 0))
                    ->
                    ,@(for/list ([c (in-string (list-ref pr 1))])
                        (string->symbol (string c))))))))
         (for ([pr (in-list (hash-ref sections 'rules))])
           (hash-set! non-terminals (string->symbol (list-ref pr 0)) #t))
         (cond [eof? (values (reverse (cons l-system l-systems))
                             (sort (hash-map non-terminals (λ (x y) x))
                                   symbol<?))]
               [else (loop (+ n 1) (cons l-system l-systems))])])))
  (define (parse-front port src)
    (define-values (start-line start-col start-pos) (port-next-location port))
    (define sections (make-hash))
    (with-handlers ([exn:fail:read? (λ (x) (values x #t))])
      (let loop ([current-section #f])
        (define (update-section nv) (hash-set! sections current-section nv))
        (define (section-value default) (hash-ref sections current-section default))
        (define-values (line col pos) (port-next-location port))
        (define l (read-line port))
        (define-values (after-line after-col after-pos) (port-next-location port))
        (define (failed msg) (raise-read-error msg src line col pos (- after-pos pos)))
        (define (l-system-end? l) (regexp-match? #rx"^ *---+ *$" l))
        (define (input-end? l) (or (eof-object? l) (regexp-match? #rx"^ *===+ *$" l)))
        (cond
          [(or (input-end? l) (l-system-end? l))
           (for ([sec (in-list mandatory-sections)])
             (unless (hash-ref sections sec #f)
               (raise-read-error
                (format "did not find the `## ~a ##' section" sec)
                src start-line start-col start-pos (- after-pos start-pos))))
           (hash-set! sections 'rules (reverse (hash-ref sections 'rules)))
           (values sections (input-end? l))]
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
          [else (failed (format "internal error.1 ~s ~s"
                                current-section
                                l))]))))
  
  (define (remove-whitespace l) (regexp-replace* #rx"[\u00A0 \t]" l ""))
  (define (blank-line? l) (regexp-match? #rx"^[\u00A0 \t]*$" l))
  
  (define-values (interop-read interop-read-syntax interop-get-info)
    (make-meta-reader
     'lindenmayer
     "language path"
     lang-reader-module-paths
     wrap-read
     wrap-read-syntax
     wrap-get-info))

  (define (-read port source line col position)
    (cond
      [(appears-to-have-second-lang? port)
       (interop-read port source line col position)]
      [else
       (error '-read "unimplemented")]))

  (define (-read-syntax name port source line col position)
    (cond
      [(appears-to-have-second-lang? port)
       (interop-read-syntax name port source line col position)]
      [else
       (define-values (front-result nts) (parse-fronts port source))
       (when (exn? front-result) (raise front-result))

       (datum->syntax
        #f
        `(module name racket/base
           (require lindenmayer/lang)
           (define (:::start variables) (void))
           (define (:::finish val variables) (newline))
           ,@(for/list ([nt (in-list nts)])
               `(define (,(string->symbol (format ":::~a" nt)) state vars) (display ',nt)))
           ,@front-result))]))

  (define (-get-info port source line col position)
    (cond
      [(appears-to-have-second-lang? port)
       (interop-get-info port source line col position)]
      [else
       (error '-get-info "unimplemented")]))

  (define (appears-to-have-second-lang? port)
    (define l (read-line (peeking-input-port port)))
    (not (regexp-match? #rx"^ *$" l)))
  
  (provide
   parse-fronts ;; for tests
   (rename-out
    [-read read]
    [-read-syntax read-syntax]
    [-get-info get-info])))


(module+ test
  (require rackunit (submod ".." reader))
  (define (parse-fronts/1 port source)
    (define-values (fronts nts) (parse-fronts port source))
    (map syntax->datum fronts))
  (define (parse-fronts/2 port source)
    (define-values (fronts nts) (parse-fronts port source))
    nts)
  (check-equal? (parse-fronts/1 (open-input-string "# axiom #\nA\n# rules #\nA->AA") #f)
                '((l-system 0 :::start :::finish '#hash() (A) (A -> A A))))
  (check-equal? (parse-fronts/1 (open-input-string "# axiom #\n\n\nA\n\n# rules #\nA->AA\nB->BA")
                                #f)
                '((l-system 0 :::start :::finish  '#hash() (A) (A -> A A) (B -> B A))))
  (check-equal? (parse-fronts/1 (open-input-string
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
                                #f)
                '((l-system 0 :::start :::finish
                            '#hash((w . 54) (n . 20))
                            (A)
                            (A -> A A)
                            (B -> B A))))
  (check-equal? (parse-fronts/1 (open-input-string
                                 (string-append
                                  "# axiom #\nA\n# rules #\nA->AA\n"
                                  "---\n"
                                  "# axiom #\nX\n# rules #\nB->CX\n"
                                  "---\n"
                                  "# axiom #\nA\n# rules #\nA->AA\nB->BA"))
                                #f)
                '((l-system 0 :::start :::finish '#hash() (A) (A -> A A))
                  (l-system 1 :::start :::finish '#hash() (X) (B -> C X))
                  (l-system 2 :::start :::finish  '#hash() (A) (A -> A A) (B -> B A))))

  (check-equal? (parse-fronts/2 (open-input-string
                                 (string-append
                                  "# axiom #\nA\n# rules #\nA->AA\n"
                                  "---\n"
                                  "# axiom #\nX\n# rules #\nB->CX\n"
                                  "---\n"
                                  "# axiom #\nA\n# rules #\nA->AA\nB->BA"))
                                #f)
                '(A B))
  (check-equal? (parse-fronts/2 (open-input-string
                                 (string-append
                                  "# axiom #\nA\n# rules #\nA->AA\n"
                                  "---\n"
                                  "# axiom #\nX\n# rules #\nB->CX\n"
                                  "---\n"
                                  "# axiom #\nQ\n# rules #\nQ->QQ\nW->WQ"))
                                #f)
                '(A B Q W)))
