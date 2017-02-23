#lang racket/base
(module reader racket/base
  (require (only-in syntax/module-reader
                    make-meta-reader
                    lang-reader-module-paths)
           racket/list
           racket/port
           syntax/readerr
           racket/match
           racket/syntax
           "lexer.rkt"
           "structs.rkt"
           "syntax-properties.rkt"
           "private/expression-parse.rkt")

  (define (wrap-read-syntax _read-syntax)
    (define double-hyphen (bytes-ref #"=" 0))
    (define newline (bytes-ref #"\n" 0))
    (λ (name in module-path line col pos)
      (define-values (in-line in-col in-pos) (port-next-location in))
      (define-values (_in _out) (make-pipe))
      (define front-parse-chan (make-channel))
      (thread (λ ()
                (set-port-next-location! _out line col pos)
                (port-count-lines! _in)
                (set-port-next-location! _in in-line in-col in-pos)
                (define-values (fronts nts) (parse-fronts _in name))
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
           (loop (+ n 1) line (maybe-add1 col) (maybe-add1 pos))]
          [(n . > . 3)
           (define front-result (fetch-front-result))
           ;; found the end of our double hyphens
           (define module-after-hyphens
             (cond
               [(procedure-arity-includes? _read-syntax 6)
                (_read-syntax name in module-path line col pos)]
               [else
                (_read-syntax name in)]))
           (cond
             [(syntax? module-after-hyphens)
              (syntax-case module-after-hyphens ()
                [(mod name . whatever)
                 (equal? 'module (syntax-e #'mod))
                 (datum->syntax
                  #f
                  `(,#'module ,#'name racket/base
                              ,#'(mod post-hyphens . whatever)
                              (require (prefix-in ::: (submod "." post-hyphens))
                                       lindenmayer/lang)
                              ,@front-result))]
                [_ module-after-hyphens])]
             [else module-after-hyphens])]
          [(equal? c newline)
           ;; found a non-hyphen and not at the end of a sequence of hyphens
           (read-and-put-byte)
           (loop 0 (maybe-add1 line) (and col 0) (maybe-add1 pos))]
          [else
           (read-and-put-byte)
           (loop 0 line (and col (+ col 1)) (maybe-add1 pos))]))))
  (define (wrap-get-info _get-info)
    (match-lambda**
     [('color-lexer default)
      (wrap-lexer (_get-info 'color-lexer default))]
     ;; this doesn't work (yet) for refactorings using syntax-properties
     #;[('drracket:keystrokes default)
      (cons (list new-nt-keybinding do-make-new-non-terminal)
            (default 'drracket:keystrokes))]
     [(sym def) (_get-info sym def)]))


  (define mandatory-sections '(axiom rules))
  (define valid-sections (append mandatory-sections '(variables)))
  (define (valid-section? s) (member s valid-sections))
  (define section-names-str
    (apply string-append (add-between (map (λ (x) (format "~a" x)) valid-sections) " ")))

  (define (parse-fronts port name)
    (define non-terminals (make-hash))
    (let loop ([n 0]
               [l-systems '()])
      (define-values (sections eof?)
        (parse-front port name))
      (cond
        [(exn? sections) (values sections '())]
        [else
         (define l-system
           (annotate-rule-info
            (hash-ref sections 'rules)
            (datum->syntax
            #f
            `(l-system
              ,n :::start :::finish
              (quote
               ,(for/hash ([pr (in-list (hash-ref sections 'variables '()))])
                  (values (list-ref pr 0) (list-ref pr 1))))
              ,(for/list ([c (in-list (hash-ref sections 'axiom))])
                 (sym-id c))
              ,@(for/list ([pr (in-list (hash-ref sections 'rules))])
                  `(,(sym-id (rule-nt pr))
                    ->
                    ,@(for/list ([c (in-list (rule-rhs pr))])
                        (sym-id c))))))))
         (for ([pr (in-list (hash-ref sections 'rules))])
           (hash-set! non-terminals (syntax-e (sym-id (rule-nt pr))) #t))
         (cond [eof? (values (reverse (cons l-system l-systems))
                             (sort (hash-map non-terminals (λ (x y) x))
                                   symbol<?))]
               [else (loop (+ n 1) (cons l-system l-systems))])])))
  (define (parse-front port name)
    (define-values (start-line start-col start-pos) (port-next-location port))
    (define sections (make-hash))
    (with-handlers ([exn:fail:read? (λ (x) (values x #t))])
      (let loop ([current-section #f] [pending-rule #f])
        (define (update-section nv) (hash-set! sections current-section nv))
        (define (section-value default) (hash-ref sections current-section default))
        (define-values (line col pos) (port-next-location port))
        (define l (read-line port))
        (define-values (after-line after-col after-pos) (port-next-location port))
        (define (failed msg) (raise-read-error msg name line col pos (- after-pos pos)))
        (define (l-system-end? l) (regexp-match? #rx"^ *---+ *$" l))
        (define (input-end? l) (or (eof-object? l) (regexp-match? #rx"^ *===+ *$" l)))
        (define (handle-pending-rule)
          (when pending-rule
            (update-section (cons pending-rule (section-value '())))))
        (cond
          [(or (input-end? l) (l-system-end? l))
           (handle-pending-rule)
           (for ([sec (in-list mandatory-sections)])
             (unless (hash-ref sections sec #f)
               (raise-read-error
                (format "did not find the `## ~a ##' section" sec)
                name start-line start-col start-pos (- after-pos start-pos))))
           (hash-set! sections 'rules (reverse (hash-ref sections 'rules)))
           (values sections (input-end? l))]
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
           (update-section (process-axiom l name line col pos))
           (loop current-section #f)]
          [(equal? current-section 'rules)
           (define arr1? (regexp-match? #rx"->" l))
           (define arr2? (regexp-match #rx"→" l))
           (define pending? (and pending-rule (regexp-match? #px"^\\s+" l)))
           (unless (or arr1?  arr2? pending?)
             (failed "expected either `->' or `→' in the rule or the continuation of another rule"))
           (cond
             [(or arr1? arr2?) ;; this is a new rule
              (handle-pending-rule)
              (define split (if arr1?
                                (regexp-split #rx"->" (remove-whitespace l))
                                (regexp-split #rx"→" (remove-whitespace l))))
              (unless (= 2 (length split))
                (failed (format "expected only one `~a'" (if arr1? "->" "→"))))
              (define the-rule (process-rule l name line col pos failed))
              (loop current-section the-rule)]
             [else ;; continuation of a pending rule
              (match-define (rule rule-nt rule-current-body) pending-rule)
              (define rule-continued-body (process-axiom l name line col pos))
              (loop current-section (rule rule-nt (append rule-current-body rule-continued-body)))])]
          [(equal? current-section 'variables)
           (handle-pending-rule)
           (unless (regexp-match? #rx"=" l)
             (failed "expected either `=' in the variable assignment"))
           (define split (regexp-split #rx"=" (remove-whitespace l)))
           (unless (= 2 (length split)) (failed "expected only one `='"))
           (define key (string->symbol (list-ref split 0)))
           (define val (read (open-input-string (list-ref split 1)))) ;; TODO: better error checking
           (update-section (cons (list key val) (section-value '())))
           (loop current-section #f)]
          [else (failed (format "internal error.1 ~s ~s"
                                current-section
                                l))]))))

  (define (process-axiom str name line col pos)
    (define the-port (open-input-string str))
    (port-count-lines! the-port)
    (set-port-next-location! the-port line col pos)
    (define-values (result _) (process-port the-port name))
    result)

  (define (process-rule str name line col pos failed)
    (define arr1? (regexp-match? #rx"->" str))
    (define (end? c p)
      (if arr1?
          (and (equal? c #\-)
               (equal? (peek-char p) #\>)
               (read-char p) ;; consume the character
               #t)
          (equal? c #\→)))
    (define the-port (open-input-string str))
    (port-count-lines! the-port)
    (set-port-next-location! the-port line col pos)
    (match-define-values (left _) (process-port the-port name end?))
    (define-values (right _) (process-port the-port name))
    (when (empty? left)
      (failed "expected the name of a non-terminal"))
    (when (> (length left) 1)
      (failed "expected exactly 1 non-terminal name"))
    (rule (first left) right))

  (define (process-port sp name [end? (λ (c p) (eof-object? c))])
    (let loop ([stxs '()])
      (define-values (line col pos) (port-next-location sp))
      (define c (read-char sp))
      (cond
        [(end? c sp) (values (reverse stxs) '(list line col pos))]
        [(char-whitespace? c)
         ;; skip whitespace, can assume no newlines
         (loop stxs)]
        [else
         (define id (to-identifier (string->symbol (string c)) name line col pos))
         (define non-terminal
           (cond
             [(equal? (peek-char sp) #\()
              (define-values (args-start-line args-start-col args-start-pos) (port-next-location sp))
              (define stash-arguments-port (open-output-string))
              (let loop ([d 0])
                (define args-c (read-char sp))
                (when (or (eof-object? args-c) (equal? args-c #\newline))
                  (define-values (end-line end-col end-pos) (port-next-location sp))
                  (raise-read-error
                   (format "did not find end of parameters to ~a" c)
                   name line col pos (- end-pos pos)))
                (display args-c stash-arguments-port)
                (cond
                  [(and (equal? args-c #\)) (= d 1))
                   (void)]
                  [(equal? args-c #\)) (loop (- d 1))]
                  [(equal? args-c #\() (loop (+ d 1))]
                  [else (loop d)]))
              (define args-string (get-output-string stash-arguments-port))
              (define arguments-port (open-input-string args-string))
              (port-count-lines! arguments-port)
              (set-port-next-location! arguments-port args-start-line args-start-col args-start-pos)
              (sym id (parse-arguments name arguments-port))]
             [(equal? c #\))
              (raise-read-error
               "close paren with no opening paren"
               name line col pos 1)]
             [else (sym id '())]))
         (loop (cons non-terminal stxs))])))

  (define (maybe-add1 n) (and n (add1 n)))
  (define (remove-whitespace l) (regexp-replace* #rx"[\u00A0 \t]" l ""))
  (define (blank-line? l) (regexp-match? #rx"^[\u00A0 \t]*$" l))

  (define-values (interop-read interop-read-syntax interop-get-info)
    (make-meta-reader
     'lindenmayer
     "language path"
     lang-reader-module-paths
     (λ (_r) (λ (port) (error 'lindenmayer::read "shouldn't be called")))
     wrap-read-syntax
     wrap-get-info))

  (define (-read port) (syntax->datum (-read-syntax #f port #f #f #f #f)))
  (define (-read-syntax name port source line col position)
    (cond
      [(appears-to-have-second-lang? port)
       (interop-read-syntax name port source line col position)]
      [else
       (define-values (front-result nts) (parse-fronts port name))
       (when (exn? front-result) (raise front-result))

       (datum->syntax
        #f
        `(module name racket/base
           (require lindenmayer/lang)
           (define (:::start variables) (void))
           (define (:::finish val variables) (newline))
           ,@(for/list ([nt (in-list nts)])
               `(define (,(string->symbol (format ":::~a" nt))
                         state vars)
                  (display ',nt)))
           ,@front-result))]))

  (define (-get-info port source line col position)
    (cond
      [(appears-to-have-second-lang? port)
       (interop-get-info port source line col position)]
      [else
       (match-lambda**
        [('color-lexer default)
         (wrap-lexer #f)]
        [(sym def) def])]))

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
  (require rackunit (only-in (submod ".." reader) parse-fronts))
  (define (parse-fronts/1 port source)
    (port-count-lines! port)
    (define-values (fronts nts) (parse-fronts port source))
    (map syntax->datum fronts))
  (define (parse-fronts/2 port source)
    (port-count-lines! port)
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

  (check-equal? (parse-fronts/1 (open-input-string
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
                                #f)
                '((l-system 0 :::start :::finish
                            '#hash((n . 4))
                            (A)
                            (A -> A B)
                            (B -> A))))

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
                '(A B Q W))

  (check-not-exn
   (λ ()
     (parameterize ([read-accept-reader #t])
       (read (open-input-string "#lang lindenmayer\n# axiom #\nA\n# rules #\nA -> A\n")))))

  (check-not-exn
   (λ ()
     (parameterize ([read-accept-reader #t])
       (read (open-input-string
              (string-append
               "#lang lindenmayer racket\n"
               "# axiom #\nA\n# rules #\nA -> A\n"
               "=========\n(+ 1 2)"))))))
  )
