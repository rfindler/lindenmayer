#lang racket

(require drracket/tool
         framework
         racket/gui
         racket/runtime-path
         "process-syntax.rkt"
         "search.rkt"
         data/interval-map)

(provide tool@)

(define-runtime-path process-syntax.rkt "process-syntax.rkt")

(define-local-member-name
  lsys-refactor-callback
  lsys-refactor-build-popup-menu
  lsys-refactor-key-callback)

(define lsys-refactor-text<%>
  (interface ()
    lsys-refactor-callback
    lsys-refactor-key-callback))

(define tool@
  (unit (import drracket:tool^)
        (export drracket:tool-exports^)

        (define (phase1) (void))
        (define (phase2) (void))

        (define make-refactor-text%
          (mixin (racket:text<%>) (lsys-refactor-text<%>)
            (inherit begin-edit-sequence end-edit-sequence
                     insert delete)

            (define refactor-info #f)

            (define/private (update-refactor-info! [info #f])
              (set! refactor-info (and info (make-interval-map info))))

            (define/private (invalidate-refactor-info!)
              (update-refactor-info!))

            (define/public (lsys-refactor-callback expanded-info)
              (update-refactor-info! expanded-info))

            (define/public (lsys-refactor-build-popup-menu menu pos text)
              (define refactor-menu
                (make-object menu%
                  "Refactor Rules"
                  menu))
              (send refactor-menu enable #f)
              (when (get-refactor-info)
                (new menu-item%
                     [label "Create New Non-Terminal"]
                     [parent refactor-menu]
                     [callback (λ (item evt) (make-new-non-terminal))])
                (new menu-item%
                     [label "Replace All Occurrences With New Non-Terminal"]
                     [parent refactor-menu]
                     [callback (λ (item evt) (make-new-non-terminal #t))])
                (send refactor-menu enable #t)))

            (define/private (get-refactor-info)
              (define startb (box #f))
              (define endb (box #f))
              (send this get-position startb endb)
              (define start (unbox startb))
              (define end (unbox endb))
              (and refactor-info
                   start
                   end
                   (not (equal? start end))
                   (eq? (interval-map-ref refactor-info start #f)
                        (interval-map-ref refactor-info end #f))
                   (interval-map-ref refactor-info start #f)))


            (define/private (make-new-non-terminal [lift-all? #f])
              (define startb (box #f))
              (define endb (box #f))
              (send this get-position startb endb)
              (define start (unbox startb))
              (define end (unbox endb))
              (define info (get-refactor-info))
              (when info
                (define frame
                    (let loop ([canvas (send this get-canvas)])
                      (cond
                        [(not canvas) canvas]
                        [(or (is-a? canvas frame%)
                             (is-a? canvas dialog%))
                         canvas]
                        [else (loop (send canvas get-parent))])))
                (define new-nt-name
                  (get-text-from-user "Introduce New Non-Terminal"
                                      "Non-Terminal Name"
                                      frame
                                      #:validate (λ (str) (= 1 (string-length str)))))
                (define valid-nt-name? (and new-nt-name (= 1 (string-length new-nt-name))))
                (unless valid-nt-name?
                    (message-box "Invalid Non-Terminal Name"
                                 "Non-Terminal names must contain only a single character"
                                 frame))
                (when valid-nt-name?
                  (match-define (cons rules-start
                                      (list position-map (list (cons symbol-vecs position-vecs) ...)))
                    info)
              
                (define selected-ids
                  (let/ec ret
                    (let loop ([cur start]
                               [acc null])
                      (cond
                        [(< cur end)
                         (define span+id (hash-ref position-map cur #f))
                         (cond
                           [span+id
                            (match-define (cons span the-id) span+id)
                            (loop (+ cur span) (cons the-id acc))]
                           [else
                            (loop (+ cur 1) acc)])]
                        [else (list->vector (reverse acc))]))))
                (when selected-ids
                  (cond
                    [lift-all?
                     (define selected-len (vector-length selected-ids))
                     (define locs-to-replace
                       (apply append
                              (for/list ([rule-str (in-list (reverse symbol-vecs))]
                                         [rule-posns (in-list (reverse position-vecs))])
                                (define instance-indices (reverse (find-all rule-str selected-ids)))
                                (for/list ([i (in-list instance-indices)])
                                  (match-define (cons spos sspan) (vector-ref rule-posns i))
                                  (match-define (cons epos espan)
                                    (vector-ref rule-posns (sub1 (+ i selected-len))))
                                  (list spos (+ epos espan))))))
                     (begin-edit-sequence)
                     (define selected (send this get-text start end))
                     (for ([loc (in-list locs-to-replace)])
                       (match-define (list start end) loc)
                       (send this delete start end)
                       (send this insert new-nt-name start))
                     (send this insert "\n" rules-start)
                     (send this insert selected rules-start)
                     (send this insert (string-append new-nt-name " -> ") rules-start)
                     (end-edit-sequence)]
                    [else
                     (do-add-new-non-terminal new-nt-name start end rules-start)])))))

            (define (do-add-new-non-terminal new-nt-name start end rules-start)
              ;(printf "RULES-START: ~a\n" rules-start)
              ;(printf "START: ~a\n" start)
              ;(printf "END: ~a\n" end)
              ;(printf "TEXT: ~a\n" (send this get-text start end))
              (begin-edit-sequence)
              ;(send this insert new-nt-name end)
              ;(send this insert "\n" rules-start)
              (send this insert (send this get-text start end) rules-start)
              #;(send this move/copy-to-edit
                    this
                    start
                    end
                    rules-start
                    #:try-to-move? #f)
              (send this delete (+ start (- end start)) (+ end (- end start)))
              (send this insert new-nt-name (+ start (- end start)))
              (send this insert "\n" (+ rules-start (- end start)))
              (send this insert (string-append new-nt-name " -> ") rules-start)
              (invalidate-refactor-info!)
              (end-edit-sequence)
              (void))
             
            
            (define/public (lsys-refactor-key-callback)
              (define pos-box (box #f))
              (send this get-position pos-box)
              (define pos (unbox pos-box))
              (make-new-non-terminal pos))

            (super-new)))

        (define (add-refactor-keybindings keymap)
          (send keymap add-function
                "make new non-terminal"
                (λ (obj evt)
                  (when (is-a? obj lsys-refactor-text<%>)
                    (send obj lsys-refactor-key-callback))))
          (keymap:send-map-function-meta keymap "=" "make new non-terminal"))

        (keymap:add-to-right-button-menu/before
         (let ([old (keymap:add-to-right-button-menu/before)])
           (λ (menu editor event)
             (old menu editor event)
             (define-values (pos text) (send editor get-pos/text event))
             (when (and pos (is-a? text lsys-refactor-text<%>))
               (send editor lsys-refactor-build-popup-menu menu pos text)))))

        (add-refactor-keybindings (drracket:rep:get-drs-bindings-keymap))
        (drracket:get/extend:extend-definitions-text make-refactor-text%)
        (drracket:module-language-tools:add-online-expansion-handler
         process-syntax.rkt
         'handle-expansion
         (λ (text expanded-info)
           (send text lsys-refactor-callback expanded-info)))))