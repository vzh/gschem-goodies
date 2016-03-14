;;; Maintain symbol cache
(use-modules (gschem goodies hook)      ; adds hooks for
                                        ; &file-save and
                                        ; &file-save-as actions
             (gschem symbol cache)      ; adds procedures for
                                        ; caching symbols
             )
;;; Enable symbol cache in the directory
(enable-symbol-cache "sym")

;;; Caching procedure
(define (cache)
  (if (is-symbol-cache-enabled?)
      (begin (gschem-log "Caching used symbols...\n")
             (if (cache-page-symbols (active-page))
                 (gschem-log "... caching completed.\n")
                 (gschem-msg "Something went wrong with symbol caching.\nCheck log for more information.\n")))
      (gschem-log "Caching symbols disabled.\n")))

;;; Use hooks to cache symbols automatically after page saving
(add-hook! post-save-page-hook cache)
(add-hook! post-save-page-as-hook cache)
