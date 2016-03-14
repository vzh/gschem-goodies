;;; Maintain symbol cache
(use-modules (gschem goodies hook)      ; adds hooks for
                                        ; &file-save and
                                        ; &file-save-as actions
             (gschem symbol cache)      ; adds procedures for
                                        ; caching symbols
             )
;;; Enable symbol cache in the directory
(enable-symbol-cache "sym")

;;; Use hooks to cache symbols automatically after page saving
(add-hook!
 post-save-page-hook
 (lambda ()
   (gschem-log "Caching used symbols.\n")
   (cache-page-symbols (active-page))))

(add-hook!
 post-save-page-as-hook
 (lambda ()
   (gschem-log "Caching used symbols.\n")
   (cache-page-symbols (active-page))))
