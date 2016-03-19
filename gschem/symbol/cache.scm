;;; Redefine the cache-page-symbols procedure defined in the
;;; module (geda symbol cache)
(define-module (gschem symbol cache)
  #:use-module (geda symbol cache)
  #:use-module (gschem window)
  #:use-module (gschem goodies hook)
  #:re-export (enable-symbol-cache!
               disable-symbol-cache!
               symbol-cache-dir
               symbol-cache-path
               set-symbol-cache-dir!))

;;; Since gschem doesn't export embedded procedures properly, we
;;; won't be polite with it, either.
;;; This temporary procedure will be replaced with a proper one
;;; some day.
(define log! (@@ (guile-user) gschem-log))

(define (start-log-message)
  (log!
   (format #f "Caching used symbols in ~S (~S).\n"
           (symbol-cache-dir)
           (symbol-cache-path))))

;;; Redefine cache-page-symbols
(define (cache!)
  (if (is-symbol-cache-enabled?)
      (if (is-symbol-cache-writable?)
          (begin (start-log-message)
                 (if (cache-page-symbols (active-page))
                     (log! "... caching completed.\n")
                     (log! "Something went wrong with symbol caching.\nCheck log for more information.\n")))
          (log! "Symbol cache directory does not exist or is not writable.\n"))
      (log! "Caching symbols disabled.\n")))

;;; Use hooks to cache symbols automatically after page saving
(add-hook! post-save-page-hook cache!)
(add-hook! post-save-page-as-hook cache!)
