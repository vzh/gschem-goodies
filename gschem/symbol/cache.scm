;;; Redefine the cache-page-symbols procedure defined in the
;;; module (geda symbol cache)
(define-module (gschem symbol cache)
  #:use-module (geda symbol cache)
  #:use-module (gschem window)
  #:use-module (gschem goodies hook)
  ;; #:replace
  ;; (cache-page-symbols)
  #:re-export (enable-symbol-cache
               disable-symbol-cache
               is-symbol-cache-enabled?))

(define geda-cache-page-symbols cache-page-symbols)

;;; Since gschem doesn't export embedded procedures properly, we
;;; won't be polite with it, either.
;;; This temporary procedure will be replaced with a proper one
;;; some day.
(define log! (@@ (guile-user) gschem-log))

;;; Redefine cache-page-symbols
(define (cache-page-symbols)
  (if (is-symbol-cache-enabled?)
      (begin (log! "Caching used symbols...\n")
             (if (geda-cache-page-symbols (active-page))
                 (log! "... caching completed.\n")
                 (log! "Something went wrong with symbol caching.\nCheck log for more information.\n")))
      (log! "Caching symbols disabled.\n")))

;;; Use hooks to cache symbols automatically after page saving
(add-hook! post-save-page-hook cache-page-symbols)
(add-hook! post-save-page-as-hook cache-page-symbols)

(export! cache-page-symbols)
