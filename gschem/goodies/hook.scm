(define-module (gschem goodies hook)
  #:use-module (gschem builtins)
  #:export (pre-save-page-hook
            post-save-page-hook
            pre-save-page-as-hook
            post-save-page-as-hook))

;;; New hooks
(define pre-save-page-hook (make-hook))
(define post-save-page-hook (make-hook))
(define pre-save-page-as-hook (make-hook))
(define post-save-page-as-hook (make-hook))

;;; Store functions
(define &stock-file-save &file-save)
(define &stock-file-save-as &file-save-as)

;;; Redefine menu actions
(define (&file-save)
  (run-hook pre-save-page-hook)
  (&stock-file-save)
  (run-hook post-save-page-hook))

(define (&file-save-as)
  (run-hook pre-save-page-as-hook)
  (&stock-file-save-as)
  (run-hook post-save-page-as-hook))

;;; #:replace (&file-save &file-save-as) in the module definition
;;; doesn't work for some reason, so I export the functions
;;; explicitly here
(export! &file-save)
(export! &file-save-as)
