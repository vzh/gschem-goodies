(define-module (geda file io)
  #:export (page->file)
  #:use-module (geda page))

;;; Outputs schematic PAGE to file NAME
;;; Returns PAGE
(define (page->file page name)
  (with-output-to-file name
    (lambda () (display (page->string page))))
  page)
