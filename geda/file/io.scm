;;; coding: utf-8
(define-module (geda file io)
  #:export (page->file)
  #:export (file->page)
  #:use-module (geda page)
  #:use-module ((ice-9 rdelim)
                #:select (read-string)
                #:prefix rdelim:))

;;; Outputs schematic PAGE to file NAME
;;; Returns PAGE
(define (page->file page name)
  (with-output-to-file name
    (lambda () (display (page->string page))))
  page)

;;; Reads file NAME and outputs a page named NAME
(define (file->page name)
  (with-input-from-file name
    (lambda () (string->page name (rdelim:read-string)))))
