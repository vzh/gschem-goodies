;;; coding: utf-8
(define-module (geda file io)
  #:export (page->file)
  #:export (file->page)
  #:use-module (geda page)
  #:use-module (ice-9 lineio))

;;; Outputs schematic PAGE to file NAME
;;; Returns PAGE
(define (page->file page name)
  (with-output-to-file name
    (lambda () (display (page->string page))))
  page)

;;; Reads schematic FILE and outputs a page named FILE
(define (file->page file)
  (define (file->string file)
    (let* ((port (make-line-buffering-input-port (open-file file "r"))))
      (do ((line "" (read-string port))
           (s "" (string-append s line)))
          ((eof-object? line) ; test
           (close-port port)  ; expression(s) to evaluate in the end
           s)                 ; return value
                              ; empty body
        )))

  (string->page file (file->string file)))
