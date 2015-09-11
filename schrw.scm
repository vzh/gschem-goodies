(use-modules (ice-9 lineio))
(use-modules (geda page))

; Input/output procedures
; reads FILE and outputs string
(define (file->string file)
  (let* ((port (make-line-buffering-input-port (open-file file "r"))))
    (do ((line "" (read-string port))
         (s "" (string-append s line)))
      ((eof-object? line) ; test
       (close-port port)  ; expression(s) to evaluate in the end
       s)                 ; return value
      ; empty body
      )))

; reads schematic FILE and outputs PAGE object
(define (schematic-file->page file)
    (string->page file (file->string file)))

; saves schematic PAGE to FILE
(define (page->schematic-file page file)
  (with-output-to-file file
    (lambda () (display (page->string page)))))
