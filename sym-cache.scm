;;; Warning: the functions below will silently overwrite your symbols
;;; in the directory specified in the 'cache-dir-name' variable

(use-modules (geda page) (geda object) (srfi srfi-1))

;;; Predefined cache dir name
(define cache-dir-name "sym")

;;; Stolen from the 'fold' function description in the guile info manual
(define (delete-adjacent-duplicates ls)
  (fold-right (lambda (elem ret)
                (if (string=? elem (first ret))
                    ret
                    (cons elem ret)))
              (list (last ls))
              ls))

;;; Returns name of a cached file for BASENAME
(define (get-cache-name basename)
  (string-append
   cache-dir-name
   file-name-separator-string
   basename))

;;; Outputs schematic PAGE to file NAME
;;; Returns PAGE
(define (page->file page name)
  (with-output-to-file name
    (lambda () (display (page->string page))))
  page)

;;; Saves symbol BASENAME to the cache directory
(define (cache-symbol basename)
  (let ((page (make-page basename))
        (component (make-component/library basename '(0 . 0) 0 #f #f)))
    (close-page!
     (page->file
      (apply
       page-append!
       page
       (map copy-object (component-contents component)))
      (get-cache-name basename)))))

;;; Get a list of unique component basenames of PAGE
(define (get-unique-component-names page)
  (let* ((components (filter component? (page-contents page)))
         (basenames (map component-basename components))         )
    (delete-adjacent-duplicates (sort basenames string<?))))

;;; Save all symbols of PAGE to cache directory
(define (cache-page-symbols page)
  (for-each
   cache-symbol
   (get-unique-component-names page)))

;;; Save all symbols of all open pages to cache directory
(define (cache-symbols)
  (for-each cache-page-symbols (active-pages)))

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
