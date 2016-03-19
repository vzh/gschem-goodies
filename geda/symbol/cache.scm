;;; Warning: the functions below will silently overwrite your symbols
;;; in the directory specified in the 'cache-dir-name' variable

(define-module (geda symbol cache)
  #:use-module (geda page)
  #:use-module (geda object)
  #:use-module (geda attrib)
  #:use-module (srfi srfi-1)

  #:export (enable-symbol-cache!
            disable-symbol-cache!
            symbol-cache-dir
            symbol-cache-path
            set-symbol-cache-dir!
            is-symbol-cache-enabled?
            is-symbol-cache-writable?
            cache-page-symbols))

;;; Variables

;;; Cache directory name
(define cache-dir-name #f)

;;; Absolute path to the cache directory
(define cache-dir-path #f)

;;; If caching is allowed
(define cache-enabled #f)


;;; Accessors

;;; Returns the current cache directory name or #f if it is not set.
(define (symbol-cache-dir)
  cache-dir-name)

;;; Returns absolute path for the cache directory
(define (symbol-cache-path)
  cache-dir-path)

;;; Sets symbol cache directory name to NAME
(define (set-symbol-cache-dir! name)
  (define (absolute-dir-path name)
    (if (absolute-file-name? name)
        name
        (string-append (getcwd)
                       file-name-separator-string
                       name)))
  (if (string? name)
      (let ((abs-name (absolute-dir-path name)))
        (set! cache-dir-name name)
        (set! cache-dir-path abs-name))
      (begin
        (set! cache-dir-name #f)
        (set! cache-dir-path #f))))

;;; Enables symbol cache
(define (enable-symbol-cache!)
  (set! cache-enabled #t))

;;; Disables symbol cache
(define (disable-symbol-cache!)
  (set! cache-enabled #f))

;;; Predicates

;;; Checks if symbol cache is enabled
(define (is-symbol-cache-enabled?)
  cache-enabled)

;;; Checks if symbol cache directory is valid and writable
(define (is-symbol-cache-writable?)
  (and cache-dir-path
       (or
        (access? cache-dir-path W_OK)
        (and (mkdir cache-dir-path)
             (access? cache-dir-path W_OK)))))

;;; Stolen from the 'fold' function description in the guile info manual
(define (delete-adjacent-duplicates ls)
  (if (null? ls)
      '()
      (fold-right
       (lambda (elem ret)
         (if (string=? elem (first ret))
             ret
             (cons elem ret)))
       (list (last ls))
       ls)))

;;; Returns name of a cached file for BASENAME
(define (get-cache-name basename)
  (string-append
   cache-dir-path
   file-name-separator-string
   basename))

;;; Outputs schematic PAGE to file NAME
;;; Returns PAGE
(define (page->file page name)
  (with-output-to-file name
    (lambda () (display (page->string page))))
  page)

;;; For some reason, the core function %attach-attrib doesn't have
;;; an option which would let us preserve attrib color. Hence, we
;;; do this this explicitly here.
(define (attach-attrib-preserving-color! object attrib)
  (let ((color (object-color attrib)))
    (attach-attribs! object attrib)
    (set-object-color! attrib color)))

;;; Copies OBJECT to PAGE preserving with its attached attributes
(define (copy-object-with-attachment! object #;to page)
  (and (not (attrib-attachment object))
       (let ((new-object (copy-object object))
             (new-attribs (map copy-object (object-attribs object))))
         (page-append! page new-object)
         (when (not (null? new-attribs))
           (apply page-append! page new-attribs)
           ;; The following call would сorrupt attrib color info
           ;;   (apply attach-attribs! new-object new-attribs)
           ;; See comments for attach-attrib-preserving-color!
           (map
            (lambda (attrib)
              (attach-attrib-preserving-color! new-object attrib))
            new-attribs)))))


;;; Saves symbol BASENAME to the cache directory
(define (cache-symbol basename)
  (let ((page (make-page basename))
        (component (make-component/library basename '(0 . 0) 0 #f #f)))
    (map (lambda (object)
           (copy-object-with-attachment! object #;to page))
         (component-contents component))
    (close-page! (page->file page (get-cache-name basename)))))

;;; Get a list of unique component basenames of PAGE
(define (get-unique-component-names page)
  (let* ((components (filter component? (page-contents page)))
         (basenames (map component-basename components)))
    (delete-adjacent-duplicates (sort basenames string<?))))

;;; Save all symbols of PAGE to cache directory
(define (cache-page-symbols page)
  (and (is-symbol-cache-enabled?)
       (for-each
        cache-symbol
        (get-unique-component-names page))))
