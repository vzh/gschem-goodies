; License: same as geda-gaf
; Author:  Vladimir Zhbanov vzhbanov@gmail.com

(use-modules (ice-9 lineio)
             (ice-9 rw)
             (geda page)
             (geda object)
             (geda attrib)
             (geda file io))

; Setup grid steps and color numbers
; Please see gafrc for example of how to adjust colors
;
; I set up my symbols so that 40 gschem points corresponds to 1 mm
(define step 40)
(define major-step (* 5 step))
(define mesh-grid-minor-color 22)
(define mesh-grid-major-color 23)


; removes OBJECTS from OLD-PAGE and adds them to NEW-PAGE,
; thus letting us prepend objects to a page list of objects
; instead of appending them
(define (page-move-objects! old-page new-page objects)
  (for-each
    (lambda (obj)
      ; If the object is a component we must first detach its
      ; attributes and remove them and the object, and then append
      ; and attach them in the reverse order
      (if (component? obj)
        (let ((attribs (object-attribs obj)))
          (apply detach-attribs! obj attribs)
          (apply page-remove! old-page
              (append attribs (list obj)))
          (apply page-append! new-page
               obj attribs)
          (apply attach-attribs! obj attribs))
        (if (not (and
                   (attribute? obj)
                   (attrib-attachment obj)))
          (begin
            (page-remove! old-page obj)
            (page-append! new-page obj)))))
    objects))

; gets minimum of A and B and rounds it
(define (get-rounded-minimum a b)
  (* (floor (/ (min a b) step)) step))

; gets maximum of A and B and rounds it
(define (get-rounded-maximum a b)
  (* (ceiling (/ (max a b) step)) step))

; chooses color depending on line coord A
(define (get-color a)
  (if (= (modulo a major-step) 0)
    mesh-grid-major-color
    mesh-grid-minor-color))

; main procedure
; adds grid lines to a page
; The user must specify names of the input and output files.
(define (main)
  (if (< (length (command-line)) 3)
    (error "You must give at least 2 file names")

    (let* ((input (cadr (command-line)))
           (output (caddr (command-line)))
           (input-page (file->page input))
           (output-page (make-page output))
           (objects (page-contents input-page))
           (bounds (apply object-bounds objects))
           (x1 (get-rounded-minimum (caar bounds) (cadr bounds)))
           (x2 (get-rounded-maximum (caar bounds) (cadr bounds)))
           (y1 (get-rounded-minimum (cdar bounds) (cddr bounds)))
           (y2 (get-rounded-maximum (cdar bounds) (cddr bounds)))
           )

      (let make-vertical-grids ((x x2))
        (page-append! output-page
          (make-line (cons x y1) (cons x y2) (get-color x)))
        (if (> x x1)
          (make-vertical-grids (- x step))))

      (let make-horizontal-grids ((y y2))
        (page-append! output-page
          (make-line (cons x1 y) (cons x2 y) (get-color y)))
        (if (> y y1)
          (make-horizontal-grids (- y step))))

      (page-move-objects! input-page output-page objects)

      (page->file output-page output)
      )))

; Run
(main)
