; Scripting example by vzh per request of Kai-Martin Knaak :-)
; Use at your own risk.

; The main procedure here is
; multiple-copy-move-and-rotate-selection which can be abbreviated
; as mcmars.
; Usage:
;   launch gschem so it can use this script, e.g.
;     gschem -s move-and-rotate.scm
;   select objects in gschem, then hit ':' (semicolon) and type
;     (mcmars '(1000 . 500) 90 10)
;   hit <Enter>
; Enjoy!


(use-modules (gschem selection))

; align coords by ALIGN
(define (ceiling-coords vector align)
  (cons
    (* (ceiling-quotient (car vector) align) align)
    (* (ceiling-quotient (cdr vector) align) align)
    ))

; Get minimum X and minimum Y of two pairs of coords
(define (min-coords coord1 coord2)
  (let ((x (min (car coord1) (car coord2)))
        (y (min (cdr coord1) (cdr coord2))))
    ; return value
    (cons x y)))

; Copy, move and rotate current selection. The selected objects
; are first copied, then translated by VECTOR and finally rotated
; by ANGLE about center which is calculated as rounded by 100
; lower left coordinate of all objects in selection.
; If no objects are selected, opens gschem message dialog with
; warning.
; Returns the copied objects.
(define (copy-move-and-rotate-selection vector angle)
  (let ((objects (page-selection (active-page))))
    (if (null? objects)
      (gschem-msg "Select something first!")
      ; else
      (let* ((copied-objects (map copy-object objects))
             (translated-objects (apply translate-objects! vector copied-objects))
             (bounds (apply object-bounds translated-objects))
             (rotation-center (ceiling-coords (min-coords (car bounds) (cdr bounds)) 100))
             (rotated-objects (apply rotate-objects! rotation-center angle translated-objects)))
        (apply page-append! (active-page) rotated-objects)
        rotated-objects)
      )))

; Multiply VECTOR which must be a pair by NUMBER
(define (multiply-vector-by vector number)
  (cons (* number (car vector)) (* number (cdr vector))))

; Copy, move and rotate current selection NUMBER times. Applies
; the copy-move-and-rotate-selection procedure multiple times
; increasing every time vector and angle by given values of VECTOR
; and ANGLE.
; If no objects are selected, opens gschem message dialog with
; warning.
; Returns value is unspecified.
(define (multiple-copy-move-and-rotate-selection vector angle num)
  (if (null? (page-selection (active-page)))
    (gschem-msg "Select something first!")
    ; else
    (do ((i num (1- i)))
      ((= i 0))
      (copy-move-and-rotate-selection
        (multiply-vector-by vector i) (* angle i)))
    ))

; Abbreviated name for the multiple-copy-move-and-rotate-selection
; procedure
(define mcmars multiple-copy-move-and-rotate-selection)
