;;; Procedures processing pure bounds structures, that is, the
;;; structures in the form:
;;;   '((left . bottom) . (right . top))

(define-module (geda geometry bounds)
  #:use-module (srfi srfi-1)

  #:export (bounds-left
            bounds-bottom
            bounds-right
            bounds-top
            bounds-width
            bounds-height
            normalize-bounds
            translate-bounds
            bounds))

;;; Syntax rules for getting properties of bounds
(define-syntax bounds-left
  (syntax-rules ()
    ((_ bounds) (caar bounds))))
(define-syntax bounds-bottom
  (syntax-rules ()
    ((_ bounds) (cdar bounds))))
(define-syntax bounds-right
  (syntax-rules ()
    ((_ bounds) (cadr bounds))))
(define-syntax bounds-top
  (syntax-rules ()
    ((_ bounds) (cddr bounds))))

;;; Calculates the width of BOUNDS.
(define (bounds-width bounds)
  (abs (- (bounds-right bounds) (bounds-left bounds))))

;;; Calculates the height of BOUNDS.
(define (bounds-height bounds)
  (abs (- (bounds-top bounds) (bounds-bottom bounds))))

;;; Normalizes BOUNDS so the first pair contains minimum
;;; coordinates and the second one contains maximum coordinates.
(define (normalize-bounds bounds)
  (if (null? bounds)
      '()
      (let ((x1 (bounds-left bounds))
            (y1 (bounds-bottom bounds))
            (x2 (bounds-right bounds))
            (y2 (bounds-top bounds)))
        (if (and (= x1 x2) (= y1 y2))
            '()
            (cons (cons (min x1 x2) (min y1 y2))
                  (cons (max x1 x2) (max y1 y2)))))))

;;; Translates BOUNDS by VECTOR which must be a pair of
;;; coordinates (X . Y).
(define (translate-bounds bounds vector)
  (if (null? bounds)
      '()
      (let ((vector-x (car vector))
            (vector-y (cdr vector)))
        (cons
         (cons (+ vector-x (bounds-left bounds))
               (+ vector-y (bounds-bottom bounds)))
         (cons (+ vector-x (bounds-right bounds))
               (+ vector-y (bounds-top bounds)))))))

;;; Calculates entire bounds for LS which must be a list of
;;; bounds.
(define (bounds ls)
  (fold
   (lambda (next accumulated)
     (cond
      ((null? accumulated) next)
      ((null? next) accumulated)
      (else
       (let ((normalized-next (normalize-bounds next)))
         (cons
          (cons (min (bounds-left accumulated)
                     (bounds-left normalized-next))
                (min (bounds-bottom accumulated)
                     (bounds-bottom normalized-next)))
          (cons (max (bounds-right accumulated)
                     (bounds-right normalized-next))
                (max (bounds-top accumulated)
                     (bounds-top normalized-next)))
          )))))
   '()
   ls))
