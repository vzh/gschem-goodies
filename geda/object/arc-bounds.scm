(define-module (geda object arc-bounds)
  #:export (arc-bounds)
  #:use-module (geda object)
  #:use-module (geda geometry bounds))

;;; Important definitions
(define pi (acos (- 1)))
(define radian/degree (/ pi 180))

;;; Converts degrees to radians.
(define-syntax degrees->radians
  (syntax-rules ()
    ((_ degrees) (* degrees radian/degree))))

;;; As the core guile library does not have appropriate procedures
;;; for work with degrees, we define them here.
(define (cos/dg degrees)
  (cos (degrees->radians degrees)))

(define (sin/dg degrees)
  (sin (degrees->radians degrees)))

;;; Peter B. confused end and sweep angles; redefine functions
(define arc-sweep-angle arc-end-angle)
(define (arc-end-angle arc)
  (+ (arc-start-angle arc) (arc-sweep-angle arc)))

;;; Takes START-ANGLE and SWEEP-ANGLE of an arc and returns a
;;; corresponding list of normalized angles in the form
;;;   (start-angle end-angle)
;;; Normalization of angles includes swap of the input start and
;;; end angles in order them to be positive if the sweep angle is
;;; negative and rotation of the resulted angles so that the start
;;; angle is in the range 0 .. 360 degrees.
(define (normalize-arc-angles start-angle sweep-angle)
  (define (rotate-angles start-angle end-angle)
    (let ((decrement
           (* 360 (euclidean-quotient start-angle 360))))
      (list (- start-angle decrement) (- end-angle decrement))))
  (if (negative? sweep-angle)
      ;; swap arguments if sweep angle is negative
      (rotate-angles (+ start-angle sweep-angle) start-angle)
      (rotate-angles start-angle (+ start-angle sweep-angle))))

;;; Checks if an arc defined by the pair of START-ANGLE and
;;; END-ANGLE, where the latter can be greater than 360 degrees,
;;; crosses ANGLE which must be in the range 0 .. 360 degrees on
;;; the coordinate space.
(define (arc-crosses-angle? angle
                            #;for-arc-defined-by
                            start-angle end-angle)
  (or (and (<= start-angle angle)
           (>= end-angle angle))
      (>= (- end-angle 360) angle)))

;;; Calculates extreme value for arc defined by its RADIUS,
;;; START-ANGLE and END-ANGLE along an axis in a given
;;; direction. If X? is #f, the value is calculated for the axis
;;; Y, and otherwise for X. If MIN? is equal to #f, the value is
;;; calculated for the positive direction of the axis, otherwise
;;; the negative direction is used.
(define (arc-extreme min?
                     #;along-axis
                     x?
                     #;for-arc-defined-by
                     radius start-angle end-angle)
  (let ((limit (if min? (- 1) 1))
        (extreme-func (if min? min max))
        (trigonometric-func (if x? cos/dg sin/dg))
        (check-angle (+ (if min? 180 0) (if x? 0 90)))
        )
    (inexact->exact
     (round
      (* radius
         (if (arc-crosses-angle? check-angle start-angle end-angle)
             limit
             (extreme-func (trigonometric-func start-angle)
                           (trigonometric-func end-angle))))))))

;;; Calculates arc bounds for non-zero and non-circular arcs
;;; defined by RADIUS, START-ANGLE and END-ANGLE.
(define (extreme-arc-bounds radius start-angle end-angle)
  (let ((min #t)
        (max #f)
        (x #t)
        (y #f))
    (cons
     (cons (arc-extreme min x radius start-angle end-angle)
           (arc-extreme min y radius start-angle end-angle))
     (cons (arc-extreme max x radius start-angle end-angle)
           (arc-extreme max y radius start-angle end-angle)))))

;;; Calculates arc bounds for arc OBJECT.
(define (arc-bounds object)
  (let* ((center (arc-center object))
         (radius (arc-radius object))
         (sweep-angle (arc-sweep-angle object)))
    (translate-bounds
     (cond
      ;; Zero sweep arc
      ((zero? sweep-angle) '())
      ;; Circular arc
      ((>= (abs sweep-angle) 360)
       (cons (cons (- radius) (- radius))
             (cons (+ radius) (+ radius))))
      ;; Normal arc
      (else (apply extreme-arc-bounds
             radius
             (normalize-arc-angles (arc-start-angle object)
                                   sweep-angle))))
     center)))
