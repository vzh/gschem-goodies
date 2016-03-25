; Script for converting gschem schematic/symbol into Guile script, suitable
; for generating code which can be used to generate sets of symbols

(use-modules (geda page))
(use-modules (geda object))

; Load files from current directory
(add-to-load-path ".")
; io procedures
(load-from-path "schrw.scm")

(define scale 1)

(define (scale-number number)
  (/ number scale))

(define (scale-coord pair)
  (cons (scale-number (car pair)) (scale-number (cdr pair))))

(define (scale-two l)
  (apply list (scale-coord (car l)) (scale-coord (cadr l)) (cddr l)))

(define (scale-one-and-num l)
  (apply list (scale-coord (car l)) (scale-number (cadr l)) (cddr l)))

(define (scale-one l)
  (cons (scale-coord (car l)) (cdr l)))

; Redefine info functions
(define old-line-info line-info)
(define (line-info obj) (scale-two (old-line-info obj)))
(define old-box-info box-info)
(define (box-info obj) (scale-two (old-box-info obj)))
(define old-picture-info picture-info)
(define (picture-info obj)
  (cons
    (car (old-picture-info obj))
    (scale-two (cdr (old-picture-info obj)))))

(define old-arc-info arc-info)
(define (arc-info obj) (scale-one-and-num (old-arc-info obj)))
(define old-circle-info circle-info)
(define (circle-info obj) (scale-one-and-num (old-circle-info obj)))

(define old-component-info component-info)
(define (component-info obj)
  (cons
    (car (old-component-info obj))
    (scale-one (cdr (old-component-info obj)))))

(define old-text-info text-info)
(define (text-info obj) (scale-one (old-text-info obj)))

(define old-path-ref path-ref)
(define (path-ref obj N)
  (map (lambda (elem) (if (pair? elem) (scale-coord elem) elem)) (old-path-ref obj N)))

(define (get-dash-string dash)
  (case (car dash)
    ((solid)  (apply format #f "'~A" dash))
    ((dotted) (apply format #f "'~A ~A"    dash))
    ; others (dashed, center, phantom) take two parameters
    (else     (apply format #f "'~A ~A ~A" dash)))
  )

(define (get-fill-string fill)
  (case (car fill)
    ((hollow solid)  (apply format #f "'~A" fill))
    ((hatch) (apply format #f "'~A ~A ~A ~A"   fill))
    ; mesh takes five parameters
    (else     (apply format #f "'~A ~A ~A ~A ~A ~A" fill)))
  )

(define (format-dash s obj)
  (format #f "(set-object-stroke! ~A ~A '~6A ~A)"
    s
    (object-stroke-width obj)
    (object-stroke-cap   obj)
    (get-dash-string (object-stroke-dash  obj))))

(define (format-fill s obj)
  (format #f "(set-object-fill! ~A ~A)"
    s
    (get-fill-string (object-fill  obj))))

(define (output-arc     obj)
  (let ((arc-string (apply format #f "(make-arc '~15A ~A ~A ~A ~A)" (arc-info obj))))
    (format-dash arc-string obj)))

(define (output-box     obj)
  (let ((box-string (apply format #f "(make-box '~15A '~15A ~A)" (box-info obj))))
    (format-fill
      (format-dash box-string obj) obj)))

(define (output-bus     obj)
  (apply format #f "(make-bus '~15A '~15A ~A)" (line-info obj)))

(define (output-circle  obj)
  (let ((circle-string (apply format #f "(make-circle '~15A ~A ~A)" (circle-info obj))))
    (format-fill
      (format-dash circle-string obj) obj)))

(define (output-complex obj)
  (apply format #f "(make-component/library ~S '~15A ~A ~A ~A)" (component-info obj)))

(define (output-line    obj)
  (let ((line-string (apply format #f "(make-line '~15A '~15A ~A)" (line-info obj))))
    (format-dash line-string obj)))

(define (output-net     obj)
  (apply format #f "(make-net '~15A '~15A ~A)" (line-info obj)))

(define (output-path    obj)
  (format-fill
    (format-dash
      (let ((s (format #f "\n(let ((P (make-path ~A)))\n" (object-color obj)))
            (a "")
            (K (path-length obj))
            (N 0))
        (while (< N K)
          (case (car (path-ref obj N))
            ((moveto lineto)
                         (set! a (apply format #f "  (path-insert! P -1 '~8A '~15A)" (path-ref obj N))))
            ((curveto)   (set! a (apply format #f "  (path-insert! P -1 '~8A '~A '~A '~A)" (path-ref obj N))))
            ((closepath) (set! a                  "  (path-insert! P -1 'closepath)" ))
            )
          (set! s (string-append s a "\n"))
          (set! N (1+ N)))
        (set! s (string-append s ")"))
        s)
      obj)
    obj))

(define (output-picture obj)
  (apply format #f "(make-picture/vector (bytevector->sint-list (get-bytevector-all (open-file ~S \"rb\")) (native-endianness) 1) ~S '~15A '~15A ~A ~A)" (picture-filename obj) (picture-info obj)))

(define (output-pin     obj)
  (if (net-pin? obj)
    (apply format #f "(make-net-pin '~15A '~15A ~A)" (line-info obj))
    (apply format #f "(make-bus-pin '~15A '~15A ~A)" (line-info obj))
  ))

(define (output-text    obj)
  (apply format #f "(make-text '~15A '~15A ~A ~S ~A ~A '~A ~A)" (text-info obj)))

(define (output-scheme-string obj)
  (begin
    (display "  ")
    (display
      (case (object-type obj)
        ; all output-* functions must return strings
        ('arc     (output-arc     obj))
        ('box     (output-box     obj))
        ('bus     (output-bus     obj))
        ('circle  (output-circle  obj))
        ('complex (output-complex obj))
        ('line    (output-line    obj))
        ('net     (output-net     obj))
        ('path    (output-path    obj))
        ('picture (output-picture obj))
        ('pin     (output-pin     obj))
        ('text    (output-text    obj))
        ))
    (newline)))

(define (main)
  (if (< (length (command-line)) 3)
    (error "You must give at least 2 file names"))
  (let ((input (cadr (command-line)))
        (output (caddr (command-line))))
    (set! scale (or (and (eq? (length (command-line)) 4)
                         (string->number (list-ref (command-line) 3)))
                    1))
    (with-output-to-file output
      (lambda ()
        ;header
        (display "(use-modules (geda page))\n")
        (display "(use-modules (geda object))\n")
        (display "(use-modules (rnrs io ports))\n")
        (display "(use-modules (rnrs bytevectors))\n")
        (newline)
        (if scale
          (format #t
"
; Redefine some procedures to support scale (points/mm)
; default scale is 1
(define scale ~A)
(define (scale-number number)
  (* number scale))

(define (scale-coord pair)
  (cons (scale-number (car pair)) (scale-number (cdr pair))))

(define (scale-two l)
  (apply list (scale-coord (car l)) (scale-coord (cadr l)) (cddr l)))

(define (scale-one l)
  (cons (scale-coord (car l)) (cdr l)))

; Redefine info functions
(define old-make-line make-line)
(define (make-line line-start line-end color)
  (old-make-line (scale-coord line-start) (scale-coord line-end) color))
(define old-make-net make-net)
(define (make-net net-start net-end color)
  (old-make-net (scale-coord net-start) (scale-coord net-end) color))
(define old-make-net-pin make-net-pin)
(define (make-net-pin net-pin-start net-pin-end color)
  (old-make-net-pin (scale-coord net-pin-start) (scale-coord net-pin-end) color))
(define old-make-bus make-bus)
(define (make-bus bus-start bus-end color)
  (old-make-bus (scale-coord bus-start) (scale-coord bus-end) color))
(define old-make-bus-pin make-bus-pin)
(define (make-bus-pin bus-pin-start bus-pin-end color)
  (old-make-bus-pin (scale-coord bus-pin-start) (scale-coord bus-pin-end) color))
(define old-make-box make-box)
(define (make-box top-left bottom-right color)
  (old-make-box (scale-coord top-left) (scale-coord bottom-right) color))
(define old-make-picture/vector make-picture/vector)
(define (make-picture/vector vector filename top-left bottom-right angle mirror)
  (old-make-picture/vector vector filename (scale-coord top-left) (scale-coord bottom-right) angle mirror))
(define old-make-arc make-arc)
(define (make-arc center radius start-angle end-angle color)
  (old-make-arc (scale-coord center) (scale-number radius) start-angle end-angle color))
(define old-make-circle make-circle)
(define (make-circle center radius color)
  (old-make-circle (scale-coord center) (scale-number radius) color))
(define old-make-component make-component)
(define (make-component basename position angle mirror locked)
  (old-make-component basename (scale-coord position) angle mirror locked))
(define old-make-component/library make-component/library)
(define (make-component/library basename position angle mirror locked)
  (old-make-component/library basename (scale-coord position) angle mirror locked))
(define old-make-text make-text)
(define (make-text anchor align angle string size visible show color)
  (old-make-text (scale-coord anchor) align angle string size visible show color))
(define old-path-insert! path-insert!)
(define (path-insert! p idx type . points)
  (apply old-path-insert! p idx type (map (lambda (point) (scale-coord point)) points)))
"
            scale))

        (display "(define page (make-page \"new\"))\n")
        ;; object list begin
        (display "(define object-list (list\n")

        ;body
        (for-each output-scheme-string (page-contents (schematic-file->page input)))

        ;footer
        ;;object list end
        (display "))\n\n")
        (display "(apply page-append! page object-list)\n")

        (display "(with-output-to-file \"tmp.sym\" (lambda () (display (page->string page))))")
        ))))

(main)
