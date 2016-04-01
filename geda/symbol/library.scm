;;; Symbol library

(define-module (geda symbol library)
  #:use-module (ice-9 ftw)
  #:use-module (srfi srfi-1)
  #:use-module (geda os)
  #:export (%symbol-library
            reset-symbol-library!
            set-symbol-library!
            symbol-library-append!
            symbol-library-remove!
            reload-symbol-library!))

;;; Since libgeda doesn't export embedded procedures properly, we
;;; won't be polite with it, either.
;;; This temporary procedure will be replaced with a proper one
;;; some day.
(define geda-sym-path (@@ (guile-user) geda-sym-path))
(define build-path (@@ (guile-user) build-path))
(define component-library-append! (@@ (guile-user) component-library))
(define reset-component-library! (@@ (guile-user) reset-component-library))


(define default-lib-list
    '(
      ;; Generic symbols
      ("analog" . "Basic devices")
      ("connector" . "Connectors (generic)")
      ("diode" . "Diodes (generic)")
      ("io" . "Input/output (generic)")
      ("power" . "Power rails")
      ("radio" . "Radio elements (generic)")
      ("switch" . "Switches (generic)")
      ("titleblock" . "Titleblocks (generic)")
      ("IEC417" . "IEC 60417")
      ;; Common logic series
      ("74" . "74-series logic")
      ("4000" . "4000-series logic")
      ("ecl" . "ECL logic")
      ;; Simulation
      ("cascade" . "Cascade simulation elements")
      ("spice" . "SPICE simulation elements")
      ("switcap" . "SWITCAP simulation elements")
      ;; ASIC design
      ("asic" . "Basic devices (ASIC)")
      ("asicpads" . "Contact pads (ASIC)")
      ;; Manufacturers
      ("allegro" . "Allegro Microsystems")
      ("altera" . "Altera")
      ("amphenol" . "Connectors (Amphenol)")
      ("apex" . "Apex Microtechnology")
      ("dec" . "DEC")
      ("idt" . "IDT")
      ("irf" . "International Rectifier")
      ("lattice" . "Lattice Semiconductor")
      ("linear" . "Linear Technology")
      ("maxim" . "Maxim/Dallas")
      ("minicircuits" . "Mini-Circuits")
      ("national" . "National Semiconductor")
      ("philips" . "Philips Electronics")
      ("st" . "ST Microelectronics")
      ("xilinx" . "Xilinx")
      ;; Misc. stuff
      ("bus" . "PC104 bus")
      ("memory" . "Memory devices (misc)")
      ("micro" . "Microcontrollers (misc)")
      ("transistor" . "Transistors (misc)")
      ("tube" . "Vacuum tubes (misc)")
      ("rf" . "RF elements (misc)")
      ("pla" . "Programmable logic arrays (misc)")
      ("supervisor" . "Microprocessor supervisors (misc)")
      ("opto" . "Optocouplers (misc)")
      ("relay" . "Relays (misc)")
      ("misc" . "Misc. unsorted symbols")
      ))

(define (library-list-entry-directory x)
  (if (pair? x) (car x) x))

(define (library-list-entry-name x)
  (if (pair? x) (cdr x) ""))

(define (library-list-member x ls)
  (let loop ((dir (library-list-entry-directory x))
             (ls ls))
    (and (not (null? ls))
         (or (and
              (string=? dir
                        (library-list-entry-directory (car ls)))
                  (car ls))
             (loop dir (cdr ls))))))

(define (populate-default-dir!)
  (map
   (lambda (name)
     (let ((abs-dir-name (build-path geda-sym-path name))
           (entry (library-list-member name default-lib-list)))
       (if entry
           (cons abs-dir-name (cdr entry))
           abs-dir-name)
       ))
   (scandir geda-sym-path
            (lambda (name)
              (not (or (string=? "." name)
                       (string=? ".." name)))))))

(define %default-symbol-library (populate-default-dir!))
(define %symbol-library '())

(define (reset-symbol-library!)
  (reset-component-library!)
  (set! %symbol-library '()))

(define (set-symbol-library! ls)
  (reset-symbol-library!)
  (for-each
   (lambda (entry)
     (if (pair? entry)
         (component-library-append! (car entry) (cdr entry))
         (component-library-append! entry)))
   ls)
  (set! %symbol-library ls))

(define* (symbol-library-append! . args)
  (when (not (null? args))
    (for-each
     (lambda (entry)
     (if (pair? entry)
         (component-library-append! (car entry) (cdr entry))
         (component-library-append! entry)))
     args)
    (set! %symbol-library (append %symbol-library args))))


(define* (symbol-library-remove! . args)
  (when (not (null? args))
    (set-symbol-library!
     (filter-map
      (lambda (x)
        (and (not (library-list-member x args))
             x))
      %symbol-library))))

(define (reload-symbol-library!)
  (set-symbol-library! %symbol-library))

;;; Symbol library initialization
(set-symbol-library! %default-symbol-library)
