;;; Licence: same as geda-gaf
;;; Author:  Vladimir Zhbanov vzhbanov@gmail.com
(use-modules (srfi srfi-1)
             (ice-9 match)
             (ice-9 regex)
             (ice-9 pretty-print))

;;; Regex to check if a value is in the correct RGB or RGBA format.
(define regex-hex (make-regexp "^#[0-9a-f]*$"))
;;; Regex to get hex numbers (two symbols wide) from a string.
(define regex-hexnum (make-regexp "[0-9a-f][0-9a-f]"))

;;; Translates color string S into the corresponding list of
;;; numbers of the form '(R G B A).
(define (color-string->rgba s)
  ;; Translates hex string match SM to number and adds it to the
  ;; list LS.
  (define (cons/hs sm ls)
    (cons (string->number (match:substring sm) 16) ls))
  (if s
      (or (and (regexp-exec regex-hex s)
            (reverse
             (case (string-length s)
               ;; #rrggbb
               ((7) (cons
                     255
                     (fold-matches regex-hexnum s '() cons/hs)))
               ;; #rrggbbaa
               ((9) (fold-matches regex-hexnum s '() cons/hs))
               (else #f))))
          (error "Incorrect color value" s))
      ;; if s is #f all components but alpha will be ff
      '(255 255 255 0)))

;;; Tweak COLOR-COMPONENT in the COLOR-VALUE by DELTA.
(define (tweak-color-component color-component color-value delta)
  (define (add-delta n delta)
    (let ((result (+ n delta)))
      (cond ((< result 0) 0)
            ((> result 255) 255)
            (else result))))
  (apply string-append "#"
         (map (lambda (x n)
                (let ((s (number->string
                          (if (eq? color-component x)
                              (add-delta n delta)
                              n)
                          16)))
                  (if (= (string-length s) 1)
                      (string-append "0" s)
                      (if (and (eq? x 'a) ; process alpha
                               (string=? s "ff"))
                          ;; Remove alpha info if it is not used
                          ""
                          s))))
              '(r g b a)
              (color-string->rgba color-value))))

;;; In the current implementation (as of geda-gaf v.1.9.2)
;;; (display-color-map) without arguments returns the current
;;; colormap, whereas (display-color-map color-map) sets the
;;; current colormap. The same is true for
;;; display-outline-color-map and print-color-map as well.  This
;;; procedure appends DELTA to the value of color COMPONENT (red,
;;; green, blue, or alpha) for color index NAME in COLOR-MAP.
;;; The changed color is reflected in the ATTRIB value.
(define (set-color! color-map name component delta attrib)
  (color-map
   (let loop ((ls (color-map))
              (counter 0))
     (match ls
       (((entry-name entry-value) . rest)
        (if (or (eq? entry-name name)
                (and (number? entry-name)
                     (= entry-name counter)))
            (let ((new-color (tweak-color-component component
                                                    entry-value
                                                    delta)))
              (set-attrib-value! attrib new-color)
              `((,entry-name ,new-color) . ,rest))
            `((,entry-name ,entry-value) .
              ,(loop rest (1+ counter))))))
     )))

;;; INDEX is a color index such as 'net, 'line, 'background,
;;; 'logic-bubble and so on, and COMPONENT is its color component,
;;; that is 'r for red, 'g for green, 'b for blue and 'a for
;;; alpha.
(define (tweak-color-map-value component delta)
  (let ((attrib (attrib-under-cursor)))
    (and attrib
         (let* ((index (string->symbol (attrib-name attrib))))
           (set-color! display-color-map index component delta attrib)))))

(define-syntax func-name
  (syntax-rules ()
    ((_ sym delta)
     (list
      (string->symbol
       (string-append
        (symbol->string sym) (number->string delta)))))))

;;; SYM can be one of R, G, B, or A.
;;; MOD can be, e.g. "<Shift>" or "<Ctrl><Shift>".
;;; DELTA is an increment (or a decrement, if negative) of color
;;; component value.
;;; Usage is:
;;;    (binddef "<Shift>" 'a -8)
;;; which results in:
;;;    (define (a-8) (tweak-color-map-value 'a -8) (&view-redraw))
;;;    (bind-keys! current-keymap "<Shift>A" a-8)
(define-syntax-rule (binddef mod sym delta)
  (and
   (primitive-eval
    `(define
       ,@(func-name sym delta)
       (lambda ()
         (tweak-color-map-value sym delta)
         (&view-redraw))))
   (primitive-eval
    `(bind-keys!
      current-keymap
      ,@(list (string-append mod (symbol->string sym)))
      ,@(func-name sym delta)))))

;;; Alpha is changed other way around compared with all other
;;; color values. If we increase transparency then we decrease
;;; alpha value.
(binddef "" 'a -8)
(binddef "<Shift>" 'a 8)
(binddef "<Ctrl>" 'a -1)
(binddef "<Ctrl><Shift>" 'a 1)
;;; red
(binddef "" 'r 8)
(binddef "<Shift>" 'r -8)
(binddef "<Ctrl>" 'r 1)
(binddef "<Ctrl><Shift>" 'r -1)
;;; green
(binddef "" 'g 8)
(binddef "<Shift>" 'g -8)
(binddef "<Ctrl>" 'g 1)
(binddef "<Ctrl><Shift>" 'g -1)
;;; blue
(binddef "" 'b 8)
(binddef "<Shift>" 'b -8)
(binddef "<Ctrl>" 'b 1)
(binddef "<Ctrl><Shift>" 'b -1)

;;; Origin for attributes is '(40000 . 40000) and the step by Y
;;; is 200
(define (calculate-coords n)
  `(40000 . ,(+ 40000 (* n 200))))

;;; Prepare attributes
(define color-attribs
  (let loop ((ls (display-color-map))
             (index 0)
             (als '()))
    (if (not (null? ls))
        (match ls
          (((name value) . rest)
           (loop rest
                 (1+ index)
                 (cons
                  (make-text
                   (calculate-coords index)    ; coords
                   'lower-left 0               ; alignment, angle
                   (string-append              ; text
                    (if (symbol? name)
                        (symbol->string name)
                        (number->string name))
                    "="
                    (if value value "#f"))
                   10 #t 'both                 ; size, vis, show
                   index)                      ; color index
                  als))))
        als)))

;;; Remove title page if it is here
(apply page-remove! (active-page) (page-contents (active-page)))
;;; Append attributes
(apply page-append! (active-page) color-attribs)

;;; Returns an attrib being under the mouse cursor or #f if none
;;; is found.
(define (attrib-under-cursor)
  (define (is-within? coord bounds)
    (match `(,coord . ,bounds)
      (((xc . yc) (x1 . y1) x2 . y2)
       (and (>= xc (min x1 x2))
            (<= xc (max x1 x2))
            (>= yc (min y1 y2))
            (<= yc (max y1 y2))))))
  (define (is-under-cursor? object)
    (and (is-within? (pointer-position)
                     (object-bounds object))
         object))
  (let loop ((ls color-attribs))
    (and (not (null? ls))
         (or (is-under-cursor? (car ls))
             (loop (cdr ls))))))

;;; File name where to save the modified colormap.
(define colormap-filename #f)
(define (save-colormap filename)
  (with-output-to-file filename
    (lambda () (pretty-print
                `(display-color-map ',(display-color-map))))))

(define (&save-colormap-as)
  (let ((filename (gschem-filesel "Select a colormap file..."
                                  "may_exist")))
    (set! colormap-filename filename)
    (save-colormap colormap-filename)))

(define (&save-colormap)
  (if colormap-filename
      (save-colormap colormap-filename)
      (&save-colormap-as)))

(bind-keys! current-keymap "s" &save-colormap)
(bind-keys! current-keymap "<Shift>s" &save-colormap-as)
(bind-keys! current-keymap "space" &edit-select-all)

(gschem-msg
 (format #f "
Hover the cursor over an attribute and
hit R (red), G (green), B (blue), or
A (alpha) to see the result. Try the
same with <Shift>, <Ctrl>, and
<Shift>+<Ctrl>. Some attributes may be
invisible so you may want to hit
spacebar in order to select them all.

Hit S to save the colormap you've
created. <Shift>+S will ask you if you
want to save to another file."))
