(use-modules (geda page)
             (geda object)
             (geda attrib)
             (srfi srfi-19))

;;; See guile info on how to change the date-time format
(define date-time-format "~Y-~m-~d ~H:~M")
(define date-time-attrib-name "modification-date")

;;; Modifies the necessary date-time attribute(s)
(define (update-modification-date! page)
  (define (floating? attrib)
    (not (attrib-attachment attrib)))

  (define (is-date-time-attrib? object)
    (and (attribute? object)
         (string=? (attrib-name object) date-time-attrib-name)
         ;; return only floating attributes
         (floating? object)
         object))

  (define (error-attrib-not-found)
    (gschem-msg
     (format #f "Attribute ~S not found." date-time-attrib-name))
    #f)

  (define (warning-many-attribs-found)
    (gschem-log
     (format #f "Several attributes ~S found." date-time-attrib-name)))

  (define (date-time-attrib-list page)
    (let ((attrib-list (filter-map
                        is-date-time-attrib?
                        (page-contents page))))
      (if (null? attrib-list)
          ;; no date-time attribute found
          (error-attrib-not-found)
          (begin
            (when (not (null? (cdr attrib-list)))
              ;; many attributes found, warn the user
              (warning-many-attribs-found))
            attrib-list))))

  (define (set-current-date-time-value! attrib)
    (set-attrib-value!
     attrib
     (date->string (current-date) date-time-format)))

  (let ((attrib-list (date-time-attrib-list page)))
    (and attrib-list
         (for-each set-current-date-time-value! attrib-list))))

;;; Save action procedures to use them later
(define &stock-file-save &file-save)
(define &stock-file-save-as &file-save-as)

;;; Redefine them
(define (&file-save)
  ;; update date-time attribute(s) if only the active page has
  ;; been changed
  (and (page-dirty? (active-page))
       (update-modification-date! (active-page)))
  (&stock-file-save))

(define (&file-save-as)
  ;; unconditionally update date-time attribute(s)
  (update-modification-date! (active-page))
  (&stock-file-save-as))
