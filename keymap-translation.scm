;;; Append translation to gschem keymap

;;; Hint: To define key names run xev.

(use-modules (ice-9 regex))

;;; Cyrillic jcuken keymap for gschem
(define en-ru-jcuken
  '(
    ("a" . "Cyrillic_ef")
    ("b" . "Cyrillic_i")
    ("c" . "Cyrillic_es")
    ("d" . "Cyrillic_ve")
    ("e" . "Cyrillic_u")
    ("f" . "Cyrillic_a")
    ("g" . "Cyrillic_pe")
    ("h" . "Cyrillic_er")
    ("i" . "Cyrillic_sha")
    ("j" . "Cyrillic_o")
    ("k" . "Cyrillic_el")
    ("l" . "Cyrillic_de")
    ("m" . "Cyrillic_softsign")
    ("n" . "Cyrillic_te")
    ("o" . "Cyrillic_shcha")
    ("p" . "Cyrillic_ze")
    ("q" . "Cyrillic_shorti")
    ("r" . "Cyrillic_ka")
    ("s" . "Cyrillic_yeru")
    ("t" . "Cyrillic_ie")
    ("u" . "Cyrillic_ghe")
    ("v" . "Cyrillic_em")
    ("w" . "Cyrillic_tse")
    ("x" . "Cyrillic_che")
    ("y" . "Cyrillic_en")
    ("z" . "Cyrillic_ya")
    ("bracketleft" . "Cyrillic_ha")
    ("braceleft" . "<Shift>Cyrillic_ha")
    ("bracketright" . "Cyrillic_hardsign")
    ("braceright" . "<Shift>Cyrillic_hardsign")
    ("semicolon" . "Cyrillic_zhe")
    ("colon" . "<Shift>Cyrillic_zhe")
    ("comma" . "Cyrillic_be")
    ("less" . "<Shift>Cyrillic_be")
    ("period" . "Cyrillic_yu")
    ("greater" . "<Shift>Cyrillic_yu")
    ("apostrophe" . "Cyrillic_e")
    ("quotedbl" . "<Shift>Cyrillic_e")
    ("grave" . "Cyrillic_io")
    ("asciitilde" . "<Shift>Cyrillic_io")
    ))

;;; Given KEYMAP and ALIST consisting of pairs of strings of the
;;; form (initial-key . translated-key), returns a new keymap in
;;; which every translated key carries out the same function as
;;; its initial key.
(define (keymap-add-translation #;to keymap #;from alist)
  ;; Regexp to match key with modifiers
  (define rx (make-regexp "(<[a-z<>]+>)([a-z]+)" regexp/icase))

  ;; Find translation for a KEY in ALIST
  (define (translate-key key #;using alist)
    (let* ((str (key->string key))
           (r (regexp-exec rx str))
           (mods (if r (match:substring r 1) ""))
           (mstr (if r (match:substring r 2) str))
           (sym (assoc-ref alist (string-downcase mstr))))
      (and sym
           (string->key (string-append mods sym)))))

  (let ((new-keymap (make-keymap)))
    (keymap-for-each
     (lambda (key bound)
       (let ((new-bound (if (keymap? bound)
                            (keymap-add-translation bound alist)
                            bound))
             (translated-key (translate-key key alist)))
         (keymap-bind-key! new-keymap key new-bound)
         (and translated-key
              (keymap-bind-key! new-keymap translated-key new-bound))))
     keymap)
    new-keymap))

;;; Auxiliary procedure for adding translation to the current
;;; gschem keymap from ALIST.
;;; Example usage: (current-keymap-add-translation! en-ru-jcuken)
(define (current-keymap-add-translation! #;from alist)
  (set! current-keymap
    (keymap-add-translation current-keymap alist)))
