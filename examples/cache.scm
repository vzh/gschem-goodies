;;; Maintain symbol cache
(use-modules (gschem goodies hook)      ; adds hooks for
                                        ; &file-save and
                                        ; &file-save-as actions
             (gschem symbol cache)      ; adds procedures for
                                        ; caching symbols
             )
;;; Enable symbol cache in the directory
(enable-symbol-cache "sym")

