;; guide-key
(require 'guide-key)
(setq guide-key/guide-key-sequence '(;;org-mode
                                     "C-c C-a"
                                     "C-c C-s"
                                     "C-c C-i"

                                     ;;emacs-help
                                     "C-h"

                                     "C-c"

                                     "C-x"
                                     "C-x r"
                                     "C-x x"
                                     "C-x 4"
                                     "C-x 5"
                                     "C-x 8"
                                     "C-x @"

                                     ;; alchemist
                                     "C-c a"
                                     "C-c a c"
                                     "C-c a e"
                                     "C-c a h"
                                     "C-c a i"
                                     "C-c a m"
                                     "C-c a m t"
                                     "C-c a p"
                                     "C-c a v"

                                     ;; projectile
                                     "C-c p"))
(guide-key-mode 1)

(provide 'configure-guide-key)
