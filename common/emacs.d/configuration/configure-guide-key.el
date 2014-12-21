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

                                     ;; projectile
                                     "C-c p"))
(guide-key-mode 1)

(provide 'configure-guide-key)
