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

                                     ;; smart-parens
                                     "C-M-s"

                                     ;; clj-refactor
                                     "C-c C-r"
                                     "C-c C-r a"
                                     "C-c C-r c"
                                     "C-c C-r d"
                                     "C-c C-r e"
                                     "C-c C-r f"
                                     "C-c C-r h"
                                     "C-c C-r i"
                                     "C-c C-r m"
                                     "C-c C-r p"
                                     "C-c C-r r"
                                     "C-c C-r s"
                                     "C-c C-r t"
                                     "C-c C-r u"

                                     ;; projectile
                                     "C-c p"))
(guide-key-mode 1)

(provide 'configure-guide-key)
