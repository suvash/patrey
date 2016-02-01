;; clojure mode

(require 'clj-refactor)

(defun my-clojure-mode-hook ()
  (turn-on-smartparens-strict-mode)
  (subword-mode)
  (rainbow-delimiters-mode)
  (aggressive-indent-mode)
  (clj-refactor-mode 1))

(add-hook 'clojure-mode-hook #'my-clojure-mode-hook)

(cljr-add-keybindings-with-prefix "C-c C-r")

(provide 'configure-clojure-mode)
