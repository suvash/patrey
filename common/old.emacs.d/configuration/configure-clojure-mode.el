;; clojure mode

(defun my-clojure-mode-hook ()
  (turn-on-smartparens-strict-mode)
  (subword-mode)
  (rainbow-delimiters-mode)
  (aggressive-indent-mode))

(add-hook 'clojure-mode-hook #'my-clojure-mode-hook)

(provide 'configure-clojure-mode)
