;; clojure mode

(add-hook 'clojure-mode-hook 'turn-on-smartparens-strict-mode)
(add-hook 'clojure-mode-hook 'subword-mode)
(add-hook 'clojure-mode-hook 'rainbow-delimiters-mode)

(provide 'configure-clojure-mode)
