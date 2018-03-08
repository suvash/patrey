;; awesome modeline

(require 'smart-mode-line)

(setq sml/no-confirm-load-theme t)

(sml/setup)
(sml/apply-theme 'respectful)

(add-to-list 'sml/replacer-regexp-list '("^~/Developer/scaffold/" ":Scaffold:") t)

(add-to-list 'sml/replacer-regexp-list '("^~/1928/" ":1928:") t)
(add-to-list 'sml/replacer-regexp-list '("^~/Projects/" ":Projects:") t)

(provide 'configure-smart-mode-line)
