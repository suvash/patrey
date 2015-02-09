;; awesome modeline

(require 'smart-mode-line)

(setq sml/no-confirm-load-theme t)

(sml/setup)
(sml/apply-theme 'respectful)

(add-to-list 'sml/replacer-regexp-list '("^~/Developer/scaffold/" ":Scaffold:") t)

;; ss things
(add-to-list 'sml/replacer-regexp-list '("^~/Saltside/platform-core-service" ":SS-Core:") t)

(provide 'configure-smart-mode-line)
