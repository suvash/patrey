;; smartparens
(require 'smartparens-config)

(smartparens-global-mode)
(show-smartparens-global-mode t)

(add-hook 'smartparens-enabled-hook #'evil-smartparens-mode)

(provide 'configure-smartparens)
