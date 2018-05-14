;; flycheck-everywhere
(add-hook 'after-init-hook #'global-flycheck-mode)

;; flycheck-yamllint
(add-hook 'flycheck-mode-hook 'flycheck-yamllint-setup)

(provide 'configure-flycheck)
