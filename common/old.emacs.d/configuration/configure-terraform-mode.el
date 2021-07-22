;; terraform mode file

(require 'terraform-mode)

(add-hook 'terraform-mode-hook #'terraform-format-on-save-mode)

(provide 'configure-terraform-mode)
