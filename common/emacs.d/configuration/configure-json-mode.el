;; json-mode to edit files
(require 'json-mode)

(add-hook 'json-mode-hook
          (lambda ()
            (setq js-indent-level 2)))

(provide 'configure-json-mode)
