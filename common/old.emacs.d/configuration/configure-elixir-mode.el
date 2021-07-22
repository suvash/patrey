;; Elixir mode conf


(require 'elixir-mode)

(add-hook 'elixir-mode-hook
      (lambda ()
        (setq indent-tabs-mode nil)))

(provide 'configure-elixir-mode)
