(let ((file (expand-file-name (concat "~/.emacs.d/bootstrap/scratch-messages/" (user-login-name) ".el"))))
  (when (file-exists-p file)
    (setq initial-scratch-message
          (with-temp-buffer
            (insert-file-contents file)
            (buffer-string)))))

(provide 'setup-scratch-message)
