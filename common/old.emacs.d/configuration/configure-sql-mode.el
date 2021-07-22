;; sql mode


(eval-after-load "sql"
  '(load-library "sql-indent"))

(add-hook 'sql-mode-hook 'sqlup-mode)
(add-hook 'sql-interactive-mode-hook 'sqlup-mode)

(provide 'configure-sql-mode)
