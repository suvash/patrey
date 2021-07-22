;; Python mode

(elpy-enable)
(add-hook 'python-mode-hook 'blacken-mode)

(add-hook 'before-save-hook 'py-isort-before-save)

(provide 'configure-python-mode)
