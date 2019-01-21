;; Anaconda mode

(add-hook 'python-mode-hook 'anaconda-mode)
(add-hook 'python-mode-hook 'anaconda-eldoc-mode)
(add-hook 'python-mode-hook 'blacken-mode)

(provide 'configure-python-mode)
