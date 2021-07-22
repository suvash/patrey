;; smex things

(smex-initialize)

(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "M-X") 'smex-major-mode-commands)

;; old M-x.
(global-set-key (kbd "C-c M-x") 'execute-extended-command)


(provide 'configure-smex)
