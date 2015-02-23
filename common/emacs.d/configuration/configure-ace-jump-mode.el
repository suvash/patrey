;; ace-jump-mode

(autoload
  'ace-jump-mode
  "ace-jump-mode"
  "Emacs quick move minor mode"
  t)
(define-key global-map (kbd "C-c SPC") 'ace-jump-mode)

(autoload
  'ace-jump-mode-pop-mark
  "ace-jump-mode"
  "Ace jump back:-)"
  t)
(define-key global-map (kbd "C-x SPC") 'ace-jump-mode-pop-mark)


(add-hook 'ace-jump-mode-after-jump-hook (lambda ()
                                            (message "I have jumped")))


(provide 'configure-ace-jump-mode)
