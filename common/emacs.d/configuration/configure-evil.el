;; evil
(require 'evil)
(evil-mode 1)

;; j/k in wrapped lines too, else it'll jump
(define-key evil-normal-state-map (kbd "j") 'evil-next-visual-line)
(define-key evil-normal-state-map (kbd "k") 'evil-previous-visual-line)
(define-key evil-normal-state-map (kbd "SPC") 'other-window)

;; don't move the cursor back when back to normal mode
(setq evil-move-cursor-back nil)

;; different cursor colors on different state
(setq evil-emacs-state-cursor '("white" box))
(setq evil-normal-state-cursor '("yellow" box))
(setq evil-insert-state-cursor '("green" bar))
(setq evil-visual-state-cursor '("orange" box))
(setq evil-replace-state-cursor '("red" box))
(setq evil-operator-state-cursor '("red" hollow))

;; evil-leader
(require 'evil-leader)
(global-evil-leader-mode)
(evil-leader/set-leader ",")
(evil-leader/set-key
  "po" 'projectile-switch-project
  "pk" 'projectile-kill-buffers

  "x" 'projectile-find-file-other-window

  "d" 'projectile-dired
  "a" 'projectile-ag
  "r" 'deadgrep

  "ji" 'dumb-jump-go
  "jo" 'dumb-jump-back

  "b" 'switch-to-buffer
  "k" 'kill-this-buffer

  "s" 'eshell
  "w" 'delete-trailing-whitespace
  "g" 'magit-status)

;;disable evil in certain modes
(evil-set-initial-state 'dired-mode 'emacs)

(evil-set-initial-state 'git-commit-mode 'insert)

(evil-set-initial-state 'nrepl-mode 'insert)
(evil-set-initial-state 'cider-repl-mode 'insert)

(evil-set-initial-state 'eshell-mode 'insert)
(evil-set-initial-state 'shell-mode 'insert)
(evil-set-initial-state 'term-mode 'insert)

(provide 'configure-evil)
