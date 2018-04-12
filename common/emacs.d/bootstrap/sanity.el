;;===============================================================================
;; DEFAULT SANE SETTINGS
;;===============================================================================

;; Hide splash-screen and startup-message
(setq inhibit-splash-screen t
      inhibit-startup-message t
      initial-scratch-message nil
      initial-major-mode 'org-mode)

;; Turn of UI bars forever
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)

;; Turn off ding
(setq visible-bell 0)

;; Turn off blinking cursor
(blink-cursor-mode 0)

;; Turn off ring bell function
(setq ring-bell-function 'ignore)

;; Turn off dialog boxes - use echo area
(setq use-dialog-box nil)

;; Turn on Line numbers and add a space
(global-linum-mode 1)
(setq linum-format "%d ")

;; Change yes-or-no to y-or-n
(defalias 'yes-or-no-p 'y-or-n-p)

;; No tabs only spaces
(setq tab-width 2
      indent-tabs-mode nil)

;; Display special characters
;; https://github.com/emacs-mirror/emacs/blob/master/lisp/whitespace.el#L283
(global-whitespace-mode 1)
(setq whitespace-style '(face trailing tabs empty))

;; Disable backup files and/or deal with it
(setq make-backup-files nil)
(setq backup-directory-alist `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms `((".*" ,temporary-file-directory t)))

;; sentence end config
(setq sentence-end-double-space nil)

;; prefer utf-8
(setq default-process-coding-system '(utf-8 . utf-8))
(prefer-coding-system 'utf-8)
(when (display-graphic-p)
  (setq x-select-request-type '(UTF8_STRING COMPOUND_TEXT TEXT STRING)))

;; use gpg2 instead of gpg
(setq epg-gpg-program "gpg")

;; set executable bit on executable buffers
(add-hook 'after-save-hook 'executable-make-buffer-file-executable-if-script-p)

;; Some Global keybindings
(global-set-key (kbd "RET") 'newline-and-indent)
(global-set-key (kbd "C-+") 'text-scale-increase)
(global-set-key (kbd "C--") 'text-scale-decrease)

(global-set-key (kbd "C-;") 'comment-or-uncomment-region)

(provide 'sanity)
