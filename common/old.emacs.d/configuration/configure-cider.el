;; cider-mode

(require 'cider)

(add-hook 'cider-mode-hook 'cider-turn-on-eldoc-mode)

(setq nrepl-hide-special-buffers t
      cider-repl-result-prefix ";; => "
      cider-interactive-eval-result-prefix ";; => ")

(defun cider-repl-command (cmd)
  "Execute commands on the cider repl"
  (cider-switch-to-repl-buffer)
  (goto-char (point-max))
  (insert cmd)
  (cider-repl-return)
  (cider-switch-to-last-clojure-buffer))

(defun cider-repl-reset ()
  "Assumes that tools.namespace is used to reload everything on
   the classpath (which is why we save buffers first)
   Checkout reloaded pattern if this doesn't make sense"
  (interactive)
  (save-some-buffers)
  (cider-repl-command "(user/reset)"))

(defun cider-reset-test-run-tests ()
  (interactive)
  (cider-repl-reset)
  (cider-test-run-tests))

(defun cider-reset-test-rerun-tests ()
  (interactive)
  (cider-repl-reset)
  (cider-test-rerun-tests))

(defun cider-reset-test-run-test ()
  (interactive)
  (cider-repl-reset)
  (cider-test-run-test))

(define-key cider-mode-map (kbd "C-c r") 'cider-repl-reset)
(define-key cider-mode-map (kbd "C-c .") 'cider-reset-test-run-tests)
(define-key cider-mode-map (kbd "C-c C-.") 'cider-reset-test-rerun-tests)
(define-key cider-mode-map (kbd "C-c M-.") 'cider-reset-test-run-test)

(provide 'configure-cider)
