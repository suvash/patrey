;; fix emacs env variables to be the same as in user shell
;; only on osx

(exec-path-from-shell-copy-env "SSH_AUTH_SOCK")

(provide 'configure-exec-path-from-shell)
