;; fix emacs env variables to be the same as in user shell
;; only on osx

(setq exec-path-from-shell-variables '("PATH"
                                       "MANPATH"
                                       "LANG"
                                       "LC_ALL"
                                       "SSH_AUTH_SOCK"))

(exec-path-from-shell-initialize)

(provide 'configure-exec-path-from-shell)
