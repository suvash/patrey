;; elm-mode

(require 'elm-mode)

(defun my/use-elm-if-in-node-modules ()
"Use elm if elm is found in this file's project's
node_modules. Use the elm binary from this project."
  (let* ((root (locate-dominating-file
                (or (buffer-file-name) default-directory)
                "node_modules"))
         (elm (and root
                      (expand-file-name "node_modules/elm/bin/elm"
                                        root))))
    (when (and elm (file-executable-p elm))
      (setq elm-compile-command (cons elm '("make"))))))

(defun my/use-elm-format-if-in-node-modules ()
"Use elm-format if elm-format is found in this file's project's
node_modules. Use the elm-format binary from this project."
  (let* ((root (locate-dominating-file
                (or (buffer-file-name) default-directory)
                "node_modules"))
         (elm-format (and root
                      (expand-file-name "node_modules/elm-format/bin/elm-format"
                                        root))))
    (when (and elm-format (file-executable-p elm-format))
      (setq elm-format-command elm-format))))


(add-hook 'elm-mode-hook #'my/use-elm-if-in-node-modules)
(add-hook 'elm-mode-hook #'my/use-elm-format-if-in-node-modules)
(add-hook 'elm-mode-hook 'elm-format-on-save-mode)

(provide 'configure-elm-mode)
