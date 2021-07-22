;; restclient mode

(require 'restclient)

(add-to-list 'auto-mode-alist '("\\.restclient$" . restclient-mode))

(provide 'configure-restclient-mode)
