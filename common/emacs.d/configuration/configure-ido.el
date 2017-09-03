;; IDo configuration

;; use ido
(ido-mode 1)
(ido-everywhere 1)

;; with hacks
(require 'ido-hacks)

;; and flx
(require 'flx-ido)
(flx-ido-mode 1)

;; with everywhere
(require 'ido-completing-read+)
(ido-ubiquitous-mode 1)

;; Looks cleaner vertically
(require 'ido-vertical-mode)
(ido-vertical-mode 1)

(provide 'configure-ido)
