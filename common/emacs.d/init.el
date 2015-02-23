;; Load Bootstrap Path
(add-to-list 'load-path "~/.emacs.d/bootstrap")

(require 'personalize)
(require 'sanity)
(when (string-equal system-type "darwin") (require 'osx-setup))
(when (string-equal system-type "gnu/linux") (require 'linux-setup))
(require 'setup-elpa-packages)

;; Load Config Path
(add-to-list 'load-path "~/.emacs.d/configuration")

(require 'configure-exec-path-from-shell)
(require 'configure-theme)
(require 'configure-projectile)
(require 'configure-perspective)
(require 'configure-ido)
(require 'configure-smex)
(require 'configure-ace-jump-mode)
(require 'configure-smart-mode-line)
(require 'configure-guide-key)
(require 'configure-org)
(require 'configure-git-timemachine)
(require 'configure-git-gutter)
(require 'configure-evil)
(require 'configure-ag)
(require 'configure-smooth-scrolling)
(require 'configure-linum)
(require 'configure-flycheck)
(require 'configure-rainbow)
(require 'configure-smartparens)
(require 'configure-company)
(require 'configure-golden-ratio)

(require 'configure-twittering-mode)
(require 'configure-restclient-mode)

(require 'configure-nix-mode)

(require 'configure-enh-ruby-mode)
(require 'configure-minitest)

(require 'configure-javascript-mode)
(require 'configure-json-mode)

(require 'configure-clojure-mode)
(require 'configure-cider)

(require 'configure-elixir-mode)
(require 'configure-alchemist)

(require 'configure-haskell-mode)

(require 'configure-elm-mode)

(require 'configure-rust-mode)

(require 'configure-sml-mode)

(require 'configure-nyan-mode)
(require 'configure-markdown-mode)
(require 'configure-yaml-mode)
(require 'configure-haml-mode)
(require 'configure-web-mode)
(require 'configure-thrift-mode)
