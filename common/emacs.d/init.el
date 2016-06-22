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
(require 'configure-dired)
(require 'configure-perspective)
(require 'configure-ido)
(require 'configure-smex)
(require 'configure-ace-jump-mode)
(require 'configure-smart-mode-line)
(require 'configure-guide-key)
(require 'configure-org)
(require 'configure-magit)
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
(require 'configure-aggressive-indent)
(require 'configure-yasnippet)

(require 'configure-twittering-mode)
(require 'configure-restclient-mode)

(require 'configure-nix-mode)

(require 'configure-enh-ruby-mode)
(require 'configure-rspec-mode)

(require 'configure-javascript-mode)
(require 'configure-json-mode)

(require 'configure-go-mode)

(require 'configure-clojure-mode)
(require 'configure-cider)

(require 'configure-sql-mode)

(require 'configure-elixir-mode)
(require 'configure-alchemist)

(require 'configure-haskell-mode)

(require 'configure-idris-mode)

(require 'configure-elm-mode)

(require 'configure-rust-mode)

(require 'configure-sml-mode)

(require 'configure-nyan-mode)
(require 'configure-markdown-mode)
(require 'configure-yaml-mode)
(require 'configure-haml-mode)
(require 'configure-web-mode)
(require 'configure-thrift-mode)
