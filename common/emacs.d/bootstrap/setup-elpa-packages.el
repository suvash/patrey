;;===============================================================================
;; PACKAGES & REPOS
;;===============================================================================

(require 'package)
(add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/") t)
(add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/") t)
(package-initialize)

(when (not package-archive-contents)
  (package-refresh-contents))

(defvar my-packages '(exec-path-from-shell

                      zenburn-theme

                      projectile
                      perspective
                      persp-projectile

                      flx-ido
                      ido-hacks
                      ido-ubiquitous
                      ido-vertical-mode
                      smex
                      smart-mode-line

                      guide-key

                      evil
                      evil-leader

                      magit
                      git-gutter+
                      git-gutter-fringe+
                      git-timemachine

                      smooth-scrolling
                      rainbow-delimiters
                      linum-relative
                      smartparens

                      ag

                      flycheck
                      flycheck-color-mode-line

                      ace-jump-mode

                      company

                      twittering-mode
                      restclient

                      nix-mode

                      enh-ruby-mode
                      minitest

                      nyan-mode
                      markdown-mode
                      web-mode
                      haml-mode
                      json-mode
                      yaml-mode
                      thrift

                      sml-mode

                      clojure-mode
                      cider

                      haskell-mode
                      elm-mode

                      rust-mode

                      elixir-mode
                      alchemist)
  "A list of packages to ensure are installed at launch.")

(dolist (p my-packages)
  (when (not (package-installed-p p))
    (package-install p)))

;;===============================================================================
;; END
;;===============================================================================

(provide 'setup-elpa-packages)
