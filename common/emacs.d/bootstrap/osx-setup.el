;;;==============================================================================
;;; OSX SETUP
;;==============================================================================

;;; Code:

;;; Toggle FullScreen in OSX
(defun toggle-fullscreen ()
  "Toggle full screen"
  (interactive)
  (set-frame-parameter
   nil 'fullscreen
   (when (not (frame-parameter nil 'fullscreen)) 'fullboth)))

;;; Copy-Paste to and from OSX
(defun copy-from-osx ()
  (shell-command-to-string "pbpaste"))

(defun paste-to-osx (text &optional push)
  (let ((process-connection-type nil))
    (let ((proc (start-process "pbcopy" "*Messages*" "pbcopy")))
      (process-send-string proc text)
      (process-send-eof proc))))

;; copy-paste
(setq interprogram-cut-function 'paste-to-osx)
(setq interprogram-paste-function 'copy-from-osx)

;; replace locate
(setq locate-command "mdfind")

;;; Remap mac keys
(setq mac-function-modifier 'hyper)
(setq mac-control-modifier 'control)
(setq mac-command-modifier 'meta)
(setq mac-option-modifier 'alt)
(setq mac-right-option-modifier 'super)
;;(setq mac-right-command-modifier nil)
;;(setq mac-right-control-modifier nil)

(provide 'osx-setup)

;;; osx-setup ends here
