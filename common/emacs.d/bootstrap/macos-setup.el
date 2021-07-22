;;;==============================================================================
;;; MACOS SETUP
;;==============================================================================

;;; Code:

;;; Toggle FullScreen in MACOS
(defun toggle-fullscreen ()
  "Toggle full screen"
  (interactive)
  (set-frame-parameter
   nil 'fullscreen
   (when (not (frame-parameter nil 'fullscreen)) 'fullboth)))

;;; Copy-Paste to and from MACOS
(defun copy-from-macos ()
  (shell-command-to-string "pbpaste"))

(defun paste-to-macos (text &optional push)
  (let ((process-connection-type nil))
    (let ((proc (start-process "pbcopy" "*Messages*" "pbcopy")))
      (process-send-string proc text)
      (process-send-eof proc))))

;; copy-paste
(setq interprogram-cut-function 'paste-to-macos)
(setq interprogram-paste-function 'copy-from-macos)

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

;;; Set font face
(set-face-attribute 'default nil :font "Monaco-14")

(provide 'macos-setup)

;;; macos-setup ends here
