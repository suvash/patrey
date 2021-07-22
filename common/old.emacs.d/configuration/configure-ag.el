;; ag
(require 'ag)
(setq ag-highlight-search t
      ag-reuse-buffers t)

(define-key ag-mode-map (kbd "k") nil)
(define-key ag-mode-map (kbd "q") '(lambda () (interactive)
                                     (let (kill-buffer-query-functions) (kill-buffer))))
(provide 'configure-ag)
