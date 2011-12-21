(add-hook 'java-mode-hook '(lambda () (setq c-basic-offset 2)))
(add-to-list 'load-path (expand-file-name "/Users/henry/src/emacs-eclim"))
;; only add the vendor path when you want to use the libraries provided with emacs-eclim
;; (add-to-list 'load-path (expand-file-name "/Users/henry/src/emacs-eclim/vendor"))
;; (require 'eclim)

;; (setq eclim-auto-save t)
;(global-eclim-mode)

(provide 'henry-java)
