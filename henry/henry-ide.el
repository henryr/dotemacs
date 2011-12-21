;; Use the semantic speedbar, so contextual information can be seen in it
(require 'semantic/sb)
(global-set-key "\C-B" 'speedbar-get-focus)
;; speedbar in same window
(load "sr-speedbar.el")
(global-set-key (kbd "\C-x t") 'sr-speedbar-toggle)

;; ecb - emacs code browser. kind of useful.
;; (add-to-list 'load-path "~/.emacs.d/ecb-2.40")
;; (require 'ecb)

(provide 'henry-ide)
