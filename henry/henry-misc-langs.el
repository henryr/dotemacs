;; elisp
(add-hook 'emacs-lisp-mode-hook '(lambda () (progn
                                         (define-key emacs-lisp-mode-map "\C-c\C-r" 'eval-region)
                                         (define-key emacs-lisp-mode-map "\C-c\C-b" 'eval-buffer))))

;; (add-hook 'emacs-lisp-mode-hook '(lambda () 
;; 																	 (require 'autopair)
;; 																	 (autopair-mode)
;; 																	 ))


;; haskell
(add-to-list 'load-path "~/.emacs.d/haskell-mode")
(load "haskell-site-file")
(add-hook 'haskell-mode-hook 'turn-on-haskell-doc-mode)
(add-hook 'haskell-mode-hook 'turn-on-haskell-indentation)
(setq haskell-program-name "/usr/local/bin/ghci")

;; ngxml
(defun nxml-mode-indent-setup ()
	(setq nxml-child-indent 4)
	(setq indent-tabs mode nil))
(add-hook 'nxml-mode-hook 'nxml-mode-indent-setup)

;; erlang
(add-to-list 'load-path "~/.emacs.d/erlang")
(require 'erlang)

;; clojure
(add-to-list 'load-path "~/.emacs.d/clojure-mode")
(require 'clojure-mode)

;; scala
(add-to-list 'load-path "~/.emacs.d/scala-mode")
(require 'scala-mode)
(add-hook 'scala-mode-hook
          '(lambda ()
             (yas/minor-mode-on)))


;; mako
(add-to-list 'load-path "~/.emacs.d/mmm-mode")
(require 'mmm-auto)

;; Tell emacs to treat mako like html (close enough)
(add-to-list 'auto-mode-alist '("\\.mako$" . html-mode))
(mmm-add-mode-ext-class 'html-mode "\\.mako$" 'mako)

;; go
(require 'go-mode-load)

(provide 'henry-misc-langs)
