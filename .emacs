(add-to-list 'load-path "~/.emacs.d/")
(require 'semantic/sb)
(global-set-key "\C-B" 'speedbar-get-focus)
(add-to-list 'load-path "~/.emacs.d/mmm-mode")
(require 'mmm-auto)
(add-to-list 'load-path "~/.emacs.d/git-emacs")
(add-to-list 'exec-path "/usr/local/git/bin/")
(require 'git-emacs)

;; java
;; cedet
;(global-ede-mode 1)                      ; Enable the Project management system
;(semantic-load-enable-code-helpers)      ; Enable prototype help and smart completion 
;(global-srecode-minor-mode 1)            ; Enable template insertion menu

;(add-to-list 'load-path "~/src/emacs-eclim")
;(require 'eclim)

;; xelatex
;; (defun init-xelatex nil nil (interactive)
;;   (add-to-list 'LaTeX-command-style
;; 	       '("\\`fontspec\\'" "xelatex %S%(PDFout)")))
;; (add-hook 'latex-mode-hook 'init-xelatex)

;; snippets
(add-to-list 'load-path "~/.emacs.d/yasnippet-0.5.10")
(require 'yasnippet)
(yas/initialize)
(yas/load-directory "~/.emacs.d/yasnippet-0.5.10/snippets")
(yas/load-directory "~/.emacs.d/scala-mode/contrib/yasnippet")

;; (global-set-key "\C-x\C-y" 'yas/minor-mode)

(add-to-list 'load-path "~/.emacs.d/ecb-2.40")
(require 'ecb)

(global-set-key "\C-c r" 'revert-buffer)

; Cleanup whitespace on save
(defun my-whitespace-hook ()
	(let ((ext (file-name-extension buffer-file-truename)))
					(when (or (equal ext "erl")
										(equal ext "py")
										(equal ext "java")
										(equal ext "html")
										(equal ext "mako")
										(equal ext "jsp"))
						(delete-trailing-whitespace))))

(add-hook 'before-save-hook 'my-whitespace-hook)

(defun toggle-insert-line (arg)
  (interactive)
  (save-excursion
    (back-to-indentation)
    (if (string= (buffer-substring-no-properties (point) (line-end-position)) 
                 arg)
        (progn
          (delete-region (line-beginning-position) (line-end-position))
          (delete-blank-lines))
      (progn
        (indent-for-tab-command)
        (insert arg)
        (newline-and-indent)))))

;; python
(defun insert-ipdb ()
	(toggle-insert-line "import ipdb; ipdb.set_trace()"))

(defun insert-py-copyright ()
  (toggle-insert-line "#!/usr/bin/env python
# (c) Copyright 2010 Cloudera, Inc.  All rights reserved."))

;; This is supposed to make return to newline-and-indent, and for some reason
;; python has its own default variable
(add-hook 'python-mode-hook '(lambda () (progn 
 																					(define-key python-mode-map "\C-m" 'newline-and-indent)
 																					(setq python-indent 2))))

(add-hook 'python-mode-hook '(lambda () (define-key python-mode-map "\C-\M-d" 'insert-ipdb)))
(add-hook 'python-mode-hook '(lambda () (define-key python-mode-map "\C-\M-c" 'insert-py-copyright)))

(defun comment-smart ()
	(interactive)
  (if (not (region-active-p))
      (save-excursion
        (comment-or-uncomment-region (line-beginning-position) (line-end-position)))
    (let ((start (region-beginning))
          (end (region-end)))
          (save-excursion 
            (goto-char start)
            (comment-or-uncomment-region (line-beginning-position) end))))) 

(global-set-key "\M-;" 'comment-smart)

;; pyflakes
(when (load "flymake" t)
	(defun flymake-pyflakes-init ()
		(let* ((temp-file (flymake-init-create-temp-buffer-copy
											 'flymake-create-temp-inplace))
					 (local-file (file-relative-name
												temp-file
												(file-name-directory buffer-file-name))))
			(list "/usr/local/bin/pyflakes" (list local-file))))
	(add-to-list 'flymake-allowed-file-name-masks
							 '("\\.py\\'" flymake-pyflakes-init)))

(add-hook 'find-file-hook 'flymake-find-file-hook)

;; ;; General programming
(setq indent-tabs-mode nil)
(setq default-tab-width 2)
(setq whitespace-display-mappings '((space-mark ?\  [?\u00B7]) (newline-mark ?\n [?$ ?\n]) (tab-mark ?\t [?\u00BB ?\t])))

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
(add-to-list 'auto-mode-alist '("\\.mako$" . html-mode))
(mmm-add-mode-ext-class 'html-mode "\\.mako$" 'mako)


;; ido
(require 'ido)
(ido-mode t)
(setq ido-enable-flex-matching t)

;; Snippet to add ido to command completion
(setq ido-execute-command-cache nil)
(defun ido-execute-command ()
  (interactive)
  (call-interactively
   (intern
    (ido-completing-read
     "M-x "
     (progn
       (unless ido-execute-command-cache
         (mapatoms (lambda (s)
                     (when (commandp s)
                       (setq ido-execute-command-cache
                             (cons (format "%S" s) ido-execute-command-cache))))))
       ido-execute-command-cache)))))

(add-hook 'ido-setup-hook
	  (lambda ()
	    (setq ido-enable-flex-matching t)
	    (global-set-key "\M-x" 'ido-execute-command)))

;; visuals
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(setq inhibit-startup-message t)
(set-default-font "Courier")

(add-to-list 'load-path "~/.emacs.d/color-theme-6.6.0/")
(require 'color-theme)
(eval-after-load "color-theme"
  '(progn
     (color-theme-initialize)
     (color-theme-hober)))

		
;; ; speedbar in same window
(load "sr-speedbar.el")
(global-set-key (kbd "\C-x t") 'sr-speedbar-toggle)

;; Multi-term
(require 'multi-term)
(setq multi-term-program "/Users/henry/.emacs-zsh")

(message "Finished loading .emacs")
;; ; Cloudera
;; (load "cloudera.el")
;; (put 'narrow-to-region 'disabled nil)
(custom-set-variables
  ;; custom-set-variables was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(ecb-options-version "2.40")
 '(ecb-use-speedbar-instead-native-tree-buffer (quote dir)))
(custom-set-faces
  ;; custom-set-faces was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 )
