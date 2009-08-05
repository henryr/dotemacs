(add-to-list 'load-path "~/.emacs.d/")

;; xelatex
(defun init-xelatex nil nil (interactive)
  (add-to-list 'LaTeX-command-style
	       '("\\`fontspec\\'" "xelatex %S%(PDFout)")))
(add-hook 'latex-mode-hook 'init-xelatex)

;; snippets
(add-to-list 'load-path "~/.emacs.d/yasnippet-0.5.10")
(require 'yasnippet)
(yas/initialize)
(yas/load-directory "~/.emacs.d/yasnippet-0.5.10/snippets")

(global-set-key "\C-x\C-y" 'yas/minor-mode)

;; ;cedet
;; ;(add-to-list 'load-path "~/.emacs.d/cedet-1.0pre6/common/")
;; ;(load "cedet.el")
;; ;(global-ede-mode 1)                      ; Enable the Project management system
;; ;(semantic-load-enable-code-helpers)      ; Enable prototype help and smart completion 
;; ;(global-srecode-minor-mode 1)            ; Enable template insertion menu

;; ;(add-to-list 'load-path "~/.emacs.d/ecb-2.40")
;; ;(require 'ecb)
;; ;(global-set-key "\C-b" 'speedbar-get-focus)

;; ;; python

; Cleanup whitespace on save
(defun my-whitespace-hook ()
	(let ((ext (file-name-extension buffer-file-truename)))
					(when (or (equal ext "erl")
										(equal ext "py"))
						(whitespace-cleanup))))

(add-hook 'before-save-hook 'my-whitespace-hook)

;; This is supposed to make return to newline-and-indent
(add-hook 'python-mode-hook '(lambda () (define-key python-mode-map "\C-m" 'newline-and-indent)))

;; This causes recursion (max-specpdl-depth) errors on seemingly harmless
;; functions (e.g. describe-function)
;; Seems to be fixed in new Aquamacs 2.0pre1 but that has its own issues!

;; (add-to-list 'load-path "~/.emacs.d/Pymacs-0.23/")
;; (autoload 'pymacs-apply "pymacs")
;; (autoload 'pymacs-call "pymacs")
;; (autoload 'pymacs-eval "pymacs" nil t)
;; (autoload 'pymacs-exec "pymacs" nil t)
;; (autoload 'pymacs-load "pymacs" nil t)

;; (pymacs-load "ropemacs" "rope-")

;(add-to-list 'load-path "~/.emacs.d/slime")
;(require 'slime-autoloads)
;(setq inferior-lisp-program "/usr/local/bin/openmcl") ;"/sw/bin/sbcl")
;(slime-setup)

;; git
(require 'git-emacs)

;; erlang
(add-to-list 'load-path "~/.emacs.d/erlang")
(require 'erlang)

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

;; java
;; jde

;(add-to-list 'load-path "~/.emacs.d/jde")
;(add-to-list 'load-path "~/.emacs.d/jde/lisp")
;(add-to-list 'load-path "~/.emacs.d/cedet/common")
;(add-to-list 'load-path (expand-file-name "~/.emacs.d/elib"))
;(load-file "~/.emacs.d/cedet/common/cedet.el")
;(require 'jde)
;;(load-library "ryan-java")

;; (defun load-jde nil nil (interactive)
;;   (load-file "~/.emacs.d/cedet/common/cedet.el")
;;   (require 'jde)
;;   (load-library "ryan-java"))
;; (add-hook 'java-mode-hook 'load-jde)

;;;(require 'jde-flymake)

;; line numbering
(require 'setnu)
(global-set-key "\C-n" 'setnu-mode)

;; misc key bindings
(global-set-key "\C-x\C-e" 'eval-buffer)
(global-set-key "\M-o" 'other-frame)	

(defun select-next-window ( )
  "Switch to the next window"
  (interactive)
  (select-window (next-window) ) )

(defun select-previous-window ( )
  "Switch to the previous window"
  (interactive)
  (select-window (previous-window) ) )

(global-set-key (kbd "C-M-<up>") 'windmove-up)
(global-set-key (kbd "C-M-<down>") 'windmove-down)
(global-set-key (kbd "C-M-<left>") 'windmove-left)
(global-set-key (kbd "C-M-<right>") 'windmove-right)

;; visuals

(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))

;; winring - save configurations of windows to a ring
(require 'winring)
(winring-initialize)

; irc
(setq erc-autojoin-channels-alist
          '(("freenode.net" "#emacs" "#hadoop" "#java" "#erlang" "#cloudera" "#python" "#algorithms")
            ("cloudera.com" "#eng" "#twitter" "#logs")))


(require 'my-irc)
  
(fset 'irc-windows
   [?\C-x ?r ?w ?b ?\C-x ?b ?e ?n ?g return ?\C-x ?2 ?\C-x ?3 ?\C-x ?o ?\C-x ?3 ?\C-x ?b ?t ?w ?i ?t ?t ?e ?r return ?\C-x ?o ?\C-x ?b ?l ?o ?g ?s return ?\C-x ?o ?\C-x ?3 ?\C-x ?b ?h ?a ?d ?o ?o ?p return ?\C-x ?o ?\C-x ?b ?c ?l ?o ?u ?d ?e ?r ?a return ?\C-x ?r ?w ?a ?\C-x ?r ?j ?b])

(defun jump-to-irc()
	(interactive)
	(frame-configuration-to-register ?b)
	(jump-to-register ?a))

(defun return-from-irc()
	(interactive)
	(jump-to-register ?b))

;(global-set-key (kbd "C-i") 'to-irc)
(global-set-key (kbd "M-i") 'jump-to-irc)
(global-set-key (kbd "M-q") 'return-from-irc)
		
; speedbar in same window
(load "sr-speedbar.el")
(global-set-key (kbd "C-M-t") 'sr-speedbar-toggle)