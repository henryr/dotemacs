(add-to-list 'load-path "~/.emacs.d/")

;; ido - fuzzy string-matching buffer completion
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

;; Use the semantic speedbar, so contextual information can be seen in it
(require 'semantic/sb)
(global-set-key "\C-B" 'speedbar-get-focus)
;; speedbar in same window
(load "sr-speedbar.el")
(global-set-key (kbd "\C-x t") 'sr-speedbar-toggle)

;; git-emacs, better git support
(add-to-list 'load-path "~/.emacs.d/git-emacs")
(add-to-list 'exec-path "/usr/local/git/bin/")
(require 'git-emacs)

;; xelatex
(defun init-xelatex nil nil (interactive)
  (add-to-list 'LaTeX-command-style
	       '("\\`fontspec\\'" "xelatex %S%(PDFout)")))
(add-hook 'latex-mode-hook 'init-xelatex)

;; snippets - like textmate, type a keyword then tab to expand to a template
(add-to-list 'load-path "~/.emacs.d/yasnippet-0.5.10")
(require 'yasnippet)
(yas/initialize)
(yas/load-directory "~/.emacs.d/yasnippet-0.5.10/snippets")
(yas/load-directory "~/.emacs.d/scala-mode/contrib/yasnippet")

;; ecb - emacs code browser. kind of useful. 
(add-to-list 'load-path "~/.emacs.d/ecb-2.40")
(require 'ecb)

;; Very handy after git commits which change files underneath emacs
(global-set-key "\C-cr" 'revert-buffer)

;; Cleanup whitespace on save
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

;; Inserts a string at the line where the point is, unless it is
;; already there in which case it removes it
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
  (interactive)
	(toggle-insert-line "import ipdb; ipdb.set_trace()"))

(defun insert-py-copyright ()
  (interactive)
  (insert "#!/usr/bin/env python
# (c) Copyright 2010 Cloudera, Inc.  All rights reserved."))

;; This is supposed to make return to newline-and-indent, and for some
;; reason python has its own default variable
(add-hook 'python-mode-hook '(lambda () (progn 
 																					(define-key python-mode-map "\C-m" 'newline-and-indent)
 																					(setq python-indent 2))))

(add-hook 'python-mode-hook '(lambda () (define-key python-mode-map "\C-\M-d" 'insert-ipdb)))
(add-hook 'python-mode-hook '(lambda () (define-key python-mode-map "\C-\M-c" 'insert-py-copyright)))

;; pyflakes
;; This is slightly buggy - sometimes leaves extra files around.
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
(global-set-key [f5] 'flymake-goto-prev-error)
(global-set-key [f6] 'flymake-goto-next-error)

;; pymacs and rope
(add-to-list 'load-path "~/.emacs.d/Pymacs-0.23")
(require 'pymacs)
(setq ropemacs-enable-shortcuts nil)
(setq ropemacs-local-prefix "C-c C-p")
(pymacs-load "ropemacs" "rope-")

;; General programming
(setq indent-tabs-mode nil)
(setq default-tab-width 2)

;; Either comments the region you have selected, or the current line
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

;; Fix display of whitespace characters
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
(add-to-list 'load-path "~/.emacs.d/mmm-mode")
(require 'mmm-auto)

;; Tell emacs to treat mako like html (close enough)
(add-to-list 'auto-mode-alist '("\\.mako$" . html-mode))
(mmm-add-mode-ext-class 'html-mode "\\.mako$" 'mako)

;; visuals
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(setq inhibit-startup-message t)
(set-default-font "Courier")
		
;; Mac-specific fullscreen mode
(global-set-key "\C-c\C-f" 'ns-toggle-fullscreen)

;; Multi-term
(require 'multi-term)
(setq multi-term-program "/Users/henry/.emacs-zsh")

;; Pretty colour theme
(defun color-theme-henry (&optional preview)
  (interactive)
  (color-theme-install
   '(color-theme-henry
     ((foreground-color . "#ffffff")
      (background-color . "#202020")
      (mouse-color . "black")
      (cursor-color . "medium turquoise")
      (border-color . "black")
      (background-mode . dark))
     (default ((t (nil))))
     (modeline ((t (:foreground "white" :background "darkslateblue"))))
     (modeline-buffer-id ((t (:foreground "white" :background "darkslateblue"))))
     (modeline-mousable ((t (:foreground "white" :background "darkslateblue"))))
     (modeline-mousable-minor-mode ((t (:foreground "white" :background "darkslateblue"))))
     (highlight ((t (:foreground "black" :background "#c0c0c0"))))
     (bold ((t (:bold t))))
     (italic ((t (:italic t))))
     (bold-italic ((t (:bold t :italic t))))
     (region ((t (:foreground "white" :background "darkslateblue"))))
     (zmacs-region ((t (:foreground "white" :background "darkslateblue"))))
     (secondary-selection ((t (:background "paleturquoise"))))
     (underline ((t (:underline t))))
     (diary-face ((t (:foreground "red"))))
     (calendar-today-face ((t (:underline t))))
     (holiday-face ((t (:background "pink"))))
     (widget-documentation-face ((t (:foreground "dark green" :background "white"))))
     (widget-button-face ((t (:bold t))))
     (widget-button-pressed-face ((t (:foreground "red" :background "black"))))
     (widget-field-face ((t (:background "gray85" :foreground "black"))))
     (widget-single-line-field-face ((t (:background "gray85" :foreground "black"))))
     (widget-inactive-face ((t (:foreground "dim gray" :background "red"))))
     (fixed ((t (:bold t))))
     (excerpt ((t (:italic t))))
     (term-default-fg ((t (nil))))
     (term-default-bg ((t (nil))))
     (term-default-fg-inv ((t (nil))))
     (term-default-bg-inv ((t (nil))))
     (term-bold ((t (:bold t))))
     (term-underline ((t (:underline t))))
     (term-invisible ((t (nil))))
     (term-invisible-inv ((t (nil))))
     (term-white ((t (:foreground "#c0c0c0"))))
     (term-whitebg ((t (:background "#c0c0c0"))))
     (term-black ((t (:foreground "black"))))
     (term-blackbg ((t (:background "black"))))
     (term-red ((t (:foreground "#ef8171"))))
     (term-redbg ((t (:background "#ef8171"))))
     (term-green ((t (:foreground "#e5f779"))))
     (term-greenbg ((t (:background "#e5f779"))))
     (term-yellow ((t (:foreground "#fff796"))))
     (term-yellowbg ((t (:background "#fff796"))))
     (term-blue ((t (:foreground "#4186be"))))
     (term-bluebg ((t (:background "#4186be"))))
     (term-magenta ((t (:foreground "#ef9ebe"))))
     (term-magentabg ((t (:background "#ef9ebe"))))
     (term-cyan ((t (:foreground "#71bebe"))))
     (term-cyanbg ((t (:background "#71bebe"))))
     (font-lock-keyword-face ((t (:foreground "#00ffff"))))
     (font-lock-comment-face ((t (:foreground "Red"))))
     (font-lock-string-face ((t (:foreground "#ffff00"))))
     (font-lock-constant-face ((t (:foreground "#00ff00"))))
     (font-lock-builtin-face ((t (:foreground "#ffaa00"))))
     (font-lock-type-face ((t (:foreground "Coral"))))
     (font-lock-warning-face ((t (:foreground "Red" :bold t))))
     (font-lock-function-name-face ((t (:foreground "#4186be"))))
     (font-lock-variable-name-face ((t (:foreground "white" :bold t))))
     (message-header-to-face ((t (:foreground "#4186be" :bold t))))
     (message-header-cc-face ((t (:foreground "#4186be"))))
     (message-header-subject-face ((t (:foreground "#4186be" :bold t))))
     (message-header-newsgroups-face ((t (:foreground "Coral" :bold t))))
     (message-header-other-face ((t (:foreground "steel blue"))))
     (message-header-name-face ((t (:foreground "white"))))
     (message-header-xheader-face ((t (:foreground "blue"))))
     (message-separator-face ((t (:foreground "brown"))))
     (message-cited-text-face ((t (:foreground "white"))))
     (gnus-header-from-face ((t (:foreground "Coral"))))
     (gnus-header-subject-face ((t (:foreground "#4186be"))))
     (gnus-header-newsgroups-face ((t (:foreground "#4186be" :italic t))))
     (gnus-header-name-face ((t (:foreground "white"))))
     (gnus-header-content-face ((t (:foreground "#4186be" :italic t))))
     (gnus-cite-attribution-face ((t (:italic t))))
     (gnus-cite-face-list ((t (:bold nil :foreground "red"))))
     (gnus-group-news-1-face ((t (:foreground "ForestGreen" :bold t))))
     (gnus-group-news-1-empty-face ((t (:foreground "ForestGreen"))))
     (gnus-group-news-2-face ((t (:foreground "CadetBlue4" :bold t))))
     (gnus-group-news-2-empty-face ((t (:foreground "CadetBlue4"))))
     (gnus-group-news-3-face ((t (:bold t))))
     (gnus-group-news-3-empty-face ((t (nil))))
     (gnus-group-news-low-face ((t (:foreground "DarkGreen" :bold t))))
     (gnus-group-news-low-empty-face ((t (:foreground "DarkGreen"))))
     (gnus-group-mail-1-face ((t (:foreground "DeepPink3" :bold t))))
     (gnus-group-mail-1-empty-face ((t (:foreground "DeepPink3"))))
     (gnus-group-mail-2-face ((t (:foreground "HotPink3" :bold t))))
     (gnus-group-mail-2-empty-face ((t (:foreground "HotPink3"))))
     (gnus-group-mail-3-face ((t (:foreground "magenta4" :bold t))))
     (gnus-group-mail-3-empty-face ((t (:foreground "magenta4"))))
     (gnus-group-mail-low-face ((t (:foreground "DeepPink4" :bold t))))
     (gnus-group-mail-low-empty-face ((t (:foreground "DeepPink4"))))
     (gnus-summary-selected-face ((t (:underline t))))
     (gnus-summary-cancelled-face ((t (:foreground "yellow" :background "black"))))
     (gnus-summary-high-ticked-face ((t (:foreground "firebrick" :bold t))))
     (gnus-summary-low-ticked-face ((t (:foreground "firebrick" :italic t))))
     (gnus-summary-normal-ticked-face ((t (:foreground "firebrick"))))
     (gnus-summary-high-ancient-face ((t (:foreground "RoyalBlue" :bold t))))
     (gnus-summary-low-ancient-face ((t (:foreground "RoyalBlue" :italic t))))
     (gnus-summary-normal-ancient-face ((t (:foreground "RoyalBlue"))))
     (gnus-summary-high-unread-face ((t (:bold t))))
     (gnus-summary-low-unread-face ((t (:italic t))))
     (gnus-summary-normal-unread-face ((t (nil))))
     (gnus-summary-high-read-face ((t (:foreground "DarkGreen" :bold t))))
     (gnus-summary-low-read-face ((t (:foreground "DarkGreen" :italic t))))
     (gnus-summary-normal-read-face ((t (:foreground "DarkGreen"))))
     (gnus-splash-face ((t (:foreground "ForestGreen"))))
     (gnus-emphasis-bold ((t (:bold t))))
     (gnus-emphasis-italic ((t (:italic t))))
     (gnus-emphasis-underline ((t (:underline t))))
     (gnus-emphasis-underline-bold ((t (:bold t :underline t))))
     (gnus-emphasis-underline-italic ((t (:italic t :underline t))))
     (gnus-emphasis-bold-italic ((t (:bold t :italic t))))
     (gnus-emphasis-underline-bold-italic ((t (:bold t :italic t :underline t))))
     (gnus-signature-face ((t (:foreground "white"))))
     (gnus-cite-face-1 ((t (:foreground "Khaki"))))
     (gnus-cite-face-2 ((t (:foreground "Coral"))))
     (gnus-cite-face-3 ((t (:foreground "#4186be"))))
     (gnus-cite-face-4 ((t (:foreground "yellow green"))))
     (gnus-cite-face-5 ((t (:foreground "IndianRed"))))
     (highlight-changes-face ((t (:foreground "red"))))
     (highlight-changes-delete-face ((t (:foreground "red" :underline t))))
     (show-paren-match-face ((t (:foreground "white" :background "purple"))))
     (show-paren-mismatch-face ((t (:foreground "white" :background "red"))))
     (cperl-nonoverridable-face ((t (:foreground "chartreuse3"))))
     (cperl-array-face ((t (:foreground "Blue" :bold t :background "lightyellow2"))))
     (cperl-hash-face ((t (:foreground "Red" :bold t :italic t :background "lightyellow2"))))
     (makefile-space-face ((t (:background "hotpink"))))
     (sgml-start-tag-face ((t (:foreground "mediumspringgreen"))))
     (sgml-ignored-face ((t (:foreground "gray20" :background "gray60"))))
     (sgml-doctype-face ((t (:foreground "orange"))))
     (sgml-sgml-face ((t (:foreground "yellow"))))
     (sgml-end-tag-face ((t (:foreground "greenyellow"))))
     (sgml-entity-face ((t (:foreground "gold"))))
     (flyspell-incorrect-face ((t (:foreground "OrangeRed" :bold t :underline t))))
     (flyspell-duplicate-face ((t (:foreground "Gold3" :bold t :underline t)))))))

(add-to-list 'load-path "~/.emacs.d/color-theme-6.6.0/")
(require 'color-theme)
(eval-after-load "color-theme"
  '(progn
     (color-theme-initialize)
     (color-theme-henry)))

(message "Finished loading .emacs")
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
