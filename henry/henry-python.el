(require 'henry-utility)

;; python
(defun insert-ipdb ()
  (interactive)
  (toggle-insert-line "import ipdb; ipdb.set_trace()"))

(defun insert-py-copyright ()
  (interactive)
  (insert "#!/usr/bin/env python
# (c) Copyright 2011 Cloudera, Inc.  All rights reserved."))

;; This is supposed to make return to newline-and-indent, and for some
;; reason python has its own default variable
(add-hook 'python-mode-hook '(lambda () (progn
                                           (define-key python-mode-map "\C-m" 'newline-and-indent)
                                           (setq python-indent 2)
																					 (semantic-mode))))

(add-hook 'python-mode-hook '(lambda () (define-key python-mode-map "\C-\M-d" 'insert-ipdb)))
(add-hook 'python-mode-hook '(lambda () (define-key python-mode-map "\C-\M-c" 'insert-py-copyright)))
(add-hook 'python'mode-hook '(lambda () (progn 
																					(define-key python-mode-map [f5] 'flymake-goto-prev-error)
																					(define-key python-mode-map [f6] 'flymake-goto-next-error))))

;; pyflakes
;; This is slightly buggy - sometimes leaves extra files around.
(defun load-pyflakes () 
	(when (load "flymake" t)
		(defun flymake-pyflakes-init ()
			(let* ((temp-file (flymake-init-create-temp-buffer-copy
												 'flymake-create-temp-inplace))
						 (local-file (file-relative-name
													temp-file
													(file-name-directory buffer-file-name))))
				(list "/usr/local/bin/pyflakes" (list local-file))))
		(add-to-list 'flymake-allowed-file-name-masks
								 '("\\.py\\'" flymake-pyflakes-init))))

;; (add-hook 'find-file-hook 'flymake-find-file-hook)

;; pymacs and rope
;(add-to-list 'load-path "~/.emacs.d/Pymacs-0.23")
;(require 'pymacs)
;(setq ropemacs-enable-shortcuts nil)
;(setq ropemacs-local-prefix "C-c C-p")
;(pymacs-load "ropemacs" "rope-")

(eval-after-load "python" '(progn (load-pyflakes) (message "Loaded my Python customisations")))

(provide 'henry-python)
