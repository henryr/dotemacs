;; xelatex
;; (defun init-xelatex nil nil (interactive)
;;   (add-to-list 'LaTeX-command-style
;;          '("\\`fontspec\\'" "xelatex %S%(PDFout)")))
;; (add-hook 'latex-mode-hook 'init-xelatex)

(add-to-list 'load-path "~/.emacs.d/auctex")
; (load "auctex.el")
;; (defun preview-pdf ()
;;   (interactive)
;;   (let* ((filename (file-name-sans-extension buffer-file-truename))
;;          (pdf-filename (concat filename ".pdf")))
;;     (shell-command-to-string (format "open %s" pdf-filename))))

(autoload 'LaTeX-mode "auctex.el" t)

(add-hook 'latex-mode-hook (lambda () (local-set-key "\C-c\C-p" preview-pdf)))
; (load "preview-latex.el")

(provide 'henry-latex)
