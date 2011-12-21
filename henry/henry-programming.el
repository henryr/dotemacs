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

(add-hook 'java-mode-hook (lambda () (linum-mode)))
(add-hook 'haskell-mode-hook (lambda () (linum-mode)))
(add-hook 'c-mode-hook (lambda () (linum-mode)))
(add-hook 'python-mode-hook (lambda () (linum-mode)))
(add-hook 'emacs-lisp-mode-hook (lambda () (linum-mode)))

;; snippets - like textmate, type a keyword then tab to expand to a template
(add-to-list 'load-path "~/.emacs.d/yasnippet")
(require 'yasnippet)
(yas/initialize)
(yas/load-directory "~/.emacs.d/yasnippet/snippets")
(yas/load-directory "~/.emacs.d/scala-mode/contrib/yasnippet")

(provide 'henry-programming)
