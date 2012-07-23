;; Browse kill ring
(require 'browse-kill-ring)
(global-set-key (kbd "C-c k") 'browse-kill-ring)

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

;; Very handy after git commits which change files underneath emacs
(global-set-key "\C-cr" 'revert-buffer)

;; Fix display of whitespace characters
(setq whitespace-display-mappings '((space-mark ?\  [?\u00B7]) (newline-mark ?\n [?$ ?\n]) (tab-mark ?\t [?\u00BB ?\t])))

;; visuals
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(setq inhibit-startup-message t)
(set-default-font "DejaVu Sans Mono")
(set-face-attribute 'default nil :height 100)

(setq-default frame-title-format 
   (list '((buffer-file-name " %f" 
             (dired-directory 
              dired-directory 
              (revert-buffer-function " %b" 
              ("%b - Dir:  " default-directory))))))) 

(line-number-mode)
(setq linum-format " %d ")
(column-number-mode)
(fringe-mode 0)

(require 'uniquify)
(setq uniquify-buffer-name-style 'post-forward-angle-brackets)

(global-set-key "\M-[" 'previous-multiframe-window)
(global-set-key "\M-]" 'next-multiframe-window)

(global-set-key "\C-cw" 'whitespace-mode)
(global-set-key "\C-cu" 'untabify)

(provide 'henry-emacs)
