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

(provide 'henry-utility)
