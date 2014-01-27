;;; Misc functions
(defun select-line ()
  "Mark a whole line"
  (interactive)
  (move-beginning-of-line ())
  (set-mark-command ())
  (move-end-of-line ()))

(defun insert-line-below ()
  "Directly opens a new line without affecting the content of the current one"
  (interactive)
  (end-of-line)
  (newline-and-indent))


(provide 'util)
