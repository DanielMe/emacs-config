(add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)


(defun start-new-shell ()
    (shell (generate-new-buffer-name "*shell*"))
    (delete-other-windows))


