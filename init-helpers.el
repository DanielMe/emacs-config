;;; init-helpers.el
;;; Credit goes to Travis B. Hartwell

;; The following are convenience macros and functions helpful in loading code from
;; user-local configurations and managing the load path.
;;
;; This file is designed to be `load'ed from a specific path (usually $HOME/.emacs.d/)
;; directly by init.el.

;;; History:
;; Started on 2010/02/07.

;;; Code:
; Variables referenced here, but defined elsewhere.
(eval-and-compile
  (defvar load-path)
  (require 'esh-util))

(defmacro define-add-to-load-path-using-const (dirconst dir parent &optional docstring)
  "Define DIRCONST as the path of DIR under PARENT and add to `load-path'.
Optional argument DOCSTRING is supplied to documenting the directory's purpose."
  `(progn
     (defconst ,dirconst
       (expand-file-name ,dir ,parent)
       ,docstring)
     (add-to-list 'load-path ,dirconst)))

;; The with- macros are based on code originally found near the bottom of
;; http://www.emacswiki.org/emacs/LoadingLispFiles
(defmacro with-library-require (symbol &rest body)
  "If SYMBOL can successfully be 'required', evaluate BODY.
Displays a message of the error if the library cannot be required."
  `(condition-case err
       (progn
	 (message "with-library-require requiring %s" ,symbol)
	 (require ,symbol)
	 ,@body)
     (error (message (format "Library %s cannot be loaded, with error: %s." ',symbol err))
	    nil)))

(put 'with-library-require 'lisp-indent-function 1)

(defmacro with-library-load (file &rest body)
  "If FILE can successfully be `load'ed, evaluate BODY.
Displays a message of the error if the library cannot be loaded."
  `(condition-case err
       (progn
	 (message "with-library-load loading %s" ,file)
	 (load ,file)
	 ,@body)
     (error (message (format "File %s cannot be loaded, with error: %s." ',file err))
	    nil)))

(put 'with-library-load 'lisp-indent-function 1)

(defun add-subdirs-to-load-path (dir)
  "Add every immediate subdirectory of DIR to `load-path'."
  (mapc
   (lambda (path)
     (when (file-directory-p path)
       (add-to-list 'load-path path)))
   (file-expand-wildcards
    (expand-file-name "*" dir))))

(defun add-elisp-subdirs-to-load-path (dir)
  "Add all subdirectories of DIR to `load-path' that have elisp files in them."
  (mapc
   (lambda (path)
     (add-to-list 'load-path path))
   (find-dirs #'is-elisp-dir dir)))

(defun is-elisp-dir (dir)
  (not (null (delq nil (mapcar (lambda (f)
				 (when (and (not (file-directory-p (expand-file-name f dir)))
					    (not (string= ".dir-locals.el" f))
					    (not (string= ".dir-settings.el" f)))
				   f))
			       (directory-files dir nil "\\.el"))))))

(defun find-dirs (desired-dir-p dir)
  (when (file-directory-p dir)
    (or (char-equal ?/ (aref dir (1- (length dir))))
	(setq dir (file-name-as-directory dir)))
    (if (funcall desired-dir-p dir) dir
      (let ((files (directory-files dir t nil t))
	    fullname file)
	(eshell-flatten-list (remove-if
			      'null
			      (mapcar
			       (lambda (file)
				 (unless
				     (or
				      (string= (substring file -3) "/..")
				      (string= (substring file -2) "/."))
				   (find-dirs desired-dir-p file)))
			       files)))))))
  
(defun require-libraries-from-list (lst)
  "Require the libraries specified in LST, usually `libraries-to-require'."
  (mapc (lambda (library)
	  (with-library-require library)) lst))

(provide 'init-helpers)
;;; init-helpers.el ends here
