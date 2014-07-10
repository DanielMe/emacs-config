;;; init.el
;;; Credit:
;;; Inspired by Travis B. Hartwell's Emacs configuration

;; The general philosophy in this Emacs configuration is init.el is
;; only for setting up the basic load environment and then all
;; configuration and locally-defined functions are done in mode- and
;; task-specific files. Mechanisms are in place or will be defined
;; to make it easy to optionally load and configure these depending
;; on the local environment, essentially allowing for a flexible
;; configuration to be used on multiple machines.

;; Additional philosophy is to avoid polluting the global namespace. 
;; So, only variables that will be used more than immediately after 
;; their definition should be `defvar''d, the rest should be enclosed 
;; in a `let' or `let*' sexp.

(eval-when-compile
  (require 'cl))

; General constant used everywhere
(defconst my-home-dir
  (getenv "HOME")
  "The full path of the user's home directory.")

(defconst my-hostname
  (car (split-string system-name "\\." t))
  "The hostname for the current system.")

;; Set up load path to include all user-local directories that may contain
;; configuration or library files.
(defconst my-emacs-d-dir
  (expand-file-name ".emacs.d" my-home-dir)
  "Top level directory for local configuration and code.")

; Load helper functions and macros to help with the rest.
(load (expand-file-name "init-helpers.el" my-emacs-d-dir))

; Make elint and the byte compiler happy, it complains when this file is just
; `load'ed, so also require it.
(eval-when-compile
  (require 'init-helpers))

(define-add-to-load-path-using-const
  my-emacs-d-local-dir "local" my-emacs-d-dir
  "Defaults and local configuration definitions are located here.
Files in this directory only contain code to set variable values, and
do not define functions or execute code not related to variable setting.")

(define-add-to-load-path-using-const
 my-emacs-d-config-dir "config" my-emacs-d-dir
  "Configuration and local utility functions are located here, with files for major function areas.
These files should `provide' 'filename', so they can be easily loaded and configuration checked. A good
practice for configuration around a major mode is to name the file major-mode-config.el, thus being able to:
require major-mode-config.")

(define-add-to-load-path-using-const
  my-lib-emacs-dir "emacs" (expand-file-name "lib" my-home-dir)
  "Emacs features not installed via the package manager live here.
These are most often as symlinks to directories or files in $HOME/Source
which contains VCS source check outs. Ultimately I would like to find a better
method for managing the combo of $HOME/Source and $HOME/lib/emacs.")

(add-subdirs-to-load-path my-lib-emacs-dir)


; Load generally applicable customizations and definitions first
(require-libraries-from-list '(package-config
                               look 
                               editing 
                               keybindings
                               haskell-config
                               org-config
                               pkgbuild-mode-config
                               flyspell-config
                               python-config
                               projectile-config))

; Then load mode- and task-specific configurations. This is different
; from machine-to-machine, so use a machine-specific list to know what
; to load.
;(require-libraries-from-list libraries-to-require)

;;; init.el ends here
