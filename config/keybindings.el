(require 'util)

(add-hook 'comint-mode-hook
 (lambda ()
 (define-key comint-mode-map (kbd "M-s") 'move-cursor-next-pane)
 )
)

(add-hook 'flyspell-mode-hook
  (lambda ()
    (define-key flyspell-mode-map (kbd "M-TAB") nil)))

(add-hook 'LaTeX-mode-hook '(lambda ()
      (local-set-key (kbd "RET") 'newline-and-indent)))
(add-hook 'org-mode-hook '(lambda ()
      (define-key org-mode-map (kbd "M-<tab>") nil)
      (local-set-key (kbd "M-a") 'execute-extended-command)))


;(add-hook 'wl-hook '(lambda ()
;      (local-set-key (kbd "") 'newline-and-indent)))

(defvar custom-keymap (make-keymap) "custom, ergonomic keymap.")
(define-key custom-keymap (kbd "M-*") 'select-line)
(define-key custom-keymap (kbd "M-RET") 'insert-line-below)
(define-key custom-keymap (kbd "C-b") 'switch-to-buffer)
(define-key custom-keymap (kbd "C-f") 'isearch-forward)
(define-key custom-keymap (kbd "C-r") 'recentf-open-files)

;; Single char cursor movement
(define-key custom-keymap (kbd "M-j") 'backward-char)
(define-key custom-keymap (kbd "M-l") 'forward-char)
(define-key custom-keymap (kbd "M-i") 'previous-line)
(define-key custom-keymap (kbd "M-k") 'next-line)

;; Move by word
(define-key custom-keymap (kbd "M-u") 'backward-word)
(define-key custom-keymap (kbd "M-o") 'forward-word) ; was (prefix)

;; Move by paragraph
(define-key custom-keymap (kbd "M-U") 'backward-paragraph)
(define-key custom-keymap (kbd "M-O") 'forward-paragraph)

;; Move to beginning/ending of line
(define-key custom-keymap (kbd "M-h") 'move-beginning-of-line)
(define-key custom-keymap (kbd "M-H") 'move-end-of-line)

;; Move by screen (page up/down)
(define-key custom-keymap (kbd "M-I") 'scroll-down)
(define-key custom-keymap (kbd "M-K") 'scroll-up)

;; Move to beginning/ending of file
(define-key custom-keymap (kbd "M-J") 'beginning-of-buffer)
(define-key custom-keymap (kbd "M-L") 'end-of-buffer)

;; isearch
(define-key custom-keymap (kbd "M-;") 'isearch-forward)
(define-key custom-keymap (kbd "M-:") 'isearch-backward)

(define-key custom-keymap (kbd "M-p")
                (if (fboundp 'recenter-top-bottom)
                    'recenter-top-bottom
                  'recenter
                  ))

;;; MAJOR EDITING COMMANDS

;; Delete previous/next char.
(define-key custom-keymap (kbd "M-d") 'delete-backward-char)
(define-key custom-keymap (kbd "M-f") 'delete-char)

; Delete previous/next word.
(define-key custom-keymap (kbd "M-e") 'backward-kill-word)
(define-key custom-keymap (kbd "M-r") 'kill-word)

; Copy Cut Paste, Paste previous
(define-key custom-keymap (kbd "M-x") 'kill-region)
(define-key custom-keymap (kbd "M-c") 'kill-ring-save)
(define-key custom-keymap (kbd "M-v") 'yank)
(define-key custom-keymap (kbd "M-V") 'yank-pop)
(define-key custom-keymap (kbd "M-C") 'copy-all)
(define-key custom-keymap (kbd "M-X") 'cut-all)

;; undo and redo
(define-key custom-keymap (kbd "M-Z") 'redo)
(define-key custom-keymap (kbd "M-z") 'undo)

; Kill line
(define-key custom-keymap (kbd "M-g") 'kill-line)
(define-key custom-keymap (kbd "M-G") 'kill-line-backward)

;;; Textual Transformation

(define-key custom-keymap (kbd "M-S-SPC") 'mark-paragraph)
(define-key custom-keymap (kbd "M-w") 'shrink-whitespaces)
(define-key custom-keymap (kbd "M-'") 'comment-dwim)
(define-key custom-keymap (kbd "M-/") 'toggle-letter-case)

; keyword completion, because Alt+Tab is used by OS
(define-key custom-keymap (kbd "M-t") 'call-keyword-completion) 

; Hard-wrap/un-hard-wrap paragraph
(define-key custom-keymap (kbd "M-q") 'compact-uncompact-block)

;;; EMACS'S SPECIAL COMMANDS

; Mark point.
(define-key custom-keymap (kbd "M-SPC") 'set-mark-command)

(define-key custom-keymap (kbd "M-a") 'execute-extended-command)
(define-key custom-keymap (kbd "M-A") 'shell-command)

;;; WINDOW SPLITING
(define-key custom-keymap (kbd "M-s") 'other-window)

;;; --------------------------------------------------
;;; STANDARD SHORTCUTS

;-*- coding: utf-8 -*-

(setq mac-pass-command-to-system nil) ; so that Cmd+H won't activate Hide Current App and Cmd+Shift+q won't logout user.

(define-key custom-keymap (kbd "C-S-n") 'make-frame-command) ; open a new window.
(define-key custom-keymap (kbd "C-o") 'find-file) ; Open
(define-key custom-keymap (kbd "C-w") 'kill-this-buffer) ; Close
(define-key custom-keymap (kbd "C-s") 'save-buffer) ; Save
(define-key custom-keymap (kbd "C-S-s") 'write-file) ; Save As.
(define-key custom-keymap (kbd "C-p") 'print-buffer) ; Print
(define-key custom-keymap (kbd "C-a") 'mark-whole-buffer) ; Select All
(define-key custom-keymap (kbd "C-S-w") 'delete-frame) ; close Window.

(define-key custom-keymap (kbd "<delete>") 'delete-char) ; the Del key for forward delete. Needed if C-d is set to nil.

(define-key custom-keymap (kbd "C-<prior>") 'previous-user-buffer)
(define-key custom-keymap (kbd "C-<next>") 'next-user-buffer)

(define-key custom-keymap (kbd "C-S-<prior>") 'previous-emacs-buffer)
(define-key custom-keymap (kbd "C-S-<next>") 'next-emacs-buffer)

(define-key custom-keymap (kbd "M-S-<prior>") 'backward-page)
(define-key custom-keymap (kbd "M-S-<next>") 'forward-page)

(define-key custom-keymap (kbd "M-5") 'query-replace)
(define-key custom-keymap (kbd "M-%") 'query-replace-regexp)

(define-key custom-keymap (kbd "M-1") 'delete-other-windows)
(define-key custom-keymap (kbd "M-!") 'delete-window)

(define-key custom-keymap (kbd "M-2") 'split-window-vertically)
(define-key custom-keymap (kbd "M-@") 'split-window-horizontally)

(define-key custom-keymap (kbd "M-8") 'extend-selection)
;(define-key custom-keymap (kbd "M-*") 'select-text-in-quote)

(define-key custom-keymap (kbd "C-x C-b") 'ibuffer)
(define-key custom-keymap (kbd "C-h m") 'describe-major-mode)


(define-minor-mode custom-keys
      " Defines custom, ergonomic keybindings"
      ;; The initial value.
      nil
      ;; The indicator for the mode line.
      " CKeys"
      ;; The minor mode bindings.
      custom-keymap
      :global 1
)

(add-hook 'emulation-mode-map-alists 'custom-keys)
(custom-keys 1)

(provide 'keybindings)
