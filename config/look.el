(scroll-bar-mode nil)
(tool-bar-mode nil)
(menu-bar-mode -1)
;;(setq pop-up-frames t)

;;; Transparency:
;;(set-frame-parameter (selected-frame) 'alpha '(<active> [<inactive>]))
(set-frame-parameter (selected-frame) 'alpha '(90 90))
(add-to-list 'default-frame-alist '(alpha 90 90))

;(add-to-list 'custom-theme-load-path "~/.emacs.d/themes/emacs-color-themes/themes/")
;(load-theme 'hickey t)

;(require 'color-theme)
;(eval-after-load "color-theme"
;  '(progn
;     (color-theme-initialize)
;     (color-theme-charcoal-black)))

;(require 'color-theme-solarized)
(load-theme 'wombat t)

(set-default-font "DejaVu Sans Mono-10")

(tool-bar-mode -1)
(scroll-bar-mode -1)

(require 'powerline)
(powerline-default-theme)

(provide 'look)
