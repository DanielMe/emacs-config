;;; Workaround for flyspell bug.. remove once fixed!
(setq flyspell-issue-welcome-flag nil)

; set default dictionary 
(setq ispell-local-dictionary "british")
(setq ispell-program-name "aspell")
;;(setq flyspell-use-meta-tab nil)
(ispell-set-spellchecker-params)
(setq ispell-list-command "list")
(ispell-start-process)
(add-hook 'text-mode-hook 'turn-on-flyspell)

(provide 'flyspell-config)
