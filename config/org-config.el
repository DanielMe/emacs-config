(require 'org)

(add-to-list 'load-path "/usr/share/emacs/site-lisp/org_contrib/lisp" t)

(defun org-mode-reftex-setup ()
  (load-library "reftex")
  (and (buffer-file-name)
       (file-exists-p (buffer-file-name))
       (setq reftex-bibpath-environment-variables '("/home/daniel/data/doc/references//"))       
       (reftex-parse-all))
  (define-key org-mode-map (kbd "C-c )") 'reftex-citation)
  )


(add-hook 'org-mode-hook 'org-mode-reftex-setup)

(add-hook 'org-babel-after-execute-hook 'org-display-inline-images 'append)

(org-babel-do-load-languages
 (quote org-babel-load-languages)
 (quote ((emacs-lisp . t)
         (dot . t)
         (ditaa . t)
         (R . t)
         (python . t)
         (ruby . t)
         (gnuplot . t)
         (clojure . t)
         (sh . t)
         (ledger . t)
         (org . t)
         (plantuml . t)
         (haskell . t)
         (latex . t))))

(setq org-src-fontify-natively t)

; Do not prompt to confirm evaluation
; This may be dangerous - make sure you understand the consequences
; of setting this -- see the docstring for details
(setq org-confirm-babel-evaluate nil)
;;(setq org-export-babel-evaluate nil)

(unless (boundp 'org-latex-classes)
  (setq org-export-latex-classes nil))

;; 'djcb-org-article' for export org documents to the LaTex 'article', using
;; XeTeX and some fancy fonts; requires XeTeX (see org-latex-to-pdf-process)
(add-to-list 'org-latex-classes
             '("std"
               "\\documentclass[11pt,a4paper]{article}
                \\usepackage[T1]{fontenc}
                \\usepackage{fontspec}
                \\usepackage{graphicx}
                \\usepackage{amsmath,amsthm,amssymb}
                \\usepackage{minted}
                \\usepackage{hyperref}
                \\usemintedstyle{emacs} 
                \\defaultfontfeatures{Mapping=tex-text}
                \\setromanfont{Gentium}
                \\setromanfont [BoldFont={Gentium Basic Bold},
                                ItalicFont={Gentium Basic Italic}]{Gentium Basic}
                \\setsansfont{Charis SIL}
                \\setmonofont[Scale=0.8]{DejaVu Sans Mono}
                \\usepackage{geometry}
                \\geometry{a4paper, textwidth=6.5in, textheight=10in,
                           marginparsep=7pt, marginparwidth=.6in}
                \\pagestyle{empty}
                \\title{}
                [NO-DEFAULT-PACKAGES]
                [PACKAGES]
                [EXTRA]"
               ("\\section{%s}" . "\\section*{%s}")
               ("\\subsection{%s}" . "\\subsection*{%s}")
               ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
               ("\\paragraph{%s}" . "\\paragraph*{%s}")
               ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))

(add-to-list 'org-latex-classes
             '("ba-kecs"
               "\\documentclass{ba-kecs}
                \\usepackage[T1]{fontenc}
                \\usepackage{fontspec}
                \\usepackage{graphicx}
                \\usepackage{amsmath,amsthm,amssymb}
                \\usepackage{minted}
                \\usepackage{hyperref}
                \\usemintedstyle{emacs} 
                \\defaultfontfeatures{Mapping=tex-text}
                \\setromanfont{Gentium}
                \\setromanfont [BoldFont={Gentium Basic Bold},
                                ItalicFont={Gentium Basic Italic}]{Gentium Basic}
                \\setsansfont{Charis SIL}
                \\setmonofont[Scale=0.8]{DejaVu Sans Mono}
                \\title{}
                [NO-DEFAULT-PACKAGES]
                [PACKAGES]
                [EXTRA]"
               ("\\section{%s}" . "\\section*{%s}")
               ("\\subsection{%s}" . "\\subsection*{%s}")
               ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
               ("\\paragraph{%s}" . "\\paragraph*{%s}")
               ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))

;(setq org-latex-to-pdf-process
;   '("texi2dvi -p -b -c -V %f"))


(setq org-latex-to-pdf-process '("xelatex -shell-escape -interaction nonstopmode %f"
                                 "export BIBINPUTS=\"/home/daniel/data/doc/references//:\" && bibtex %b"
                                 "xelatex -shell-escape -interaction nonstopmode %f"
                                 "xelatex -shell-escape -interaction nonstopmode %f"))

(setq org-export-latex-listings 'minted)
(setq org-export-latex-minted t)
(setq org-export-latex-hyperref-format "\\ref{%s}")

(setq org-export-latex-minted-options
      '(("frame" "lines")
        ("fontsize" "\\scriptsize")
        ("linenos" "")))


;(setq org-html-head "<style type=\"text/css\">
;                     .org-src-container {
;                       border-left: 4px solid gray;
;                       padding: 0.5em 0.5em 0.5em 1em; }
;                      .org-src-container pre {
;                       margin-left: 1em; } 
;                     </style>")

;; activate org-indent-mode and visual-lines
(add-hook 'org-mode-hook
          (lambda ()
            (org-indent-mode t)
            (turn-on-visual-line-mode))
          t)



;; Agenda Files
(org-agenda-files nil)

;; yasnippets
(add-hook 'org-mode-hook
          (lambda ()
            (make-variable-buffer-local 'yas/trigger-key)
            (setq yas/trigger-key [tab]))) 


;; PDFs visited in Org-mode are opened in Evince (and not in the default choice) http://stackoverflow.com/a/8836108/789593
(add-hook 'org-mode-hook
      '(lambda ()
         (delete '("\\.pdf\\'" . default) org-file-apps)
         (add-to-list 'org-file-apps '("\\.pdf\\'" . "evince %s"))))

(require 'org-special-blocks)

;; org2blog
(setq org-directory "~/.emacs.d/org")
(require 'org2blog)
(require 'netrc)
 (setq blog (netrc-machine (netrc-parse "~/.netrc") "blog" t))
 (setq org2blog/wp-blog-alist
       '(("danielmescheder"
          :url "http://danielmescheder.wordpress.com/xmlrpc.php"
          :username (netrc-get blog "login")
          :password (netrc-get blog "password"))))


;; (require 'htmlize)
;; (setq org2blog/wp-use-sourcecode-shortcode 't)

;; Export bibtex to html
(require 'org-bibtex-extras)


;; Override org-mode functions for better wordpress export

(require 'ox-html)

(defun org-html-src-block (src-block contents info)
  "Transcode a SRC-BLOCK element from Org to HTML.
CONTENTS holds the contents of the item.  INFO is a plist holding
contextual information."
  (if (org-export-read-attribute :attr_html src-block :textarea)
      (org-html--textarea-block src-block)
    (let ((lang (org-element-property :language src-block))
	  (caption (org-export-get-caption src-block))
	  (code (org-html-format-code src-block info))
	  (label (let ((lbl (org-element-property :name src-block)))
		   (if (not lbl) ""
		     (format " id=\"%s\""
			     (org-export-solidify-link-text lbl))))))
      (if (not lang) (format "<pre class=\"example\"%s>\n%s</pre>" label code)
	(format
	 "<div class=\"org-src-container\" 
           style=\"background-color:#333333;color:white;padding=10px;margin=5px\">\n%s%s</div>"
	 (if (not caption) ""
	   (format "<label class=\"org-src-name\">%s</label>"
		   (org-export-data caption info)))
	 (format "\n<pre class=\"src src-%s\"%s>%s</pre>" lang label code))))))


(defun org-html-latex-environment (latex-environment contents info)
  "Transcode a LATEX-ENVIRONMENT element from Org to HTML.
CONTENTS is nil.  INFO is a plist holding contextual information."
  (let ((processing-type (plist-get info :with-latex))
	(latex-frag (org-remove-indentation
		     (org-element-property :value latex-environment)))
	(attributes (org-export-read-attribute :attr_html latex-environment)))
    (case processing-type
      ((t mathjax)
       (org-html-format-latex latex-frag 'mathjax))
      ((dvipng imagemagick)
       (let ((formula-link (org-html-format-latex latex-frag processing-type)))
	 (when (and formula-link (string-match "file:\\([^]]*\\)" formula-link))
	   ;; Do not provide a caption or a name to be consistent with
	   ;; `mathjax' handling.
	   (org-html--wrap-image
	    (org-html--format-image
	     (match-string 1 formula-link) 
         (org-combine-plists (list :style "vertical-align:middle;border-style:none;" 
                                   :alt "") 
                             attributes) 
         info) info))))
      (t latex-frag))))

(defun org-html-latex-fragment (latex-fragment contents info)
  "Transcode a LATEX-FRAGMENT object from Org to HTML.
CONTENTS is nil.  INFO is a plist holding contextual information."
  (let ((latex-frag (org-element-property :value latex-fragment))
	(processing-type (plist-get info :with-latex)))
    (case processing-type
      ((t mathjax)
       (org-html-format-latex latex-frag 'mathjax))
      ((dvipng imagemagick)
       (let ((formula-link (org-html-format-latex latex-frag processing-type)))
	 (when (and formula-link (string-match "file:\\([^]]*\\)" formula-link))
	   (org-html--format-image (match-string 1 formula-link) 
                               (list :style "vertical-align:middle;border-style:none;" 
                                     :alt "") info))))
      (t latex-frag))))


(provide 'org-config)
