;;; Commentary:  -*- lexical-binding: t; -*-

;;; Code:

;; Org

(setup org

  (:option org-todo-keywords
           '((sequence
              "TODO(t)"     ; A task that needs doing & is ready to do
              "PROJ(p)" ; A project, which usually contains other tasks
              "LOOP(r)" ; A recurring task
              "STRT(s)" ; A task that is in progress
              "WAIT(w)" ; Something external is holding up this task
              "HOLD(h)" ; This task is paused/on hold because of me
              "IDEA(i)" ; An unconfirmed and unapproved task or notion
              "INITIATED(I)"
              "WAITING(w)"
              "SOMEDAY(smd)"
              "|"
              "DONE(d)"                 ; Task successfully completed
              "CANCELLED(c)"
              "KILL(k)") ; Task was cancelled, aborted, or is no longer applicable
             (sequence
              "[ ](T)"               ; A task that needs doing
              "[-](S)"               ; Task is in progress
              "[?](W)"               ; Task is being held up or paused
              "|"
              "[X](D)")                 ; Task was completed
             (sequence
              "|"
              "OKAY(o)"
              "YES(y)"
              "NO(n)"))
           org-entities-user
           '(("flat"  "\\flat" nil "" "" "266D" "♭")
             ("sharp" "\\sharp" nil "" "" "266F" "♯")))

  (:custom org-emphasis-alist
           '(("*" bold)
             ("/" org-emphasis-italic)
             ("_" underline)
             ("=" org-emphasis-verbatim org-verbatim verbatim)
             ("~" org-emphasis-code org-code verbatim)
             ("+" (:strike-through t)))

           org-auto-align-tags nil
           org-special-ctrl-a/e t
           org-insert-heading-respect-content t
           org-use-sub-superscripts '{}
           org-support-shift-select t
           org-ellipsis "…"
           org-tags-column 0
           org-image-actual-width '(800)
           org-effort-property "EFFORT"
           org-indirect-buffer-display 'new-frame
           org-enforce-todo-dependencies t
           org-columns-default-format "%50ITEM(Task) %10CLOCKSUM %16TIMESTAMP_IA"
           org-directory "etc/org"
           org-agenda-files (list org-directory)
           org-modules '(ol-doi ol-w3m ol-bbdb ol-bibtex ol-docview ol-gnus ol-info ol-irc ol-mhe ol-rmail ol-eww)
           org-export-backends '(ascii html latex man md odt texinfo)
           org-image-actual-width nil ;; '450
           org-format-latex-options '(:foreground default
                                                  :background "Transparent"
                                                  :scale 1.0
                                                  :html-foreground auto
                                                  :html-background "Transparent"
                                                  :html-scale 1.0
                                                  :matchers ("begin" "$1" "$" "$$" "\\(" "\\["))
           org-highlight-latex-and-related '(native
                                             latex
                                             script
                                             entities)
           org-preview-latex-default-process 'dvisvgm
           org-preview-latex-process-alist '((dvisvgm :programs
                                                      ("xelatex" "dvisvgm")
                                                      :description "xdv > svg"
                                                      :message "you need to install the programs: xelatex and dvisvgm."
                                                      :use-xcolor t
                                                      :image-input-type "xdv"
                                                      :image-output-type "svg"
                                                      :image-size-adjust (1.7 . 1.5)
                                                      :latex-compiler
                                                      ("xelatex -no-pdf -interaction nonstopmode -shell-escape -output-directory %o %f")
                                                      :image-converter
                                                      ("dvisvgm %f -e -n -b min -c %S -o %O"))
                                             (imagemagick :programs
                                                          ("xelatex" "convert")
                                                          :description "pdf > png"
                                                          :message "you need to install the programs: xelatex and imagemagick."
                                                          :use-xcolor t
                                                          :image-input-type "pdf"
                                                          :image-output-type "png"
                                                          :image-size-adjust (1.0 . 1.0)
                                                          :latex-compiler
                                                          ("xelatex -interaction nonstopmode -output-directory %o %f")
                                                          :image-converter
                                                          ("convert -density %D -trim -antialias %f -quality 100 %O")))
           org-latex-packages-alist '(("" "color")
                                      ("" "minted")
                                      ("" "parskip")
                                      ("" "tikz")
                                      ("" "amsfonts")
                                      ("" "amsmath")
                                      ("" "amsthm")
                                      ("fontset=macnew,UTF8" "ctex"))

           org-fontify-whole-block-delimiter-line t
           org-fontify-whole-heading-line t
           org-fontify-todo-headline t
           org-fontify-done-headline t

           org-hide-emphasis-markers t
           org-hide-leading-stars t
           org-hide-macro-markers t
           org-hide-block-startup nil

           org-startup-folded 'showeverything
           org-startup-with-inline-images t
           org-startup-indented nil
           org-startup-tuncated t
           org-startup-with-latex-preview nil

           org-pretty-entities t
           org-pretty-entities-include-sub-superscripts nil

           org-log-into-drawer t
           org-log-done 'time)

  (:hooks org-after-refile-insert-hook (lambda () (when (bound-and-true-p org-capture-is-refiling) (save-buffer)))
          org-capture-mode-hook (lambda () (setq-local header-line-format
                                                  (format "%s%s%s"
                                                          (propertize (abbreviate-file-name (buffer-file-name (buffer-base-buffer)))
                                                                      'face 'font-lock-string-face)
                                                          org-eldoc-breadcrumb-separator
                                                          header-line-format)))
          org-mode-hook (lambda () (when (bound-and-true-p LaTeX-mode) (turn-on-org-cdlatex))))

  (:bind "C-S-<left>" windower-move-border-left
         "C-S-<right>" windower-move-border-right
         "C-S-<up>" windower-move-border-above
         "C-S-<down>" windower-move-border-below))

(setup ob-latex
  (:custom org-babel-latex-preamble
           (lambda (_)
             "\\documentclass{standalone}")
           org-babel-latex-pdf-svg-process
           "inkscape \
-n 1 \
--pdf-poppler \
--export-area-drawing \
--export-text-to-path \
--export-plain-svg \
--export-filename=%O \
%f"
           org-babel-default-header-args:latex
           '((:results . "file raw")
             (:exports . "results")
             (:eval . "never-export")
             (:file . (lambda ()
                        (let* ((elt (org-element-at-point))
                               (name (org-element-property :name elt))
                               (cap (org-export-get-caption elt))
                               (sha (concat (sha1 (org-element-property :value elt)))))
                          (concat (or name cap )
                                  ".svg"))))))
  (:hooks org-babel-after-execute-hook org-redisplay-inline-images))

(setup ox
  (:option org-export-with-sub-superscripts nil)
  (:custom org-export-with-drawers nil
           org-export-with-todo-keywords nil
           org-export-with-toc nil
           org-export-with-smart-quotes t
           org-export-date-timestamp-format "%e %B %Y"))

(setup ox-latex
  (:custom org-latex-compiler "xelatex"
           org-latex-prefer-user-labels t
           org-latex-pdf-process
           (if (executable-find "latexmk")
               '("latexmk -shell-escape -bibtex -f -pdf -%latex -interaction=nonstopmode -output-directory=%o %f")
             '("pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"
               "pdflatex -interaction nonstopmode -output-directory %o %f"
               "bibtex %b"))
           org-latex-logfiles-extensions
           '("lof" "lot" "tex~" "aux" "idx" "log" "out"
             "toc" "nav" "snm" "vrb" "dvi" "fdb_latexmk"
             "blg" "brf" "fls" "entoc" "ps" "spl" "bbl"
             "tex" "bcf")
           org-latex-classes
           '(("ews"
              "\\documentclass[11pt, twoside, hidelinks]{memoir}
           \\setstocksize{9.25in}{7.5in}
           \\settrimmedsize{\\stockheight}{\\stockwidth}{*}
           \\setlrmarginsandblock{1.5in}{1in}{*}
           \\setulmarginsandblock{1in}{1.5in}{*}
           \\checkandfixthelayout
           \\layout
           \\setcounter{tocdepth}{0}
           \\renewcommand{\\baselinestretch}{1.25}
           \\setheadfoot{0.5in}{0.75in}
           \\setlength{\\footskip}{0.8in}
           \\chapterstyle{bianchi}
           \\setsecheadstyle{\\normalfont \\raggedright \\textbf}
           \\setsubsecheadstyle{\\normalfont \\raggedright \\emph}
           \\setsubsubsecheadstyle{\\normalfont\\centering}
           \\pagestyle{myheadings}
           \\usepackage[font={small, it}]{caption}
           \\usepackage{ccicons}
           \\usepackage{ebgaramond}
           \\usepackage[authoryear]{natbib}
           \\bibliographystyle{apalike}
           \\usepackage{svg}
   \\hyphenation{mini-buffer}"
              ("\\chapter{%s}" . "\\chapter*{%s}")
              ("\\section{%s}" . "\\section*{%s}")
              ("\\subsection{%s}" . "\\subsection*{%s}")
              ("\\subsubsection{%s}" . "\\subsubsection*{%s}"))
             ("Notes" "\\documentclass{ctexart}\n[NO-DEFAULT-PACKAGES]\n[NO-PACKAGES]\n\\usepackage{/home/eli/.emacs.d/private/NotesTeXV3}"
              ("\\part{%s}" . "\\part*{%s}")
              ("\\section{%s}" . "\\section*{%s}")
              ("\\subsection{%s}" . "\\subsection*{%s}")
              ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
              ("\\paragraph{%s}" . "\\paragraph*{%s}")
              ("\\subparagraph{%s}" . "\\subparagraph*{%s}"))
             ("article_cn" "\\documentclass[11pt]{ctexart}\n[DEFAULT-PACKAGES]\n[PACKAGES]\n[EXTRA]\n\\definecolor{bg}{rgb}{0.95,0.95,0.95}"
              ("\\section{%s}" . "\\section*{%s}")
              ("\\subsection{%s}" . "\\subsection*{%s}")
              ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
              ("\\paragraph{%s}" . "\\paragraph*{%s}")
              ("\\subparagraph{%s}" . "\\subparagraph*{%s}"))
             ("beamer" "\\documentclass[ignorenonframetext,presentation]{beamer}"
              ("\\section{%s}" . "\\section*{%s}")
              ("\\subsection{%s}" . "\\subsection*{%s}"))
             ("article" "\\documentclass[11pt]{article}"
              ("\\section{%s}" . "\\section*{%s}")
              ("\\subsection{%s}" . "\\subsection*{%s}")
              ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
              ("\\paragraph{%s}" . "\\paragraph*{%s}")
              ("\\subparagraph{%s}" . "\\subparagraph*{%s}"))
             ("report" "\\documentclass[11pt]{report}"
              ("\\part{%s}" . "\\part*{%s}")
              ("\\chapter{%s}" . "\\chapter*{%s}")
              ("\\section{%s}" . "\\section*{%s}")
              ("\\subsection{%s}" . "\\subsection*{%s}")
              ("\\subsubsection{%s}" . "\\subsubsection*{%s}"))
             ("book" "\\documentclass[11pt]{book}"
              ("\\part{%s}" . "\\part*{%s}")
              ("\\chapter{%s}" . "\\chapter*{%s}")
              ("\\section{%s}" . "\\section*{%s}")
              ("\\subsection{%s}" . "\\subsection*{%s}")
              ("\\subsubsection{%s}" . "\\subsubsection*{%s}")))))

(setup ox-html
  (:custom org-html-validation-link nil))

(setup (:elpaca ox-gfm))

(setup (:elpaca org-contrib))

(setup org-faces
  (:custom org-fontify-quote-and-verse-blocks t)
  (:option org-todo-keyword-faces
           '(("[-]"  . org-todo-active)
             ("STRT" . org-todo-active)
             ("[?]"  . org-todo-onhold)
             ("WAIT" . org-todo-onhold)
             ("HOLD" . org-todo-onhold)
             ("PROJ" . org-todo-project)
             ("NO"   . org-todo-cancel)
             ("KILL" . org-todo-cancel))
           org-agenda-deadline-faces ;; Don't monopolize the whole frame just for the agenda
           '((1.001 . error)
             (1.0 . org-warning)
             (0.5 . org-upcoming-deadline)
             (0.0 . org-upcoming-distant-deadline))
           org-priority-faces
           '((?A . error)
             (?B . warning)
             (?C . shadow))))

;; (setup org-num
;;   (:option org-num-skip-tags '("export" "nonum"))
;;   (:hooks org-mode-hook (lambda () (when (bound-and-true-p org-indent-mode)
;;                                 (setq-local org-num-mode t
;;                                             org-indent-mode-turns-on-hiding-stars nil)))))

(setup org-src
  (:option org-src-preserve-indentation nil ; use native major-mode indentation
           org-src-window-setup 'current-window
           org-src-tab-acts-natively nil ; we do this ourselves
           ))

(setup org-id
  (:option org-id-locations-file-relative t
           org-id-link-to-org-use-id t)
  (:custom org-id-locations-file '(expand-file-name ".orgids" org-directory)))

(setup (:elpaca toc-org)
  (:hooks org-mode-hook toc-org-mode))

(setup (:elpaca org-make-toc))

(setup (:elpaca org-edit-indirect)
  (:hooks org-mode-hook org-edit-indirect-mode))

(setup org-archive
  (:custom org-archive-subtree-save-file-p t))

(setup  calendar
  (:option calendar-week-start-day t))

(setup org-list
  (:option org-list-allow-alphabetical t))

(setup org-fold
  (:option org-fold-invisible-edits 'showeverything ;; 'show-and-error
           org-fold-catch-invisible-edits 'show))

(setup org-refile
  (:option org-refile-targets '((nil :maxlevel . 3)
                                (org-agenda-files :maxlevel . 3))
           org-refile-use-outline-path 'file
           org-outline-path-complete-in-steps nil))

(setup org-compat
  (:option org-imenu-depth 4
           org-latex-src-block-backend 'minted))

(setup (:elpaca org-appear)
  (:hooks org-mode-hook org-appear-mode)
  (:custom org-appear-inside-latex t
           org-appear-autokeywords t
           org-appear-autoentities t
           org-appear-autoemphasis t
           org-appear-autosubmarkers t
           org-appear-autolinks 'just-brackets))

(setup ol
  (:option org-display-remote-inline-images 'download))

(setup org-agenda
  (:option org-agenda-window-setup 'current-window
           org-agenda-skip-unavailable-files t
           org-agenda-span 10
           org-agenda-start-on-weekday nil
           org-agenda-start-with-log-mode 'clockcheck
           org-agenda-start-day "-3d"
           org-agenda-inhibit-startup t
           org-agenda-tags-column 0)
  (:custom org-agenda-custom-commands
           '(("P"
              "List of all projects"
              tags
              "LEVEL=2/PROJ")

             ("E"
              "Agenda, next actions and waiting"
              ((agenda "" ((org-agenda-overriding-header "Next three days:")
                           (org-agenda-span 3)
                           (org-agenda-start-on-weekday nil)))
               (todo "NEXT" ((org-agenda-overriding-header "Next Actions:")))
               (todo "WAIT" ((org-agenda-overriding-header "Waiting:"))))))))

(setup (:elpaca htmlize))

(setup (:elpaca org-superstar)
  (:custom org-superstar-item-bullet-alist
           '((?* . ?•)
             (?+ . ?+)
             (?- . ?–))
           org-superstar-headline-bullets-list
           '(?◉ ?🞛 ?○ ?▷))
  (:hooks org-mode-hook org-superstar-mode))

(setup (:elpaca valign)
  (:custom valign-fancy-bar t)
  (:hooks org-mode-hook valign-mode))

(setup (:elpaca mixed-pitch)
  (:require mixed-pitch)
  (:init (add-to-list 'mixed-pitch-fixed-cookie
                      (face-remap-add-relative
                       'org-date
                       'org-footnote
                       'org-special-keyword
                       'org-property-value
                       'org-ref-cite-face
                       'org-tag
                       'org-todo-keyword-todo
                       'org-todo-keyword-habt
                       'org-todo-keyword-done
                       'org-todo-keyword-wait
                       'org-todo-keyword-kill
                       'org-todo-keyword-outd
                       'org-todo
                       'org-done
                       'font-lock-comment-face
                       :family (face-attribute 'variable-pitch :family)
                       :height (face-attribute 'variable-pitch :height)))
         (set-face-attribute 'variable-pitch nil :font "Hasklug Nerd Font")
         (set-face-attribute 'fixed-pitch nil :font "CaskaydiaCove NF")
         (with-eval-after-load 'corfu
           (define-advice corfu--make-buffer (:around (oldfun &rest args))
             (let ((face-remapping-alist nil))
               (apply oldfun args)))))
  (:hooks org-mode-hook mixed-pitch-mode
          mixed-pitch-mode-hook (lambda () (progn (setopt cursor-type 'box)
                                             (kill-local-variable 'cursor-type)))))

(provide 'lang-org)

;; ends here
