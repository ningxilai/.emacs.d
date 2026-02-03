;;; Commentary:  -*- lexical-binding: t; -*-

;;; Code:

;; Org

(setup (:elpaca org)

  (:option org-todo-keywords
           '((sequence
              "TODO(t)"  ; A task that needs doing & is ready to do
              "PROJ(p)"  ; A project, which usually contains other tasks
              "LOOP(r)"  ; A recurring task
              "STRT(s)"  ; A task that is in progress
              "WAIT(w)"  ; Something external is holding up this task
              "HOLD(h)"  ; This task is paused/on hold because of me
              "IDEA(i)"  ; An unconfirmed and unapproved task or notion
              "INITIATED(I)"
              "WAITING(w)"
              "SOMEDAY(smd)"
              "|"
              "DONE(d)"  ; Task successfully completed
              "CANCELLED(c)"
              "KILL(k)") ; Task was cancelled, aborted, or is no longer applicable
             (sequence
              "[ ](T)"   ; A task that needs doing
              "[-](S)"   ; Task is in progress
              "[?](W)"   ; Task is being held up or paused
              "|"
              "[X](D)")  ; Task was completed
             (sequence
              "|"
              "OKAY(o)"
              "YES(y)"
              "NO(n)"))
           org-todo-keyword-faces
           '(("[-]"  . org-todo-active)
             ("STRT" . org-todo-active)
             ("[?]"  . org-todo-onhold)
             ("WAIT" . org-todo-onhold)
             ("HOLD" . org-todo-onhold)
             ("PROJ" . org-todo-project)
             ("NO"   . org-todo-cancel)
             ("KILL" . org-todo-cancel))

           ;; pdflatex is not very efficient, but only pdflatex supports tikz

           org-latex-logfiles-extensions
           '("lof" "lot" "tex~" "aux" "idx" "log" "out"
             "toc" "nav" "snm" "vrb" "dvi" "fdb_latexmk"
             "blg" "brf" "fls" "entoc" "ps" "spl" "bbl"
             "tex" "bcf")
           org-latex-classes
           '("ews"
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
             ("\\subsubsection{%s}" . "\\subsubsection*{%s}")))

  (:hooks org-after-refile-insert (lambda ()
                                    (when (bound-and-true-p org-capture-is-refiling)
                                      (save-buffer)))
          org-capture-mode (lambda ()
                             (setq header-line-format
                                   (format "%s%s%s"
                                           (propertize (abbreviate-file-name (buffer-file-name (buffer-base-buffer)))
                                                       'face 'font-lock-string-face)
                                           org-eldoc-breadcrumb-separator
                                           header-line-format))))

  (:custom
   org-emphasis-alist
   '(("*" bold)
     ("/" org-emphasis-italic)
     ("_" underline)
     ("=" org-emphasis-verbatim org-verbatim verbatim)
     ("~" org-emphasis-code org-code verbatim)
     ("+" (:strike-through t)))

   org-directory "etc/org"
   org-id-locations-file '(expand-file-name ".orgids" org-directory)
   org-list-allow-alphabetical t

   calendar-week-start-day t ;; test
   org-agenda-files (list org-directory)
   org-agenda-deadline-faces
   '((1.001 . error)
     (1.0 . org-warning)
     (0.5 . org-upcoming-deadline)
     (0.0 . org-upcoming-distant-deadline))
   ;; Don't monopolize the whole frame just for the agenda
   org-agenda-window-setup 'current-window
   org-agenda-skip-unavailable-files t
   org-agenda-span 10
   org-agenda-start-on-weekday nil
   org-agenda-start-with-log-mode t
   org-agenda-start-day "-3d"
   org-agenda-inhibit-startup t
   org-agenda-tags-column 0

   org-agenda-custom-commands
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
       (todo "WAIT" ((org-agenda-overriding-header "Waiting:")))
       )))

   org-modules '(ol-doi ol-w3m ol-bbdb ol-bibtex ol-docview ol-gnus ol-info ol-irc ol-mhe ol-rmail ol-eww)
   org-export-backends '(ascii html latex man md odt texinfo)
   org-export-with-sub-superscripts nil

   org-fontify-inline-src-blocks t
   org-fontify-todo-headline t
   org-fontify-quote-and-verse-blocks t
   org-fontify-whole-block-delimiter-line t
   org-fontify-whole-heading-line t
   org-fontify-done-headline t

   org-hide-emphasis-markers t
   org-hide-leading-stars t
   org-hide-macro-markers t
   org-hide-block-startup nil

   org-format-latex-options '(:foreground default :background "Transparent" :scale 1.0 :html-foreground auto :html-background "Transparent" :html-scale 1.0 :matchers ("begin" "$1" "$" "$$" "\\(" "\\["))
   org-highlight-latex-and-related '(native latex script entities)
   org-image-actual-width nil ;; '450
   org-latex-prefer-user-labels t
   org-latex-compiler "xelatex"
   org-latex-listings 'minted

   org-latex-src-block-backend 'minted

   org-latex-packages-alist '(("" "color") ("" "minted") ("" "parskip") ("" "tikz") ("" "amsfonts") ("" "amsmath") ("" "amsthm") ("fontset=macnew,UTF8" "ctex"))
   org-latex-pdf-process
   (if (executable-find "latexmk")
       '("latexmk -shell-escape -bibtex -f -pdf -%latex -interaction=nonstopmode -output-directory=%o %f")
     '("pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"
       "pdflatex -interaction nonstopmode -output-directory %o %f"
       "bibtex %b"))

   org-pretty-entities t
   org-pretty-entities-include-sub-superscripts nil
   org-support-shift-select t
   org-startup-folded 'showeverything
   org-startup-with-inline-images t
   org-startup-indented nil
   org-startup-tuncated t
   org-use-sub-superscripts '{}

   org-refile-targets
   '((nil :maxlevel . 3)
     (org-agenda-files :maxlevel . 3)
     org-refile-use-outline-path 'file
     org-outline-path-complete-in-steps nil)

   org-entities-user
   '(("flat"  "\\flat" nil "" "" "266D" "♭")
     ("sharp" "\\sharp" nil "" "" "266F" "♯"))

   org-auto-align-tags nil
   org-special-ctrl-a/e t
   org-insert-heading-respect-content t

   org-src-preserve-indentation nil  ; use native major-mode indentation
   org-src-window-setup 'current-window
   org-src-tab-acts-natively nil     ; we do this ourselves

   org-ellipsis "…"
   org-tags-column 0
   org-log-into-drawer t
   org-log-done 'time
   org-image-actual-width '(800)
   org-catch-invisible-edits 'showeverything ;; 'show-and-error
   org-fold-catch-invisible-edits 'show
   ;; org-indent-mode-turns-on-hiding-stars nil

   org-archive-subtree-save-file-p t
   org-num-skip-tags '("export" "nonum")

   org-effort-property "EFFORT"
   org-id-locations-file-relative t
   org-id-link-to-org-use-id t
   org-display-remote-inline-images 'download ; TRAMP urls

   org-export-with-drawers nil
   org-export-with-todo-keywords nil
   org-export-with-toc nil
   org-export-with-smart-quotes t
   org-export-date-timestamp-format "%e %B %Y"

   org-html-validation-link nil

   org-indirect-buffer-display 'new-frame
   org-enforce-todo-dependencies t

   org-columns-default-format
   "%50ITEM(Task) %10CLOCKSUM %16TIMESTAMP_IA"

   org-imenu-depth 4
   org-priority-faces
   '((?A . error)
     (?B . warning)
     (?C . shadow)))

  (:bind "C-S-<left>" windower-move-border-left
         "C-S-<right>" windower-move-border-right
         "C-S-<up>" windower-move-border-above
         "C-S-<down>" windower-move-border-below))

(setup (:elpaca org-contrib))

(setup (:elpaca toc-org)
  (:hooks org-mode-hook toc-org-mode))

(setup (:elpaca htmlize))

(setup (:elpaca one :host github :repo "tonyaldon/one.el" :build (:not compile)))

(setup (:elpaca jack :host github :repo "tonyaldon/jack.el" :build (:not compile)))

(setup (:elpaca org-margin :host github :repo "rougier/org-margin")
  (:hooks org-mode-hook (lambda()(org-margin-mode 1))))

(setup (:elpaca valign)
  (:custom valign-fancy-bar t)
  (:hooks org-mode-hook valign-mode))

(setup (:elpaca org-edit-indirect)
  (:hooks org-mode-hook org-edit-indirect-mode))

(setup (:elpaca org-appear)
  (:hooks org-mode-hook org-appear-mode)
  (:custom org-appear-inside-latex t
           org-appear-autokeywords t
           org-appear-autoentities t
           org-appear-autoemphasis t
           org-appear-autosubmarkers t
           org-appear-autolinks 'just-brackets))

(provide 'lang-org)

;; ends here
