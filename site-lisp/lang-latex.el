;; -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(setup (:elpaca latex-change-env)
  (:after latex-mode)
  (define-key LaTeX-mode-map (kbd "C-c r") 'latex-change-env))

(setup LaTeX-mode (:elpaca auctex)
       (:option
        TeX-PDF-mode t
        TeX-view-program-list '(("PDF Tools" TeX-pdf-tools-sync-view))
        TeX-view-program-selection '((output-pdf "PDF Tools"))
        TeX-output-view-style  '(("^pdf$" "." "xpdf %o %(outpage)"))

        TeX-auto-save t
        TeX-parse-self t
        TeX-show-compilation t
        TeX-electric-math '("$" . "$")
        TeX-electric-sub-and-superscript t
        LaTeX-babel-hyphen nil
        LaTeX-indent-level 4
        LaTeX-item-indent 0
        LaTeX-math-mode

        LaTeX-command "latexmk -shell-escape -bibtex -pdf -g -f %f"
        ;; "latex -shell-escape --synctex=1"
        TeX-command-default "XeLaTeX"

        TeX-master nil
        TeX-engine 'xetex
        TeX-source-correlate-start-server t
        TeX-source-correlate-mode t
        TeX-source-correlate-method '((dvi . source-specials)
                                      (pdf . synctex))

        (add-to-list 'auto-mode-alist '("\\.tex" "\\.stex" "\\.texi" . bibtex-mode))
        (add-to-list 'TeX-command-list '("xelatex" "%`xelatex --synctex=1%(mode)%' %t" tex-run-tex nil t))

        (add-hook 'LaTeX-mode-hook #'turn-on-reftex)
        (add-hook 'tex-after-compilation-finished-functions #'tex-revert-document-buffer)
        ;; font setting
        (add-hook 'LaTeX-mode-hook #'(lambda () (progn (buffer-face-set '(:family "JetBrains Mono"))
                                                  (buffer-face-mode)
                                                  (visual-line-mode)
                                                  (auto-fill-mode))))

        (define-key latex-mode-map (kbd "C-c C-g") 'pdf-sync-forward-search)

        (defun move-line-region-down (arg)
          "Move region (transient-mark-mode active) or current line
          arg lines down."
          (interactive "*p")
          (move-text-internal arg))

        (defun move-line-region-up (arg)
          "Move region (transient-mark-mode active) or current line
          arg lines up."
          (interactive "*p")
          (move-text-internal (- arg)))

        (define-key latex-mode-map (kbd "M-<down>") 'move-line-region-down)
        (define-key latex-mode-map (kbd "M-<up>" 'move-line-region-up))

        (defun description (beg end)
          "wrap the active region in an 'itemize' environment,
          converting hyphens at the beginning of a line to \item"
          (interactive "r")
          (save-restriction
            (narrow-to-region beg end)
            (beginning-of-buffer)
            (insert "\\begin{description}\n")
            (while (re-search-forward "^- " nil t)
              (replace-match "\\\\item[ ]"))
            (end-of-buffer)
            (insert "\\end{description}\n")))

        (defun enumerate (beg end)
          "wrap the active region in an 'itemize' environment,
          converting hyphens at the beginning of a line to \item"
          (interactive "r")
          (save-restriction
            (narrow-to-region beg end)
            (beginning-of-buffer)
            (insert "\\begin{enumerate}\n")
            (while (re-search-forward "^- " nil t)
              (replace-match "\\\\item "))
            (end-of-buffer)
            (insert "\\end{enumerate}\n")))

        (defun itemize (beg end)
          "wrap the active region in an 'itemize' environment,
          converting hyphens at the beginning of a line to \item"
          (interactive "r")
          (save-restriction
            (narrow-to-region beg end)
            (beginning-of-buffer)
            (insert "\\begin{itemize}\n")
            (while (re-search-forward "^- " nil t)
              (replace-match "\\\\item "))
            (end-of-buffer)
            (insert "\\end{itemize}\n")))))

(setup (:elpaca preview-auto)
  (preview-auto-setup)
  (:after LaTeX-mode)
  (:option preview-protect-point t
           preview-locating-previews-message nil
           preview-leave-open-previews-visible t)
  (:custom preview-auto-interval 0.1))

(setup (:elpaca cdlatex)
  (:hooks LaTeX-mode turn-on-cdlatex))

(setup (:elpaca laas)
  (:after LaTeX-mode)
  (:hook LaTeX-mode)
  (:option laas-enable-auto-space nil)
  (aas-set-snippets 'laas-mode
                    ;; set condition!
                    :cond #'texmathp ; expand only while in math
                    "supp" "\\supp"
                    "On" "O(n)"
                    "O1" "O(1)"
                    "Olog" "O(\\log n)"
                    "Olon" "O(n \\log n)"
                    ;; bind to functions!
                    "Sum" (lambda () (interactive)
                            (yas-expand-snippet "\\sum_{$1}^{$2} $0"))
                    "Span" (lambda () (interactive)
                             (yas-expand-snippet "\\Span($1)$0"))
                    ;; add accent snippets
                    :cond #'laas-object-on-left-condition
                    "qq" (lambda () (interactive) (laas-wrap-previous-object "sqrt"))))

;; # -*- mode: snippet -*-
;; # name: doint
;; # key: doint
;; # --
;; ${1:Enter Your Formula here$(when yas-moving-away-p (cm/calc-int yas-text))}$0


;; **** Useful keybindings for viewing PDFs
;; |------------------------------------------+-----------------|
;; | Display                                  |                 |
;; |------------------------------------------+-----------------|
;; | Zoom in / Zoom out                       | ~+~ / ~-~       |
;; | Fit height / Fit width / Fit page        | ~H~ / ~W~ / ~P~ |
;; | Trim margins (set slice to bounding box) | ~s b~           |
;; | Reset margins                            | ~s r~           |
;; | Reset z oom                              | ~0~             |
;; |------------------------------------------+-----------------|
;;
;; **** Useful keybindings for navigating PDFs
;;
;; |-----------------------------------------------+-----------------------|
;; | Navigation                                    |                       |
;; |-----------------------------------------------+-----------------------|
;; | Scroll Up / Down by Page-full                 | ~space~ / ~backspace~ |
;; | Scroll Up / Down by Line                      | ~C-n~ / ~C-p~         |
;; | Scroll Right / Left                           | ~C-f~ / ~C-b~         |
;; | First Page / Last Page                        | ~<~ / ~>~             |
;; | Next Page / Previous Page                     | ~n~ / ~p~             |
;; | First Page / Last Page                        | ~M-<~ / ~M->~         |
;; | Incremental Search Forward / Backward         | ~C-s~ / ~C-r~         |
;; | Occur (list all lines containing a phrase)    | ~M-s o~               |
;; | Jump to Occur Line                            | ~RETURN~              |
;; | Pick a Link and Jump                          | ~F~                   |
;; | Incremental Search in Links                   | ~f~                   |
;; | History Back / Forwards                       | ~l~ / ~r~             |
;; | Display Outline                               | ~o~                   |
;; | Jump to Section from Outline                  | ~RETURN~              |
;; | Jump to Page                                  | ~M-g g~               |
;; | Store position / Jump to position in register | ~m~ / ~'~             |
;; |-----------------------------------------------+-----------------------|

(setup (:elpaca pdf-tools)
  (pdf-tools-install :no-query)
  (:init doc-view-continuous t
         doc-view-resolution 144
         large-file-warning-threshold (* 50 (expt 2 20)))
  (:option pdf-sync-backward-display-action t
           pdf-sync-forward-display-action t)
  (:custom pdf-view-use-scaling t
           pdf-view-use-imagemagick t
           pdf-view-resize-factor 1.1
           pdf-view-display-size 'fit-page ;; 'fit-width
           pdf-annot-activate-created-annotations t)
  (:hooks pdf-view-mode (lambda () (progn (line-number-mode nil)
                                     (pdf-view-themed-minor-mode)
                                     (pdf-view-auto-slice-minor-mode)
                                     (pdf-isearch-minor-mode)))))

(setup bibtex

  (add-to-list 'auto-mode-alist '("\\.bib" . bibtex-mode))

  (defun get-bibtex-from-doi (doi)
    "Get a BibTeX entry from the DOI"
    (interactive "MDOI: ")
    (let ((url-mime-accept-string "text/bibliography;style=bibtex"))
      (with-current-buffer
          (url-retrieve-synchronously
           (format "http://dx.doi.org/%s"
                   (replace-regexp-in-string "http://dx.doi.org/" "" doi)))
        (switch-to-buffer (current-buffer))
        (goto-char (point-max))
        (setq bibtex-entry
              (buffer-substring
               (string-match "@" (buffer-string))
               (point)))
        (kill-buffer (current-buffer))))
    (insert (decode-coding-string bibtex-entry 'utf-8))
    (define-key bibtex-mode-map (kbd "C-c C-b") 'get-bibtex-from-doi)
    (bibtex-fill-entry))
  ;; I want run the above function to define it upon entry into a Bibtex file.

  (defun add-doi ()
    (interactive)
    (progn
      (setq doi-to-query (read-string "DOI "))
      (find-file "~/Documents/global.bib")
      (end-of-buffer)
      (doi-insert-bibtex doi-to-query)))

  (:custom bibtex-dialect 'biblatex
           bibtex-align-at-equal-sign t
           bibtex-user-optional-fields '(("keywords" "Keywords to describe the entry" "")
                                         ("file"     "Relative or absolute path to attachments" "" )))

  (:hooks bibtex-mode (lambda () (get-bibtex-from-doi nil))))

(setup reftex
  (eval-after-load 'reftex '(reftex-isearch-minor-mode))
  (:option reftex-cite-prompt-optional-args t   ; Prompt for empty optional arguments in cite
           reftex-cite-format 'biblatex
           reftex-plug-into-AUCTeX t
           reftex-insert-label-flags t
           reftex-save-parse-info t
           reftex-enable-partial-scans t
           reftex-use-multiple-selection-buffers t))

(provide 'lang-latex)

;;; lang-latex.el ends here
