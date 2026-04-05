;;; Commentary:  -*- lexical-binding: t; -*-

;;; Code:

;; Markdown

(setup (:elpaca markdown-mode)
  (:hooks markdown-mode-hook electric-quote-mode)
  (defun md-to-pdf ()
    "Export current buffer to PDF using pandoc with xelatex."
    (interactive)
    (unless buffer-file-name
      (user-error "Buffer has no associated file"))
    (let* ((in-file (expand-file-name buffer-file-name))
           (default-directory (file-name-directory in-file))
           (out-file (concat (file-name-sans-extension in-file) ".pdf"))
           (head-file (expand-file-name "var/Eisvogel/eisvogel.latex" user-emacs-directory))
           (template-arg (if (file-exists-p head-file) (format "--template %s" head-file) "")))
      (shell-command
       (format "pandoc %s --from 'markdown+smart+tex_math_dollars-blank_before_header+link_attributes' --to pdf --strip-comments -s --toc --toc-depth=6 --pdf-engine=xelatex -V 'geometry:b5paper' -V 'mainfont=Linux Libertine O' -V 'CJKmainfont=LXGW WenKai' -V 'colorlinks=true' %s -o '%s'"
               in-file template-arg out-file))
      (message "Exporting to PDF: %s" out-file))))

(provide 'lang-markdown)
;; ends here.
