;;; Commentary:  -*- lexical-binding: t; -*-

;;; Code:

;; Markdown

(setup (:elpaca markdown-mode)
  (:with-hook markdown-mode-hook (:hook electric-quote-mode)))

(setup (:elpaca jinx)
  (:hooks text-mode-hook jinx-mode)
  (:option jinx-camel-modes '(prog-mode))
  (:custom jinx-exclude-regexps  '((emacs-lisp-mode "Package-Requires:.*$")
                                   (t "\\cc"
                                      "[A-Z]+\\>"         ;; Uppercase words
                                      "-+\\>"             ;; Hyphens used as lines or bullet points
                                      "\\w*?[0-9]\\w*\\>" ;; Words with numbers, hex codes
                                      "[a-z]+://\\S-+"    ;; URI
                                      "<?[-+_.~a-zA-Z][-+_.~:a-zA-Z0-9]*@[-.a-zA-Z0-9]+>?" ;; Email
                                      "\\(?:Local Variables\\|End\\):\\s-*$" ;; Local variable indicator
                                      "jinx-\\(?:languages\\|local-words\\):\\s-+.*$")) ;; Local variables
           ))

(provide 'lang-markdown)
;; ends here.
