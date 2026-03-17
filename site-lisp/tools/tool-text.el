;;;   -*- lexical-binding: t; -*-

;;; Code:

;; text-mode

(setup text-mode
  (defun text-mode-enhanced  () (progn (abbrev-mode)
                                       (visual-line-mode)
                                       (setopt visual-wrap-extra-indent 2)
                                       (visual-wrap-prefix-mode)
                                       (setq-local auto-composition-mode nil
                                                   text-mode-ispell-word-completion nil)))

  (add-hook 'text-mode-hook #'text-mode-enhanced))

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

(setup flymake
  (:hooks text-mode-hook flymake-mode)
  (:option flymake-show-diagnostics-at-end-of-line 'fancy))

(setup (:elpaca flymake-vale :host github :repo "tpeacock19/flymake-vale")
  (:require flymake-vale)
  (:hooks text-mode-hook flymake-vale-load
          latex-mode-hook flymake-vale-load
          org-mode-hook flymake-vale-load
          markdown-mode-hook flymake-vale-load
          message-mode-hook flymake-vale-load))

(provide 'tool-text)

;; ends here.
