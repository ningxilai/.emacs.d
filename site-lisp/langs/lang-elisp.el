;;;   -*- lexical-binding: t; -*-

;;; Code:

;; elisp

(setup elisp-mode
  (:option elisp-fontify-semantically t)

  (defun lisp-setup-check-parens () (add-hook 'write-file-functions #'check-parens nil t))
  (add-hook 'emacs-lisp-mode-hook #'lisp-setup-check-parens)

  (defun enhance.eval-last-sexp ()
    ;; AIGC
    (defun enhance.eval-last-sexp-advice (_orig-func arg)
      "Evaluate last sexp and insert result as comment."
      (let* ((opoint (point))
             (end (point))
             (start (if arg (mark) (progn (forward-sexp -1) (point))))
             (exp (buffer-substring-no-properties start end))
             (value (eval (read exp))))
        (goto-char opoint)
        (insert "\n;; =>\n;; ")
        (let* ((str (format "%S" value))
               (lines (split-string str "\n" t)))
          (if (cdr lines)
              (progn
                (insert (car lines))
                (dolist (line (cdr lines))
                  (insert "\n;; ")
                  (insert line)))
            (insert str)))))
    (advice-add #'eval-last-sexp :around #'enhance.eval-last-sexp-advice))
  (add-hook 'lisp-interaction-mode-hook #'enhance.eval-last-sexp))

(setup (:elpaca macrostep)
  (define-key emacs-lisp-mode-map (kbd "C-c e") #'macrostep-expand)
  (define-key lisp-interaction-mode-map (kbd "C-c e") #'macrostep-expand))

(setup (:elpaca helpful)
  (:init (with-no-warnings
           (with-eval-after-load 'apropos
             ;; patch apropos buttons to call helpful instead of help
             (dolist (fun-bt '(apropos-function apropos-macro apropos-command))
               (button-type-put
                fun-bt 'action
                (lambda (button)
                  (helpful-callable (button-get button 'apropos-symbol)))))
             (dolist (var-bt '(apropos-variable apropos-user-option))
               (button-type-put
                var-bt 'action
                (lambda (button)
                  (helpful-variable (button-get button 'apropos-symbol))))))))
  (:option apropos-do-all t
           help-enable-completion-autoload nil
           help-enable-autoload nil
           help-enable-symbol-autoload nil
           help-window-select t)
  (:bind [remap describe-function] helpful-callabl
         [remap describe-command]  helpful-command
         [remap describe-variable] helpful-variable
         [remap describe-key] helpful-key
         [remap describe-symbol] helpful-symbol)
  (:with-map emacs-lisp-mode-map (:bind "C-c C-d" helpful-at-point))
  (:with-map lisp-interaction-mode-map (:bind "C-c C-d" helpful-at-point))
  (:with-map help-mode-map (:bind "r" remove-hook-at-point))
  (:hooks helpful-mode-hook cursor-sensor-mode))

(setup (:elpaca elisp-autofmt)
  (:hooks emacs-lisp-mode-hook elisp-autofmt-mode))

(setup (:elpaca aggressive-indent :host github :repo "Malabarba/aggressive-indent-mode")
  (:hooks emacs-lisp-mode-hook aggressive-indent-mode))

(setup eldoc
  (global-eldoc-mode -1)
  (:hooks ielm-mode-hook turn-on-eldoc-mode
          emacs-lisp-mode-hook turn-on-eldoc-mode
          lisp-interaction-mode-hook turn-on-eldoc-mode)
  (:option eldoc-idle-delay 2
           eldoc-print-after-edit t
           eldoc-minor-mode-string nil
           eldoc-echo-area-display-truncation-message nil
           eldoc-documentation-function 'eldoc-documentation-compose))

(setup (:elpaca eldoc-box)
  (:autoload eldoc-box-hover-mode eldoc-box-hover-at-point-mode)
  (:option eldoc-box-lighter nil
           eldoc-box-only-multi-line t
           eldoc-box-clear-with-C-g t))

(provide 'lang-elisp)

;; ends here.
