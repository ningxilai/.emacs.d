;;; meow-pretty-paren.el -*- lexical-binding: t; -*-

;;; Code:

(defvar meow--rainbow-delimiters-enabled nil
  "Track if rainbow-delimiters was enabled before entering insert mode.")

(defvar meow-pretty-paren-modes
  '(clojure-mode-hook
    scheme-mode-hook
    lisp-mode-hook
    emacs-lisp-mode-hook
    racket-mode-hook
    hy-mode-hook)
  "Hooks to enable pretty-paren behavior.")

(defun meow-pretty-paren-enable ()
  "Enable pretty parens for non-insert states.
Enable rainbow-delimiters and disable paren-face-mode."
  (when (fboundp 'rainbow-delimiters-mode)
    (setq meow--rainbow-delimiters-enabled
          (if (bound-and-true-p rainbow-delimiters-mode) t nil))
    (rainbow-delimiters-mode-enable))
  (when (fboundp 'paren-face-mode)
    (paren-face-mode -1)))

(defun meow-pretty-paren-disable ()
  "Disable pretty parens for insert state.
Disable rainbow-delimiters and enable paren-face-mode."
  (when (fboundp 'rainbow-delimiters-mode)
    (rainbow-delimiters-mode-disable))
  (when (fboundp 'paren-face-mode)
    (paren-face-mode 1)))

(defun meow-pretty-paren--setup-buffer ()
  "Setup pretty-paren for current buffer if it's a lisp-like mode."
  (when (or (derived-mode-p 'clojure-mode
                            'scheme-mode
                            'lisp-mode
                            'emacs-lisp-mode
                            'racket-mode
                            'hy-mode)
            (member major-mode '(sly-mrepl-mode)))
    (add-hook 'meow-insert-enter-hook #'meow-pretty-paren-disable nil t)
    (add-hook 'meow-insert-exit-hook #'meow-pretty-paren-enable nil t)
    (meow-pretty-paren-enable)))

(defun meow-setup-pretty-paren ()
  "Setup pretty-paren behavior for meow.
Add hooks to switch parenthesis highlighting based on insert/non-insert states."
  (dolist (hook meow-pretty-paren-modes)
    (add-hook hook #'meow-pretty-paren--setup-buffer)))

(provide 'meow-pretty-paren)
;;;  ends here
