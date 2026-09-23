;;; setup-dsl.el --- setup-integrated DSL facade -*- lexical-binding: t; -*-

(require 'setup)
(require 'setup-dsl-surface)
(require 'setup-dsl-backend)

;;;###autoload
(defmacro setup-dsl (name &rest body)
  "Configure NAME with the rewrite language hosted by `setup'."
  (declare (indent 1))
  `(setup ,name ,@(setup-dsl-expand-setup-body body)))

(defun setup-dsl-eval-string (string)
  "Parse and evaluate STRING as standalone DSL forms." 
  (mapc (lambda (form)
          (eval (setup-dsl-compile form :elisp) lexical-binding))
        (setup-dsl-read-string string)))

(provide 'setup-dsl)
;;; setup-dsl.el ends here
