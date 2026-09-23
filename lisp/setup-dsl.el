;;; setup-dsl.el --- setup integration entry points -*- lexical-binding: t; -*-

(require 'setup-dsl-backend)
(require 'setup-peg)

(defun setup-dsl-eval-string (string)
  "Parse and evaluate STRING as standalone trusted configuration.
This is an explicit utility, not a setup keyword."
  (mapc (lambda (form)
          (eval (setup-dsl-compile form :elisp) lexical-binding))
        (setup-peg-read-string string)))

(provide 'setup-dsl)
;;; setup-dsl.el ends here
