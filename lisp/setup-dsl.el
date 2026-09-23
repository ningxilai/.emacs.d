;;; setup-dsl.el --- direct primitive setup integration -*- lexical-binding: t; -*-

(require 'setup-peg)
(require 'setup-dsl-backend)

(defun setup-dsl-read-string (string)
  "Read STRING through the bare PEG reader."
  (setup-peg-read-string string))

(defun setup-dsl-eval-string (string)
  "Evaluate trusted primitive forms from STRING."
  (mapc (lambda (form) (eval (setup-dsl-compile form) lexical-binding))
        (setup-dsl-read-string string)))

(provide 'setup-dsl)
;;; setup-dsl.el ends here
