;;; setup-dsl.el --- setup-integrated DSL facade -*- lexical-binding: t; -*-

(require 'setup)
(require 'setup-dsl-surface)
(require 'setup-dsl-backend)

;;;###autoload
(defmacro setup-dsl (name &rest body)
  "Configure NAME using the new DSL and the uploaded `setup' engine.
The DSL is rewritten first; the resulting setup terms are then expanded by
`setup', preserving its context inference, error wrappers, delayed loading,
and package modifier semantics."
  (declare (indent 1))
  `(setup ,name
     ,@(apply #'append
              (mapcar (lambda (form)
                        (let ((out (setup-dsl-compile form :setup)))
                          (if (and (listp out) (eq (car out) :seq))
                              (cdr out)
                            (list out))))
                      body))))

(defun setup-dsl-eval-string (string)
  "Parse and evaluate STRING as standalone DSL forms." 
  (mapc (lambda (form) (eval (setup-dsl-compile form :elisp) lexical-binding))
        (setup-dsl-read-string string)))

(provide 'setup-dsl)
;;; setup-dsl.el ends here
