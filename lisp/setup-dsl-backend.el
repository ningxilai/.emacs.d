;;; setup-dsl-backend.el --- primitive emitters -*- lexical-binding: t; -*-

(require 'setup-dsl-rewrite)

(define-error 'setup-dsl-backend-error "Setup backend error")

(defun setup-dsl--emit (form)
  "Emit checked primitive FORM as Emacs Lisp."
  (if (not (setup-dsl--term-p form)) form
    (pcase (car form)
      (:seq `(progn ,@(mapcar #'setup-dsl--emit (cdr form))))
      (:eval (cadr form))
      (:raw (cadr form))
      (:set (pcase (nth 3 form)
              (:default `(set-default-toplevel-value ',(nth 1 form) ,(nth 2 form)))
              (:custom `(customize-set-variable ',(nth 1 form) ,(nth 2 form)))
              (:local `(setq-local ,(nth 1 form) ,(nth 2 form)))))
      (:require `(require ',(nth 1 form) nil t))
      (:bind `(if (vectorp ,(nth 2 form))
                  (define-key ,(nth 1 form) ,(nth 2 form) ,(nth 3 form))
                (keymap-set ,(nth 1 form) ,(nth 2 form) ,(nth 3 form))))
      (:unbind `(if (vectorp ,(nth 2 form))
                    (define-key ,(nth 1 form) ,(nth 2 form) nil)
                  (keymap-unset ,(nth 1 form) ,(nth 2 form))))
      (:hook `(add-hook ',(nth 1 form) #',(nth 2 form)))
      (:autoload `(autoload #',(nth 1 form) ,(nth 2 form) nil t))
      (:load-after `(with-eval-after-load ',(nth 1 form)
                      ,(setup-dsl--emit (nth 2 form))))
      (:defer `(run-with-idle-timer ,(nth 1 form) nil
                                    (lambda () ,(setup-dsl--emit (nth 2 form)))))
      (:context (setup-dsl--emit (nth 3 form)))
      (:advice `(advice-add ',(nth 1 form) ,(nth 2 form) #',(nth 3 form)))
      (head (signal 'setup-dsl-backend-error (list "Unknown primitive" head))))))

(defun setup-dsl-compile (form)
  "Check FORM and emit it."
  (setup-dsl--emit (setup-dsl-rewrite form)))

(provide 'setup-dsl-backend)
;;; setup-dsl-backend.el ends here
