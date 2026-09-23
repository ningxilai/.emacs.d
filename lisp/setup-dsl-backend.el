;;; setup-dsl-backend.el --- Emacs Lisp and setup backends -*- lexical-binding: t; -*-

(require 'cl-lib)
(require 'setup-dsl-rewrite)

(define-error 'setup-dsl-backend-error "Setup DSL backend error")

(defun setup-dsl--emit-elisp (form)
  "Emit a core term as ordinary Emacs Lisp." 
  (if (not (setup-dsl--term-p form))
      form
    (pcase (car form)
      (:seq `(progn ,@(mapcar #'setup-dsl--emit-elisp (cdr form))))
      (:eval (cadr form))
      (:raw (cadr form))
      (:require `(require ',(cadr form) nil t))
      (:set (pcase (nth 3 form)
              (:custom `(customize-set-variable ',(nth 1 form) ,(nth 2 form)))
              (:default `(set-default-toplevel-value ',(nth 1 form) ,(nth 2 form)))
              (:local `(setq-local ,(nth 1 form) ,(nth 2 form)))
              (kind (signal 'setup-dsl-backend-error (list "Unknown setter" kind)))))
      (:bind `(if (vectorp ,(nth 2 form))
                  (define-key ,(nth 1 form) ,(nth 2 form) ,(nth 3 form))
                (keymap-set ,(nth 1 form) ,(nth 2 form) ,(nth 3 form))))
      (:unbind `(if (vectorp ,(nth 2 form))
                    (define-key ,(nth 1 form) ,(nth 2 form) nil)
                  (keymap-unset ,(nth 1 form) ,(nth 2 form))))
      (:hook `(add-hook ',(nth 1 form) #',(nth 2 form) ,(or (nth 3 form) nil)))
      (:autoload `(autoload #',(nth 1 form) ,(symbol-name (nth 2 form)) nil t))
      (:advice `(advice-add ',(nth 1 form) ,(nth 2 form) #',(nth 3 form)))
      (:load-after
       `(with-eval-after-load ',(nth 1 form)
          ,(setup-dsl--emit-elisp (nth 2 form))))
      (:defer
       `(run-with-idle-timer ,(nth 1 form) nil
                             (lambda () ,(setup-dsl--emit-elisp (nth 2 form)))))
      (:context (setup-dsl--emit-elisp (nth 3 form)))
      (head (signal 'setup-dsl-backend-error (list "No Elisp backend" head)))))))

(defun setup-dsl--setup-body (form)
  "Emit a core term as a form accepted by the uploaded `setup'."
  (if (not (setup-dsl--term-p form))
      form
    (pcase (car form)
      (:seq (mapcar #'setup-dsl--setup-body (cdr form)))
      (:eval (cadr form))
      (:raw (cadr form))
      (:require `(:require ,(nth 1 form)))
      (:set (pcase (nth 3 form)
              (:custom `(:custom ,(nth 1 form) ,(nth 2 form)))
              (:default `(:option* ,(nth 1 form) ,(nth 2 form)))
              (:local `(:local-set ,(nth 1 form) ,(nth 2 form)))
              (kind (signal 'setup-dsl-backend-error (list "Unknown setter" kind)))))
      (:bind (let ((map (nth 1 form)))
               (if (equal map '(current-global-map))
                   `(:global ,(nth 2 form) ,(nth 3 form))
                 `(:with-map ,map (:bind ,(nth 2 form) ,(nth 3 form))))))
      (:unbind `(:with-map ,(nth 1 form) (:unbind ,(nth 2 form))))
      (:hook `(:with-hook ,(nth 1 form) (:hook ,(nth 2 form))))
      (:autoload `(:autoload ,(nth 1 form)))
      (:advice `(:advice ,(nth 1 form) ,(nth 2 form) ,(nth 3 form)))
      (:load-after `(:with-feature ,(nth 1 form)
                      (:when-loaded ,(setup-dsl--setup-body (nth 2 form)))))
      (:defer `(:delay ,(nth 1 form)
                ,(setup-dsl--setup-body (nth 2 form))))
      (:context (signal 'setup-dsl-backend-error
                        (list "Context needs a host rule" form)))
      (head (signal 'setup-dsl-backend-error (list "No setup backend" head))))))

(defun setup-dsl-compile (form &optional backend)
  "Rewrite FORM and emit it using BACKEND, `:elisp' by default." 
  (let ((core (setup-dsl-rewrite form)))
    (pcase (or backend :elisp)
      (:elisp (setup-dsl--emit-elisp core))
      (:setup (setup-dsl--setup-body core))
      (backend (signal 'setup-dsl-backend-error (list "Unknown backend" backend))))))

(defun setup-dsl-expand-setup-body (body)
  "Rewrite BODY and return forms for direct insertion into `setup'.
This is the host integration point; it is not a second setup implementation."
  (apply #'append
         (mapcar (lambda (form)
                   (let ((out (setup-dsl-compile form :setup)))
                     (if (and (listp out) (eq (car out) :seq))
                         (cdr out)
                       (list out))))
                 body)))

(provide 'setup-dsl-backend)
;;; setup-dsl-backend.el ends here
