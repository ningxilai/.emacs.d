;;; setup-dsl-rewrite.el --- minimal typed setup term core -*- lexical-binding: t; -*-

(require 'cl-lib)
(require 'seq)
(require 'setup-value)

(define-error 'setup-dsl-rewrite-error "Setup term error")

(defconst setup-dsl-phases '(:check :lower)
  "The only phases in the minimal core: validation, then lowering.")

(cl-defstruct (setup-dsl-primitive
               (:constructor setup-dsl-primitive-create))
  name signature validator)

(defvar setup-dsl-primitives nil
  "Registry of primitive terms keyed by keyword.")

(defvar setup-dsl-extensions nil
  "Optional user extensions, keyed by keyword.
An extension is a function taking one term and returning a primitive term.")

(defconst setup-dsl-opaque-forms '(:eval :raw))

(defun setup-dsl-register-primitive (name signature validator)
  "Register primitive NAME with SIGNATURE and VALIDATOR."
  (unless (and (keywordp name) (listp signature) (functionp validator))
    (signal 'setup-dsl-rewrite-error (list "Invalid primitive" name)))
  (setf (alist-get name setup-dsl-primitives)
        (setup-dsl-primitive-create :name name
                                     :signature signature
                                     :validator validator))
  name)

(defun setup-dsl-register-extension (name function)
  "Register optional surface extension NAME implemented by FUNCTION."
  (unless (and (keywordp name) (functionp function))
    (signal 'setup-dsl-rewrite-error (list "Invalid extension" name)))
  (setf (alist-get name setup-dsl-extensions) function)
  name)

(defun setup-dsl-primitive (name)
  (alist-get name setup-dsl-primitives))

(defun setup-dsl--term-p (form)
  (and (consp form) (keywordp (car form))))

(defun setup-dsl--arity (form n)
  (when (< (length form) n)
    (signal 'setup-dsl-rewrite-error (list "Too few arguments" form))))

(defun setup-dsl--check-seq (form)
  (setup-value-list 'terms (cdr form)) form)
(defun setup-dsl--check-eval (form)
  (setup-dsl--arity form 2) form)
(defun setup-dsl--check-set (form)
  (setup-dsl--arity form 4)
  (setup-value-variable 'variable (nth 1 form))
  (setup-value-code 'value (nth 2 form))
  (unless (memq (nth 3 form) '(:default :custom :local))
    (setup-value-error 'setter (nth 3 form) '(:default :custom :local))) form)
(defun setup-dsl--check-bind (form)
  (setup-dsl--arity form 4)
  (unless (or (symbolp (nth 1 form))
              (equal (nth 1 form) '(current-global-map))
              (consp (nth 1 form)))
    (setup-value-error 'map (nth 1 form) 'map))
  (setup-value-key 'key (nth 2 form))
  (setup-value-function 'command (nth 3 form)) form)
(defun setup-dsl--check-unbind (form)
  (setup-dsl--arity form 3)
  (setup-value-key 'key (nth 2 form)) form)
(defun setup-dsl--check-hook (form)
  (setup-dsl--arity form 3)
  (setup-value-hook 'hook (nth 1 form))
  (setup-value-function 'function (nth 2 form)) form)
(defun setup-dsl--check-require (form)
  (setup-dsl--arity form 2)
  (setup-value-feature 'feature (nth 1 form)) form)
(defun setup-dsl--check-autoload (form)
  (setup-dsl--arity form 3)
  (setup-value-function 'function (nth 1 form))
  (setup-value-text 'file (nth 2 form)) form)
(defun setup-dsl--check-load-after (form)
  (setup-dsl--arity form 3)
  (setup-value-feature 'feature (nth 1 form)) form)
(defun setup-dsl--check-defer (form)
  (setup-dsl--arity form 3)
  (unless (numberp (nth 1 form))
    (setup-value-error 'seconds (nth 1 form) 'number)) form)
(defun setup-dsl--check-context (form)
  (setup-dsl--arity form 4)
  (unless (memq (nth 1 form) '(:feature :mode :map :hook :function))
    (setup-value-error 'context (nth 1 form) 'context-kind)) form)
(defun setup-dsl--check-advice (form)
  (setup-dsl--arity form 4)
  (setup-value-function 'function (nth 3 form)) form)

(dolist (spec '((:seq (:terms list) setup-dsl--check-seq)
                (:eval (:form code) setup-dsl--check-eval)
                (:raw (:form opaque) setup-dsl--check-eval)
                (:set (:variable symbol :value code :setter keyword) setup-dsl--check-set)
                (:bind (:map map :key key :command function) setup-dsl--check-bind)
                (:unbind (:map map :key key) setup-dsl--check-unbind)
                (:hook (:hook symbol :function function) setup-dsl--check-hook)
                (:require (:feature symbol) setup-dsl--check-require)
                (:autoload (:function function :file text) setup-dsl--check-autoload)
                (:load-after (:feature symbol :body terms) setup-dsl--check-load-after)
                (:defer (:seconds number :body terms) setup-dsl--check-defer)
                (:context (:kind context :value opaque :body terms) setup-dsl--check-context)
                (:advice (:symbol symbol :where keyword :function function) setup-dsl--check-advice)))
  (apply #'setup-dsl-register-primitive spec))

(defun setup-dsl-check (form)
  "Validate FORM recursively against the primitive schema."
  (cond
   ((not (setup-dsl--term-p form)) form)
   ((memq (car form) setup-dsl-opaque-forms) form)
   (t (let ((primitive (setup-dsl-primitive (car form))))
        (if primitive
            (funcall (setup-dsl-primitive-validator primitive) form)
          (signal 'setup-dsl-rewrite-error
                  (list "Unknown term; core has no implicit sugar" form)))))))

(defun setup-dsl-rewrite (form)
  "Check FORM and return its canonical primitive term.
The minimal core intentionally performs no built-in sugar rewriting.  Optional
extensions must explicitly lower to registered primitives before checking."
  (let ((lowered (if-let ((extension (alist-get (car-safe form)
                                                 setup-dsl-extensions)))
                     (funcall extension form)
                   form)))
    (setup-dsl-check lowered)))

(provide 'setup-dsl-rewrite)
;;; setup-dsl-rewrite.el ends here
