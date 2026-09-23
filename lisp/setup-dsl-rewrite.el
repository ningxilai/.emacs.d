;;; setup-dsl-rewrite.el --- typed phase-ordered term rewriting -*- lexical-binding: t; -*-

(require 'cl-lib)
(require 'seq)
(require 'setup-value)

(define-error 'setup-dsl-rewrite-error "Setup DSL rewrite error")

(defconst setup-dsl-phases
  '(:surface :normalize :desugar :control :load :action)
  "Rewrite phases, ordered from surface syntax to core terms.")

(cl-defstruct (setup-dsl-rule (:constructor setup-dsl-rule-create))
  name phase function)

(cl-defstruct (setup-dsl-primitive (:constructor setup-dsl-primitive-create))
  name phase signature validator)

(defvar setup-dsl-rules nil)
(defvar setup-dsl-primitives nil)

(defconst setup-dsl-opaque-forms '(:eval :raw)
  "Forms whose arguments are code/opaque data and are not rewritten.")

(defun setup-dsl--phase-index (phase)
  (or (cl-position phase setup-dsl-phases)
      (signal 'setup-dsl-rewrite-error (list "Unknown phase" phase))))

(defun setup-dsl-register-primitive (name phase signature validator)
  "Register primitive NAME with SIGNATURE and VALIDATOR." 
  (unless (and (keywordp name) (memq phase setup-dsl-phases)
               (listp signature) (functionp validator))
    (signal 'setup-dsl-rewrite-error (list "Invalid primitive" name)))
  (setf (alist-get name setup-dsl-primitives)
        (setup-dsl-primitive-create :name name :phase phase
                                     :signature signature :validator validator)))

(defun setup-dsl-define-rule (name phase function)
  "Register rewrite FUNCTION as the NAME rule for PHASE." 
  (unless (and (keywordp name) (memq phase setup-dsl-phases)
               (functionp function))
    (signal 'setup-dsl-rewrite-error (list "Invalid rule" name phase function)))
  (setf (alist-get name setup-dsl-rules)
        (setup-dsl-rule-create :name name :phase phase :function function))
  name)

(defun setup-dsl--term-p (form) (and (consp form) (keywordp (car form))))

(defun setup-dsl--children (form phase)
  (cons (car form) (mapcar (lambda (x) (setup-dsl--walk x phase)) (cdr form))))

(defun setup-dsl--walk (form phase)
  (cond
   ((not (setup-dsl--term-p form)) form)
   ((memq (car form) setup-dsl-opaque-forms) form)
   (t (let* ((rule (alist-get (car form) setup-dsl-rules))
             (rule-phase (and rule (setup-dsl-rule-phase rule))))
        (if (and rule (= (setup-dsl--phase-index rule-phase)
                         (setup-dsl--phase-index phase)))
            (let ((result (funcall (setup-dsl-rule-function rule) form)))
              (when (or (null result) (equal result form))
                (signal 'setup-dsl-rewrite-error
                        (list "Non-progressing or empty rewrite" form)))
              (setup-dsl--walk result phase))
          (setup-dsl--children form phase))))))

(defun setup-dsl--require-arity (form minimum)
  (when (< (length form) minimum)
    (signal 'setup-dsl-rewrite-error (list "Too few arguments" form))))

(defun setup-dsl--arg (form n position validator)
  (funcall validator position (nth n form)))

(defun setup-dsl--validate-core (form)
  "Type-check and normalize one core FORM.
This is intentionally separate from rewriting: rewrite produces structure,
then this pass checks the complete primitive boundary before code generation."
  (if (not (setup-dsl--term-p form)) form
    (let ((primitive (alist-get (car form) setup-dsl-primitives)))
      (unless primitive
        (signal 'setup-dsl-rewrite-error (list "Unknown core term" form)))
      (funcall (setup-dsl-primitive-validator primitive) form))))

(defun setup-dsl-check (form)
  "Type-check a lowered FORM recursively and return it unchanged."
  (if (or (not (setup-dsl--term-p form))
          (memq (car form) setup-dsl-opaque-forms)) form
    (let ((checked (setup-dsl--validate-core form)))
      (cons (car checked)
            (mapcar #'setup-dsl-check (cdr checked))))))

(defun setup-dsl-rewrite (form)
  "Rewrite, type-check, and normalize FORM into a core term."
  (setup-dsl-check
   (seq-reduce (lambda (term phase) (setup-dsl--walk term phase))
               setup-dsl-phases form)))

(defun setup-dsl--primitive-identity (form) form)

;; Core primitive validators.  Their signatures are executable contracts.
(defun setup-dsl--check-seq (form)
  (unless (>= (length form) 1) (signal 'setup-dsl-rewrite-error (list "Empty :seq" form))) form)
(defun setup-dsl--check-set (form)
  (setup-dsl--require-arity form 4)
  (setup-value-variable 'variable (nth 1 form))
  (setup-value-code 'value (nth 2 form))
  (unless (memq (nth 3 form) '(:default :custom :local))
    (setup-value-error 'setter (nth 3 form) '(:default :custom :local))) form)
(defun setup-dsl--check-bind (form)
  (setup-dsl--require-arity form 4)
  (or (setup-value-map 'map (nth 1 form))
      (setup-value-computed 'map (nth 1 form)))
  (setup-value-key 'key (nth 2 form))
  (setup-value-function 'command (nth 3 form)) form)
(defun setup-dsl--check-hook (form)
  (setup-dsl--require-arity form 3)
  (setup-value-hook 'hook (nth 1 form))
  (setup-value-function 'function (nth 2 form)) form)
(defun setup-dsl--check-require (form)
  (setup-dsl--require-arity form 2)
  (setup-value-feature 'feature (nth 1 form)) form)
(defun setup-dsl--check-load-after (form)
  (setup-dsl--require-arity form 3)
  (setup-value-feature 'feature (nth 1 form)) form)
(defun setup-dsl--check-autoload (form)
  (setup-dsl--require-arity form 3)
  (setup-value-function 'function (nth 1 form))
  (setup-value-text 'file (nth 2 form)) form)
(defun setup-dsl--check-mode (form)
  (setup-dsl--require-arity form 3)
  (setup-value-mode 'mode (nth 1 form))
  (setup-value-text 'pattern (nth 2 form)) form)
(defun setup-dsl--check-interpreter (form)
  (setup-dsl--require-arity form 3)
  (setup-value-mode 'mode (nth 1 form))
  (setup-value-text 'interpreter (nth 2 form)) form)

(dolist (spec '((:seq :action (:terms) setup-dsl--check-seq)
                (:set :action (:variable symbol :value code :kind setter) setup-dsl--check-set)
                (:bind :action (:map map :key key :command function) setup-dsl--check-bind)
                (:hook :action (:hook hook :function function) setup-dsl--check-hook)
                (:require :load (:feature feature) setup-dsl--check-require)
                (:autoload :load (:function function :file text) setup-dsl--check-autoload)
                (:load-after :load (:feature feature :body terms) setup-dsl--check-load-after)
                (:mode :action (:mode mode :pattern text) setup-dsl--check-mode)
                (:interpreter :action (:mode mode :interpreter text) setup-dsl--check-interpreter)
                (:eval :action (:form opaque) setup-dsl--primitive-identity)
                (:raw :action (:form opaque) setup-dsl--primitive-identity)))
  (apply #'setup-dsl-register-primitive spec))

;; Surface rewrites.
(setup-dsl-define-rule :option :desugar
  (lambda (form) (setup-dsl--require-arity form 3)
    `(:set ,(setup-dsl--arg form 1 'variable #'setup-value-variable)
           ,(nth 2 form) :default)))
(setup-dsl-define-rule :custom :desugar
  (lambda (form) (setup-dsl--require-arity form 3)
    `(:set ,(setup-dsl--arg form 1 'variable #'setup-value-variable)
           ,(nth 2 form) :custom)))
(setup-dsl-define-rule :global :desugar
  (lambda (form) (setup-dsl--require-arity form 3)
    `(:bind (current-global-map) ,(setup-value-key 'key (nth 1 form))
            ,(setup-value-function 'command (nth 2 form)))))
(setup-dsl-define-rule :bind :normalize
  (lambda (form) (setup-dsl--require-arity form 4) form))
(setup-dsl-define-rule :hook :normalize
  (lambda (form) (setup-dsl--require-arity form 3) form))
(setup-dsl-define-rule :require :normalize
  (lambda (form) (setup-dsl--require-arity form 2) form))
(setup-dsl-define-rule :autoload :normalize
  (lambda (form) (setup-dsl--require-arity form 3) form))
(setup-dsl-define-rule :mode :normalize
  (lambda (form) (setup-dsl--require-arity form 3) form))
(setup-dsl-define-rule :interpreter :normalize
  (lambda (form) (setup-dsl--require-arity form 3) form))
(setup-dsl-define-rule :hooks :desugar
  (lambda (form) (let ((args (cdr form)))
    (unless (zerop (% (length args) 2))
      (signal 'setup-dsl-rewrite-error (list "`:hooks' expects pairs" form)))
    `(:seq ,@(cl-loop for (hook function) on args by #'cddr
                      collect `(:hook ,(setup-value-hook 'hook hook)
                                         ,(setup-value-function 'function function)))))))
(setup-dsl-define-rule :after :load
  (lambda (form) (setup-dsl--require-arity form 3)
    `(:load-after ,(setup-value-feature 'feature (nth 1 form))
                  (:seq ,@(cddr form)))))
(setup-dsl-define-rule :when-loaded :load
  (lambda (form) `(:load-after current-feature (:seq ,@(cdr form)))))

(provide 'setup-dsl-rewrite)
;;; setup-dsl-rewrite.el ends here
