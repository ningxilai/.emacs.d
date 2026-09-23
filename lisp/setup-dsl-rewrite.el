;;; setup-dsl-rewrite.el --- phase-ordered term rewriting -*- lexical-binding: t; -*-

(require 'cl-lib)
(require 'seq)

(define-error 'setup-dsl-rewrite-error "Setup DSL rewrite error")

(defconst setup-dsl-phases
  '(:surface :normalize :desugar :control :load :action)
  "Rewrite phases, ordered from surface syntax to core terms.")

(cl-defstruct (setup-dsl-rule (:constructor setup-dsl-rule-create))
  name phase function)

(defvar setup-dsl-rules nil
  "Rules keyed by keyword, each value a `setup-dsl-rule'.")

(defconst setup-dsl-opaque-forms '(:eval :raw)
  "Forms whose arguments are ordinary Lisp and must not be rewritten.")

(defun setup-dsl--phase-index (phase)
  (or (cl-position phase setup-dsl-phases)
      (signal 'setup-dsl-rewrite-error (list "Unknown phase" phase))))

(defun setup-dsl-define-rule (name phase function)
  "Register FUNCTION as the NAME rule for PHASE." 
  (unless (and (keywordp name) (memq phase setup-dsl-phases)
               (functionp function))
    (signal 'setup-dsl-rewrite-error (list "Invalid rule" name phase function)))
  (setf (alist-get name setup-dsl-rules)
        (setup-dsl-rule-create :name name :phase phase :function function))
  name)

(defun setup-dsl--term-p (form)
  (and (consp form) (keywordp (car form))))

(defun setup-dsl--children (form phase)
  (cons (car form)
        (mapcar (lambda (child) (setup-dsl--walk child phase))
                (cdr form))))

(defun setup-dsl--walk (form phase)
  (cond
   ((not (setup-dsl--term-p form)) form)
   ((memq (car form) setup-dsl-opaque-forms) form)
   (t
    (let* ((rule (alist-get (car form) setup-dsl-rules))
           (rule-phase (and rule (setup-dsl-rule-phase rule))))
      (if (and rule (= (setup-dsl--phase-index rule-phase)
                       (setup-dsl--phase-index phase)))
          (let ((result (funcall (setup-dsl-rule-function rule) form)))
            (when (or (null result) (equal result form))
              (signal 'setup-dsl-rewrite-error
                      (list "Non-progressing or empty rewrite" form)))
            (setup-dsl--walk result phase))
        (setup-dsl--children form phase))))))

(defun setup-dsl-rewrite (form)
  "Rewrite FORM through every phase, returning a core term." 
  (seq-reduce (lambda (term phase) (setup-dsl--walk term phase))
              setup-dsl-phases form))

(defun setup-dsl--require-arity (form minimum)
  (when (< (length form) minimum)
    (signal 'setup-dsl-rewrite-error
            (list "Too few arguments" form))))

(setup-dsl-define-rule
 :option :desugar
 (lambda (form)
   (setup-dsl--require-arity form 3)
   `(:set ,(nth 1 form) ,(nth 2 form) :default)))

(setup-dsl-define-rule
 :custom :desugar
 (lambda (form)
   (setup-dsl--require-arity form 3)
   `(:set ,(nth 1 form) ,(nth 2 form) :custom)))

(setup-dsl-define-rule
 :global :desugar
 (lambda (form)
   (setup-dsl--require-arity form 3)
   `(:bind (current-global-map) ,(nth 1 form) ,(nth 2 form))))

(setup-dsl-define-rule
 :hooks :desugar
 (lambda (form)
   (let ((args (cdr form)))
     (unless (zerop (% (length args) 2))
       (signal 'setup-dsl-rewrite-error (list "`:hooks' expects pairs" form)))
     `(:seq ,@(cl-loop for (hook function) on args by #'cddr
                       collect `(:hook ,hook ,function)))))

(setup-dsl-define-rule
 :after :load
 (lambda (form)
   (setup-dsl--require-arity form 3)
   `(:load-after ,(nth 1 form) (:seq ,@(cddr form)))))

(setup-dsl-define-rule
 :when-loaded :load
 (lambda (form)
   `(:load-after (current-feature) (:seq ,@(cdr form)))))

(provide 'setup-dsl-rewrite)
;;; setup-dsl-rewrite.el ends here
