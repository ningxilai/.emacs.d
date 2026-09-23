;;; setup-dsl-rewrite.el --- phase-ordered term rewriting -*- lexical-binding: t; -*-

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

(defvar setup-dsl-rules nil
  "Rules keyed by keyword, each value a `setup-dsl-rule'.")

(defvar setup-dsl-primitives nil
  "Primitive registry keyed by keyword, each value a `setup-dsl-primitive'.")

(defconst setup-dsl-opaque-forms '(:eval :raw)
  "Forms whose arguments are ordinary Lisp and must not be rewritten.")

(defun setup-dsl--phase-index (phase)
  (or (cl-position phase setup-dsl-phases)
      (signal 'setup-dsl-rewrite-error (list "Unknown phase" phase))))

(defun setup-dsl-register-primitive (name phase &rest spec)
  "Register a primitive.  SPEC is a plist describing the primitive contract.
The minimal target contract is table-driven and allows a single source of
truth for the core setup term language."
  (unless (keywordp name)
    (signal 'setup-dsl-rewrite-error (list "Invalid primitive name" name)))
  (unless (memq phase setup-dsl-phases)
    (signal 'setup-dsl-rewrite-error (list "Invalid primitive phase" name phase)))
  (let ((entry (plist-put (copy-sequence spec) :name name :phase phase)))
    (setf (alist-get name setup-dsl-primitives) entry)))

(defun setup-dsl-primitive (name)
  "Return the primitive specification for NAME, or nil."
  (alist-get name setup-dsl-primitives))

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

(defun setup-dsl--arg (form n position validator)
  (funcall validator position (nth n form)))

(defun setup-dsl--context-arg (position value)
  (if (keywordp value)
      value
    (setup-value-error position value 'keyword)))

(defconst setup-dsl-primitive-table
  '((:seq    (:phase :action :signature (:terms list)))
    (:context (:phase :control :signature (:kind keyword :target symbol :body list)))
    (:set     (:phase :action :signature (:variable symbol :value form :kind keyword)))
    (:bind    (:phase :action :signature (:map symbol :key key :command function)))
    (:hook    (:phase :action :signature (:hook symbol :function function)))
    (:autoload (:phase :load :signature (:function function :file string)))
    (:require (:phase :load :signature (:feature symbol :when boolean)))
    (:load-after (:phase :load :signature (:feature symbol :body list)))
    (:mode    (:phase :control :signature (:mode symbol :body list))))
  "Minimal primitive table for the setup DSL.
Each entry is `(NAME (:phase PHASE :signature (...)))'.  These are the
non-compressible semantic primitives we aim to lower to before interacting with
`setup.el` itself.")

(dolist (entry setup-dsl-primitive-table)
  (apply #'setup-dsl-register-primitive entry))

(setup-dsl-define-rule
 :with-feature :control
 (lambda (form)
   (setup-dsl--require-arity form 3)
   `(:context :feature
             ,(setup-value-feature 'feature (nth 1 form))
             (:seq ,@(cddr form)))))

(setup-dsl-define-rule
 :with-mode :control
 (lambda (form)
   (setup-dsl--require-arity form 3)
   `(:context :mode
             ,(setup-value-mode 'mode (nth 1 form))
             (:seq ,@(cddr form)))))

(setup-dsl-define-rule
 :with-map :control
 (lambda (form)
   (setup-dsl--require-arity form 3)
   `(:context :map
             ,(setup-value-map 'map (nth 1 form))
             (:seq ,@(cddr form)))))

(setup-dsl-define-rule
 :option :desugar
 (lambda (form)
   (setup-dsl--require-arity form 3)
   `(:set ,(setup-dsl--arg form 1 'variable #'setup-value-variable)
          ,(nth 2 form) :default)))

(setup-dsl-define-rule
 :custom :desugar
 (lambda (form)
   (setup-dsl--require-arity form 3)
   `(:set ,(setup-dsl--arg form 1 'variable #'setup-value-variable)
          ,(nth 2 form) :custom)))

(setup-dsl-define-rule
 :global :desugar
 (lambda (form)
   (setup-dsl--require-arity form 3)
   `(:bind (current-global-map)
           ,(setup-value-key 'key (nth 1 form))
           ,(setup-value-function 'command (nth 2 form)))))

(setup-dsl-define-rule
 :hook :desugar
 (lambda (form)
   (setup-dsl--require-arity form 3)
   `(:hook ,(setup-value-hook 'hook (nth 1 form))
           ,(setup-value-function 'function (nth 2 form)))))

(setup-dsl-define-rule
 :hooks :desugar
 (lambda (form)
   (let ((args (cdr form)))
     (unless (zerop (% (length args) 2))
       (signal 'setup-dsl-rewrite-error (list "`:hooks' expects pairs" form)))
     `(:seq ,@(cl-loop for (hook function) on args by #'cddr
                       collect `(:hook ,(setup-value-hook 'hook hook)
                                      ,(setup-value-function 'function function)))))))

(setup-dsl-define-rule
 :after :load
 (lambda (form)
   (setup-dsl--require-arity form 3)
   `(:load-after ,(setup-value-feature 'feature (nth 1 form))
                 (:seq ,@(cddr form)))))

(setup-dsl-define-rule
 :when-loaded :load
 (lambda (form)
   `(:load-after ,(setup-value-feature 'feature (or (car (cdr form)) 'current-feature))
                 (:seq ,@(cdr (cdr form)))))

(provide 'setup-dsl-rewrite)
;;; setup-dsl-rewrite.el ends here
