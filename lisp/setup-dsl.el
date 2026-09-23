;;; setup-dsl.el --- PEG front-end and term rewriting core -*- lexical-binding: t; -*-

;; Copyright (C) 2026 include-yy
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:
;;
;; This is intentionally independent of `setup.el'.  It provides two small
;; layers only: a PEG-backed reader for a Lisp-shaped configuration language,
;; and a terminating, phase-ordered term rewriter.  Backends are ordinary
;; functions, so package managers do not become part of the core language.
;;
;; The reader accepts ordinary s-expressions plus [] and {} as grouping
;; delimiters.  This makes the surface syntax pleasant without pretending
;; that package declarations are ordinary Emacs Lisp.
;;
;; Example:
;;
;;   (setup-dsl-eval
;;    "(:package foo (:require foo) (:set foo-option t :custom))")
;;
;; A future PEG surface syntax can lower to the same forms without changing
;; the rewrite or backend layers.

;;; Code:

(require 'cl-lib)
(require 'peg)
(require 'seq)
(require 'subr-x)

(defgroup setup-dsl nil
  "A small PEG and term-rewriting configuration language."
  :group 'convenience)

(define-error 'setup-dsl-error "Setup DSL error")
(define-error 'setup-dsl-parse-error "Setup DSL parse error" 'setup-dsl-error)
(define-error 'setup-dsl-rewrite-error "Setup DSL rewrite error" 'setup-dsl-error)
(define-error 'setup-dsl-backend-error "Setup DSL backend error" 'setup-dsl-error)

(cl-defstruct (setup-dsl-location (:constructor setup-dsl-location-create))
  file line column)

(cl-defstruct (setup-dsl-rule (:constructor setup-dsl-rule-create))
  name phase rewrite)

(defconst setup-dsl-phases
  '(:surface :normalize :desugar :control :load :action :backend)
  "The legal rewrite phases, in increasing order.")

(defvar setup-dsl-rules nil
  "Alist of registered rules, keyed by keyword.
Each value is a `setup-dsl-rule'.")

(defvar setup-dsl-backends nil
  "Alist of backend functions, keyed by primitive keyword.")

(defun setup-dsl--phase-index (phase)
  (or (cl-position phase setup-dsl-phases)
      (signal 'setup-dsl-rewrite-error (list "Unknown rewrite phase" phase))))

(defun setup-dsl-register-rule (name phase function)
  "Register rewrite FUNCTION for keyword NAME at PHASE.
FUNCTION receives a form and must return a simpler form.  A rule may only
emit terms in the same or a later phase; the engine checks this property by
running phases monotonically."
  (unless (keywordp name)
    (signal 'setup-dsl-rewrite-error (list "Rule name is not a keyword" name)))
  (unless (functionp function)
    (signal 'setup-dsl-rewrite-error (list "Rule is not callable" name)))
  (setf (alist-get name setup-dsl-rules)
        (setup-dsl-rule-create :name name :phase phase :rewrite function))
  name)

(defun setup-dsl-register-backend (name function)
  "Register FUNCTION as the emitter for primitive keyword NAME."
  (unless (keywordp name)
    (signal 'setup-dsl-backend-error (list "Backend name is not a keyword" name)))
  (setf (alist-get name setup-dsl-backends) function)
  name)

;; PEG is used for lexical structure.  Keeping the recursive list parser in
;; Lisp makes the grammar small and, importantly, keeps the value stack out of
;; the semantic layer.  `peg-run' still supplies exact failure positions.
(defconst setup-dsl--token-matcher
  (peg
   (or
    (and "\\"" (* (or "\\\\" "\\\"" (and (not "\\\"") (any)))) "\\"")
    [(){}\[\],']
    (+ (and (not (or (syntax-class whitespace) [(){}\[\],'])) (any)))))
  "PEG matcher for one setup DSL token.")

(defun setup-dsl--skip-space ()
  "Skip whitespace and semicolon comments at point."
  (skip-chars-forward " \t\r\n")
  (when (eq (char-after) ?;)
    (forward-line 1)
    (setup-dsl--skip-space)))

(defun setup-dsl--token-value (text)
  "Turn token TEXT into its Lisp value without evaluating it."
  (cond
   ((member text '("(" ")" "{" "}" "[" "]" "," "'")) text)
   ((string-prefix-p "\"" text)
    (condition-case err
        (car (read-from-string text))
      (error (signal 'setup-dsl-parse-error
                     (list "Invalid string token" text err)))))
   (t
    (condition-case nil
        (car (read-from-string text))
      (error (intern text))))))

(defun setup-dsl-tokenize (text)
  "Return a list of tokens read from TEXT.
The function is deliberately pure from the caller's perspective: all buffer
state is confined to a temporary buffer."
  (with-temp-buffer
    (insert text)
    (goto-char (point-min))
    (let (tokens)
      (while (progn (setup-dsl--skip-space) (not (eobp)))
        (let ((start (point)))
          (unless (peg-run setup-dsl--token-matcher
                           (lambda (_fail)
                             (signal 'setup-dsl-parse-error
                                     (list "Unexpected input" (point)))))
            (signal 'setup-dsl-parse-error
                    (list "PEG token match failed" (point))))
          (push (setup-dsl--token-value
                 (buffer-substring-no-properties start (point)))
                tokens)))
      (nreverse tokens))))

(defun setup-dsl--parse-list (tokens open)
  "Parse a list after OPEN and return (VALUE . REST)."
  (let (values)
    (while tokens
      (let ((token (pop tokens)))
        (cond
         ((or (and (equal open "(" ) (equal token ")"))
              (and (equal open "[" ) (equal token "]"))
              (and (equal open "{" ) (equal token "}")))
          (cl-return-from setup-dsl--parse-list
            (cons (nreverse values) tokens)))
         ((member token '( ")" "]" "}"))
          (signal 'setup-dsl-parse-error (list "Mismatched delimiter" token)))
         (t
          (pcase-let ((`(,value . ,rest) (setup-dsl--parse-one (cons token tokens))))
            (push value values)
            (setq tokens rest))))))
    (signal 'setup-dsl-parse-error (list "Unclosed delimiter" open)))

(defun setup-dsl--parse-one (tokens)
  "Parse one value from TOKENS and return (VALUE . REST)."
  (let ((token (pop tokens)))
    (cond
     ((null token) (signal 'setup-dsl-parse-error (list "Unexpected end of input")))
     ((member token '("(" "[" "{"))
      (setup-dsl--parse-list tokens token))
     ((equal token "'")
      (pcase-let ((`(,value . ,rest) (setup-dsl--parse-one tokens)))
        (cons (list 'quote value) rest)))
     ((member token '( ")" "]" "}"))
      (signal 'setup-dsl-parse-error (list "Unexpected delimiter" token)))
     (t (cons token tokens)))))

(defun setup-dsl-parse-string (text)
  "Parse TEXT into a list of surface terms.
The returned value contains data only; no form is evaluated."
  (let ((tokens (setup-dsl-tokenize text)) forms)
    (while tokens
      (pcase-let ((`(,form . ,rest) (setup-dsl--parse-one tokens)))
        (push form forms)
        (setq tokens rest)))
    (nreverse forms)))

(defun setup-dsl--dsl-form-p (form)
  (and (consp form) (keywordp (car form))))

(defun setup-dsl--rewrite-tree (form phase)
  "Rewrite DSL FORM at PHASE, preserving ordinary Lisp expressions.
Only lists whose car is a keyword are interpreted as DSL terms; this is the
important boundary that lets `:eval' contain arbitrary Elisp."
  (if (not (setup-dsl--dsl-form-p form))
      form
    (let* ((rule (alist-get (car form) setup-dsl-rules))
           (rule-phase (and rule (setup-dsl-rule-phase rule))))
      (if (and rule (= (setup-dsl--phase-index rule-phase)
                       (setup-dsl--phase-index phase)))
          (let ((new (funcall (setup-dsl-rule-rewrite rule) form)))
            (when (equal new form)
              (signal 'setup-dsl-rewrite-error
                      (list "Non-progressing rewrite" form)))
            (setup-dsl--rewrite-tree new phase))
        (cons (car form)
              (mapcar (lambda (arg) (setup-dsl--rewrite-tree arg phase))
                      (cdr form)))))))

(defun setup-dsl-rewrite (form)
  "Rewrite FORM through all registered phases.
Rules run in a fixed order, which makes expansion deterministic and prevents
higher-level sugar from reappearing after it has been eliminated."
  (seq-reduce (lambda (term phase) (setup-dsl--rewrite-tree term phase))
              setup-dsl-phases form))

(defun setup-dsl--emit (form)
  "Emit primitive FORM as Emacs Lisp."
  (if (not (setup-dsl--dsl-form-p form))
      form
    (let* ((name (car form))
           (backend (alist-get name setup-dsl-backends)))
      (if backend
          (apply backend (mapcar #'setup-dsl--emit (cdr form)))
        (signal 'setup-dsl-backend-error (list "No backend for primitive" name))))))

(defun setup-dsl-compile (form)
  "Rewrite and emit one surface FORM."
  (setup-dsl--emit (setup-dsl-rewrite form)))

(defun setup-dsl-eval-string (text)
  "Parse, rewrite and evaluate all forms in TEXT.
This is intended for loading a generated or explicitly trusted config file."
  (mapc (lambda (form) (eval (setup-dsl-compile form) lexical-binding))
        (setup-dsl-parse-string text)))

;; Primitive forms.  They intentionally use direct Emacs 32 APIs and do not
;; depend on `setup.el'.
(setup-dsl-register-backend
 :seq (lambda (&rest forms) `(progn ,@forms)))
(setup-dsl-register-backend :eval (lambda (form) form))
(setup-dsl-register-backend
 :require (lambda (feature) `(require ',feature nil t)))
(setup-dsl-register-backend
 :set (lambda (variable value kind)
        (pcase kind
          (:custom `(customize-set-variable ',variable ,value))
          (:default `(set-default-toplevel-value ',variable ,value))
          (:local `(setq-local ,variable ,value))
          (_ (signal 'setup-dsl-backend-error (list "Unknown setter" kind))))))
(setup-dsl-register-backend
 :bind (lambda (map key command)
         `(if (vectorp ,key)
              (define-key ,map ,key #',command)
            (keymap-set ,map ,key #',command))))
(setup-dsl-register-backend
 :unbind (lambda (map key)
           `(if (vectorp ,key)
                (define-key ,map ,key nil)
              (keymap-unset ,map ,key))))
(setup-dsl-register-backend
 :hook (lambda (hook function &optional local)
         `(add-hook ',hook #',function ,local)))
(setup-dsl-register-backend
 :autoload (lambda (function feature &optional interactive)
             `(autoload #',function ,(symbol-name feature) nil ,interactive)))
(setup-dsl-register-backend
 :advice (lambda (symbol where function)
           `(advice-add ',symbol ,where #',function)))
(setup-dsl-register-backend
 :context (lambda (_key _value &rest body) `(progn ,@body)))
(setup-dsl-register-backend
 :load-after (lambda (feature &rest body)
               `(with-eval-after-load ',feature (progn ,@body))))
(setup-dsl-register-backend
 :defer (lambda (seconds &rest body)
          `(run-with-idle-timer ,seconds nil (lambda () ,@body))))

;; Minimal, deliberately explicit sugar.  More elaborate package rules can be
;; registered without changing the parser or emitter.
(setup-dsl-register-rule
 :option :desugar
 (lambda (form) `(:set ,(nth 1 form) ,(nth 2 form) :default)))
(setup-dsl-register-rule
 :custom :desugar
 (lambda (form) `(:set ,(nth 1 form) ,(nth 2 form) :custom)))
(setup-dsl-register-rule
 :global :desugar
 (lambda (form) `(:bind (current-global-map) ,(nth 1 form) ,(nth 2 form))))
(setup-dsl-register-rule
 :after :load
 (lambda (form) `(:load-after ,(nth 1 form) ,@(cddr form))))

(provide 'setup-dsl)
;;; setup-dsl.el ends here
