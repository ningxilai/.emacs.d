;;; setup-value.el --- explicit setup value validation -*- lexical-binding: t; -*-

;; Copyright (C) 2026 include-yy
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:
;;
;; Configuration terms distinguish names from textual values.  Feature,
;; variable, map, hook, mode and function positions are symbols (or explicit
;; Lisp forms under `:eval'); strings remain valid only where the Emacs API
;; itself asks for text, such as key descriptions, filename patterns.
;;
;; This file is intentionally independent of `setup.el' and has no runtime
;; state.  It is used by lowering code before a term reaches the host.

;;; Code:

(define-error 'setup-value-error "Invalid setup value")

(defun setup-value-error (position value expected)
  "Signal an explicit value error for POSITION and VALUE."
  (signal 'setup-value-error
          (list :position position :value value :expected expected)))

(defun setup-value-symbol (position value)
  "Return VALUE when it is a symbol, otherwise signal an error."
  (if (symbolp value)
      value
    (setup-value-error position value 'symbol)))

(defun setup-value-key (position value)
  "Return VALUE when it is a string or vector key."
  (if (or (stringp value) (vectorp value))
      value
    (setup-value-error position value '(or string vector))))

(defun setup-value-function (position value)
  "Return VALUE when it explicitly denotes a function.
Bare symbols are accepted as function names; quoted/function forms are
preserved as explicitly supplied Lisp expressions."
  (cond
   ((symbolp value) value)
   ((and (consp value)
         (memq (car value) '(quote function)))
    value)
   (t (setup-value-error position value 'function))))

(defun setup-value-variable (position value)
  "Return VALUE when it names a variable."
  (setup-value-symbol position value))

(defun setup-value-feature (position value)
  "Return VALUE when it names a feature."
  (setup-value-symbol position value))

(defun setup-value-hook (position value)
  "Return VALUE when it names a hook."
  (setup-value-symbol position value))

(defun setup-value-mode (position value)
  "Return VALUE when it names a mode."
  (setup-value-symbol position value))

(defun setup-value-map (position value)
  "Return VALUE when it names a keymap."
  (setup-value-symbol position value))

(defun setup-value-boolean (position value)
  "Return VALUE when it is t/nil or a boolean-form."
  (if (or (eq value t) (eq value nil) (keywordp value))
      value
    (setup-value-error position value 'boolean)))

(provide 'setup-value)
;;; setup-value.el ends here
