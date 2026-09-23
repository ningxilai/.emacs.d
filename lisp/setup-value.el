;;; setup-value.el --- explicit setup value validation -*- lexical-binding: t; -*-

;; Copyright (C) 2026 include-yy
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:
;; Values are classified before lowering: semantic names are symbols, textual
;; values are strings/vectors, code is explicit Lisp, and computed/opaque
;; values are never inspected by the rewriter.

;;; Code:

(define-error 'setup-value-error "Invalid setup value")

(defun setup-value-error (position value expected)
  "Signal an explicit value error for POSITION and VALUE."
  (signal 'setup-value-error
          (list :position position :value value :expected expected)))

(defun setup-value-symbol (position value)
  "Return VALUE when it is a semantic symbol."
  (if (symbolp value) value
    (setup-value-error position value 'symbol)))

(defun setup-value-text (position value)
  "Return VALUE when it is textual data."
  (if (stringp value) value
    (setup-value-error position value 'string)))

(defun setup-value-key (position value)
  "Return VALUE when it is an Emacs key description."
  (if (or (stringp value) (vectorp value)) value
    (setup-value-error position value '(or string vector))))

(defun setup-value-code (_position value)
  "Return arbitrary Lisp VALUE as code data.
This validator deliberately does not evaluate or recursively inspect VALUE."
  value)

(defun setup-value-computed (position value)
  "Return computed VALUE when it is an explicit Lisp form."
  (if (consp value) value
    (setup-value-error position value 'form)))

(defun setup-value-function (position value)
  "Return VALUE when it explicitly denotes a function name/form."
  (cond ((symbolp value) value)
        ((and (consp value) (memq (car value) '(quote function))) value)
        (t (setup-value-error position value 'function))))

(defun setup-value-variable (position value) (setup-value-symbol position value))
(defun setup-value-feature (position value) (setup-value-symbol position value))
(defun setup-value-hook (position value) (setup-value-symbol position value))
(defun setup-value-mode (position value) (setup-value-symbol position value))
(defun setup-value-map (position value) (setup-value-symbol position value))

(provide 'setup-value)
;;; setup-value.el ends here
