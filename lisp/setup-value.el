;;; setup-value.el --- typed values for setup terms -*- lexical-binding: t; -*-

;; Copyright (C) 2026 include-yy
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:
;; Values are checked at the primitive boundary.  The four classes are:
;; semantic symbols, textual values, code values, and opaque/computed values.

;;; Code:

(define-error 'setup-value-error "Invalid setup value")

(defun setup-value-error (position value expected)
  (signal 'setup-value-error
          (list :position position :value value :expected expected)))

(defun setup-value-symbol (position value)
  (if (symbolp value) value
    (setup-value-error position value 'symbol)))

(defun setup-value-text (position value)
  (if (stringp value) value
    (setup-value-error position value 'string)))

(defun setup-value-key (position value)
  (if (or (stringp value) (vectorp value)) value
    (setup-value-error position value '(or string vector))))

(defun setup-value-code (_position value) value)

(defun setup-value-opaque (_position value) value)

(defun setup-value-function (position value)
  (if (or (symbolp value)
          (and (consp value) (memq (car value) '(quote function))))
      value
    (setup-value-error position value 'function)))

(defun setup-value-boolean (position value)
  (if (or (eq value t) (null value)) value
    (setup-value-error position value 'boolean)))

(defun setup-value-list (position value)
  (if (listp value) value
    (setup-value-error position value 'list)))

(defalias 'setup-value-variable #'setup-value-symbol)
(defalias 'setup-value-feature #'setup-value-symbol)
(defalias 'setup-value-map #'setup-value-symbol)
(defalias 'setup-value-mode #'setup-value-symbol)
(defalias 'setup-value-hook #'setup-value-symbol)

(provide 'setup-value)
;;; setup-value.el ends here
