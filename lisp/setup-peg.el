;;; setup-peg.el --- PEG reader infrastructure for setup -*- lexical-binding: t; -*-

;; Copyright (C) 2026 include-yy
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:
;;
;; This file is deliberately not a DSL and defines no setup keyword.  It is
;; only a safe, data-producing reader facility.  PEG validates the complete
;; input before the ordinary Lisp reader constructs the tree; no parsed form
;; is evaluated here.  Semantic rewriting belongs to the setup expansion
;; stage, where it can share setup's source location and error machinery.

;;; Code:

(require 'peg)

(define-error 'setup-peg-error "Setup PEG input error")

(defconst setup-peg--grammar
  (peg
   (and
    (* (or (syntax-class whitespace)
           (and ";" (* (and (not "\n") (any))) (or "\n" (eob)))
           (and "\""
                (* (or "\\\\" "\\\""
                       (and (not "\"") (not (eob)) (any))))
                "\"")
           (and (not (or (syntax-class whitespace) [(){}\[\];,']))
                (any))))
    (eob)))
  "PEG matcher for the complete setup surface input.")

(defun setup-peg--error (message &optional data)
  (signal 'setup-peg-error (list message data)))

(defun setup-peg-read-string (string)
  "Return the unevaluated Lisp forms read from STRING.
The PEG pass rejects unterminated strings and malformed surface characters;
the second pass uses Emacs's reader for the Lisp-shaped representation."
  (with-temp-buffer
    (insert string)
    (goto-char (point-min))
    (unless (peg-run setup-peg--grammar
                     (lambda (failure)
                       (setup-peg--error "PEG validation failed" failure)))
      (setup-peg--error "PEG validation failed" (point)))
    (goto-char (point-min))
    (let (forms)
      (while (progn (forward-comment (buffer-size)) (not (eobp)))
        (push (condition-case err
                  (read (current-buffer))
                (error (setup-peg--error "Invalid Lisp-shaped form" err)))
              forms))
      (nreverse forms))))

(defun setup-peg-read-file (file)
  "Return unevaluated forms read from FILE." 
  (with-temp-buffer
    (insert-file-contents file)
    (setup-peg-read-string (buffer-string))))

(provide 'setup-peg)
;;; setup-peg.el ends here
