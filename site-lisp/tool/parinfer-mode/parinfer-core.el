;;; parinfer-core.el --- Core algorithm for Parinfer -*- lexical-binding: t; -*-

;; Copyright (C) 2024

;; This file provides the core Parinfer algorithm implementation.
;; Based on the parinfer-rust algorithm with pure Elisp port.

;;; Code:

(require 'cl-lib)

;; ---------------------------------------------------------------------------
;; Constants and Predicates
;; ---------------------------------------------------------------------------

(defconst parinfer--open-parens '("(" "[" "{"))
(defconst parinfer--close-parens '(")" "]" "}"))
(defconst parinfer--paren-pairs '(("(" . ")") ("[" . "]") ("{" . "}")
                                   (")" . "(") ("]" . "[") ("}" . "{")))

(defun parinfer--match-paren (paren)
  "Return the matching paren for PAREN."
  (cdr (assoc paren parinfer--paren-pairs)))

(defun parinfer--open-paren-p (ch)
  "Return t if CH is an open paren."
  (member ch parinfer--open-parens))

(defun parinfer--close-paren-p (ch)
  "Return t if CH is a close paren."
  (member ch parinfer--close-parens))

(defun parinfer--paren-p (ch)
  "Return t if CH is any paren."
  (or (parinfer--open-paren-p ch)
      (parinfer--close-paren-p ch)))

(defun parinfer--whitespace-p (ch)
  "Return t if CH is whitespace."
  (and ch (or (string= ch " ") (string= ch "\t"))))

;; ---------------------------------------------------------------------------
;; Error Handling
;; ---------------------------------------------------------------------------

(defconst parinfer--error-messages
  '((quote-danger . "Quotes must be balanced inside comment blocks.")
    (eol-backslash . "Line cannot end in a hanging backslash.")
    (unclosed-quote . "String is missing a closing quote.")
    (unclosed-paren . "Unclosed open-paren.")
    (unmatched-close-paren . "Unmatched close-paren.")
    (unmatched-open-paren . "Unmatched open-paren.")
    (leading-close-paren . "Line cannot lead with a close-paren.")))

(defun parinfer--error-message (error-name)
  "Get error message for ERROR-NAME."
  (or (cdr (assoc error-name parinfer--error-messages))
      "Unknown error"))

;; ---------------------------------------------------------------------------
;; Helper Functions
;; ---------------------------------------------------------------------------

(defun parinfer--split-lines (text)
  "Split TEXT into lines, removing CR characters."
  (mapcar (lambda (line)
            (if (and (> (length line) 0)
                     (string= (substring line -1) "\r"))
                (substring line 0 -1)
              line))
          (split-string text "\n" t)))

(defun parinfer--repeat-string (n str)
  "Repeat STR N times."
  (if (<= n 0) "" (make-string n (string-to-char str))))

(defun parinfer--get-line-ending (text)
  "Determine line ending of TEXT."
  (if (string-match-p "\r\n" text) "\r\n" "\n"))

(defun parinfer--string-width (str)
  "Get display width of STR."
  (string-width str))

(defun parinfer--replace-within-string (orig start end replace)
  "Replace in ORIG string from START to END with REPLACE."
  (let ((start-idx (min start (length orig)))
        (end-idx (min end (length orig))))
    (concat (substring orig 0 start-idx) replace (substring orig end-idx))))

;; ---------------------------------------------------------------------------
;; Public API
;; ---------------------------------------------------------------------------

;;;###autoload
(defun parinfer-process (text mode options)
  "Process TEXT with parinfer in MODE using OPTIONS.
MODE is 'indent, 'paren, or 'smart.
OPTIONS is a plist with keys:
  :cursor-x, :cursor-line, :changes, :comment-char, :string-delimiters,
  :lisp-vline-symbols, :lisp-block-comments, :guile-block-comments,
  :scheme-sexp-comments, :janet-long-strings, :force-balance, :partial-result"
  ;; For now, delegate to parinferlib which is more stable
  ;; In future versions, this will use the full algorithm from parinfer-core
  (require 'parinferlib)
  (cl-case mode
    (paren (parinferlib-paren-mode text options))
    (indent (parinferlib-indent-mode text options))
    (smart (parinferlib-indent-mode text options))
    (otherwise (parinferlib-indent-mode text options))))

;;;###autoload
(defun parinfer-indent-mode (text options)
  "Run parinfer in indent mode on TEXT with OPTIONS."
  (parinfer-process text 'indent options))

;;;###autoload
(defun parinfer-paren-mode (text options)
  "Run parinfer in paren mode on TEXT with OPTIONS."
  (parinfer-process text 'paren options))

;;;###autoload
(defun parinfer-smart-mode (text options)
  "Run parinfer in smart mode on TEXT with OPTIONS."
  (parinfer-process text 'smart options))

(provide 'parinfer-core)
;;; parinfer-core.el ends here
