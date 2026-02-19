;;; parinfer.el --- Parinfer mode for Emacs -*- lexical-binding: t; -*-

;; Copyright (C) 2016-2024  Shi Tianshu, Justin Barclay

;; Author: Shi Tianshu, Justin Barclay
;; Homepage: https://github.com/DogLooksGood/parinfer-mode
;; Version: 1.0.0
;; Package-Requires: ((emacs "29.2") (cl-lib "0.5"))
;; Keywords: lisp tools

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Parinfer is a simpler way to write Lisp.  This package provides a
;; Parinfer mode for Emacs, supporting indent mode, paren mode, and
;; smart mode.
;;
;; Usage:
;;   M-x parinfer-mode
;;
;; Commands:
;;   parinfer-toggle-mode - Toggle between indent and paren mode
;;   parinfer-switch-mode - Switch to a specific mode
;;   parinfer-indent-buffer - Fix indentation for whole buffer

;;; Code:

(require 'cl-lib)
(require 'parinferlib)
(require 'parinfer-core)

;; -----------------------------------------------------------------------------
;; Customization
;; -----------------------------------------------------------------------------

(defgroup parinfer nil
  "Parinfer mode for Emacs."
  :group 'lisp
  :prefix "parinfer-")

(defcustom parinfer-preferred-mode 'smart
  "Preferred mode for parinfer.
Can be 'smart, 'indent, or 'paren."
  :type '(choice (const :tag "Smart" smart)
                 (const :tag "Indent" indent)
                 (const :tag "Paren" paren))
  :group 'parinfer)

(defcustom parinfer-lighters
  '(" Parinfer:Indent" . " Parinfer:Paren")
  "Parinfer lighters in mode line.
The car is used in indent mode, the cdr in paren mode."
  :type '(cons string string)
  :group 'parinfer)

(defcustom parinfer-delay-invoke-threshold 6000
  "Threshold for delay processing (characters)."
  :type 'integer
  :group 'parinfer)

(defcustom parinfer-delay-invoke-idle 0.3
  "Delay time for parinfer delay processing (seconds)."
  :type 'number
  :group 'parinfer)

(defcustom parinfer-display-error nil
  "If non-nil, display error when parinfer failed."
  :type 'boolean
  :group 'parinfer)

(defcustom parinfer-smart-yank t
  "If non-nil, use smart yank behavior.
In indent mode, yanked text will be processed by parinfer.
In paren mode, yanked text is inserted as-is."
  :type 'boolean
  :group 'parinfer)

(defcustom parinfer-optimization-level 'balanced
  "Optimization level for parinfer processing.
- 'minimal: No optimization, process full sexp (most accurate)
- 'balanced: Process current line + context (default, good balance)
- 'maximum: Only process current line (fastest, less accurate)"
  :type '(choice (const :tag "Minimal" minimal)
                 (const :tag "Balanced" balanced)
                 (const :tag "Maximum" maximum))
  :group 'parinfer)

;; -----------------------------------------------------------------------------
;; Language-specific Options
;; -----------------------------------------------------------------------------

(defvar parinfer--last-processed-region nil
  "Cache of last processed region and result.
Format: (START END HASH RESULT)
where HASH is hash of the text content.")
(make-variable-buffer-local 'parinfer--last-processed-region)

(defcustom parinfer-clojure-options '()
  "Options for Clojure mode."
  :type 'plist
  :group 'parinfer)

(defcustom parinfer-janet-options
  '(:comment-char "#" :janet-long-strings t)
  "Options for Janet mode."
  :type 'plist
  :group 'parinfer)

(defcustom parinfer-lisp-options
  '(:lisp-vline-symbols t :lisp-block-comments t)
  "Options for Lisp mode."
  :type 'plist
  :group 'parinfer)

(defcustom parinfer-racket-options
  '(:lisp-vline-symbols t :lisp-block-comments t :scheme-sexp-comments t)
  "Options for Racket mode."
  :type 'plist
  :group 'parinfer)

(defvar parinfer-major-mode-options
  (list 'clojure-mode parinfer-clojure-options
        'clojurec-mode parinfer-clojure-options
        'clojurescript-mode parinfer-clojure-options
        'clojure-ts-mode parinfer-clojure-options
        'janet-mode parinfer-janet-options
        'janet-ts-mode parinfer-janet-options
        'common-lisp-mode parinfer-lisp-options
        'lisp-mode parinfer-lisp-options
        'racket-mode parinfer-racket-options)
  "Alist of major mode specific options.")

;; -----------------------------------------------------------------------------
;; Internal Variables
;; -----------------------------------------------------------------------------

(defvar parinfer--mode 'paren
  "Current parinfer mode: 'paren, 'indent, or 'smart.")
(make-variable-buffer-local 'parinfer--mode)

(defvar parinfer--first-load t
  "If non-nil, haven't switched to indent mode yet.")
(make-variable-buffer-local 'parinfer--first-load)

(defvar parinfer--text-modified nil
  "If last command modified text.")
(make-variable-buffer-local 'parinfer--text-modified)

(defvar parinfer--last-line-number -1
  "Last line number after invoke.")
(make-variable-buffer-local 'parinfer--last-line-number)

(defvar parinfer--delay-timer nil
  "Current delay timer.")
(make-variable-buffer-local 'parinfer--delay-timer)

;; -----------------------------------------------------------------------------
;; Hooks
;; -----------------------------------------------------------------------------

(defvar parinfer-mode-enable-hook nil
  "Hook called after parinfer mode is enabled.")

(defvar parinfer-mode-disable-hook nil
  "Hook called after parinfer mode is disabled.")

(defvar parinfer-switch-mode-hook nil
  "Hook called after switching mode.
Called with one argument: the new mode.")

(defvar parinfer-after-execute-hook nil
  "Hook called after parinfer executed.")

;; -----------------------------------------------------------------------------
;; Helper Functions
;; -----------------------------------------------------------------------------

(defun parinfer--lighter ()
  "Return the mode line indicator."
  (if (eq 'paren parinfer--mode)
      (cdr parinfer-lighters)
    (car parinfer-lighters)))

(defun parinfer-current-mode ()
  "Return the current parinfer mode."
  parinfer--mode)

(defun parinfer--in-string-p ()
  "Return non-nil if point is in a string."
  (nth 3 (syntax-ppss)))

(defun parinfer--in-comment-p ()
  "Return non-nil if point is in a comment."
  (nth 4 (syntax-ppss)))

(defun parinfer--in-comment-or-string-p ()
  "Return non-nil if point is in comment or string."
  (or (parinfer--in-string-p) (parinfer--in-comment-p)))

(defun parinfer--empty-line-p ()
  "Return non-nil if current line is empty."
  (or (eq (line-beginning-position) (line-end-position))
      (string-match-p "^[[:blank:]]+$"
                      (buffer-substring-no-properties
                       (line-beginning-position)
                       (line-end-position)))))

(defun parinfer--toplevel-line-p ()
  "Return non-nil if current line starts with a toplevel form."
  (string-match-p "^[({\\[#`]"
                  (buffer-substring-no-properties
                   (line-beginning-position)
                   (line-end-position))))

(defun parinfer--goto-line (n)
  "Go to beginning of line N."
  (goto-char (point-min))
  (forward-line (1- n)))

(defun parinfer--goto-current-toplevel ()
  "Go to beginning of current toplevel sexp."
  (back-to-indentation)
  (let ((prev-pos (point-max)))
    (while (and (not (eq (point) (point-min)))
                (not (eq (point) prev-pos))
                (or (parinfer--in-comment-or-string-p)
                    (parinfer--empty-line-p)
                    (not (eq (point) (line-beginning-position)))
                    (not (parinfer--toplevel-line-p))))
      (setq prev-pos (point))
      (forward-line -1)
      (back-to-indentation))
    (when (eq prev-pos (point))
      (beginning-of-line))))

(defun parinfer--goto-next-toplevel ()
  "Go to beginning of next toplevel sexp."
  (if (eq (line-end-position) (point-max))
      (end-of-line)
    (forward-line 1)
    (let ((found nil))
      (while (not found)
        (if (eq (line-end-position) (point-max))
            (progn (end-of-line) (setq found t))
          (progn
            (back-to-indentation)
            (if (and (not (or (parinfer--in-comment-or-string-p)
                              (parinfer--empty-line-p)))
                     (eq (point) (line-beginning-position))
                     (parinfer--toplevel-line-p))
                (progn (beginning-of-line) (setq found t))
              (forward-line 1))))))))

(defun parinfer--goto-previous-toplevel ()
  "Go to beginning of previous toplevel sexp."
  (parinfer--goto-current-toplevel)
  (forward-line -1)
  (parinfer--goto-current-toplevel))

(defun parinfer--cursor-x ()
  "Get cursor x position."
  (- (point) (line-beginning-position)))

(defun parinfer--cursor-line ()
  "Get cursor line number (0-indexed)."
  (1- (line-number-at-pos)))

;; -----------------------------------------------------------------------------
;; Mode Switching
;; -----------------------------------------------------------------------------

(defun parinfer--switch-to-indent-mode-1 ()
  "Switch to indent mode."
  (setq parinfer--mode 'indent)
  (setq parinfer--first-load nil)
  (run-hook-with-args 'parinfer-switch-mode-hook 'indent)
  (force-mode-line-update))

(defun parinfer--switch-to-paren-mode ()
  "Switch to paren mode."
  (when parinfer--delay-timer
    (cancel-timer parinfer--delay-timer)
    (setq parinfer--delay-timer nil))
  (setq parinfer--mode 'paren)
  (run-hook-with-args 'parinfer-switch-mode-hook 'paren)
  (force-mode-line-update))

(defun parinfer--switch-to-smart-mode ()
  "Switch to smart mode."
  (setq parinfer--mode 'smart)
  (setq parinfer--first-load nil)
  (run-hook-with-args 'parinfer-switch-mode-hook 'smart)
  (force-mode-line-update))

(defun parinfer--set-default-state ()
  "Set up default state."
  (setq parinfer--text-modified nil))

;; -----------------------------------------------------------------------------
;; Execution
;; -----------------------------------------------------------------------------

(defun parinfer--prepare ()
  "Prepare input for parinfer.
Uses `parinfer-optimization-level' to determine processing scope."
  (let* ((window-start-pos (window-start))
         (line-number (line-number-at-pos))
         (cursor-x (parinfer--cursor-x))
         start end text
         (major-mode-opts (cdr (assq major-mode parinfer-major-mode-options)))
         (opts (append (list :cursor-x cursor-x
                             :cursor-line 0)
                       major-mode-opts)))
    (cl-case parinfer-optimization-level
      ;; Maximum optimization: only current line
      (maximum
       (setq start (line-beginning-position))
       (setq end (line-end-position))
       (setq text (buffer-substring-no-properties start end))
       (plist-put opts :cursor-line 0))
      
      ;; Balanced: current line + 3 lines before and after for context
      (balanced
       (setq start (save-excursion
                     (forward-line -3)
                     (line-beginning-position)))
       (setq end (save-excursion
                   (forward-line 3)
                   (line-end-position)))
       (setq text (buffer-substring-no-properties start end))
       (plist-put opts :cursor-line (min 3 (count-lines start (point)))))
      
      ;; Minimal: full toplevel (original behavior)
      (otherwise
       (setq start (save-excursion (parinfer--goto-previous-toplevel) (point)))
       (setq end (save-excursion (parinfer--goto-next-toplevel) (point)))
       (setq text (buffer-substring-no-properties start end))
       (plist-put opts :cursor-line (- line-number (line-number-at-pos start)))))
    
    (list :text text :opts opts :start start :end end
          :window-start-pos window-start-pos :line-number line-number)))

(defun parinfer--apply-result (result context)
  "Apply parinfer RESULT to current buffer.
CONTEXT contains execution context.
Optimized to only apply actual changes."
  (let* ((start (plist-get context :start))
         (end (plist-get context :end))
         (window-start-pos (plist-get context :window-start-pos))
         (line-number (plist-get context :line-number))
         (err (plist-get result :error)))
    (if (and parinfer-display-error err)
        (let ((err-line (+ (line-number-at-pos start) (plist-get err :line-no))))
          (message "Parinfer error: %s at line: %s column: %s"
                   (plist-get err :message)
                   err-line
                   (save-excursion
                     (parinfer--goto-line err-line)
                     (forward-char (plist-get err :x))
                     (current-column))))
      (let ((changed-lines (plist-get result :changed-lines)))
        (when (and (plist-get result :success) changed-lines)
          ;; Only apply changes if there are actual differences
          (let ((has-changes nil))
            (save-excursion
              (dolist (line changed-lines)
                (let ((line-idx (plist-get line :line-no))
                      (new-content (plist-get line :line)))
                  (parinfer--goto-line (+ (line-number-at-pos start) line-idx))
                  (let ((old-content (buffer-substring-no-properties
                                      (line-beginning-position)
                                      (line-end-position))))
                    (when (not (string= old-content new-content))
                      (setq has-changes t)
                      (delete-region (line-beginning-position) (line-end-position))
                      (insert new-content))))))
            ;; Only reposition cursor if we made changes
            (when has-changes
              (parinfer--goto-line line-number)
              (forward-char (plist-get result :cursor-x)))))
        (setq parinfer--text-modified nil)
        (run-hooks 'parinfer-after-execute-hook)))))

(defun parinfer--invoke-parinfer ()
  "Invoke parinfer on current buffer.
Uses caching to avoid reprocessing unchanged regions."
  (when parinfer--delay-timer
    (cancel-timer parinfer--delay-timer)
    (setq parinfer--delay-timer nil))
  (let* ((context (parinfer--prepare))
         (text (plist-get context :text))
         (start (plist-get context :start))
         (end (plist-get context :end))
         (current-hash (sxhash text)))
    ;; Check cache first
    (if (and parinfer--last-processed-region
             (= start (car parinfer--last-processed-region))
             (= end (cadr parinfer--last-processed-region))
             (= current-hash (caddr parinfer--last-processed-region)))
        ;; Use cached result
        (parinfer--apply-result (nth 3 parinfer--last-processed-region) context)
      ;; Process normally
      (if (> (length text) parinfer-delay-invoke-threshold)
          (setq parinfer--delay-timer
                (run-with-idle-timer
                 parinfer-delay-invoke-idle
                 nil
                 #'parinfer-indent-instantly))
        (let ((result (parinfer-process text parinfer--mode (plist-get context :opts))))
          ;; Cache the result
          (setq parinfer--last-processed-region
                (list start end current-hash result))
          (parinfer--apply-result result context))))))

(defun parinfer--invoke-parinfer-instantly ()
  "Invoke parinfer immediately.
Skips processing if region is empty or unchanged."
  (unless (or (bound-and-true-p multiple-cursors-mode)
              (use-region-p))
    (let* ((context (parinfer--prepare))
           (text (plist-get context :text))
           (start (plist-get context :start))
           (end (plist-get context :end)))
      ;; Skip if text is empty or too small
      (when (> (length text) 0)
        (let ((current-hash (sxhash text)))
          ;; Check cache
          (if (and parinfer--last-processed-region
                   (= start (car parinfer--last-processed-region))
                   (= end (cadr parinfer--last-processed-region))
                   (= current-hash (caddr parinfer--last-processed-region)))
              (parinfer--apply-result (nth 3 parinfer--last-processed-region) context)
            (let* ((opts (plist-get context :opts))
                   (result (parinfer-process text parinfer--mode opts)))
              (setq parinfer--last-processed-region
                    (list start end current-hash result))
              (parinfer--apply-result result context))))))))

(defun parinfer--invoke-parinfer-when-necessary ()
  "Invoke parinfer when necessary."
  (when this-command
    (cond
     ((not (symbolp this-command)) nil)
     ((eq parinfer--mode 'paren)
      (parinfer--invoke-parinfer-instantly))
     ((eq parinfer--mode 'indent)
      (parinfer--invoke-parinfer))
     ((eq parinfer--mode 'smart)
      (parinfer--invoke-parinfer-instantly))
     (t nil)))
  (setq parinfer--last-line-number (line-number-at-pos)))

;; -----------------------------------------------------------------------------
;; Smart Yank
;; -----------------------------------------------------------------------------

(defun parinfer--smart-yank ()
  "Yank with parinfer processing if in indent mode."
  (interactive)
  (if (and parinfer-smart-yank (eq parinfer--mode 'indent))
      (let ((yank-str nil)
            (m major-mode))
        (with-temp-buffer
          (yank)
          (funcall m)
          (parinfer-indent-buffer)
          (setq yank-str (buffer-substring-no-properties (point-min) (point-max))))
        (insert yank-str))
    (yank)))

;; -----------------------------------------------------------------------------
;; Commands
;; -----------------------------------------------------------------------------

;;;###autoload
(defun parinfer-toggle-mode ()
  "Toggle between indent and paren mode."
  (interactive)
  (if (eq 'paren parinfer--mode)
      (parinfer--switch-to-indent-mode-1)
    (parinfer--switch-to-paren-mode)))

;;;###autoload
(defun parinfer-switch-mode (&optional mode)
  "Switch to specific MODE ('indent, 'paren, or 'smart)."
  (interactive)
  (let ((new-mode (or mode
                      (intern (completing-read "Choose parinfer mode: "
                                               '("smart" "indent" "paren")
                                               nil t)))))
    (cl-case new-mode
      (indent (parinfer--switch-to-indent-mode-1))
      (paren (parinfer--switch-to-paren-mode))
      (smart (parinfer--switch-to-smart-mode))
      (otherwise (parinfer--switch-to-smart-mode)))))

;;;###autoload
(defun parinfer-indent ()
  "Run parinfer in indent mode."
  (interactive)
  (parinfer--invoke-parinfer))

;;;###autoload
(defun parinfer-indent-instantly ()
  "Run parinfer instantly."
  (interactive)
  (parinfer--invoke-parinfer-instantly))

;;;###autoload
(defun parinfer-indent-buffer ()
  "Run parinfer on whole buffer."
  (interactive)
  (let* ((window-start-pos (window-start))
         (cursor-line (parinfer--cursor-line))
         (cursor-x (parinfer--cursor-x))
         (major-mode-opts (cdr (assq major-mode parinfer-major-mode-options)))
         (opts (append (list :cursor-x cursor-x
                             :cursor-line cursor-line)
                       major-mode-opts))
         (text (buffer-substring-no-properties (point-min) (point-max)))
         (result (parinfer-process text 'indent opts))
         (changed-lines (plist-get result :changed-lines)))
    (when (and (plist-get result :success) changed-lines)
      (save-excursion
        (dolist (line changed-lines)
          (parinfer--goto-line (1+ (plist-get line :line-no)))
          (delete-region (line-beginning-position) (line-end-position))
          (insert (plist-get line :line))))
      (parinfer--goto-line (1+ cursor-line))
      (forward-char (plist-get result :cursor-x))
      (set-window-start (selected-window) window-start-pos))))

;;;###autoload
(defun parinfer-toggle-debug ()
  "Toggle debug mode."
  (interactive)
  (message "Parinfer debug toggle not implemented yet"))

;; -----------------------------------------------------------------------------
;; Mode Enable/Disable
;; -----------------------------------------------------------------------------

(defun parinfer--mode-enable ()
  "Enable parinfer mode."
  (setq parinfer--mode parinfer-preferred-mode)
  (setq parinfer--last-line-number (line-number-at-pos))
  (setq-local indent-tabs-mode nil)
  (add-hook 'post-command-hook #'parinfer--invoke-parinfer-when-necessary nil t)
  (parinfer--set-default-state)
  (when parinfer-smart-yank
    (define-key parinfer-mode-map [remap yank] 'parinfer--smart-yank))
  (run-hooks 'parinfer-mode-enable-hook))

(defun parinfer--mode-disable ()
  "Disable parinfer mode."
  (remove-hook 'post-command-hook #'parinfer--invoke-parinfer-when-necessary t)
  (when parinfer--delay-timer
    (cancel-timer parinfer--delay-timer)
    (setq parinfer--delay-timer nil))
  ;; Clear cache when disabling
  (setq parinfer--last-processed-region nil)
  (run-hooks 'parinfer-mode-disable-hook))

;; -----------------------------------------------------------------------------
;; Keymaps
;; -----------------------------------------------------------------------------

(defvar parinfer-mode-map
  (let ((map (make-sparse-keymap)))
    map)
  "Keymap for parinfer mode.")

;; -----------------------------------------------------------------------------
;; Mode Definition
;; -----------------------------------------------------------------------------

;;;###autoload
(define-minor-mode parinfer-mode
  "Parinfer mode for simpler Lisp editing.

Parinfer automatically manages parentheses based on indentation.
Three modes are available:
- Indent Mode: You control indentation, parinfer fixes parens
- Paren Mode: You control parens, parinfer fixes indentation  
- Smart Mode: Best of both worlds

Commands:
\\[parinfer-toggle-mode] - Toggle between indent and paren mode
\\[parinfer-switch-mode] - Switch to a specific mode
\\[parinfer-indent-buffer] - Fix indentation for whole buffer"
  :lighter (:eval (parinfer--lighter))
  :init-value nil
  :keymap parinfer-mode-map
  (if parinfer-mode
      (parinfer--mode-enable)
    (parinfer--mode-disable)))

;; -----------------------------------------------------------------------------
;; Provide
;; -----------------------------------------------------------------------------

(provide 'parinfer)
;;; parinfer.el ends here
