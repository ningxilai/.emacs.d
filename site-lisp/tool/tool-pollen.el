;;; tool-pollen.el --- Minor mode for editing Pollen files -*- lexical-binding: t; -*-

;; Copyright (C) 2016 Junsong Li
;; Author: Junsong Li <ljs.darkfish AT GMAIL>
;; Keywords: languages, pollen, pollenpub
;; License: LGPL-3.0-only
;; Version: 3.0
;; Package-Requires: ((emacs "25.1"))
;; URL: https://github.com/lijunsong/pollen-mode

;;; Commentary:
;; Pollen mode is a minor mode for editing Pollen files.
;; It extends racket-mode with Pollen-specific features.
;;
;; To use, ensure racket-mode is loaded and add to your init:
;;   (add-hook 'racket-mode-hook 'pollen-minor-mode)
;;
;; Or use auto-mode-alist for specific extensions:
;;   (add-to-list 'auto-mode-alist '("\\.pm\\'" . racket-mode))
;;   (add-hook 'racket-mode-hook
;;             (lambda () (when (string-match-p "\\.pm\\'" buffer-file-name)
;;                          (pollen-minor-mode 1))))

;;; Code:

(require 'cl-lib)
(require 'rx)

;; Command char constants
(defconst pollen-command-char-code ?\u25CA)
(defconst pollen-command-char (char-to-string pollen-command-char-code))
(defconst pollen-command-char-target "@")

;; Racket identifier pattern
(defconst pollen-racket-id-rx
  (rx (+ (not (any space "\n" "(" ")" "[" "]" "{" "}\"\'`\;\#\|\\")))))

;; Font lock keywords for Pollen-specific constructs
(defconst pollen-font-lock-keywords
  `((,(rx bol "#lang " (* nonl) eol) . font-lock-comment-face)
    (,(concat "◊\\(" pollen-racket-id-rx "\\)") . (1 font-lock-variable-name-face))
    (,(rx "◊" (or (+ space) "{")) . font-lock-warning-face)))

;; Tag structure for navigation
(cl-defstruct (pollen-tag
               (:constructor pollen--make-tag)
               (:copier nil))
  name lb rb)

(defun pollen--matched-right-brace-pos (pos)
  "Return position of matching right brace for left brace at POS."
  (save-excursion
    (goto-char pos)
    (when (looking-at "|") (forward-char))
    (when (= (char-after) ?{)
      (ignore-errors
        (forward-sexp 1)
        (1- (point))))))

;; Thing at point for tags
(put 'pollen--tag 'bounds-of-thing-at-point 'pollen--bounds-of-tag-at-point)

(defconst pollen--skip-chars "^[:space:]()[]{}\"'`;#|\\\n")

(defun pollen--bounds-of-tag-at-point ()
  "Return bounds of tag at point, or nil if not on a tag."
  (let ((beg (save-excursion
               (skip-chars-backward pollen--skip-chars)
               (when (looking-at (regexp-quote pollen-command-char))
                 (point)))))
    (when beg
      (let ((end (save-excursion
                   (goto-char beg)
                   (skip-chars-forward pollen--skip-chars)
                   (if (and (= (point) (1+ beg)) (looking-at ";"))
                       (1+ (point))
                     (point)))))
        (when (> end beg)
          (cons beg end))))))

(defun pollen-tag-at-point (&optional no-properties)
  "Return the tag at point, or nil if none."
  (thing-at-point 'pollen--tag no-properties))

(defun pollen--get-current-tagobj ()
  "Return tag object at point or enclosing point."
  (let ((parse-sexp-lookup-properties nil))
    (cl-labels ((make-tag ()
                  (when-let* ((tag (pollen-tag-at-point t))
                              (bounds (bounds-of-thing-at-point 'pollen--tag))
                              (name (substring tag 1))
                              ((not (string-empty-p name)))
                              (lb-pos (if (= (char-after (cdr bounds)) ?|)
                                          (1+ (cdr bounds))
                                        (cdr bounds)))
                              (rb-pos (pollen--matched-right-brace-pos lb-pos)))
                    (pollen--make-tag :name name :lb lb-pos :rb rb-pos))))
      (or (make-tag)
          (save-excursion
            (let (result)
              (while (and (null result)
                          (progn (skip-chars-backward "^{}") (not (bobp))))
                (cond ((= ?} (char-before))
                       (backward-sexp 1))
                      ((= ?{ (char-before))
                       (backward-char 2)
                       (setq result (make-tag)))))
              result))))))

(defun pollen-insert-target ()
  "Insert command char or @. Toggle between them if already inserted."
  (interactive)
  (cond ((use-region-p)
         (let* ((beg (region-beginning))
                (end (region-end))
                (text (buffer-substring-no-properties beg end)))
           (delete-region beg end)
           (insert pollen-command-char "{" text "}")
           (backward-char (1+ (length text)))))
        ((string= (char-to-string (preceding-char)) pollen-command-char)
         (delete-char -1)
         (insert pollen-command-char-target))
        (t (insert pollen-command-char))))

;; Syntax propertize for comments
(defun pollen--put-text-property-at (pos ty)
  "Set syntax property at POS to type TY."
  (put-text-property pos (1+ pos) 'syntax-table
                     (if ty (string-to-syntax ty) nil)))

(defun pollen--propertize-comment (semicolon-pos)
  "Propertize comment at SEMICOLON-POS."
  (goto-char semicolon-pos)
  (when-let* ((tag (pollen--get-current-tagobj))
              (beg (pollen-tag-lb tag))
              (end (pollen-tag-rb tag)))
    (pollen--put-text-property-at semicolon-pos ".")
    (when (string= (pollen-tag-name tag) ";")
      (pollen--put-text-property-at beg "< bn")
      (pollen--put-text-property-at end "> bn"))))

;; Minor mode keymap
(defvar pollen-minor-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd pollen-command-char-target) 'pollen-insert-target)
    map))

;;;###autoload
(define-minor-mode pollen-minor-mode
  "Minor mode for editing Pollen files.

This mode extends racket-mode with Pollen-specific features:
- Syntax highlighting for ◊ tags
- Command char insertion (bound to @)
- Tag navigation

When enabled, pollen-specific font-lock keywords are added
and the @ key is bound to insert the lozenge character."
  :lighter " ◊"
  :keymap pollen-minor-mode-map
  (if pollen-minor-mode
      (progn
        ;; Add pollen-specific font-lock keywords
        (font-lock-add-keywords nil pollen-font-lock-keywords)
        ;; Ensure font-lock is updated
        (when font-lock-mode
          (font-lock-flush)))
    ;; Remove keywords when disabled
    (font-lock-remove-keywords nil pollen-font-lock-keywords)
    (when font-lock-mode
      (font-lock-flush))))

;; Convenience function to enable pollen-mode for specific files
;;;###autoload
(defun pollen-enable-if-pollen-file ()
  "Enable pollen-minor-mode if current file is a Pollen file.
Checks file extension against known Pollen extensions."
  (when (and buffer-file-name
             (string-match-p "\\.\\(pm\\|pmd\\|pp\\|p\\)\\'" buffer-file-name))
    (pollen-minor-mode 1)))

(provide 'tool-pollen)
;;; tool-pollen.el ends here
