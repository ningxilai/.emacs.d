;;; eww-kp-direct.el --- Direct integration of emacs-kp with EWW -*- lexical-binding: t; -*-

;; This file directly integrates emacs-kp's Knuth-Plass algorithm into EWW
;; for improved text rendering with better CJK support and optimal line breaking.
;;
;; KNOWN LIMITATIONS:
;; - Only works reliably on plain text paragraphs
;; - May interfere with complex HTML structures (tables, code blocks, etc.)
;; - Text properties may not be perfectly preserved after justification

;;; Code:

(require 'ekp)
(require 'eww)

(defgroup eww-kp nil
  "Knuth-Plass line breaking for EWW."
  :group 'eww)

(defcustom eww-kp-use-kp nil
  "Enable Knuth-Plass algorithm for EWW text rendering.
WARNING: This feature is experimental and may cause layout issues."
  :type 'boolean
  :group 'eww-kp)

(defcustom eww-kp-line-width 65
  "Target line width in characters for EWW rendering."
  :type 'integer
  :group 'eww-kp)

(defvar eww-kp--original-fill-column nil
  "Store original `fill-column' value before enabling KP.")
(make-variable-buffer-local 'eww-kp--original-fill-column)

(defun eww-kp--get-fill-column ()
  "Calculate the target fill column based on window width."
  (let ((window (get-buffer-window (current-buffer))))
    (if window
        (max 40 (- (window-width window) 4))  ; Leave some margin
      eww-kp-line-width)))

(defun eww-kp--paragraph-text-p (start end)
  "Check if region from START to END contains plain paragraph text only.
Returns nil if region contains special elements like code blocks, tables, etc."
  (save-excursion
    (save-restriction
      (narrow-to-region start end)
      (goto-char (point-min))
      ;; Check for problematic text properties
      (let ((has-problematic-props nil))
        (while (and (< (point) (point-max)) (not has-problematic-props))
          (let ((props (text-properties-at (point))))
            ;; Skip if has display property (images, special formatting)
            (when (or (plist-get props 'display)
                      (plist-get props 'intangible)
                      (plist-get props 'composition)
                      ;; Skip if has special shr/eww faces that indicate special elements
                      (memq (plist-get props 'face) '(shr-code shr-pre shr-h1 shr-h2 shr-h3)))
              (setq has-problematic-props t)))
          (goto-char (or (next-property-change (point)) (point-max))))
        (not has-problematic-props)))))

(defun eww-kp--justified-p (start end)
  "Check if region from START to END has already been justified."
  (get-text-property start 'eww-kp-justified))

(defun eww-kp--apply-justification (start end)
  "Apply Knuth-Plass justification to region from START to END.
This function modifies the buffer content and should be used with caution.
Returns the new end position after modification, or nil if not applied."
  (when (and (> end start)
             (not (eww-kp--justified-p start end))
             (eww-kp--paragraph-text-p start end))
    (let* ((plain-text (buffer-substring-no-properties start end))
           ;; Check if this looks like a paragraph (contains words, not too short)
           (word-count (length (split-string plain-text "[[:space:]]+" t))))
      (when (>= word-count 5)  ; Only process paragraphs with 5+ words
        (condition-case err
            (let* ((line-pixel (* (eww-kp--get-fill-column) (frame-char-width)))
                   ;; Use ekp-dp-line-breaks to get break points only
                   (break-points (ekp-dp-line-breaks plain-text line-pixel)))
              (when (and break-points (> (length break-points) 0))
                ;; Create justified text by inserting newlines at break points
                (let ((justified-text "")
                      (text-pos 0))
                  (dolist (break break-points)
                    (setq justified-text
                          (concat justified-text
                                  (substring plain-text text-pos break)
                                  "\n"))
                    (setq text-pos break))
                  ;; Add remaining text
                  (setq justified-text
                        (concat justified-text (substring plain-text text-pos)))
                  ;; Only apply if text actually changed
                  (when (not (string= justified-text plain-text))
                    (let ((inhibit-read-only t)
                          (original-props (mapcar (lambda (pos)
                                                    (cons pos (text-properties-at pos)))
                                                  (number-sequence start (1- end))))
                          (new-end (+ start (length justified-text))))
                      (delete-region start end)
                      (insert justified-text)
                      ;; Try to restore some properties (best effort)
                      (eww-kp--restore-properties start original-props)
                      ;; Mark as justified
                      (put-text-property start new-end 'eww-kp-justified t)
                      new-end)))))
          (error
           (message "eww-kp: Error applying justification: %s" err)
           nil))))))

(defun eww-kp--restore-properties (start original-props)
  "Restore text properties from ORIGINAL-PROPS starting at START.
This is a best-effort restoration that may not be perfect."
  (let ((offset 0))
    (dolist (prop-pair original-props)
      (let ((original-pos (car prop-pair))
            (props (cdr prop-pair)))
        (when (and props (> (length props) 0))
          ;; Apply properties at corresponding position (best effort)
          (condition-case nil
              (add-text-properties (+ start offset) (+ start offset 1) props)
            (error nil)))
        (setq offset (1+ offset))))))

(defun eww-kp--process-visible-paragraphs ()
  "Process visible paragraphs in the current buffer with KP algorithm."
  (when (and eww-kp-use-kp
             (derived-mode-p 'eww-mode))
    (save-excursion
      (let ((inhibit-read-only t))
        (goto-char (point-min))
        (while (not (eobp))
          ;; Find paragraph start (skip whitespace and already processed text)
          (while (and (not (eobp))
                      (or (looking-at "^[[:space:]]*$")
                          (eww-kp--justified-p (point) (1+ (point)))))
            (forward-line 1))
          (unless (eobp)
            (let ((para-start (point)))
              ;; Find paragraph end (blank line, special element, or buffer end)
              (while (and (not (eobp))
                          (not (looking-at "^[[:space:]]*$"))
                          (not (eww-kp--justified-p (point) (1+ (point))))
                          (eww-kp--paragraph-text-p (point) (1+ (point))))
                (forward-line 1))
              (let ((para-end (point)))
                (when (> para-end para-start)
                  ;; Apply justification and update position if changed
                  (let ((new-end (eww-kp--apply-justification para-start para-end)))
                    (when new-end
                      (goto-char new-end))))))))))))

(defun eww-kp--setup ()
  "Setup KP integration for current EWW buffer."
  (when (and eww-kp-use-kp (derived-mode-p 'eww-mode))
    ;; Store original fill-column
    (setq eww-kp--original-fill-column fill-column)
    ;; Set fill-column based on window width
    (setq fill-column (eww-kp--get-fill-column))
    ;; Process paragraphs after a short delay (let EWW finish rendering)
    (run-with-idle-timer 0.5 nil #'eww-kp--process-visible-paragraphs)))

(defun eww-kp--cleanup ()
  "Cleanup KP integration for current buffer."
  (when eww-kp--original-fill-column
    (setq fill-column eww-kp--original-fill-column)
    (setq eww-kp--original-fill-column nil)))

(defun eww-kp-toggle ()
  "Toggle Knuth-Plass justification for current EWW buffer."
  (interactive)
  (if eww-kp-use-kp
      (progn
        (setq eww-kp-use-kp nil)
        (eww-kp--cleanup)
        (message "KP justification disabled"))
    (setq eww-kp-use-kp t)
    (eww-kp--setup)
    (message "KP justification enabled")))

;; Hook into EWW mode
(add-hook 'eww-mode-hook #'eww-kp--setup)
(add-hook 'kill-buffer-hook #'eww-kp--cleanup)

(provide 'eww-kp-direct)
;;; eww-kp-direct.el ends here
