;;; eww-kp-direct.el --- Direct integration of emacs-kp with EWW -*- lexical-binding: t; -*-

;; This file directly integrates emacs-kp's Knuth-Plass algorithm into EWW
;; for improved text rendering with better CJK support and optimal line breaking.

;;; Code:

(require 'cl-lib)
(require 'ekp)
(require 'shr)
(require 'eww)

(defgroup eww-kp nil
  "Knuth-Plass line breaking for EWW."
  :group 'eww)

(defcustom eww-kp-use-kp nil
  "Enable Knuth-Plass algorithm for EWW text rendering."
  :type 'boolean
  :group 'eww-kp)

(defcustom eww-kp-char-width nil
  "Character width for line width calculation.
If nil, auto-detect from current font."
  :type '(choice (const nil) number)
  :group 'eww-kp)

(defvar eww-kp--processed-regions nil
  "List of already processed regions (START . END).")
(make-variable-buffer-local 'eww-kp--processed-regions)

(defun eww-kp--pixel-width ()
  "Get the pixel width for line breaking."
  (or (and (boundp 'shr-internal-width)
           (local-variable-if-set-p 'shr-internal-width)
           (symbol-value 'shr-internal-width))
      (and (boundp 'shr-width)
           (local-variable-if-set-p 'shr-width)
           (symbol-value 'shr-width)
           (* (symbol-value 'shr-width) (frame-char-width)))
      (and (fboundp 'pixel-fill-width)
           (pixel-fill-width))
      (if-let* ((window (get-buffer-window (current-buffer))))
          (* (- (window-width window) 4) (frame-char-width))
        (* 65 (frame-char-width)))))

(defun eww-kp--should-skip-p (pos)
  "Check if position POS should be skipped for KP processing."
  (when-let* ((props (text-properties-at pos)))
    (or (plist-get props 'display)
        (plist-get props 'image)
        (plist-get props 'shr-url)
        (plist-get props 'shr-indentation)
        (memq (plist-get props 'face)
              '(shr-code shr-pre shr-h1 shr-h2 shr-h3 shr-h4
                shr-h5 shr-h6 shr-heading)))))

(defun eww-kp--find-paragraphs (start end)
  "Find paragraph bounds in region START..END.
Returns list of (START . END) pairs."
  (save-excursion
    (goto-char start)
    (let (paragraphs)
      (while (< (point) end)
        (skip-chars-forward "\n" end)
        (when (and (< (point) end)
                   (not (get-text-property (point) 'eww-kp-justified))
                   (not (eww-kp--should-skip-p (point))))
          (let ((para-start (point)))
            ;; Find paragraph end (two consecutive newlines or buffer end)
            (while (and (< (point) end)
                      (not (looking-at "\n\\s-*\n")))
              (forward-char 1))
            (let ((para-end (point)))
              (push (cons para-start para-end) paragraphs))))
        (forward-char 1))
      (nreverse paragraphs))))

(defun eww-kp--apply-kp-to-paragraph (start end)
  "Apply Knuth-Plass line breaking to paragraph START..END."
  (let* ((start (max start (point-min)))
         (end (min end (point-max)))
         (text (buffer-substring-no-properties start end))
         (line-pixel (eww-kp--pixel-width))
         (word-count (length (split-string text "[[:space:]]+" t))))
    (when (and (> end start)
               (> word-count 4)
               (not (string-match-p "^[[:space:]]*$" text))
               (not (string-match-p "^https?://" text)))
      (condition-case err
          (let* ((justified-text (ekp-pixel-justify text line-pixel))
                 (changed-p (not (string= justified-text text)))
                 (inhibit-read-only t)
                 (new-end (+ start (length justified-text))))
            (when changed-p
              (delete-region start end)
              (insert justified-text)
              (put-text-property start new-end 'eww-kp-justified t)
              (put-text-property start new-end 'rear-nonsticky t))
            new-end)
        (error
         (message "[eww-kp] Error: %s" (error-message-string err))
         nil)))))

(defun eww-kp--process-region (start end)
  "Process paragraphs in region START..END with KP algorithm."
  (when (and eww-kp-use-kp
             (derived-mode-p 'eww-mode))
    (let ((width (eww-kp--pixel-width)))
      (when (and width (> width 0))
        (dolist (para (eww-kp--find-paragraphs start end))
          (eww-kp--apply-kp-to-paragraph (car para) (cdr para)))))))

(defun eww-kp--after-shr-render (start end &rest _)
  "Advice: Process paragraphs after shr rendering completes."
  (when eww-kp-use-kp
    (eww-kp--process-region start end)))

(defun eww-kp--setup ()
  "Setup KP integration for current EWW buffer."
  (when (and eww-kp-use-kp
             (derived-mode-p 'eww-mode))
    (setq eww-kp--processed-regions nil)
    (advice-add 'shr-insert-document :after #'eww-kp--after-shr-render)
    (advice-add 'shr-fill-lines :after #'eww-kp--after-shr-render)
    (run-with-idle-timer 1.5 nil
      (lambda ()
        (when (derived-mode-p 'eww-mode)
          (eww-kp--process-region (point-min) (point-max)))))))

(defun eww-kp--cleanup ()
  "Cleanup KP integration for current buffer."
  (advice-remove 'shr-insert-document #'eww-kp--after-shr-render)
  (advice-remove 'shr-fill-lines #'eww-kp--after-shr-render)
  (setq eww-kp--processed-regions nil))

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

(add-hook 'eww-mode-hook #'eww-kp--setup)
(add-hook 'kill-buffer-hook #'eww-kp--cleanup)

(provide 'eww-kp-direct)
;;; eww-kp-direct.el ends here
