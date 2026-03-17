;;;  -*- lexical-binding: t; -*-

;; Window manager for Emacs with modern state machine design
;; Author: Per Vognsen (original), modernized by AI
;; License: Public Domain

(require 'cl-lib)

;; State machine state structure
(cl-defstruct wm-state
  (windows nil :type list)
  (focus 0 :type integer)
  (layout 0 :type integer)
  (workspace 0 :type integer)
  (workspaces nil :type list)
  (layouts '(wm-layout-stacked-columns
             wm-layout-stacked-rows
             wm-layout-grid
             wm-layout-bisection
             wm-layout-fullscreen))
  (window-map nil :type list))  ; maps emacs windows to wm indices

;; Window structure
(cl-defstruct wm-window
  (buffer nil)
  (point 0)
  (start 0)
  (hscroll 0)
  (dedicated nil))

;; Current state instance
(defvar wm-state-instance nil)

;; Utility functions
(defun wm-window-from-emacs-window (window)
  (make-wm-window :buffer (window-buffer window)
                  :point (window-point window)
                  :start (window-start window)
                  :hscroll (window-hscroll window)
                  :dedicated (window-dedicated-p window)))

(defun wm-window-from-buffer (buffer)
  (make-wm-window :buffer buffer))

(defun wm-restore-window (window)
  (set-window-buffer nil (wm-window-buffer window))
  (set-window-point nil (wm-window-point window))
  (set-window-start nil (wm-window-start window))
  (set-window-hscroll nil (wm-window-hscroll window))
  (set-window-dedicated-p nil (wm-window-dedicated window)))

(defun wm-emacs-windows ()
  (window-list nil nil (frame-first-window)))

;; State operations
(defun wm-state-update-windows (state)
  (setf (wm-state-windows state)
        (mapcar #'wm-window-from-emacs-window (wm-emacs-windows)))
  (setf (wm-state-window-map state) nil)
  (cl-loop for i from 0
           for window in (wm-emacs-windows)
           do (push (cons window i) (wm-state-window-map state))))

(defun wm-state-update-focus (state)
  (let* ((selected (selected-window))
         (kv (assoc selected (wm-state-window-map state))))
    (when kv
      (setf (wm-state-focus state) (cdr kv)))))

(defun wm-state-reset-layout ()
  (delete-other-windows)
  (when (> (length (wm-state-windows wm-state-instance)) 1)
    (split-window)))

;; Layout functions
(defun wm-layout-fullscreen ()
  (wm-state-reset-layout)
  (let ((state wm-state-instance))
    (wm-restore-window (nth (wm-state-focus state) (wm-state-windows state)))
    (setf (wm-state-window-map state)
          (list (cons (selected-window) (wm-state-focus state))))))

(defun wm-layout-grid ()
  (wm-state-reset-layout)
  (let* ((state wm-state-instance)
         (windows (wm-state-windows state))
         (n 0)
         (len (length windows))
         (sqrt-len (truncate (sqrt len)))
         (dim (if (= len (* sqrt-len sqrt-len)) sqrt-len (1+ sqrt-len)))
         (rows (1- (/ (+ len (1- dim)) dim))))

    (when (> len 1)
      (cl-dotimes (_ rows)
        (split-window-vertically))

      (while (< n len)
        (cl-dotimes (col dim)
          (when (< n len)
            (wm-restore-window (nth n windows))
            (incf n))
          (when (and (< col (1- dim)) (< n len))
            (split-window-horizontally)
            (other-window 1)))
        (other-window 1)))

    (balance-windows)
    (other-window (wm-state-focus state))
    (setf (wm-state-window-map state)
          (cl-loop for window in (wm-emacs-windows)
                   for i from 0
                   collect (cons window i)))))

(defun wm-layout-bisection ()
  (wm-state-reset-layout)
  (let* ((state wm-state-instance)
         (windows (wm-state-windows state)))
    (cl-dotimes (n (length windows))
      (wm-restore-window (nth n windows))
      (when (< n (1- (length windows)))
        (funcall (nth (mod n 2) '(split-window-horizontally split-window-vertically)))
        (other-window 1)))
    (other-window (1+ (wm-state-focus state)))
    (balance-windows)
    (setf (wm-state-window-map state)
          (cl-loop for window in (wm-emacs-windows)
                   for i from 0
                   collect (cons window i)))))

(defun wm-layout-stacked-columns ()
  (wm-state-reset-layout)
  (let* ((state wm-state-instance)
         (windows (wm-state-windows state)))
    (when windows
      (wm-restore-window (car windows))
      (when (cdr windows)
        (split-window-horizontally)
        (other-window 1)
        (cl-loop for (window . more-windows) on (cdr windows)
                 do (progn
                      (wm-restore-window window)
                      (when more-windows
                        (split-window-vertically)
                        (other-window 1))))
        (balance-windows)
        (other-window (1+ (wm-state-focus state))))
      (setf (wm-state-window-map state)
            (cl-loop for window in (wm-emacs-windows)
                     for i from 0
                     collect (cons window i))))))

(defun wm-layout-stacked-rows ()
  (wm-state-reset-layout)
  (let* ((state wm-state-instance)
         (windows (wm-state-windows state)))
    (when windows
      (wm-restore-window (car windows))
      (when (cdr windows)
        (split-window-vertically)
        (other-window 1)
        (cl-loop for (window . more-windows) on (cdr windows)
                 do (progn
                      (wm-restore-window window)
                      (when more-windows
                        (split-window-horizontally)
                        (other-window 1))))
        (balance-windows)
        (other-window (1+ (wm-state-focus state))))
      (setf (wm-state-window-map state)
            (cl-loop for window in (wm-emacs-windows)
                     for i from 0
                     collect (cons window i))))))

(defun wm-update-layout ()
  (let ((state wm-state-instance))
    (wm-state-update-windows state)
    (funcall (nth (wm-state-layout state) (wm-state-layouts state)))
    (wm-display-status)))

(defun wm-display-status ()
  (let* ((state wm-state-instance)
         (windows (wm-state-windows state))
         (focus (wm-state-focus state))
         (message-log-max nil)
         (message-truncate-lines t)
         (status ""))
    (cl-dotimes (n (length windows))
      (let ((focused (= n focus))
            (window (nth n windows)))
        (setq status (format "%s%-20s"
                             status
                             (concat (if focused "[" " ")
                                     (format "%d: %s" (1+ n) (buffer-name (wm-window-buffer window)))
                                     (if focused "] " "  "))))))
    (message "%-35s %s"
             (format "<%d> %s: " (1+ (wm-state-workspace state))
                     (symbol-name (nth (wm-state-layout state) (wm-state-layouts state))))
             status)))

;; Interactive commands
(defun wm-cycle-layout ()
  (interactive)
  (let ((state wm-state-instance))
    (setf (wm-state-layout state)
          (mod (1+ (wm-state-layout state)) (length (wm-state-layouts state))))
    (wm-update-layout)))

(defun wm-focus-next-window ()
  (interactive)
  (let ((state wm-state-instance))
    (wm-state-update-windows state)
    (wm-state-update-focus state)
    (if (assoc (selected-window) (wm-state-window-map state))
        (setf (wm-state-focus state)
              (mod (1+ (wm-state-focus state)) (length (wm-state-windows state))))
      (other-window 1)
      (wm-state-update-focus state))
    (wm-update-layout)))

(defun wm-focus-previous-window ()
  (interactive)
  (let ((state wm-state-instance))
    (wm-state-update-windows state)
    (wm-state-update-focus state)
    (if (assoc (selected-window) (wm-state-window-map state))
        (progn
          (decf (wm-state-focus state))
          (when (< (wm-state-focus state) 0)
            (setf (wm-state-focus state) (1- (length (wm-state-windows state))))))
      (other-window -1)
      (wm-state-update-focus state))
    (wm-update-layout)))

(defun wm-remove-nth (n lst)
  (when lst
    (if (zerop n)
        (cl-rest lst)
      (cons (cl-first lst) (wm-remove-nth (1- n) (cl-rest lst))))))

(defun wm-delete (n)
  (let* ((state wm-state-instance)
         (windows (wm-state-windows state)))
    (when (and (> (length windows) 1) (< n (length windows)))
      (setf (wm-state-windows state) (wm-remove-nth n windows))
      (setf (wm-state-window-map state) nil)
      (when (= (wm-state-focus state) (length (wm-state-windows state)))
        (decf (wm-state-focus state))))
    (wm-update-layout)))

(defun wm-insert-nth (lst n val)
  (if (= n 0)
      (cons val lst)
    (cons (cl-first lst) (wm-insert-nth (cl-rest lst) (1- n) val))))

(defun wm-insert (window n)
  (let ((state wm-state-instance))
    (wm-state-update-focus state)
    (wm-state-update-windows state)
    (setf (wm-state-windows state)
          (wm-insert-nth (wm-state-windows state) n window))
    (setf (wm-state-window-map state) nil)
    (when (>= (wm-state-focus state) n)
      (incf (wm-state-focus state)))
    (wm-update-layout)))

(defun wm-push-window ()
  (interactive)
  (let ((state wm-state-instance))
    (wm-state-update-focus state)
    (wm-state-update-windows state)
    (setf (wm-state-windows state)
          (append (wm-state-windows state)
                  (list (wm-window-from-emacs-window (selected-window)))))
    (setf (wm-state-window-map state) nil)
    (wm-update-layout)))

(defun wm-insert-window ()
  (interactive)
  (let ((state wm-state-instance))
    (wm-state-update-focus state)
    (wm-state-update-windows state)
    (wm-insert (wm-window-from-emacs-window (selected-window))
               (1+ (wm-state-focus state)))))

(defun wm-pop-window ()
  (interactive)
  (let ((state wm-state-instance))
    (wm-delete (1- (length (wm-state-windows state))))))

(defun wm-delete-next-window ()
  (interactive)
  (let ((state wm-state-instance))
    (wm-state-update-focus state)
    (if (assoc (next-window) (wm-state-window-map state))
        (wm-delete (mod (1+ (wm-state-focus state)) (length (wm-state-windows state))))
      (delete-window (next-window)))))

(defun wm-delete-window ()
  (interactive)
  (let ((state wm-state-instance))
    (wm-state-update-focus state)
    (if (assoc (selected-window) (wm-state-window-map state))
        (wm-delete (wm-state-focus state))
      (delete-window))))

(defun wm-move-window-forward ()
  (interactive)
  (let ((state wm-state-instance))
    (wm-state-update-windows state)
    (wm-state-update-focus state)
    (when (< (1+ (wm-state-focus state)) (length (wm-state-windows state)))
      (cl-rotatef (nth (wm-state-focus state) (wm-state-windows state))
                  (nth (1+ (wm-state-focus state)) (wm-state-windows state)))
      (setf (wm-state-window-map state) nil)
      (incf (wm-state-focus state)))
    (wm-update-layout)))

(defun wm-move-window-backward ()
  (interactive)
  (let ((state wm-state-instance))
    (wm-state-update-windows state)
    (wm-state-update-focus state)
    (when (> (wm-state-focus state) 0)
      (cl-rotatef (nth (wm-state-focus state) (wm-state-windows state))
                  (nth (1- (wm-state-focus state)) (wm-state-windows state)))
      (setf (wm-state-window-map state) nil)
      (decf (wm-state-focus state)))
    (wm-update-layout)))

(defun wm-move-window-to-back ()
  (interactive)
  (let ((state wm-state-instance))
    (wm-state-update-focus state)
    (wm-state-update-windows state)
    (wm-push-window)
    (wm-delete-window)
    (setf (wm-state-focus state) (1- (length (wm-state-windows state))))
    (wm-update-layout)))

(defun wm-move-window-to-front ()
  (interactive)
  (let ((state wm-state-instance))
    (wm-state-update-windows state)
    (wm-insert (wm-window-from-emacs-window (selected-window)) 0)
    (wm-delete-window)
    (setf (wm-state-focus state) 0)
    (wm-update-layout)))

(defun wm-focus-window (n)
  (interactive "p")
  (let ((state wm-state-instance))
    (when (< n (length (wm-state-windows state)))
      (setf (wm-state-focus state) n))
    (wm-update-layout)))

(defun wm-manage-windows ()
  (interactive)
  (unless wm-state-instance
    (setq wm-state-instance (make-wm-state)))
  (let ((state wm-state-instance)
        (emacs-windows (wm-emacs-windows)))
    (setf (wm-state-focus state)
          (or (cl-position (selected-window) emacs-windows) 0))
    (setf (wm-state-windows state)
          (mapcar #'wm-window-from-emacs-window emacs-windows))
    (setf (wm-state-window-map state) nil)
    (wm-update-layout)))

;; Workspace management
(defun wm-restore-workspace (layout focus windows)
  (let ((state wm-state-instance))
    (setf (wm-state-layout state) layout)
    (setf (wm-state-focus state) focus)
    (setf (wm-state-windows state) windows)
    (setf (wm-state-window-map state) nil)
    (wm-update-layout)))

(defun wm-save-workspace (workspace)
  (let ((state wm-state-instance))
    (wm-state-update-focus state)
    (wm-state-update-windows state)
    (unless (assoc workspace (wm-state-workspaces state))
      (push (cons workspace nil) (wm-state-workspaces state)))
    (setf (cdr (assoc workspace (wm-state-workspaces state)))
          (list (wm-state-layout state)
                (wm-state-focus state)
                (wm-state-windows state)))))

(defun wm-switch-workspace (workspace)
  (interactive "p")
  (let ((state wm-state-instance))
    (wm-save-workspace (wm-state-workspace state))
    (unless (assoc workspace (wm-state-workspaces state))
      (push (cons workspace (list (wm-state-layout state) 0
                                  (list (wm-window-from-emacs-window (selected-window)))))
            (wm-state-workspaces state)))
    (let ((state-data (cdr (assoc workspace (wm-state-workspaces state)))))
      (setf (wm-state-workspace state) workspace)
      (wm-restore-workspace (cl-first state-data) (cl-second state-data) (cl-third state-data)))))

;; Mode definition
(define-minor-mode wm-mode
  "Window manager mode for Emacs with modern state machine design."
  :init-value t
  :local t
  :keymap (let ((keymap (make-sparse-keymap)))
            (define-key keymap (kbd "M-RET") 'wm-cycle-layout)
            (define-key keymap (kbd "C-=") 'wm-push-window)
            (define-key keymap (kbd "C-+") 'wm-insert-window)
            (define-key keymap (kbd "C--") 'wm-pop-window)
            (define-key keymap (kbd "C-_") 'wm-delete-next-window)
            (define-key keymap (kbd "C-0") 'wm-delete-window)
            (define-key keymap (kbd "s-]") 'wm-move-window-backward)
            (define-key keymap (kbd "s-[") 'wm-move-window-forward)
            (define-key keymap (kbd "s-}") 'wm-move-window-to-front)
            (define-key keymap (kbd "s-{") 'wm-move-window-to-back)
            (define-key keymap (kbd "C-<tab>") 'wm-focus-next-window)
            (define-key keymap (kbd "C-S-<tab>") 'wm-focus-previous-window)
            (define-key keymap (kbd "M-1") (lambda () (interactive) (wm-focus-window 0)))
            (define-key keymap (kbd "M-2") (lambda () (interactive) (wm-focus-window 1)))
            (define-key keymap (kbd "M-3") (lambda () (interactive) (wm-focus-window 2)))
            (define-key keymap (kbd "M-4") (lambda () (interactive) (wm-focus-window 3)))
            (define-key keymap (kbd "M-5") (lambda () (interactive) (wm-focus-window 4)))
            (define-key keymap (kbd "M-6") (lambda () (interactive) (wm-focus-window 5)))
            (define-key keymap (kbd "M-7") (lambda () (interactive) (wm-focus-window 6)))
            (define-key keymap (kbd "M-8") (lambda () (interactive) (wm-focus-window 7)))
            (define-key keymap (kbd "M-9") (lambda () (interactive) (wm-focus-window 8)))
            (define-key keymap (kbd "<f1>") (lambda () (interactive) (wm-switch-workspace 0)))
            (define-key keymap (kbd "<f2>") (lambda () (interactive) (wm-switch-workspace 1)))
            (define-key keymap (kbd "<f3>") (lambda () (interactive) (wm-switch-workspace 2)))
            (define-key keymap (kbd "<f4>") (lambda () (interactive) (wm-switch-workspace 3)))
            keymap)
  (wm-manage-windows))

(provide 'wm)
;;; wm.el ends here
