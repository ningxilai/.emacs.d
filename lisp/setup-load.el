;; setup-load.el -*- lexical-binding: t -*-

(require 'cl-lib)

(defgroup setup-load nil
  "Incremental package loader and compile-time :load helpers."
  :group 'convenience)

;; -------------------------
;; Customization (defaults)
;; -------------------------
(defcustom setup-load-first-idle-timer (if (daemonp) 0 1.5)
  "Idle seconds before starting incremental loading.  Set to nil to disable.
0 means start immediately (but still obey time budget)."
  :type '(choice (const :tag "Disabled" nil) number)
  :group 'setup-load)

(defcustom setup-load-idle-timer 1.5
  "Idle seconds between incremental loads."
  :type 'number
  :group 'setup-load)

(defcustom setup-load-time-budget 0.02
  "Maximum seconds to spend loading packages per idle wake-up (or immediate pass)."
  :type 'number
  :group 'setup-load)

(defcustom setup-load-debug nil
  "Enable debug messages for the loader."
  :type 'boolean
  :group 'setup-load)

(defcustom setup-load-user-lisp-dir "user-lisp"
  "Name (string or symbol) of the `user-lisp' directory under `user-emacs-directory'."
  :type '(choice string symbol)
  :group 'setup-load)

(defcustom setup-load-default-dirs '("lisp" "site-lisp")
  "Default subdirectories (other than `user-lisp') under `user-emacs-directory'."
  :type '(repeat (choice string symbol))
  :group 'setup-load)

(defun setup-load-log (fmt &rest args)
  "Debug log for setup-load."
  (when setup-load-debug
    (apply #'message (concat "[setup-load] " fmt) args)))

(defun setup-load--combined-default-dirs ()
  "Return combined default dirs list with `setup-load-user-lisp-dir' first."
  (let ((user-dir (if (symbolp setup-load-user-lisp-dir)
                      (symbol-name setup-load-user-lisp-dir)
                    setup-load-user-lisp-dir)))
    (cons user-dir setup-load-default-dirs)))

;; -------------------------
;; Compile-time helpers
;; -------------------------
(defun setup-load--normalize-arg-compile (arg)
  "Normalize ARG at compile time to a (PKG . REQ-P) cons.

Accepted ARG forms:
- SYMBOL                 => (SYMBOL . nil)
- (SYMBOL)               => treated as SYMBOL
- (require SYMBOL)       => (SYMBOL . t)
- (:require SYMBOL)      => (SYMBOL . t)
- (SYMBOL . t) / (SYMBOL . nil)

Signals `user-error' for invalid shapes."
  (cond
   ((symbolp arg) (cons arg nil))
   ((and (consp arg) (memq (car arg) '(require :require)))
    (let ((pkg (cadr arg)))
      (unless (and pkg (symbolp pkg))
        (user-error "setup :load helper: expected a symbol after (require ...): %S" arg))
      (cons pkg t)))
   ((and (listp arg) (= (length arg) 1) (symbolp (car arg)))
    (cons (car arg) nil))
   ((and (consp arg) (not (listp arg)))
    (let ((pkg (car arg)) (flag (cdr arg)))
      (unless (symbolp pkg)
        (user-error "setup :load helper: feature must be a symbol in %S" arg))
      (cond
       ((or (eq flag t) (null flag)) (cons pkg (if (eq flag t) t nil)))
       (t (user-error "setup :load helper: cdr must be `t' or nil in %S" arg)))))
   (t
    (user-error "setup :load helper: invalid argument %S" arg))))

(defun setup-load--parse-compile-args (raw-args)
  "Parse RAW-ARGS supplied to the setup helper at compile time.

Recognized keywords:
  :dirs LIST-OF-STRINGS-OR-SYMBOLS   -- overrides dirs (default includes user-lisp)
  :noerror t|nil                      -- global noerror preference
  :time-budget NUMBER                 -- per-call time budget (seconds)
  :idle-timer NUMBER                  -- per-call idle timer (seconds)
  :first-idle NUMBER                  -- per-call first idle delay (seconds)
  :now t|nil                          -- whether to start immediately

Returns a plist: (:dirs DIRS :noerror BOOL :time-budget NUM :idle-timer NUM :first-idle NUM :now BOOL :pkgs PKG-LIST)."
  (let ((pos 0)
        (n (length raw-args))
        (dirs (copy-sequence (setup-load--combined-default-dirs)))
        (global-noerror nil)
        (time-budget nil)
        (idle-timer nil)
        (first-idle nil)
        (now-flag nil)
        (pkgs '()))
    (while (< pos n)
      (let ((a (nth pos raw-args)))
        (cond
         ((eq a :dirs)
          (cl-incf pos)
          (unless (< pos n) (user-error "setup :load helper: :dirs requires a following list"))
          (let ((val (nth pos raw-args)))
            (unless (and (listp val)
                         (cl-every (lambda (x) (or (stringp x) (symbolp x))) val))
              (user-error "setup :load helper: :dirs value must be list of strings or symbols, got: %S" val))
            (setq dirs val))
          (cl-incf pos))

         ((eq a :noerror)
          (cl-incf pos)
          (unless (< pos n) (user-error "setup :load helper: :noerror requires a following t/nil"))
          (let ((val (nth pos raw-args)))
            (unless (or (eq val t) (null val))
              (user-error "setup :load helper: :noerror expects `t' or `nil', got: %S" val))
            (setq global-noerror (eq val t)))
          (cl-incf pos))

         ((eq a :time-budget)
          (cl-incf pos)
          (unless (< pos n) (user-error "setup :load helper: :time-budget requires a following number"))
          (let ((val (nth pos raw-args)))
            (unless (numberp val) (user-error ":time-budget expects a number, got: %S" val))
            (setq time-budget val))
          (cl-incf pos))

         ((eq a :idle-timer)
          (cl-incf pos)
          (unless (< pos n) (user-error "setup :load helper: :idle-timer requires a following number"))
          (let ((val (nth pos raw-args)))
            (unless (numberp val) (user-error ":idle-timer expects a number, got: %S" val))
            (setq idle-timer val))
          (cl-incf pos))

         ((eq a :first-idle)
          (cl-incf pos)
          (unless (< pos n) (user-error "setup :load helper: :first-idle requires a following number"))
          (let ((val (nth pos raw-args)))
            (unless (numberp val) (user-error ":first-idle expects a number, got: %S" val))
            (setq first-idle val))
          (cl-incf pos))

         ((eq a :now)
          (cl-incf pos)
          (unless (< pos n) (user-error "setup :load helper: :now requires a following t/nil"))
          (let ((val (nth pos raw-args)))
            (unless (or (eq val t) (null val)) (user-error ":now expects t or nil"))
            (setq now-flag (eq val t)))
          (cl-incf pos))

         (t
          (push (setup-load--normalize-arg-compile a) pkgs)
          (cl-incf pos)))))
    (list :dirs dirs :noerror global-noerror
          :time-budget time-budget :idle-timer idle-timer :first-idle first-idle
          :now now-flag :pkgs (nreverse pkgs))))

(defun setup-load--build-add-dirs-form (dirs)
  "Return a form that adds DIRS (a list of string/symbol literals) under
`user-emacs-directory' to `load-path' if not already present."
  `(let ((subdirs ',dirs))
     (dolist (d subdirs)
       (let ((path (expand-file-name (if (symbolp d) (symbol-name d) d)
                                     user-emacs-directory)))
         (unless (member path load-path)
           (push path load-path))))))

(defun setup-load--build-require-form (entry global-noerror)
  "Return a form to require the package described by ENTRY (PKG . REQ-P)."
  (let ((pkg (car entry))
        (req (cdr entry)))
    (if req
        `(require ',pkg)
      (if global-noerror
          `(require ',pkg nil t)
        `(require ',pkg)))))

(when (fboundp 'setup-define)
  (setup-define :load
    (lambda (&rest raw-args)
      "Compile-time handler for (setup (:load ...))."
      (let* ((parsed (setup-load--parse-compile-args raw-args))
             (dirs (plist-get parsed :dirs))
             (global-noerror (plist-get parsed :noerror))
             (pkgs (plist-get parsed :pkgs))
             (add-dirs-form (setup-load--build-add-dirs-form dirs))
             (require-forms (mapcar (lambda (e) (setup-load--build-require-form e global-noerror)) pkgs)))
        `(progn ,add-dirs-form ,@require-forms)))
    :documentation "Load libraries from `user-emacs-directory' subdirs."))

(when (fboundp 'setup-define)
  (setup-define :load-incremental
    (lambda (&rest raw-args)
      "Compile-time handler for (setup (:load-incremental ...)).

Generates code that:
 - adds requested `:dirs' under `user-emacs-directory' to `load-path'
 - calls `setup-load-packages-incrementally' with optional per-call overrides
 - if the runtime loader isn't available, falls back to immediate requires."
      (let* ((parsed (setup-load--parse-compile-args raw-args))
             (dirs (plist-get parsed :dirs))
             (global-noerror (plist-get parsed :noerror))
             (time-budget (plist-get parsed :time-budget))
             (idle-timer (plist-get parsed :idle-timer))
             (first-idle (plist-get parsed :first-idle))
             (now-flag (plist-get parsed :now))
             (pkgs (plist-get parsed :pkgs))
             (add-dirs-form (setup-load--build-add-dirs-form dirs))
             (quoted-list (mapcar (lambda (e) `(cons ',(car e) ,(if (cdr e) 't 'nil))) pkgs))
             (fallback-forms (mapcar (lambda (e) (setup-load--build-require-form e global-noerror)) pkgs))
             (opts (cl-loop for (k v) on (list :time-budget time-budget :idle-timer idle-timer :first-idle first-idle :now now-flag) by #'cddr
                            unless (null v) collect k and collect v)))
        ;; Build runtime call with any provided opts
        `(progn
           ,add-dirs-form
           (if (fboundp 'setup-load-packages-incrementally)
               (setup-load-packages-incrementally (list ,@quoted-list) ,@opts)
             ,@fallback-forms))))
    :documentation "Enqueue libraries for incremental loading; fallback to require."))

;; -------------------------
;; Runtime loader
;; -------------------------
(defvar setup-load-queue nil
  "FIFO queue of normalized entries (symbol or (symbol . t/nil)).")

(defun setup-load--validate-entry (entry)
  "Validate and normalize a single ENTRY at runtime."
  (cond
   ((symbolp entry) entry)
   ((and (listp entry) (= (length entry) 1) (symbolp (car entry))) (car entry))
   ((and (consp entry) (memq (car entry) '(require :require)) (symbolp (cadr entry)))
    (cons (cadr entry) t))
   ((consp entry)
    (let ((pkg (car entry)) (flag (cdr entry)))
      (unless (symbolp pkg) (user-error "Invalid setup-load entry: feature must be a symbol: %S" entry))
      (unless (or (eq flag t) (null flag)) (user-error "Invalid setup-load entry: cdr must be t or nil: %S" entry))
      (cons pkg (if (eq flag t) t nil))))
   (t (user-error "Invalid setup-load entry: %S" entry))))

(defun setup-load-validate-packages (packages)
  "Validate and normalize a list PACKAGES."
  (unless (or (null packages) (listp packages))
    (user-error "setup-load-packages-incrementally expects a list (or nil); got: %S" packages))
  (cl-loop for e in packages collect (setup-load--validate-entry e)))

(defun setup-load-queue-enqueue (&rest entries)
  "Enqueue ENTRIES (or a single list) into the internal queue."
  (when entries
    (let* ((xs (if (and (= 1 (length entries)) (listp (car entries)) (not (symbolp (car entries))))
                   (car entries)
                 entries))
           (normalized (setup-load-validate-packages xs)))
      (setq setup-load-queue (append setup-load-queue normalized))
      (setup-load-log "enqueued %d entries (new length: %d)" (length normalized) (length setup-load-queue)))))

(defun setup-load-queue-pop ()
  "Pop and return the next entry from the queue, or nil if empty."
  (when setup-load-queue
    (let ((e (car setup-load-queue)))
      (setq setup-load-queue (cdr setup-load-queue))
      e)))

(defun setup-load-queue-empty-p ()
  "Return non-nil if the internal queue is empty."
  (null setup-load-queue))

(defun setup-load-queue-length ()
  "Return the number of elements currently queued."
  (length setup-load-queue))

(defun setup-load--ensure-default-dirs ()
  "Ensure `setup-load-default-dirs' (with `setup-load-user-lisp-dir') are on `load-path'."
  (dolist (d (setup-load--combined-default-dirs))
    (let ((path (expand-file-name (if (symbolp d) (symbol-name d) d)
                                  user-emacs-directory)))
      (unless (member path load-path)
        (push path load-path)))))

(defun setup-load--now () (float-time))
(defun setup-load--within-budget-p (start budget) (< (- (setup-load--now) start) (or budget 0.0)))

(defun setup-load--process-entry (entry)
  "Process a single normalized ENTRY (honor per-entry strict require)."
  (cond
   ((symbolp entry)
    (setup-load-log "processing symbol: %S" entry)
    (ignore-errors (require entry nil t)))
   ((consp entry)
    (let ((pkg (car entry)) (req (cdr entry)))
      (setup-load-log "processing cons: %S (req=%S)" pkg req)
      (if req
          (require pkg)
        (ignore-errors (require pkg nil t)))))
   (t
    (setup-load-log "ignoring invalid entry: %S" entry))))

(defun setup-load-drain-with-budget (budget)
  "Drain the queue until BUDGET seconds elapsed or queue empty.
Errors from strict requires will propagate (per-entry)."
  (let ((start (setup-load--now)))
    (while (and (not (setup-load-queue-empty-p))
                (setup-load--within-budget-p start budget))
      (let ((entry (setup-load-queue-pop)))
        (when entry (setup-load--process-entry entry))))))

;; Create a processing closure that captures per-call budget/idle parameters.
(defun setup-load--make-processor (budget idle)
  "Return a zero-arg function that drains up to BUDGET then reschedules after IDLE seconds."
  (lambda ()
    (when (not (setup-load-queue-empty-p))
      (setup-load-drain-with-budget budget)
      (when (not (setup-load-queue-empty-p))
        (run-with-idle-timer idle nil (setup-load--make-processor budget idle))))))

;;;###autoload
(cl-defun setup-load-packages-incrementally (packages &key now time-budget idle-timer first-idle &allow-other-keys)
  "Register PACKAGES for incremental loading and optionally start processing.

PACKAGES: list of features or normalized cons (FEATURE . REQ-P).
Keyword args:
:now         t|nil        -- start immediately (honor first-idle semantics)
:time-budget number       -- seconds to spend per slice (overrides default)
:idle-timer number        -- seconds between slices (overrides default)
:first-idle number        -- initial delay before first slice (overrides default)

Examples (generated by compile-time helper):
 (setup-load-packages-incrementally (list (cons 'pkg t) ...) :time-budget 0.05 :idle-timer 1.0 :first-idle 2.0 :now t)"
  (when packages
    ;; Ensure default dirs so direct runtime calls can find files.
    (setup-load--ensure-default-dirs)
    (let ((budget (or time-budget setup-load-time-budget))
          (idle    (or idle-timer setup-load-idle-timer)))
      (let ((normalized (setup-load-validate-packages packages)))
        (apply #'setup-load-queue-enqueue normalized))
      ;; If start requested, create and schedule a per-call processor that captures the options.
      (when now
        (let ((proc (setup-load--make-processor budget idle))
              (first (if (numberp first-idle) first-idle setup-load-first-idle-timer)))
          (if (and (numberp first) (zerop first))
              ;; immediate single drain then schedule if needed
              (progn
                (setup-load-drain-with-budget budget)
                (when (not (setup-load-queue-empty-p))
                  (run-with-idle-timer idle nil proc)))
            ;; schedule first run after 'first'
            (when (and (numberp first) (not (null first)))
              (run-with-idle-timer first nil proc))))))))

(defun setup-load-start-h ()
  "Hook to start incremental loading after Emacs startup, using global defaults."
  (when (numberp setup-load-first-idle-timer)
    (if (zerop setup-load-first-idle-timer)
        (setup-load-packages-incrementally nil :now t)
      (run-with-idle-timer setup-load-first-idle-timer nil
                           (setup-load--make-processor setup-load-time-budget setup-load-idle-timer)))))

(add-hook 'emacs-startup-hook #'setup-load-start-h)

(provide 'setup-load)
;;; setup-load.el ends here
