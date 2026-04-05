;;;   -*- lexical-binding: t; -*-

;;; Code:

;; elisp

(setup (:elpaca no-littering)
  (:init (let ((dir (no-littering-expand-var-file-name "lock-files/")))
           (make-directory dir t)
           (setq lock-file-name-transforms `((".*" ,dir t))))

         (dolist (list '(no-littering-etc-directory no-littering-var-directory))
           (add-to-list 'recentf-exclude list))))

(setup recentf
  (:autoload recentf-cleanup)
  (:hooks elpaca-after-init-hook recentf-mode
          kill-emacs-hook recentf-cleanup)
  (:option recentf-max-saved-items 256
           recentf-auto-cleanup 360
           recentf-show-file-shortcuts-flag nil
           recentf-exclude
           '("\\.tar$" "\\.tbz2$" "\\.tbz$" "\\.tgz$" "\\.bz2$"
             "\\.bz$" "\\.gz$" "\\.gzip$" "\\.xz$" "\\.zip$"
             "\\.7z$" "\\.rar$"
             "COMMIT_EDITMSG\\'"
             "\\.\\(?:gz\\|gif\\|svg\\|png\\|jpe?g\\|bmp\\|xpm\\)$"
             "-autoloads\\.el$" "autoload\\.el$")))

(setup savehist
  (:hooks elpaca-after-init-hook savehist-mode)
  (:option savehist-save-minibuffer-history t
           savehist-additional-variables '(search-ring
                                           regexp-search-ring
                                           extended-command-history
                                           kill-ring
                                           mark-ring
                                           global-mark-ring
                                           kmacro-ring
                                           log-edit-comment-ring
                                           register-alist)
           ;; We use an idle timer instead, as saving can cause
           ;; noticable delays with large histories.
           savehist-autosave-interval nil))

(setup saveplace
  (:hooks elpaca-after-init-hook save-place-mode
          after-save-hook save-place-local-mode)
  (:option save-place-limit 600)
  (:custom save-place-ignore-files-regexp
           (replace-regexp-in-string "\\\\)\\$" "\\|^/tmp/.+\\)$"
                                     save-place-ignore-files-regexp t t)))

(setup files
  (auto-save-visited-mode +1)
  (:option auto-save-default t
           make-backup-files nil
           backup-directory-alist `(("." . ,(file-name-concat user-emacs-directory "var/backup/")))
           backup-by-copying-when-linked t
           backup-by-copying t

           delete-old-versions t
           version-control t)
  (:custom find-file-suppress-same-file-warnings t
           find-file-visit-truename t
           large-file-warning-threshold (* 50 (expt 2 20))

           save-abbrevs 'silently
           require-final-newline t))

(setup hl-line
  (:option  hl-line-sticky-flag nil
            global-hl-line-sticky-flag nil
            hl-line-range-function (lambda () (cons (line-end-position)
                                                    (line-beginning-position 2))))
  (:hooks prog-mode-hook hl-line-mode))

(setup pixel-scroll

  (pixel-scroll-precision-mode 1)

  (defun +pixel-scroll-interpolate-down (&optional lines)
    (interactive)
    (if lines
        (pixel-scroll-precision-interpolate (* -1 lines (pixel-line-height)))
      (pixel-scroll-interpolate-down)))

  (defun +pixel-scroll-interpolate-up (&optional lines)
    (interactive)
    (if lines
        (pixel-scroll-precision-interpolate (* lines (pixel-line-height)))
      (pixel-scroll-interpolate-up)))

  (defalias 'scroll-up-command '+pixel-scroll-interpolate-down)
  (defalias 'scroll-down-command '+pixel-scroll-interpolate-up)

  (:custom
   pixel-scroll-precision-scroll-down t
   pixel-scroll-precision-scroll-up t
   pixel-scroll-precision-interpolate-page t)
  (:option
   ;; Other
   scroll-error-top-bottom t
   ;; nice scroll
   scroll-step 1
   scroll-preserve-screen-position t
   scroll-margin 3
   scroll-conservatively 10
   maximum-scroll-margin 0.3
   scroll-up-aggressively 0.0
   scroll-down-aggressively 0.0))

(setup time
  (display-time-mode t)
  (:custom display-time-default-load-average nil
           display-time-day-and-date t
           display-time-24hr-format t))

(setup uniquify
  (:custom uniquify-buffer-name-style 'forward
           uniquify-strip-common-suffix t
           uniquify-after-kill-buffer-p t
           uniquify-separator "/"))

(setup linum
  (dolist (mode '(bookmark-bmenu-mode-hook))
    (add-hook mode (lambda () (progn (display-line-numbers-mode 1)))))
  (:option display-line-numbers-type 'visual
           display-line-numbers-width 3
           display-line-numbers-widen t))

(setup autorevert
  (global-auto-revert-mode t)
  (:option revert-without-query (list ".")
           auto-revert-stop-on-user-input nil
           auto-revert-mode-text ""
           auto-revert-avoid-polling t
           auto-revert-verbose nil
           auto-revert-remote-files nil)
  (:custom global-auto-revert-non-file-buffers t
           global-auto-revert-ignore-modes '(Buffer-menu-mode)))

(setup feature

  (:init (ffap-bindings))

  (:option delete-selection-mode t
           global-so-long-mode t
           global-subword-mode t
           global-prettify-symbols-mode t)

  (:custom global-text-scale-adjust-resizes-frames nil ;; face-remap
           tramp-backup-directory-alist backup-directory-alist) ;; Tramp

  (:hooks emacs-startup-hook (lambda () (setopt kill-buffer-delete-auto-save-files t
                                           word-wrap t
                                           truncate-lines nil
                                           interprogram-cut-function #'gui-select-text
                                           word-wrap-by-category t
                                           truncate-partial-width-windows nil
                                           ring-bell-function nil
                                           indicate-buffer-boundaries nil
                                           indicate-empty-lines nil
                                           history-length 1000
                                           create-lockfiles nil
                                           delete-auto-save-files t
                                           auto-save-no-message t
                                           auto-save-include-big-deletions t
                                           use-short-answers t
                                           read-buffer-completion-ignore-case t
                                           ;; C Source
                                           save-interprogram-paste-before-kill t
                                           kill-ring-max 200
                                           kill-do-not-save-duplicates t
                                           indent-tabs-mode nil
                                           eval-expression-print-length nil
                                           eval-expression-print-level nil
                                           read-extended-command-predicate #'command-completion-default-include-p
                                           blink-matching-paren t
                                           blink-matching-paren-on-screen t
                                           ;; simple
                                           select-enable-clipboard t
                                           select-enable-primary nil
                                           ;; select
                                           comment-multi-line t
                                           comment-empty-lines t
                                           ;; newcomment
                                           read-answer-short t
                                           ;; map-ynp
                                           sentence-end-double-space nil
                                           ;; paragraph
                                           ad-redefinition-action 'accept
                                           ;; advice
                                           delete-pair-blink-delay 0.03
                                           ;; isearch
                                           read-file-name-completion-ignore-case t
                                           ;; minibuffer
                                           auto-save-list-file-prefix (file-name-concat user-emacs-directory "var/auto-save-list/.saves-")
                                           ;; startup
                                           )))

  (setq-default custom-file (expand-file-name "custom.el" user-emacs-directory)
                custom-buffer-done-kill t)
  (load custom-file :no-error-if-file-is-missing)

  (defun logging-disabled-command (&optional cmd keys)
    (unless cmd (setq cmd this-command))
    (message "%s was disabled." cmd)
    (call-interactively cmd nil keys))

  (setq-default disabled-command-function #'logging-disabled-command)

  ;; copy by ldbeth

  (defun switch-to-enlight-buffer (&optional arg)
    "Switch to the `*scratch*' buffer, creating it first if needed.
    if prefix argument ARG is given, switch to it in an other, possibly new window."
    (interactive "P")
    (let ((exists (get-buffer "*enlight*")))
      (if arg
          (switch-to-buffer-other-window (get-buffer-create "*enlight*"))
        (switch-to-buffer (get-buffer-create "*enlight*")))
      (unless (or exists
                  (eq major-mode initial-major-mode))
        (funcall initial-major-mode))))

  (global-set-key (kbd "C-c C-e") 'switch-to-enlight-buffer)

  ;; copy by nano

  (defun nano-quit ()
    "Quit minibuffer from anywhere (code from Protesilaos Stavrou)."
    (interactive)
    (cond ((region-active-p) (keyboard-quit))
          ((derived-mode-p 'completion-list-mode) (delete-completion-window))
          ((> (minibuffer-depth) 0) (abort-recursive-edit))
          (t (keyboard-quit))))

  (global-set-key (kbd "C-g") 'nano-quit)

  (defun nano-kill ()
    "Delete frame or kill emacs if there is only one frame left."
    (interactive)
    (condition-case nil
        (delete-frame)
      (error (save-buffers-kill-terminal))))

  (global-set-key (kbd "C-x C-c") 'nano-kill))

(provide 'tool-base)

;; ends here.
