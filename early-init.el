;; early-init --- early-init.el -*- no-byte-compile:t; lexical-binding: t; -*-

;;; Code:

;; (require 'xdg)

;; (startup-redirect-eln-cache
;;  (expand-file-name  "emacs/eln-cache/" (xdg-cache-home)))

(setq-default gc-cons-threshold most-positive-fixnum
	      gc-cons-percentage 1.0)

;; --- Frame / windows layout & behavior --------------------------------------
(setq default-frame-alist
      '((height . 60)
        (width  . 100)
        (alpha-background . 90)
        (left-fringe . 0)
        (right-fringe . 0)
        (internal-border-width . 24)
        (inhibit-double-buffering . nil))
      frame-title-format '(:eval (concat
	                          (if (and buffer-file-name (buffer-modified-p)) "•")
	                          (buffer-name)
	                          (if buffer-file-name
		                      (concat " (" (directory-file-name (abbreviate-file-name default-directory)) ")")) " - Emacs"))
      bottom-divider-width nil
      right-divider-width nil
      window-divider-default-bottom-width 1
      window-divider-default-places t
      window-divider-default-right-width 1)

(modify-frame-parameters (selected-frame) default-frame-alist)

(when (display-graphic-p)
  (context-menu-mode -1))

(if (fboundp 'set-scroll-bar-mode)
    (set-scroll-bar-mode -1))

(when (bound-and-true-p blink-cursor-mode)
  (blink-cursor-mode -1))

;;(modify-frame-parameters nil default-frame-alist)

(tool-bar-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)
(horizontal-scroll-bar-mode -1)

(setq horizontal-scroll-bars nil
      vertical-scroll-bars nil)

(setq make-cursor-line-fully-visible nil
      track-eol t
      kill-whole-line t)

;; --- Sane settings ------ CJK && UTF-8 ---------------------------------------
(set-language-environment "utf-8")
(set-buffer-file-coding-system 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(set-clipboard-coding-system 'utf-8)
(modify-coding-system-alist 'process "*" 'utf-8)
(prefer-coding-system 'utf-8)
(setq-default pathname-coding-system 'utf-8
              default-buffer-file-coding-system 'utf-8)
(setq default-process-coding-system '(utf-8 . utf-8)
      locale-coding-system 'utf-8
      file-name-coding-system 'utf-8)

(setq-default system-time-locale "C")

(setenv "LC_CTYPE" "UTF-8")
(setenv "LC_ALL" "en_US.UTF-8")
(setenv "LANG" "en_US.UTF-8")

;; (setq-default mode-line-format (add-to-list 'mode-line-format '(:eval (if (buffer-modified-p) " ●" " ○"))))

;; (setq-default frame-title-format "%b")


;; (setq-default header-line-format '("GC: " (:eval (number-to-string gcs-done)) " - " (:eval (number-to-string gc-elapsed)) "s"))

(setq-default package-enable-at-startup nil)

(startup-redirect-eln-cache
 (convert-standard-filename
  (expand-file-name  "var/eln-cache/" user-emacs-directory)))

(setq byte-compile-warnings t
      byte-compile-verbose t)

(setq-default native-comp-async-report-warnings-errors t
              native-comp-async-query-on-exit t) ;; 'silent

(setq-default bidi-display-reordering 'left-to-right
              bidi-paragraph-direction 'left-to-right)

(setq bidi-inhibit-bpa t
      long-line-threshold 1000
      large-hscroll-threshold 1000
      syntax-wholeline-max 1000)

(setq-default cursor-in-non-selected-windows nil)

(setq highlight-nonselected-windows nil)

(setq fast-but-imprecise-scrolling t)

(setq frame-inhibit-implied-resize t)

(setq idle-update-delay 1.0)

(setq inhibit-compacting-font-caches t)

(setq pgtk-wait-for-event-timeout 0.001)

(setq read-process-output-max (* 64 1024))  ; 64kb

(setq redisplay-skip-fontification-on-input t)

(setq load-prefer-newer t)

(setq auto-mode-case-fold nil)

(setq debug-on-error t)

;;; early-init.el ends here
