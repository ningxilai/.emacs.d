;;;   -*- lexical-binding: t; -*-

;;; Code:

;; WM

;; Copy/ by Centaur Emacs ".emacs.d/lisp/init-window.el"

(setup (:elpaca ace-window)
  (:custom-face
   aw-leading-char-face ((t (:inherit font-lock-keyword-face :foreground unspecified :bold t :height 3.0)))
   aw-minibuffer-leading-char-face ((t (:inherit font-lock-keyword-face :bold t :height 1.0)))
   aw-mode-line-face ((t (:inherit mode-line-emphasis :bold t))))
  (:global [remap other-window] ace-window
           "C-x |" split-window-horizontally-instead
           "C-x _" split-window-vertically-instead)
  (:hook emacs-startup ace-window-display-mode)
  (:require ace-window)
  (:init (defun toggle-window-split ()
           (interactive)
           (if (= (count-windows) 2)
               (let* ((this-win-buffer (window-buffer))
                      (next-win-buffer (window-buffer (next-window)))
                      (this-win-edges (window-edges (selected-window)))
                      (next-win-edges (window-edges (next-window)))
                      (this-win-2nd (not (and (<= (car this-win-edges)
                                                  (car next-win-edges))
                                              (<= (cadr this-win-edges)
                                                  (cadr next-win-edges)))))
                      (splitter
                       (if (= (car this-win-edges)
                              (car (window-edges (next-window))))
                           'split-window-horizontally
                         'split-window-vertically)))
                 (delete-other-windows)
                 (let ((first-win (selected-window)))
                   (funcall splitter)
                   (if this-win-2nd (other-window 1))
                   (set-window-buffer (selected-window) this-win-buffer)
                   (set-window-buffer (next-window) next-win-buffer)
                   (select-window first-win)
                   (if this-win-2nd (other-window 1))))
             (user-error "`toggle-window-split' only supports two windows")))))

(setup (:elpaca popper)
  (:custom popper-group-function #'popper-group-by-directory
           popper-echo-dispatch-actions t)
  (:with-map popper-mode-map
    (:bind "C-h z"       popper-toggle
           "C-<tab>"     popper-cycle
           "C-M-<tab>"   popper-toggle-type))
  (:hooks emacs-startup-hook popper-echo-mode)
  (:option popper-mode-line ""
           popper-reference-buffers
           '("\\*Messages\\*$"
             "Output\\*$" "\\*Pp Eval Output\\*$"
             "^\\*eldoc.*\\*$"
             "\\*Compile-Log\\*$"
             "\\*Completions\\*$"
             "\\*Warnings\\*$"
             "\\*Async Shell Command\\*$"
             "\\*Apropos\\*$"
             "\\*Backtrace\\*$"
             "\\*Calendar\\*$"
             "\\*Fd\\*$" "\\*Find\\*$" "\\*Finder\\*$"
             "\\*Kill Ring\\*$"
             "\\*Embark \\(Collect\\|Live\\):.*\\*$"

             bookmark-bmenu-mode
             comint-mode
             compilation-mode
             help-mode
             helpful-mode
             tabulated-list-mode
             Buffer-menu-mode

             flymake-diagnostics-buffer-mode

             gnus-article-mode devdocs-mode
             grep-mode occur-mode rg-mode
             osx-dictionary-mode fanyi-mode
             "^\\*gt-result\\*$" "^\\*gt-log\\*$"

             "^\\*Process List\\*$" process-menu-mode
             list-environment-mode cargo-process-mode

             "^\\*.*eat.*\\*.*$"
             "^\\*.*eshell.*\\*.*$"
             "^\\*.*shell.*\\*.*$"
             "^\\*.*terminal.*\\*.*$"
             "^\\*.*vterm[inal]*.*\\*.*$"

             "\\*DAP Templates\\*$" dap-server-log-mode
             "\\*ELP Profiling Restuls\\*" profiler-report-mode
             "\\*package update results\\*$" "\\*Package-Lint\\*$"
             "\\*[Wo]*Man.*\\*$"
             "\\*ert\\*$" overseer-buffer-mode
             "\\*gud-debug\\*$"
             "\\*lsp-help\\*$" "\\*lsp session\\*$"
             "\\*quickrun\\*$"
             "\\*tldr\\*$"
             "\\*vc-.*\\**"
             "\\*diff-hl\\**"
             "^\\*macro expansion\\**"

             "\\*Agenda Commands\\*" "\\*Org Select\\*" "\\*Capture\\*" "^CAPTURE-.*\\.org*"
             "\\*Gofmt Errors\\*$" "\\*Go Test\\*$" godoc-mode
             "\\*docker-.+\\*"
             "\\*prolog\\*" inferior-python-mode inf-ruby-mode swift-repl-mode
             "\\*rustfmt\\*$" rustic-compilation-mode rustic-cargo-clippy-mode
             rustic-cargo-outdated-mode rustic-cargo-run-mode rustic-cargo-test-mode))
  (:require popper)
  (:init (with-no-warnings
           (defun my-popper-fit-window-height (win)
             "Adjust the height of popup window WIN to fit the buffer's content."
             (let ((desired-height (floor (/ (frame-height) 3))))
               (fit-window-to-buffer win desired-height desired-height)))
           (setq popper-window-height #'my-popper-fit-window-height)

           (defun popper-close-window-hack (&rest _args)
             "Close popper window via `C-g'."
             (when (and ; (called-interactively-p 'interactive)
                    (not (region-active-p))
                    popper-open-popup-alist)
               (let ((window (caar popper-open-popup-alist))
                     (buffer (cdar popper-open-popup-alist)))
                 (when (and (window-live-p window)
                            (buffer-live-p buffer)
                            (not (with-current-buffer buffer
                                   (derived-mode-p 'eshell-mode
                                                   'shell-mode
                                                   'term-mode
                                                   'vterm-mode))))
                   (delete-window window)))))
           (advice-add #'keyboard-quit :before #'popper-close-window-hack))))

(setup ibuffer
  (:hooks ibuffer-mode-hook hl-line-mode)
  (:custom ibuffer-default-sorting-mode 'recency  ;; can use alphabetic
           ibuffer-use-other-window t                ;; ibuffer in other windows
           ibuffer-jump-offer-only-visible-buffers t
           ibuffer-human-readable-size t)
  (:option ibuffer-formats
           '((mark modified read-only locked
                   " " (name 55 55 :left :elide)
                   " " (size 8 -1 :right)
                   " " (mode 18 18 :left :elide) " " filename-and-process)
             (mark " " (name 16 -1) " " filename)))
  (:bind "C-x C-b" ibuffer))

(setup wm ;; Author: Per Vognsen <per.vognsen@gmail.com>

  (:init(defun toggle-window-split () ;; copy by emacswiki
          (interactive)
          (if (= (count-windows) 2)
              (let* ((this-win-buffer (window-buffer))
                     (next-win-buffer (window-buffer (next-window)))
                     (this-win-edges (window-edges (selected-window)))
                     (next-win-edges (window-edges (next-window)))
                     (this-win-2nd (not (and (<= (car this-win-edges)
                                                 (car next-win-edges))
                                             (<= (cadr this-win-edges)
                                                 (cadr next-win-edges)))))
                     (splitter
                      (if (= (car this-win-edges)
                             (car (window-edges (next-window))))
                          'split-window-horizontally
                        'split-window-vertically)))
                (delete-other-windows)
                (let ((first-win (selected-window)))
                  (funcall splitter)
                  (if this-win-2nd (other-window 1))
                  (set-window-buffer (selected-window) this-win-buffer)
                  (set-window-buffer (next-window) next-win-buffer)
                  (select-window first-win)
                  (if this-win-2nd (other-window 1))))))
        (global-set-key (kbd "C-x t") 'toggle-window-split))

  (:load wm :dirs ("site-lisp/tools/wm/"))
  (:hooks emacs-startup-hook wm-mode))

(provide 'tool-wm)

;; ends here.
