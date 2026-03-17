;;;   -*- lexical-binding: t; -*-

;;; Code:

;; WM

(setup winner
  (:init (defun transient-winner-undo ()
           "Transient version of winner-undo."
           (interactive)
           (let ((echo-keystrokes nil)
                 (back (lambda ()
                         (interactive)
                         (if tab-bar-mode
                             (tab-bar-history-back)
                           (winner-undo))))
                 (forward (lambda ()
                            (interactive)
                            (if tab-bar-mode
                                (tab-bar-history-forward)
                              (winner-redo)))))
             (funcall back)
             (message "Winner: [u]ndo [r]edo")
             (set-transient-map
              (let ((map (make-sparse-keymap)))
                (define-key map [?u] back)
                (define-key map [?r] forward)
                map)
              t))))
  (:global "C-c u" transient-winner-undo)
  (:hooks emacs-startup-hook (lambda () (winner-mode t))))

;; Copy/ by Centaur Emacs ".emacs.d/lisp/init-window.el"

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
             (when (and        ; (called-interactively-p 'interactive)
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

(setup (:elpaca sort-tab :host github :repo "manateelazycat/sort-tab")
  (:hooks emacs-startup-hook (lambda () (sort-tab-mode t)))
  (:custom sort-tab-align 'center)
  (:custom-face sort-tab-current-tab-face
                ((((background light))
                  :background "#d5c9c0" :foreground "#282828" :bold t)
                 (t
                  :background "#2e3440" :foreground "#fbf1c7" :bold t))
                sort-tab-other-tab-face
                ((((background light))
                  :foreground "#3f3f3f" :bold nil)
                 (t
                  :foreground "#bdae93" :bold nil))
                sort-tab-separator-face
                ((((background light))
                  :foreground "#bdae93" :bold t)
                 (t
                  :foreground "#665c54" :bold t)))
  (:global "C-0" sort-tab-close-current-tab
           "C-1" sort-tab-select-visible-tab
           "C-2" sort-tab-select-visible-tab
           "C-3" sort-tab-select-visible-tab
           "C-4" sort-tab-select-visible-tab))

(setup ibuffer
  (:hooks ibuffer-mode-hook hl-line-mode)
  (:custom ibuffer-default-sorting-mode 'recency ;; can use alphabetic
           ibuffer-use-other-window t ;; ibuffer in other windows
           ibuffer-jump-offer-only-visible-buffers t
           ibuffer-human-readable-size t)
  (:option ibuffer-formats
           '((mark modified read-only locked
                   " " (name 55 55 :left :elide)
                   " " (size 8 -1 :right)
                   " " (mode 18 18 :left :elide) " " filename-and-process)
             (mark " " (name 16 -1) " " filename)))
  (:bind "C-x C-b" ibuffer))

(setup wm
  (:load wm :dirs ("site-lisp/tools/wm/"))
  (:require wm)
  (wm-mode)
  (defun wm-toggle-window-split ()
    "Toggle the window split direction for the current window and its neighbor.
This function is adapted from emacswiki and integrates with wm state management.
It only works when exactly two windows are visible."
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
                       (car next-win-edges))
                    'split-window-horizontally
                  'split-window-vertically)))
          (delete-other-windows)
          (let ((first-win (selected-window)))
            (funcall splitter)
            (if this-win-2nd (other-window 1))
            (set-window-buffer (selected-window) this-win-buffer)
            (set-window-buffer (next-window) next-win-buffer)
            (select-window first-win)
            (if this-win-2nd (other-window 1)))
          ;; Update wm state to reflect the new window configuration
          (when (and (boundp 'wm-mode) wm-mode)
            (let ((state wm-state-instance))
              (when state
                ;; Use aset to avoid (setf ...) issues
                (aset state 1 (mapcar #'wm-window-from-emacs-window (wm-emacs-windows)))
                (aset state 7 nil)
                (wm-state-update-focus state)
                (wm-display-status)))))
      (message "Can only toggle split with exactly 2 windows.")))
  (:bind "C-!" wm-toggle-window-split))

(provide 'tool-wm)

;; ends here.
