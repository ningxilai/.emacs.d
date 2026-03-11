;;;   -*- lexical-binding: t; -*-

;;; Code:

;; Completion

(setup (:elpaca cape)

  (add-to-list 'completion-at-point-functions #'cape-file)
  (add-to-list 'completion-at-point-functions #'cape-elisp-block)
  (add-to-list 'completion-at-point-functions #'cape-keyword)
  (add-to-list 'completion-at-point-functions #'cape-dabbrev)

  (defun yy/dabbrev-capf () ;; https://emacs-china.org/t/emacs-29-dabbrev-capf-wrong-type-argument-stringp-nil/26271/4
    "对 `dabbrev-capf' 的简单包装"
    (dabbrev--reset-global-variables)
    (setq dabbrev--check-other-buffers nil)
    (setq dabbrev--check-all-buffers nil)
    (let* ((abbrev (dabbrev--abbrev-at-point))
           (beg (progn (search-backward abbrev) (point)))
           (end (progn (search-forward abbrev) (point)))
           (ignore-case-p (dabbrev--ignore-case-p abbrev))
           (list 'uninitialized)
           (table
            (lambda (s p a)
              (if (eq a 'metadata)
                  `(metadata (cycle-sort-function . ,#'identity)
                             (category . dabbrev))
                (when (eq list 'uninitialized)
                  (save-excursion
                    ;;--------------------------------
                    ;; New abbreviation to expand.
                    ;;--------------------------------
                    (setq dabbrev--last-abbreviation abbrev)
                    ;; Find all expansion
                    (with-temp-message (current-message)
                      (let* ((inhibit-message t)
                             (completion-list
                              (dabbrev--find-all-expansions abbrev ignore-case-p))
                             (completion-ignore-case ignore-case-p))
                        (setq list
                              (cond
                               ((not (and ignore-case-p dabbrev-case-replace))
                                completion-list)
                               ((string= abbrev (upcase abbrev))
                                (mapcar #'upcase completion-list))
                               ((string= (substring abbrev 0 1)
                                         (upcase (substring abbrev 0 1)))
                                (mapcar #'capitalize completion-list))
                               (t
                                (mapcar #'downcase completion-list))))))))
                (complete-with-action a list s p)))))
      (list beg end table)))

  (:option completion-at-point-functions (list (cape-capf-debug #'cape-dict)))

  (:advice lsp-completion-at-point :around cape-wrap-noninterruptible
           lsp-completion-at-point :around cape-wrap-nonexclusive
           comint-completion-at-point :around cape-wrap-nonexclusive
           pcomplete-completions-at-point :around cape-wrap-nonexclusive)

  (:hooks emacs-lisp-mode-hook (lambda ()
                                 (setopt completion-at-point-functions
                                         (list (cape-capf-super
                                                #'cape-dabbrev
                                                #'cape-file
                                                #'yy/dabbrev-capf
                                                #'elisp-completion-at-point))

                                         cape-dabbrev-min-length 2
                                         cape-dabbrev-check-other-buffers t))))

(setup (:elpaca prescient :host github :repo "radian-software/prescient.el")
  (:custom prescient-aggressive-file-save t
           prescient-sort-length-enable nil
           prescient-sort-full-matches-first t
           prescient-history-length 200
           prescient-frequency-decay 1
           prescient-frequency-threshold 0.05)
  (:option prescient-persist-mode t))

(setup (:elpaca corfu)
  (global-corfu-mode)

  (defun corfu-move-to-minibuffer ()
    (interactive)
    (pcase completion-in-region--data
      (`(,beg ,end ,table ,pred ,extras)
       (let ((completion-extra-properties extras)
             completion-cycle-threshold completion-cycling)
         (consult-completion-in-region beg end table pred)))))

  (add-to-list 'corfu-continue-commands #'corfu-move-to-minibuffer)

  (:option corfu-auto t
           corfu-auto-delay 0.3
           corfu-auto-prefix 3
           corfu-quit-no-match 'separator)
  (:custom corfu-preview-current t
           corfu-popupinfo-delay '(0.4 . 0.2)
           global-corfu-minibuffer (lambda ()
                                     (not (or (bound-and-true-p mct--active)
                                              (bound-and-true-p vertico--input)
                                              (eq (current-local-map) read-passwd-map))))
           read-extended-command-predicate #'command-completion-default-include-p)
  (:bind "M-SPC" corfu-quick-complete
         "M-m" corfu-move-to-minibuffer)
  (:hooks prog-mode-hook corfu-mode
          corfu-mode-hook corfu-popupinfo-mode
          eshell-mode-hook (lambda ()  (setq-local corfu-auto nil) (corfu-mode))))

(setup (:elpaca corfu-prescient)
  (:custom corfu-prescient-enable-sorting t
           corfu-prescient-override-sorting nil
           corfu-prescient-enable-filtering nil )
  (corfu-prescient-mode))

(setup (:elpaca consult)
  (:init (setq register-preview-delay 0.5
               register-preview-function #'consult-register-format)
         (with-eval-after-load 'xref
           (setq xref-show-xrefs-function #'consult-xref
                 xref-show-definitions-function #'consult-xref)
           (setq xref-prompt-for-identifier '(not xref-find-definitions
                                                  xref-find-definitions-other-window
                                                  xref-find-definitions-other-frame
                                                  xref-find-references)))

         (define-key read-expression-map (kbd "C-r") #'consult-history))

  (:advice register-preview :override consult-register-window)

  (:option consult-narrow-key "<"
           consult-line-numbers-widen t
           consult-line-start-from-top nil
           consult-async-min-input 2
           consult-async-refresh-delay  0.15
           consult-async-input-throttle 0.2
           consult-async-input-debounce 0.1
           consult-fd-args
           '((if (executable-find "fdfind" 'remote) "fdfind" "fd")
             "--color=never"
             ;; https://github.com/sharkdp/fd/issues/839
             "--full-path --absolute-path"
             "--hidden --exclude .git"
             (if (featurep :system 'windows) "--path-separator=/"))
           consult-project-root-function (lambda ()
                                           (when-let* ((project (project-current)))
                                             (car (project-root project)))))


  (:hooks completion-list-mode-hook consult-preview-at-point-mode
          completion-setup-hook (lambda () (consult-customize)
                                  consult-ripgrep consult-git-grep consult-grep
                                  consult-bookmark consult-recent-file
                                  consult--source-recent-file
                                  consult--source-project-recent-file
                                  consult--source-bookmark :preview-key "C-SPC")
          completion-setup-hook (lambda () (progn)
                                  (add-to-list 'consult-buffer-sources 'compleseus--source-window-buffers)
                                  (add-to-list 'consult-buffer-sources 'compleseus--source-workspace-buffers)))

  (:global "C-c h" consult-history
           "C-c m" consult-mode-command
           "C-c b"  consult-bookmark
           "C-c k"  consult-kmacro
           "C-x b"  consult-buffer
           "M-#"  consult-register-load
           "M-'"  consult-register-store
           "C-M-#"  consult-register
           "M-y"  consult-yank-pop
           "M-g e"  consult-compile-error
           "M-g f"  consult-flymake
           "M-g g"  consult-goto-line
           "M-g M-g" consult-goto-line
           "M-g o"  consult-outline
           "M-g m"  consult-mark
           "M-g k"  consult-global-mark
           "M-g i"  consult-imenu
           "M-g I"  consult-imenu-multi
           "M-s f"  consult-find
           "M-s L"  consult-locate
           "M-s g"  consult-grep
           "M-s G"  consult-git-grep
           "M-s r"  consult-ripgrep
           "M-s k"  consult-keep-lines
           "M-s u"  consult-focus-lines)

  (:global "M-s e"  consult-isearch-history
           "C-s"  consult-line))

(setup (:elpaca consult-dir)
  (:require consult-dir)
  (:init
   ;; Zoxide
   (setq consult-dir-default-command #'consult-dir-dired)
   (defun consult-dir--zoxide-dirs ()
     "Return list of zoxide dirs."
     (split-string (shell-command-to-string "zoxide query -l") "\n" t))
   (defvar consult-dir--source-zoxide
     `(:name "zoxide"
             :narrow ?z
             :category file
             :face consult-file
             :history file-name-history
             :enabled ,(lambda () (executable-find "zoxide"))
             :items ,#'consult-dir--zoxide-dirs)
     "zoxide directory source for `consult-dir'.")
   (add-to-list 'consult-dir-sources 'consult-dir--source-zoxide t)))

(setup icomplete-mode
  (:option tab-always-indent 'complete
           icomplete-delay-completions-threshold 0
           icomplete-compute-delay 0
           icomplete-show-matches-on-no-input t
           icomplete-hide-common-prefix nil
           icomplete-prospects-height 9
           icomplete-separator " . "
           icomplete-with-completion-tables t
           icomplete-in-buffer t
           icomplete-max-delay-chars 0
           icomplete-scroll t
           resize-mini-windows 'grow-only
           icomplete-matches-format nil)
  (:bind "M-/" completion-at-point)
  (:custom icomplete-vertical-mode -1))

(setup (:elpaca vertico)
  (:init (vertico-mode))
  (:option vertico-cycle t))

(setup (:elpaca vertico-prescient)
  (vertico-prescient-mode)
  (:custom vertico-prescient-enable-sorting t
           vertico-prescient-override-sorting nil
           vertico-prescient-enable-filtering nil))

(setup (:elpaca orderless)
  (:option completion-styles '(orderless basic)
           completion-category-defaults nil
           completion-category-overrides '((file (styles basic partial-completion)))
           orderless-component-separator #'orderless-escapable-split-on-space))

(setup (:elpaca embark)
  (:init (setq prefix-help-command #'embark-prefix-help-command))
  (add-to-list 'display-buffer-alist
               '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                 nil
                 (window-parameters (mode-line-format . none))))

  (:global "C-." embark-act
           "C-;" embark-dwim
           "C-h B" embark-bindings))

(setup (:elpaca embark-consult)
  (:hooks embark-collect-mode-hook consult-preview-at-point-mode))

(setup (:elpaca marginalia)
  (:init (marginalia-mode))
  (:custom marginalia-max-relative-age 0
           marginalia-align 'right)
  (:hooks marginalia-mode-hook nerd-icons-completion-marginalia-setup))

(setup abbrev
  (:hooks org-mode-hook abbrev-mode
          markdown-mode-hook abbrev-mode)
  (:option abbrev-suggest 't
           abbrev-suggest-hint-threshold 2

           abbrev-file-name (expand-file-name "var/abbrev_defs" user-emacs-directory)))

(setup dabbrev
  (:option dabbrev-upcase-means-case-search t
           dabbrev-check-all-buffers nil
           dabbrev-ignored-buffer-modes
           '(archive-mode authinfo-mode image-mode doc-view-mode pdf-view-mode tags-table-mode)
           dabbrev-ignored-buffer-regexps
           ;; '("\\`[ *]")
           '(;; - Buffers starting with a space (internal or temporary buffers)
             "\\` "
             ;; Tags files such as ETAGS, GTAGS, RTAGS, TAGS, e?tags, and GPATH,
             ;; including versions with numeric extensions like <123>
             "\\(?:\\(?:[EG]?\\|GR\\)TAGS\\|e?tags\\|GPATH\\)\\(<[0-9]+>\\)?")))

(provide 'tool-completion)

;; ends here.
