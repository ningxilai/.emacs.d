;;; init.el --- my emacs init file -*- lexical-binding:t; -*-

;;; Code:

;; init

(let ((gc-cons-threshold most-positive-fixnum)
      (file-name-handler-alist nil))

  (eval-when-compile

    (require 'xdg)

    (defvar elpaca-installer-version 0.11)
    (defvar elpaca-directory (expand-file-name "elpaca/" user-emacs-directory))
    (defvar elpaca-builds-directory (expand-file-name "emacs/elpaca/builds/" (xdg-cache-home)))
    (defvar elpaca-repos-directory (expand-file-name "repos/" elpaca-directory))
    (defvar elpaca-order '(elpaca :repo "https://github.com/progfolio/elpaca.git"
                                  :ref nil :depth 1 :inherit ignore
                                  :files (:defaults "elpaca-test.el" (:exclude "extensions"))
                                  :build (:not elpaca--activate-package)))
    (let* ((repo  (expand-file-name "elpaca/" elpaca-repos-directory))
           (build (expand-file-name "elpaca/" elpaca-builds-directory))
           (order (cdr elpaca-order))
           (default-directory repo))
      (add-to-list 'load-path (if (file-exists-p build) build repo))
      (unless (file-exists-p repo)
        (make-directory repo t)
        (when (<= emacs-major-version 28) (require 'subr-x))
        (condition-case-unless-debug err
            (if-let* ((buffer (pop-to-buffer-same-window "*elpaca-bootstrap*"))
                      ((zerop (apply #'call-process `("git" nil ,buffer t "clone"
                                                      ,@(when-let* ((depth (plist-get order :depth)))
                                                          (list (format "--depth=%d" depth) "--no-single-branch"))
                                                      ,(plist-get order :repo) ,repo))))
                      ((zerop (call-process "git" nil buffer t "checkout"
                                            (or (plist-get order :ref) "--"))))
                      (emacs (concat invocation-directory invocation-name))
                      ((zerop (call-process emacs nil buffer nil "-Q" "-L" "." "--batch"
                                            "--eval" "(byte-recompile-directory \".\" 0 'force)")))
                      ((require 'elpaca))
                      ((elpaca-generate-autoloads "elpaca" repo)))
                (progn (message "%s" (buffer-string)) (kill-buffer buffer))
              (error "%s" (with-current-buffer buffer (buffer-string))))
          ((error) (warn "%s" err) (delete-directory repo 'recursive))))
      (unless (require 'elpaca-autoloads nil t)
        (require 'elpaca)
        (elpaca-generate-autoloads "elpaca" repo)
        (let ((load-source-file-function nil)) (load "./elpaca-autoloads"))))
    (add-hook 'after-init-hook #'elpaca-process-queues)
    (elpaca `(,@elpaca-order))

    )

  (dolist (dir '("lisp" "site-lisp"))
    (push (expand-file-name dir user-emacs-directory) load-path))

  (require 'setup)
  (require 'setup-load)

  )


(setup (:elpaca gcmh)
  (:hooks elpaca-after-init-hook gcmh-mode)
  (:option gcmh-idle-delay 'auto
           gcmh-auto-idle-delay-factor 10
           gcmh-high-cons-threshold (* 16 1024 1024)))

(setup (:elpaca async)
  (:option async-bytecomp-package-mode t))

(setup (:elpaca cond-let :host github :repo "tarsius/cond-let"))

(setup (:elpaca exec-path-from-shell)
  (:init (exec-path-from-shell-initialize)))

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
  (:option
   savehist-save-minibuffer-history t
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

(setup dired
  (:init
   (let ((args (list "-ahl" "-v" "--group-directories-first")))
     (when (featurep :system 'bsd)
       ;; Use GNU ls as `gls' from `coreutils' if available. Add `(setq
       ;; dired-use-ls-dired nil)' to your config to suppress the Dired warning
       ;; when not using GNU ls.
       (if-let* ((gls (executable-find "gls")))
           (setq insert-directory-program gls)
         ;; BSD ls doesn't support -v or --group-directories-first
         (setq args (list (car args)))))
     (setq dired-listing-switches (string-join args " ")))

   (put 'dired-find-alternate-file 'disabled nil)
   (setq dired-dwim-target t  ; suggest a target for moving/copying intelligently
         ;; don't prompt to revert, just do it
         dired-auto-revert-buffer (lambda (dirname)
                                    (and (not (file-remote-p dirname))
                                         (dired-directory-changed-p dirname)))
         ;; Always copy/delete recursively
         dired-recursive-copies  'always
         dired-recursive-deletes 'top
         ;; Where to store image caches
         image-dired-dir (file-name-concat user-emacs-directory "var/image-dired/")
         image-dired-db-file (concat image-dired-dir "db.el")
         image-dired-gallery-dir (concat image-dired-dir "gallery/")
         image-dired-temp-image-file (concat image-dired-dir "temp-image")
         image-dired-temp-rotate-image-file (concat image-dired-dir "temp-rotate-image")
         ;; Screens are larger nowadays, we can afford slightly larger thumbnails
         image-dired-thumb-size 150
         image-dired-external-viewer "gimp"))
  (:option dired-at-point-require-prefix t
           dired-movement-style 'cycle
           delete-by-moving-to-trash t
           dired-async-mode t
           dired-isearch-filenames 'dwim
           dired-maybe-use-globstar t
           dired-mouse-drag-files t
           dired-kill-when-opening-new-dired-buffer t
           dired-filter-verbose nil
           dired-hide-details-hide-symlink-targets nil
           dired-free-space nil)
  (:hooks dired-mode-hook dired-omit-mode
          dired-mode-hook auto-revert-mode
          dired-mode-hook dired-hide-details-mode
          dired-mode-hook nerd-icons-dired-mode))

(setup dired-x
  (when-let (cmd (cond ((featurep :system 'macos) "open")
                       ((featurep :system 'linux) "xdg-open")
                       ((featurep :system 'windows) "start")))
    (setq dired-guess-shell-alist-user
          `(("\\.\\(?:docx\\|pdf\\|djvu\\|eps\\)\\'" ,cmd)
            ("\\.\\(?:jpe?g\\|png\\|gif\\|xpm\\)\\'" ,cmd)
            ("\\.\\(?:xcf\\)\\'" ,cmd)
            ("\\.csv\\'" ,cmd)
            ("\\.tex\\'" ,cmd)
            ("\\.\\(?:mp4\\|mkv\\|avi\\|flv\\|rm\\|rmvb\\|ogv\\)\\(?:\\.part\\)?\\'" ,cmd)
            ("\\.\\(?:mp3\\|flac\\)\\'" ,cmd)
            ("\\.html?\\'" ,cmd)
            ("\\.md\\'" ,cmd))))
  (:option dired-omit-verbose nil
           dired-clean-confirm-killing-deleted-buffers nil)
  (:custom dired-omit-files
           (concat dired-omit-files
                   "\\|^\\.DS_Store\\'"
                   "\\|^flycheck_.*"
                   "\\|^\\.project\\(?:ile\\)?\\'"
                   "\\|^\\.\\(?:svn\\|git\\)\\'"
                   "\\|^\\.ccls-cache\\'"
                   "\\|\\(?:\\.js\\)?\\.meta\\'"
                   "\\|\\.\\(?:elc\\|o\\|pyo\\|swp\\|class\\)\\'")))

(setup dired-aux
  (:option dired-create-destination-dirs 'ask
           dired-vc-rename-file t))

(setup (:elpaca diredfl)
  (:hooks dired-mode-hook diredfl-mode))

(setup (:elpaca dirvish)
  (:hooks elpaca-after-init-hook (lambda() (progn (dirvish-peek-mode)
                                             (dirvish-side-follow-mode)
                                             (dirvish-override-dired-mode))))
  (:advice dired--find-file :override dirvish--find-entry
           dired-noselect :around dirvish-dired-noselect-a)
  (:option dirvish-override-dired-mode t
           dirvish-reuse-session 'open
           dirvish-attributes nil
           dirvish-subtree-always-show-state t
           dirvish-use-header-line nil
           dirvish-use-mode-line t
           dirvish-cache-dir (file-name-concat user-emacs-directory "var/dirvish/")
           dirvish-hide-details '(dirvish dirvish-side)
           dirvish-hide-cursor '(dirvish dirvish-side))
  (:custom dirvish-header-line-format
           '(:left (path) :right (free-space))
           dirvish-mode-line-format
           '(:left (sort symlink) :right (vc-info yank index))
           dirvish-side-attributes
           '(vc-state nerd-icons collapse file-size)
           dirvish-attributes
           '(vc-state file-size git-msg subtree-state collapse file-time))
  (:bind "C-c C-r" dirvish-rsync)
  (:hooks dirvish-directory-view-mode-hook diredfl-mode))

(setup (:elpaca nerd-icons-dired)
  (:init (defface nerd-icons-dired-dir-face
           '((t (:inherit 'font-lock-doc-face)))
           "Face for the directory icon."
           :group 'nerd-icons-faces)
         (defun my-nerd-icons-icon-for-dir (dir)
           (nerd-icons-icon-for-dir dir :face 'nerd-icons-dired-dir-face))
         (setq nerd-icons-dired-dir-icon-function #'my-nerd-icons-icon-for-dir)))

(setup (:elpaca doom-themes)

  (defun +nanolize (&rest args)
    (interactive)
    (let* ((background
            (plist-get (custom-face-attributes-get 'default nil)
                       :background))
           (mode-line
            (plist-get (custom-face-attributes-get 'mode-line nil)
                       :background))
           (mode-line-active
            (or (plist-get (custom-face-attributes-get 'mode-line-active nil)
                           :background)
                mode-line))
           (mode-line-inactive
            (or (plist-get (custom-face-attributes-get 'mode-line-inactive nil)
                           :background)
                mode-line)))
      (dolist (face '(window-divider
                      window-divider-first-pixel
                      window-divider-last-pixel))
        (set-face-attribute face nil :foreground background))
      (set-face-attribute 'mode-line-active nil
                          :box `(:line-width 1 :color ,mode-line-active :style nil))
      (set-face-attribute 'mode-line-inactive nil
                          :box `(:line-width 1 :color ,mode-line-inactive :style nil))))

  (+nanolize)
  (advice-add 'enable-theme :after '+nanolize)

  (:init (load-theme 'doom-nord t nil)))

(setup (:elpaca doom-modeline)
  (doom-modeline-mode +1)
  (:require advance-words-count)
  (:option doom-modeline-bar-width 3
           doom-modeline-github nil
           doom-modeline-mu4e nil
           doom-modeline-persp-name nil
           doom-modeline-minor-modes nil
           doom-modeline-major-mode-icon nil
           doom-modeline-buffer-file-name-style 'relative-from-project
           doom-modeline-buffer-encoding 'nondefault
           doom-modeline-default-eol-type (if (featurep :system 'windows) 1 0)
           doom-modeline-enable-buffer-position nil)

  (doom-modeline-def-segment word-count-advance ()
    (let* ((start (point-min))
           (end (point-max))
           (list (advance-words-count start end))
           (cjk (car list))
           (ascii (car (last list)))
           (wc (+ cjk ascii)))
      (propertize (format " CJK:%d Total:%d" cjk wc)
                  'face (doom-modeline-face))))

  (doom-modeline-def-modeline 'main
    '(eldoc bar window-state workspace-name window-number modals matches follow buffer-info remote-host buffer-position word-count-advance parrot selection-info)
    '(compilation objed-state misc-info project-name persp-name battery grip irc mu4e gnus github debug repl lsp minor-modes input-method indent-info buffer-encoding major-mode process vcs check time)))

(setup (:elpaca nano-modeline)
  (:require nano-modeline)
  (nano-modeline-text-mode +1)
  (:hooks prog-mode-hook            nano-modeline-prog-mode
          text-mode-hook            nano-modeline-text-mode
          org-mode-hook             nano-modeline-org-mode
          pdf-view-mode-hook        nano-modeline-pdf-mode
          term-mode-hook            nano-modeline-term-mode
          messages-buffer-mode-hook nano-modeline-message-mode
          org-capture-mode-hook     nano-modeline-org-capture-mode
          org-agenda-mode-hook      nano-modeline-org-agenda-mode))

(setup (:elpaca advance-words-count :host github :repo "LdBeth/advance-words-count.el"))

(setup (:elpaca enlight)

  (setopt initial-buffer-choice #'enlight)

  ;; copy by ldbeth

  (defun switch-to-scratch-buffer (&optional arg)
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

  (global-set-key (kbd "C-c s") 'switch-to-scratch-buffer)
  (:require grid)
  (:custom enlight-content
           (concat
            (propertize "MENU" 'face 'highlight)
            "\n\n"
            (enlight-menu
             '(("\nOrg Mode"
                ("Org-Agenda (current day)" (org-agenda nil "a") "a"))
               ("\nFolder"
                ("Hypr folder" (dired "~/.config/hypr/") "h")
                ("Documents folder" (dired "~/Documents/") "d"))
               ("\nInit"
                ("init.el" (dired "~/.config/emacs/") "i"))
               ("\nOther"
                ("Projects" project-switch-project "p")))))))

(setup (:elpaca grid :host github :repo "ichernyshovvv/grid.el"))

(setup (:elpaca macrostep)
  (define-key emacs-lisp-mode-map (kbd "C-c e") #'macrostep-expand)
  (define-key lisp-interaction-mode-map (kbd "C-c e") #'macrostep-expand))

(setup electric

  (defun lisp-setup-check-parens () (add-hook 'write-file-functions #'check-parens nil t))

  (:hooks emacs-lisp-mode-hook  lisp-setup-check-parens)

  (:option electric-pair-mode t
           electric-indent-mode t
           electric-layout-mode t)
  (setopt electric-pair-inhibit-predicate 'electric-pair-conservative-inhibit
          electric-pair-text-pairs '((34 . 34)
                                     (8216 . 8217)
                                     (8220 . 8221)
                                     (?\“ . ?\”)
                                     (?\{ . ?\})
                                     (?\《. ?\》)
                                     (?\「. ?\」)
                                     (?\< . ?\>)
                                     (?\【. ?\】))))

(setup (:elpaca smartparens)
  (:require smartparens-config)
  (:option sp-max-prefix-length 25
           sp-max-pair-length 4)
  (add-to-list 'sp-lisp-modes 'sly-mrepl-mode)
  (defadvice hungry-delete-backward
      (before sp-delete-pair-advice activate)
    (save-match-data (sp-delete-pair (ad-get-arg 0))))
  (:hooks emacs-lisp-mode-hook smartparens-mode)
  (sp-local-pair '(minibuffer-mode minibuffer-inactive-mode) "'" nil :actions nil)
  (sp-local-pair '(minibuffer-mode minibuffer-inactive-mode) "`" nil :actions nil)
  (:autoload sp-pair sp-local-pair sp-with-modes sp-point-in-comment sp-point-in-string)
  (:option sp-highlight-pair-overlay nil
           sp-highlight-wrap-overlay nil
           sp-highlight-wrap-tag-overlay nil))

(setup show-paren
  (show-paren-mode t)
  (:option
   show-paren-delay 0.1
   show-paren-style 'parenthesis
   ;; show-paren-context-when-offscreen 'overlay
   show-paren-highlight-openparen t
   show-paren-when-point-in-periphery t
   show-paren-when-point-inside-paren t))

(setup (:elpaca mic-paren) (paren-activate))

(setup (:elpaca rainbow-delimiters)
  (:hooks prog-mode-hook rainbow-delimiters-mode))

(setup (:elpaca indent-bars)
  (:custom indent-bars-face '((t (:height 1.08))))
  (:option
   indent-bars-display-on-blank-lines t
   indent-bars-width-frac 0.1
   indent-bars-zigzag nil
   indent-bars-highlight-current-depth nil
   indent-bars-pattern "│"
   indent-bars-prefer-character nil
   indent-bars-color '(highlight :face-bg t :blend 0.225)
   indent-bars-no-descend-string t)
  (require 'indent-bars-ts)
  (setopt indent-bars-treesit-ignore-blank-lines-types '("module")
          indent-bars-treesit-scope '((python
                                       function_definition
                                       class_definition
                                       for_statement
                                       if_statement
                                       with_statement
                                       while_statement)))
  (:hooks prog-mode-hook indent-bars-mode
          yaml-mode-hook indent-bars-mode))

(setup hl-line
  (:option  hl-line-sticky-flag nil
            global-hl-line-sticky-flag nil
            hl-line-range-function (lambda () (cons (line-end-position)
                                               (line-beginning-position 2))))
  (:hooks prog-mode-hook hl-line-mode))

(setup (:elpaca diff-hl)
  (:hooks prog-mode-hook diff-hl-mode))

(setup ediff
  (:option ediff-keep-variants nil
           ediff-split-window-function 'split-window-horizontally
           ediff-window-setup-function 'ediff-setup-windows-plain))

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
  (global-auto-revert-mode 1)
  (:option revert-without-query (list ".")
           auto-revert-stop-on-user-input nil
           auto-revert-mode-text ""
           auto-revert-avoid-polling t
           auto-revert-verbose nil
           auto-revert-remote-files nil)
  (:init (setopt global-auto-revert-non-file-buffers t
                 global-auto-revert-ignore-modes '(Buffer-menu-mode))))

(setup (:elpaca elisp-autofmt)
  (:hooks emacs-lisp-mode-hook elisp-autofmt-mode))

(setup (:elpaca aggressive-indent :host github :repo "Malabarba/aggressive-indent-mode")
  (:hooks emacs-lisp-mode-hook aggressive-indent-mode))

(setup (:elpaca colorful-mode)
  (:init (setq-default colorful-use-prefix t))
  (dolist (mode '(html-mode php-mode help-mode helpful-mode))
    (add-to-list 'global-colorful-modes mode))
  (:hooks prog-mode-hook colorful-mode))

(setup (:elpaca region-occurrences-highlighter)
  (:hooks prog-mode-hook region-occurrences-highlighter-mode
          text-mode-hook region-occurrences-highlighter-mode)
  (:with-map prog-mode-map (:bind "M-n" region-occurrences-highlighter-next
                                  "M-p" region-occurrences-highlighter-prev)))

(setup whitespace
  (whitespace-mode +1)
  (:init
   (setq-default which-func-update-delay 0.2
                 show-trailing-whitespace nil))
  (:option whitespace-line-column nil)
  (:custom indicate-empty-lines nil
           whitespace-style '(faces
                              tab-mark
                              missing-newline-at-eof
                              trailing
                              space-before-tab
                              indentation
                              empty
                              space-after-tab))
  (:custom-face whitespace-display-mappings `((tab-mark ?\t [,(make-glyph-code ?» 'whitespace-tab) ?\t] )))
  (:hooks before-save-hook delete-trailing-whitespace-mode
          prog-mode-hook (lambda () (setq-local show-trailing-whitespace t))))

(setup (:elpaca whitespace-cleanup-mode)
  (:hooks before-save-hook whitespace-cleanup))

(setup (:elpaca dtrt-indent)
  (defmacro space/hide-lighter (mode)
    "Diminish MODE name in mode line to LIGHTER."
    `(eval-after-load 'diminish '(diminish ',mode)))
  (space/hide-lighter dtrt-indent-mode)

  (:hooks prog-mode-hook (lambda () (dtrt-indent-mode)
                           (dtrt-indent-adapt))))

(setup (:elpaca unfill)
  (:bind [remap fill-paragraph] unfill-toggle))

(setup (:elpaca filladapt)
  (filladapt-mode t))

(setup (:elpaca visual-fill-column :host codeberg :repo "tarsiiformes/visual-fill-column")
  (:init (advice-add 'text-scale-increase :after #'visual-fill-column-adjust)
         (advice-add 'text-scale-decrease :after #'visual-fill-column-adjust))
  (:option visual-fill-column-center-text t
           visual-fill-column-width 100))

(setup vundo-mode (:elpaca vundo)
       (:bind "C-x C-u" vundo
              "<ESC>" vundo-quit)
       (:option vundo-glyph-alist vundo-unicode-symbols
                vundo-compact-display t))

(setup (:elpaca undohist)
  (:hooks first-change-hook undohist-initialize))

(setup (:elpaca mwim)
  (:bind "C-a" mwim-beginning-of-code-or-line
         "C-e" mwim-end-of-code-or-line))

(setup (:elpaca page-break-lines)
  (:init (global-page-break-lines-mode t))
  (set-fontset-font "fontset-default"
                    (cons page-break-lines-char page-break-lines-char)
                    (face-attribute 'default :family)))

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
             help-mode helpful-mode
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

(setup ui

  (set-display-table-slot standard-display-table 'truncation (make-glyph-code ?…))
  (set-display-table-slot standard-display-table 'wrap (make-glyph-code ?–))

  (add-hook 'emacs-startup-hook
            #'(lambda()(with-no-warnings)
                (setopt ;; window.el config
                 split-height-threshold 80
                 split-width-threshold 120
                 pop-up-windows nil)))

  (setopt minibuffer-prompt-properties
          '(read-only t intangible t cursor-intangible t face minibuffer-prompt))
  (add-hook 'minibuffer-setup-hook #'cursor-intangible-mode)

  (setopt enable-recursive-minibuffers t)

  (setopt window-resize-pixelwise t))

(setup feature

  (:init (ffap-bindings))

  (:option delete-selection-mode t
           global-so-long-mode t
           global-subword-mode t
           global-prettify-symbols-mode t)

  (:custom global-text-scale-adjust-resizes-frames nil ;; face-remap
           custom-buffer-done-kill t ;; cus-edit
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
                                           elisp-fontify-semantically t
                                           ;; elisp-mode
                                           ad-redefinition-action 'accept
                                           ;; advice
                                           delete-pair-blink-delay 0.03
                                           ;; lisp
                                           lazy-highlight-initial-delay 0
                                           ;; isearch
                                           read-file-name-completion-ignore-case t
                                           ;; minibuffer
                                           auto-save-list-file-prefix (file-name-concat user-emacs-directory "var/auto-save-list/.saves-"))))
  ;; startup


  (setq-default custom-file (expand-file-name "custom.el" user-emacs-directory))
  (load custom-file :no-error-if-file-is-missing)

  (add-hook 'text-mode-hook #'(lambda () (progn (abbrev-mode)
                                           (visual-line-mode)
                                           (setq-local auto-composition-mode nil
                                                       text-mode-ispell-word-completion nil))))

  (defun logging-disabled-command (&optional cmd keys)
    (unless cmd (setq cmd this-command))
    (message "%s was disabled." cmd)
    (call-interactively cmd nil keys))

  (setq-default disabled-command-function #'logging-disabled-command)

  ;; copy by emacswiki

  (defun toggle-window-split ()
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

  (global-set-key (kbd "C-x t") 'toggle-window-split)

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

           save-abbrevs 'silently
           require-final-newline t))

(setup fontset
  (:init (set-face-attribute 'default (selected-frame)
                             :height 110
                             :weight 'light :family "Lilex Nerd Font") ;; SF Mono
         (set-face-attribute 'bold (selected-frame)
                             :weight 'regular)
         (set-face-attribute 'bold-italic (selected-frame)
                             :weight 'regular))

  (set-char-table-range composition-function-table ?f '(["\\(?:ff?[fijlt]\\)" 0 font-shape-gstring]))
  (set-char-table-range composition-function-table ?T '(["\\(?:Th\\)" 0 font-shape-gstring]))

  (set-fontset-font t 'unicode (font-spec :family "Unifont" :weight 'normal :slant 'normal))
  (set-fontset-font t 'latin (font-spec :family "IBM Plex Mono" :weight 'light :slant 'normal))
  (set-fontset-font t 'greek (font-spec :family "Catrinity" :weight 'normal :slant 'normal))

  (set-fontset-font t 'emoji
                    (cond
                     ((member "Noto Emoji" (font-family-list)) "Noto Emoji")
                     ((member "Symbola" (font-family-list)) "Symbola")
                     ((member "Apple Color Emoji" (font-family-list)) "Apple Color Emoji")
                     ((member "Noto Color Emoji" (font-family-list)) "Noto Color Emoji")
                     ((member "Segoe UI Emoji" (font-family-list)) "Segoe UI Emoji")
                     ))

  (set-fontset-font t 'han
                    (cond
                     ((member "LXGW WenKai" (font-family-list)) "LXGW WenKai")
                     ((member "Zhuque Fangsong (technical preview)" (font-family-list)) "Zhuque Fangsong (technical preview)")
                     ((member "PingFang SC" (font-family-list)) "PingFang SC")
                     ((member "方正柳公权楷书 简繁" (font-family-list)) "方正柳公权楷书 简繁")
                     ((member "Sarasa Term SC Nerd" (font-family-list)) "Sarasa Term SC Nerd")
                     ((member "WenQuanYi Micro Hei" (font-family-list)) "WenQuanYi Micro Hei")
                     ((member "WenQuanYi Zen Hei" (font-family-list)) "WenQuanYi Zen Hei")
                     ((member "Microsoft YaHei" (font-family-list)) "Microsoft YaHei")
                     ))

  (set-fontset-font t 'cjk-misc
                    (cond
                     ((member "Noto Serif CJK SC" (font-family-list)) "Noto Serif CJK SC")
                     ((member "Sarasa UI J" (font-family-list)) "Sarasa UI J")
                     ))
  (set-fontset-font t 'kana (font-spec :family "Sarasa Gothic" :weight 'normal :slant 'normal))
  (set-fontset-font t 'bopomofo (font-spec :family "Symbola" :weight 'normal :slant 'normal))

  (dolist (char/ligature-re
           `((?-  . ,(rx (or (or "-->" "-<<" "->>" "-|" "-~" "-<" "->") (+ "-"))))
             (?/  . ,(rx (or (or "/==" "/=" "/>" "/**" "/*") (+ "/"))))
             (?*  . ,(rx (or (or "*>" "*/") (+ "*"))))
             (?<  . ,(rx (or (or "<<=" "<<-" "<|||" "<==>" "<!--" "<=>" "<||" "<|>" "<-<" "<==" "<=<" "<-|" "<~>" "<=|" "<~~" "<$>" "<+>" "</>" "<*>" "<->" "<=" "<|" "<:" "<>"  "<$" "<-" "<~" "<+" "</" "<*")
                             (+ "<"))))
             (?:  . ,(rx (or (or ":?>" "::=" ":>" ":<" ":?" ":=") (+ ":"))))
             (?=  . ,(rx (or (or "=>>" "==>" "=/=" "=!=" "=>" "=:=") (+ "="))))
             (?!  . ,(rx (or (or "!==" "!=") (+ "!"))))
             (?>  . ,(rx (or (or ">>-" ">>=" ">=>" ">]" ">:" ">-" ">=") (+ ">"))))
             (?&  . ,(rx (+ "&")))
             (?|  . ,(rx (or (or "|->" "|||>" "||>" "|=>" "||-" "||=" "|-" "|>" "|]" "|}" "|=")
                             (+ "|"))))
             (?.  . ,(rx (or (or ".?" ".=" ".-" "..<") (+ "."))))
             (?+  . ,(rx (or "+>" (+ "+"))))
             (?\[ . ,(rx (or "[<" "[|")))
             (?\{ . ,(rx "{|"))
             (?\? . ,(rx (or (or "?." "?=" "?:") (+ "?"))))
             (?#  . ,(rx (or (or "#_(" "#[" "#{" "#=" "#!" "#:" "#_" "#?" "#(")
                             (+ "#"))))
             (?\; . ,(rx (+ ";")))
             (?_  . ,(rx (or "_|_" "__")))
             (?~  . ,(rx (or "~~>" "~~" "~>" "~-" "~@")))
             (?$  . ,(rx "$>"))
             (?^  . ,(rx "^="))
             (?\] . ,(rx "]#"))))
    (let ((char (car char/ligature-re))
          (ligature-re (cdr char/ligature-re)))
      (set-char-table-range composition-function-table char
                            `([,ligature-re 0 font-shape-gstring]))
      ))
  (:option face-font-rescale-alist `(("Symbola"             . 1.3)
                                     ("Microsoft YaHei"     . 1.2)
                                     ("WenQuanYi Zen Hei"   . 1.2)
                                     ("Sarasa Term SC Nerd" . 1.2)
                                     ("PingFang SC"         . 1.16)
                                     ("Lantinghei SC"       . 1.16)
                                     ("Kaiti SC"            . 1.16)
                                     ("Yuanti SC"           . 1.16)
                                     ("Apple Color Emoji"   . 0.91)))

  (:custom fixed-pitch  '(t((:family "SF Mono")))
           fixed-pitch-serif '(t((:family "SF Mono"))) ;; New York
           variable-pitch '(t((:family "SF Pro"))) ;; Helvetica Neue
           ))

(setup (:elpaca fontaine)
  (:init
   (setq fontaine-presets
         '((regular
            :default-height 120
            :default-weight regular
            :fixed-pitch-height 1.0
            :variable-pitch-height 1.0)
           (large
            :default-height 140
            :default-weight normal
            :fixed-pitch-height 1.0
            :variable-pitch-height 1.05)
           (t
            :default-family "Lilex Nerd Font Mono" ;; CaskaydiaCove Nerd Font Mono
            :fixed-pitch-family "Lilex Nerd Font Mono" ;; CaskaydiaCove Nerd Font Mono
            :variable-pitch-family "Lilex Nerd Font Mono" ;; CaskaydiaCove Nerd Font Mono
            :italic-family "IBM Plex Mono" ;; CaskaydiaCove Nerd Font Mono Italic
            :blod-family "IBM Plex Serif" ;; CaskaydiaCove Nerd Font Mono Bold
            :variable-pitch-weight normal
            :bold-weight bold
            :italic-slant italic
            :line-spacing 0.1)))
   )
  (:option fontaine-set-preset 'regular)
  (:hooks kill-emacs-hook fontaine-store-latest-preset)
  (require 'xdg)
  (setq fontaine-latest-state-file (expand-file-name "emacs/fontaine-latest-state.eld" (xdg-cache-home))))

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
           '(archive-mode authinfo-mode image-mode doc-view-mode
                          pdf-view-mode tags-table-mode)
           dabbrev-ignored-buffer-regexps
           ;; '("\\`[ *]")
           '(;; - Buffers starting with a space (internal or temporary buffers)
             "\\` "
             ;; Tags files such as ETAGS, GTAGS, RTAGS, TAGS, e?tags, and GPATH,
             ;; including versions with numeric extensions like <123>
             "\\(?:\\(?:[EG]?\\|GR\\)TAGS\\|e?tags\\|GPATH\\)\\(<[0-9]+>\\)?")))

(setup eldoc
  (global-eldoc-mode -1)
  (:hooks ielm-mode-hook turn-on-eldoc-mode
          emacs-lisp-mode-hook turn-on-eldoc-mode
          lisp-interaction-mode-hook turn-on-eldoc-mode)
  (:option eldoc-idle-delay 2
           eldoc-print-after-edit t
           eldoc-minor-mode-string nil
           eldoc-echo-area-display-truncation-message nil
           eldoc-documentation-function 'eldoc-documentation-compose))

(setup (:elpaca eldoc-box)
  (:autoload eldoc-box-hover-mode eldoc-box-hover-at-point-mode)
  (:option eldoc-box-lighter nil
           eldoc-box-only-multi-line t
           eldoc-box-clear-with-C-g t))

(setup (:elpaca helpful)
  (:init (with-no-warnings
           (with-eval-after-load 'apropos
             ;; patch apropos buttons to call helpful instead of help
             (dolist (fun-bt '(apropos-function apropos-macro apropos-command))
               (button-type-put
                fun-bt 'action
                (lambda (button)
                  (helpful-callable (button-get button 'apropos-symbol)))))
             (dolist (var-bt '(apropos-variable apropos-user-option))
               (button-type-put
                var-bt 'action
                (lambda (button)
                  (helpful-variable (button-get button 'apropos-symbol))))))))
  (:option apropos-do-all t
           help-enable-completion-autoload nil
           help-enable-autoload nil
           help-enable-symbol-autoload nil
           help-window-select t)
  (:bind [remap describe-function] helpful-callabl
         [remap describe-command]  helpful-command
         [remap describe-variable] helpful-variable
         [remap describe-key] helpful-key
         [remap describe-symbol] helpful-symbol)
  (:with-map emacs-lisp-mode-map (:bind "C-c C-d" helpful-at-point))
  (:with-map lisp-interaction-mode-map (:bind "C-c C-d" helpful-at-point))
  (:with-map help-mode-map (:bind "r" remove-hook-at-point))
  (:hooks helpful-mode-hook cursor-sensor-mode))

(setup which-key
  (which-key-mode +1)
  (:option which-key-lighter " WK"
           which-key-sort-order 'which-key-description-order
           which-key-separator " "
           which-key-prefix-prefix "… "
           which-key-max-display-columns 3
           which-key-idle-delay 3.0
           which-key-idle-secondary-delay 0.25
           which-key-add-column-padding 1
           which-key-max-description-length 40))

(setup (:elpaca cape)

  (add-to-list 'completion-at-point-functions #'cape-file)
  (add-to-list 'completion-at-point-functions #'cape-elisp-block)
  (add-to-list 'completion-at-point-functions #'cape-keyword)
  (add-to-list 'completion-at-point-functions #'cape-dabbrev)

  (setq completion-at-point-functions (list (cape-capf-debug #'cape-dict)))

  (:advice lsp-completion-at-point :around cape-wrap-noninterruptible
           lsp-completion-at-point :around cape-wrap-nonexclusive
           comint-completion-at-point :around cape-wrap-nonexclusive
           pcomplete-completions-at-point :around cape-wrap-nonexclusive)

  (:hooks emacs-lisp-mode-hook (lambda ()
                                 (setopt completion-at-point-functions
                                         (list (cape-capf-super
                                                #'cape-dabbrev
                                                #'cape-file
                                                #'elisp-completion-at-point
                                                ))
                                         cape-dabbrev-min-length 2
                                         cape-dabbrev-check-other-buffers t))))

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

(setup (:elpaca nerd-icons-corfu)
  (:init (add-to-list 'corfu-margin-formatters #'nerd-icons-corfu-formatter)))

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
                                             (car (project-root project))))
           )

  (:hooks completion-list-mode-hook consult-preview-at-point-mode
          completion-setup-hook (lambda () (consult-customize
                                       consult-ripgrep consult-git-grep consult-grep
                                       consult-bookmark consult-recent-file
                                       consult--source-recent-file
                                       consult--source-project-recent-file
                                       consult--source-bookmark :preview-key "C-SPC"))
          completion-setup-hook (lambda () (progn
                                        (add-to-list 'consult-buffer-sources 'compleseus--source-window-buffers)
                                        (add-to-list 'consult-buffer-sources 'compleseus--source-workspace-buffers))))

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

(setup (:elpaca orderless)
  (:option completion-styles '(orderless basic)
           completion-category-defaults nil
           completion-category-overrides '((file (styles partial-completion)))
           orderless-component-separator #'orderless-escapable-split-on-space))

(setup (:elpaca nerd-icons-completion)
  (:init (nerd-icons-completion-mode))
  (:hooks elpaca-after-init-hook nerd-icons-completion-mode))

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

(setup (:elpaca nerd-icons-ibuffer)
  (nerd-icons-ibuffer-mode)
  (:hooks ibuffer-mode-hook nerd-icons-ibuffer-mode))

(setup eshell

  (defalias 'open 'find-file-other-window)
  (defalias 'clean 'eshell/clear-scrollback)

  (defun eshell/sudo-open (filename)
    "Open a file as root in Eshell."
    (let ((qual-filename (if (string-match "^/" filename)
                             filename
                           (concat (expand-file-name (eshell/pwd)) "/" filename))))
      (switch-to-buffer
       (find-file-noselect
        (concat "/sudo::" qual-filename)))))

  (defun eshell-other-window ()
    "Create or visit an eshell buffer."
    (interactive)
    (if (not (get-buffer "*eshell*"))
        (progn
          (split-window-sensibly (selected-window))
          (other-window 1)
          (eshell))
      (switch-to-buffer-other-window "*eshell*")))

  (:hooks eshell-mode-hook completion-preview-mode)

  (:option eshell-highlight-prompt nil
           eshell-prompt-regexp "^[^αλ\n]*[αλ] "
           eshell-prompt-function
           (lambda nil
             (concat
              (if (string= (eshell/pwd) (getenv "HOME"))
                  (propertize "~" 'face `(:foreground "#99CCFF"))
                (replace-regexp-in-string
                 (getenv "HOME")
                 (propertize "~" 'face `(:foreground "#99CCFF"))
                 (propertize (eshell/pwd) 'face `(:foreground "#99CCFF"))))
              (if (= (user-uid) 0)
                  (propertize " α " 'face `(:foreground "#FF6666"))
                (propertize " λ " 'face `(:foreground "#A6E22E"))))))

  )

(setup (:elpaca vterm)
  (:option vterm-shell "zsh"
           ansi-color-for-comint-mode t
           comint-prompt-read-only t
           comint-buffer-maximum-size 4096)
  (add-to-list 'comint-output-filter-functions 'ansi-color-process-output)
  (defun vterm--rename-buffer-as-title (title)
    (rename-buffer (format "vterm @ %s" title) t))
  (setq vterm-set-title-functions 'vterm-set-title-functions)
  (:hooks shell-mode-hook ansi-color-for-comint-mode-on))

(setup (:elpaca multi-vterm))

(setup (:elpaca vterm-toggle)
  (add-to-list 'display-buffer-alist
               '((display-buffer-reuse-window display-buffer-at-bottom)
                 (display-buffer-reuse-window display-buffer-in-direction)
                 ;;display-buffer-in-direction/direction/dedicated is added in emacs27
                 (direction . bottom)
                 (dedicated . t) ;dedicated is supported in emacs27
                 (reusable-frames . visible)
                 (window-height . 0.3)))
  (:option vterm-toggle-fullscreen-p nil))

(setup project
  (:init (setopt vc-follow-symlinks t          ;; Open links not open
                 vc-handled-backends '(Git Hg) ;; Only git or mercurial
                 vc-display-status nil         ;; No info on the modeline.
                 vc-ignore-dir-regexp (format "\\(%s\\)\\|\\(%s\\)" ;; disable vc on remotes
                                              vc-ignore-dir-regexp
                                              tramp-file-name-regexp)))
  (add-to-list 'auto-mode-alist
               '("\\.\\(ipe\\|qrc\\|svn\\)\\'" . xml-mode))
  (:option project-mode-line t
           project-vc-include-untracked nil))

(setup (:elpaca projectile)
  (:autoload projectile-project-root
             projectile-project-name
             projectile-project-p
             projectile-locate-dominating-file
             projectile-relevant-known-projects)
  (:init
   (setq projectile-enable-caching (if noninteractive t 'persistent)
         projectile-globally-ignored-files '(".DS_Store" "TAGS")
         projectile-globally-ignored-file-suffixes '(".elc" ".pyc" ".o")
         projectile-kill-buffers-filter 'kill-only-files
         projectile-ignored-projects '("~/")
         projectile-known-projects-file (file-name-concat user-emacs-directory "var/projects.eld")
         projectile-ignored-project-function t
         projectile-fd-executable 'fd))

  (:bind [remap evil-jump-to-tag] projectile-find-tag
         [remap find-tag]         #'projectile-find-tag)

  (put 'projectile-ag 'disabled "Use +default/search-project instead")
  (put 'projectile-ripgrep 'disabled "Use +default/search-project instead")
  (put 'projectile-grep 'disabled "Use +default/search-project instead")

  (:option projectile-project-root-files '()
           projectile-project-root-files-top-down-recurring '("Makefile")))

(setup (:elpaca majutsu :host github :repo "0WD0/majutsu"))

(setup (:elpaca lsp-mode)
  (:option lsp-completion-provider :none
           flymake-show-diagnostics-at-end-of-line 'fancy)

  (defun lsp-mode-setup-completion ()
    (setf (alist-get 'styles (alist-get 'lsp-capf completion-category-defaults))
          '(flex))) ;; Configure flex

  (:hooks lsp-mode-hook flymake-mode
          lsp-completion-mode-hook lsp-mode-setup-completion))

(setup (:elpaca lsp-ui)
  (:init (setq lsp-ui-sideline-show-diagnostics nil
               lsp-ui-sideline-ignore-duplicate t
               lsp-ui-doc-delay 0.1
               lsp-ui-doc-show-with-cursor t
               lsp-ui-imenu-auto-refresh 'after-save
               lsp-ui-imenu-colors `(,(face-foreground 'font-lock-keyword-face)
                                     ,(face-foreground 'font-lock-string-face)
                                     ,(face-foreground 'font-lock-constant-face)
                                     ,(face-foreground 'font-lock-variable-name-face))))
  (:bind [remap xref-find-definitions] lsp-ui-peek-find-definitions
         [remap xref-find-references] lsp-ui-peek-find-references)
  (:option lsp-ui-set-doc-border "#1672")
  (:hooks lsp-mode-hook lsp-ui-mode))

(setup treesit

  (:option treesit-enabled-modes t
           treesit-font-lock-level 4
           treesit--font-lock-verbose nil

           treesit--indent-verbose t

           toml-ts-mode-indent-offset 4
           rust-ts-mode-indent-offset 4
           cmake-ts-mode-indent-offset 4
           json-ts-mode-indent-offset 4
           go-ts-mode-indent-offset 4)

  (save-match-data
    (dolist (sym '(auto-mode-alist interpreter-mode-alist))
      (set sym (cl-loop for (src . fn) in (symbol-value sym)
                        unless (and (functionp fn)
                                    (string-match "-ts-mode\\(?:-maybe\\)?$" (symbol-name fn)))
                        collect (cons src fn)))))

  (:custom treesit-language-source-alist
           '((awk . ("https://github.com/Beaglefoot/tree-sitter-awk.git"))
             (bash       . ("https://github.com/tree-sitter/tree-sitter-bash.git"))
             (bibtex . ("https://github.com/latex-lsp/tree-sitter-bibtex.git"))
             (blueprint . ("https://github.com/huanie/tree-sitter-blueprint.git"))
             (commonlisp . ("https://github.com/tree-sitter-grammars/tree-sitter-commonlisp"))
             (c          . ("https://github.com/tree-sitter/tree-sitter-c.git"))
             (cmake      . ("https://github.com/uyha/tree-sitter-cmake.git"))
             (cpp        . ("https://github.com/tree-sitter/tree-sitter-cpp.git"))
             (csharp     . ("https://github.com/tree-sitter/tree-sitter-c-sharp.git"))
             (css        . ("https://github.com/tree-sitter/tree-sitter-css.git"))
             (clojure    . ("https://github.com/sogaiu/tree-sitter-clojure.git"))
             (dockerfile . ("https://github.com/camdencheek/tree-sitter-dockerfile.git"))
             (go         . ("https://github.com/tree-sitter/tree-sitter-go.git"))
             (gomod      . ("https://github.com/camdencheek/tree-sitter-go-mod.git"))
             (html       . ("https://github.com/tree-sitter/tree-sitter-html.git"))
             (haskell . ("https://github.com/tree-sitter/tree-sitter-haskell.git"))
             (java       . ("https://github.com/tree-sitter/tree-sitter-java.git"))
             (javascript . ("https://github.com/tree-sitter/tree-sitter-javascript.git"))
             (json       . ("https://github.com/tree-sitter/tree-sitter-json.git"))
             (latex . ("https://github.com/latex-lsp/tree-sitter-latex.git"))
             (make . ("https://github.com/tree-sitter-grammars/tree-sitter-make.git"))
             (nu . ("https://github.com/nushell/tree-sitter-nu.git"))
             (org . ("https://github.com/milisims/tree-sitter-org.git"))
             (markdown   . ("https://github.com/tree-sitter-grammars/tree-sitter-markdown.git" nil "tree-sitter-markdown/src"))
             (markdown-inline . ("https://github.com/tree-sitter-grammars/tree-sitter-markdown.git" nil "tree-sitter-markdown-inline/src"))
             (perl . ("https://github.com/ganezdragon/tree-sitter-perl.git"))
             (proto . ("https://github.com/mitchellh/tree-sitter-proto.git"))
             (python     . ("https://github.com/tree-sitter/tree-sitter-python.git"))
             (r . ("https://github.com/r-lib/tree-sitter-r.git"))
             (ruby       . ("https://github.com/tree-sitter/tree-sitter-ruby.git"))
             (rust       . ("https://github.com/tree-sitter/tree-sitter-rust.git"))
             (sql . ("https://github.com/DerekStride/tree-sitter-sql.git" "gh-page"))
             (surface . ("https://github.com/connorlay/tree-sitter-surface.git"))
             (toml       . ("https://github.com/tree-sitter/tree-sitter-toml.git"))
             (tsx        . ("https://github.com/tree-sitter/tree-sitter-typescript.git" nil "tsx/src"))
             (typst      . ("https://github.com/uben0/tree-sitter-typst.git"))
             (typescript . ("https://github.com/tree-sitter/tree-sitter-typescript.git" nil "typescript/src"))
             (verilog . ("https://github.com/gmlarumbe/tree-sitter-verilog.git"))
             (vhdl . ("https://github.com/alemuller/tree-sitter-vhdl.git"))
             (vue . ("https://github.com/tree-sitter-grammars/tree-sitter-vue.git"))
             (wast . ("https://github.com/wasm-lsp/tree-sitter-wasm.git" nil "wast/src"))
             (wat . ("https://github.com/wasm-lsp/tree-sitter-wasm.git" nil "wat/src"))
             (wgsl . ("https://github.com/mehmetoguzderin/tree-sitter-wgsl.git"))
             (yaml       . ("https://github.com/ikatyang/tree-sitter-yaml.git")))

           major-mode-remap-alist
           '((c-mode          . c-ts-mode)
             (c++-mode        . c++-ts-mode)
             (c-or-c++-mode   . c-or-c++-ts-mode)
             (cmake-mode      . cmake-ts-mode)
             (clojure-mode    . clojure-ts-mode)
             (conf-toml-mode  . toml-ts-mode)
             (csharp-mode     . csharp-ts-mode)
             (css-mode        . css-ts-mode)
             (html-mode       . html-ts-mode)
             (java-mode       . java-ts-mode)
             (js-mode         . js-ts-mode)
             (json-mode       . json-ts-mode)
             (mhtml-mode      . mhtml-ts-mode  )
             (python-mode     . python-ts-mode)
             (ruby-mode       . ruby-ts-mode)
             (sh-mode         . bash-ts-mode)
             (typescript-mode . typescript-ts-mode))))

(setup (:elpaca hsluv)
  (:require color)
  (:require hsluv)
  (:require cl-lib)
  (:init (require 'color)
         (require 'hsluv)
         (require 'cl-lib)

         (defun perfect-palette (p q &optional hue-start s l luv)
           "Calculate a perfect palette of P colors with step Q. Return HEX color strings."
           (let ((hue-start (or hue-start 0))
                 (s (or s 0))
                 (l (or l 0))
                 (omega (/ (float q) p)))
             (mapcar
              (lambda (x)
                (let ((hue (mod (+ hue-start (* x omega)) 1.0)))
                  (if luv
                      (hsluv-hpluv-to-hex (list (* 360 hue) s l))
                    (apply #'color-rgb-to-hex
                           (append (color-hsl-to-rgb hue (/ s 100.0) (/ l 100.0))
                                   '(2))))))
              (number-sequence 0 (1- p) 1))))

         (defvar perfect-palette-override--faces
           '(font-lock-builtin-face
             font-lock-comment-face
             font-lock-constant-face
             font-lock-function-name-face
             font-lock-keyword-face
             font-lock-string-face
             font-lock-type-face
             font-lock-variable-name-face
             font-lock-warning-face)
           "Faces to override with perfect-palette colors.")

         (defvar perfect-palette-override--saved nil
           "Saved face specs before overriding.")

         (defun perfect-palette-override--apply (palette)
           "Override font-lock faces with generated PALETTE from `perfect-palette'."
           (setq perfect-palette-override--saved nil)
           (cl-loop for face in perfect-palette-override--faces
                    for col in palette
                    do (push (list face (face-attribute face :foreground nil 'default)) perfect-palette-override--saved)
                    do (set-face-foreground face col)))

         (defun perfect-palette-override-enable
             (&optional p q hue-start s l luv)
           "Enable perfect-palette based face override for font-lock faces."
           (interactive)
           (let* ((p (or p 9))
                  (q (or q 4))
                  (hue-start (or hue-start 0.58))
                  (s (or s 45))
                  (l (or l 65))
                  (luv (or luv t))
                  (palette (perfect-palette p q hue-start s l luv)))
             (perfect-palette-override--apply
              ;; 颜色数量可能比 faces 少/多，补齐或截断
              (cl-subseq (append palette (make-list (length perfect-palette-override--faces) "#81a1c1"))
                         0 (length perfect-palette-override--faces)))))

         (defun perfect-palette-override-disable ()
           "Restore original font-lock face colors."
           (interactive)
           (when perfect-palette-override--saved
             (dolist (elem perfect-palette-override--saved)
               (let ((face (nth 0 elem))
                     (color (nth 1 elem)))
                 (set-face-foreground face color)))
             (setq perfect-palette-override--saved nil))))

  (:hooks prog-mode-hook perfect-palette-override-enable))

(setup (:elpaca apheleia)
  (:require apheleia)
  (:init (add-to-list 'apheleia-mode-alist '(sh-mode . shfmt))
         (add-to-list 'apheleia-mode-alist '(cuda-mode . clang-format))
         (add-to-list 'apheleia-mode-alist '(cuda-ts-mode . clang-format))
         (add-to-list 'apheleia-mode-alist '(protobuf-mode . clang-format))

         (add-to-list 'apheleia-formatters-mode-extension-assoc '(cuda-mode . ".cu"))
         (add-to-list 'apheleia-formatters-mode-extension-assoc '(cuda-ts-mode . ".cu"))
         (add-to-list 'apheleia-formatters-mode-extension-assoc '(glsl-ts-mode . ".glsl"))
         (add-to-list 'apheleia-formatters-mode-extension-assoc '(protobuf-mode . ".proto"))

         ;; Apheleia's default clang-format config doesn't respect `c-basic-offset', so
         ;; force it to in the absence of a .clang-format file.
         (setf (alist-get 'clang-format apheleia-formatters)
               `("clang-format"
                 "-assume-filename"
                 (or (apheleia-formatters-local-buffer-file-name)
                     (apheleia-formatters-mode-extension)
                     ".c")
                 (when apheleia-formatters-respect-indent-level
                   (unless (locate-dominating-file default-directory ".clang-format")
                     (format "--style={IndentWidth: %d}" c-basic-offset)))))

         ;; Apheleia's default config for prettier passes an explicit --tab-width N to
         ;; all prettier formatters, respecting your indent settings in Emacs, but
         ;; overriding any indent settings in your prettier config files. This changes
         ;; it to omit indent switches if any configuration for prettier is present in
         ;; the current project.
         (dolist (formatter '(prettier prettier-css prettier-html prettier-javascript
                                       prettier-json prettier-scss prettier-svelte
                                       prettier-typescript prettier-yaml))
           (setf (alist-get formatter apheleia-formatters)
                 (append (delete '(apheleia-formatters-js-indent "--use-tabs" "--tab-width")
                                 (alist-get formatter apheleia-formatters))
                         '((when apheleia-formatters-respect-indent-level
                             (unless (or (cl-loop for file
                                                  in '(".prettierrc"
                                                       ".prettierrc.json"
                                                       ".prettierrc.yml"
                                                       ".prettierrc.yaml"
                                                       ".prettierrc.json5"
                                                       ".prettierrc.js" "prettier.config.js"
                                                       ".prettierrc.mjs" "prettier.config.mjs"
                                                       ".prettierrc.cjs" "prettier.config.cjs"
                                                       ".prettierrc.toml")
                                                  if (locate-dominating-file default-directory file)
                                                  return t)
                                         (when-let ((pkg (locate-dominating-file default-directory "package.json")))
                                           (require 'json)
                                           (let ((json-key-type 'alist))
                                             (assq 'prettier
                                                   (json-read-file (expand-file-name "package.json" pkg))))))
                               (apheleia-formatters-indent "--use-tabs" "--tab-width")))))))))

(setup module
  (:load lang-markdown lang-haskell lang-racket lang-chinese lang-web lang-org :dirs ("site-lisp/lang/"))
  (:load tool-shr tool-fanyi :dirs ("site-lisp/tool/")))
