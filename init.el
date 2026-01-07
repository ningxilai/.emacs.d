;;; init.el --- my emacs init file -*- lexical-binding:t; -*-

;;; Commentary:

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

;; -setup - aka elpaca-setup

(defmacro elpaca-setup (order &rest body)
  "Execute BODY in `setup' declaration after ORDER is finished.
If the :disabled keyword is present in body, the package is completely ignored.
This happens regardless of the value associated with :disabled.
The expansion is a string indicating the package has been disabled."
  (declare (indent 1))
  (if (memq :disabled body)
      (format "%S :disabled by elpaca-setup" order)
    (let ((o order))
      (when-let* ((ensure (cl-position :ensure body)))
	(setq o (if (null (nth (1+ ensure) body)) nil order)
	      body (append (cl-subseq body 0 ensure)
			   (cl-subseq body (+ ensure 2)))))
      `(elpaca ,o (setup
		      ,(if-let* (((memq (car-safe order) '(quote \`)))
				 (feature (flatten-tree order)))
			   (cadr feature)
			 (elpaca--first order))
		    ,@body)))))

(elpaca setup (require 'setup))

(elpaca-wait)

(defun setup-wrap-to-install-package (body _name)
  "Wrap BODY in an `elpaca' block if necessary.
The body is wrapped in an `elpaca' block if `setup-attributes'
contains an alist with the key `elpaca'."
  (if (assq 'elpaca setup-attributes)
      `(elpaca ,(cdr (assq 'elpaca setup-attributes)) ,@(macroexp-unprogn body))
    body))
;; Add the wrapper function
(add-to-list 'setup-modifier-list #'setup-wrap-to-install-package)

(setup-define :elpaca
  (lambda (order &rest recipe)
    (push (cond
	   ((eq order t) `(elpaca . ,(setup-get 'feature)))
	   ((eq order nil) '(elpaca . nil))
	   (`(elpaca . (,order ,@recipe))))
	  setup-attributes)
    ;; If the macro wouldn't return nil, it would try to insert the result of
    ;; `push' which is the new value of the modified list. As this value usually
    ;; cannot be evaluated, it is better to return nil which the byte compiler
    ;; would optimize away anyway.
    nil)
  :documentation "Install ORDER with `elpaca'.
The ORDER can be used to deduce the feature context."
  :shorthand #'cadr)

(setup-define :silence
  (lambda (&rest body)
    `(cl-letf (((symbol-function 'message) #'format))
       ,(macroexp-progn body)))
  :documentation "Evaluate BODY but keep the echo era clean."
  :debug '(setup))

(setup-define :custom
  (setup-make-setter
   (lambda (name)
     `(funcall (or (get ',name 'custom-get)
		   #'symbol-value)
	       ',name))
   (lambda (name val)
     `(progn
	(custom-load-symbol ',name)
	(funcall (or (get ',name 'custom-set) #'set-default)
		 ',name ,val))))

  :documentation "Like default `:option', but set variables after the feature is loaded."
  :debug '(sexp form)
  :repeatable t
  :after-loaded t)

(setup-define :after
  (lambda (feature &rest body)
    `(:with-feature ,feature
       (:when-loaded ,@body)))
  :documentation "Eval BODY after FEATURE."
  :indent 1)

(setup-define :delay
  (lambda (time &rest body)
    `(run-with-idle-timer ,time nil
			  (lambda () ,@body)))
  :documentation "Delay loading BODY until a certain amount of idle time has passed."
  :indent 1)

;;  src: https://emacs.nasy.moe/#Setup-EL
(setup-define :autoload
  (lambda (func)
    (let ((fn (if (memq (car-safe func) '(quote function))
		  (cadr func)
		func)))
      `(unless (fboundp (quote ,fn))
	 (autoload (function ,fn) ,(symbol-name (setup-get 'feature)) nil t))))
  :documentation "Autoload COMMAND if not already bound."
  :repeatable t
  :signature '(FUNC ...))

(setup-define :hooks
  (lambda (hook func)
    `(add-hook ',hook #',func))
  :documentation "Add pairs of hooks."
  :repeatable t)

(setup-define :init
  (lambda (&rest body) (macroexp-progn body))
  :documentation "Init keywords like use-package and leaf.")

(setup-define :advice
  (lambda (symbol where function)
    `(advice-add ',symbol ,where ,function))
  :documentation "Add a piece of advice on a function.
 See `advice-add' for more details."
  :after-loaded t
  :debug '(sexp sexp function-form)
  :ensure '(nil nil func)
  :repeatable t)

;;; warning

(dolist (dir '("lisp" "site-lisp"))
  (push (expand-file-name dir user-emacs-directory) load-path))

(require 'setup-load)

)

(setup (:elpaca gcmh)
  (:hooks elpaca-after-init-hook gcmh-mode)
  (:option gcmh-idle-delay 'auto
	   gcmh-auto-idle-delay-factor 10
	   gcmh-high-cons-threshold (* 16 1024 1024)))

(setup (:elpaca on))

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
  (:init (run-with-idle-timer 10 nil #'recentf-cleanup))
  (:hooks elpaca-after-init-hook recentf-mode)
  (:option recentf-max-saved-items 256
           recentf-auto-cleanup 'mode
           recentf-exclude nil))

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
  (:hooks dirvish-directory-view-mode-hook diredfl-mode
          dirvish-directory-view-mode-hook centaur-tabs-local-mode)
  )

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

  (load-theme 'doom-nord t nil))

(setup (:elpaca doom-modeline)
  (doom-modeline-mode +1)
  (:option doom-modeline-bar-width 3
	   doom-modeline-github nil
	   doom-modeline-mu4e nil
	   doom-modeline-persp-name nil
	   doom-modeline-minor-modes nil
	   doom-modeline-major-mode-icon nil
	   doom-modeline-buffer-file-name-style 'relative-from-project
	   ;; Only show file encoding if it's non-UTF-8 and different line endings
	   ;; than the current OSes preference
	   doom-modeline-buffer-encoding 'nondefault
	   doom-modeline-default-eol-type (if (featurep :system 'windows) 1 0))
  )

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
		("Projects" project-switch-project "p"))))))
  )

(setup (:elpaca centaur-tabs)
  (:init (centaur-tabs-mode t))
  (:custom
   centaur-tabs-style "bar"
   centaur-tabs-height 32
   centaur-tabs-set-icons t
   centaur-tabs-show-new-tab-button t
   centaur-tabs-set-modified-marker t
   centaur-tabs-show-navigation-buttons t
   centaur-tabs-set-bar 'under
   centaur-tabs-show-count nil
   x-underline-at-descent-line t
   centaur-tabs-left-edge-margin nil)
  (:bind
   "C-<prior>" centaur-tabs-backward
   "C-<next>" centaur-tabs-forward))

(setup (:elpaca macrostep)
  (define-key emacs-lisp-mode-map (kbd "C-c e") #'macrostep-expand)
  (define-key lisp-interaction-mode-map (kbd "C-c e") #'macrostep-expand))

(setup electric
  (add-hook 'write-file-functions 'check-parens)

  (defadvice hungry-delete-backward
              (before sp-delete-pair-advice activate)
              (save-match-data (sp-delete-pair (ad-get-arg 0))))
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
  (require 'smartparens-config)
  (setopt sp-max-prefix-length 25
          sp-max-pair-length 4)
  (add-to-list 'sp-lisp-modes 'sly-mrepl-mode)
  (add-hook 'emacs-lisp-mode-hook #'smartparens-mode)
  (sp-local-pair '(minibuffer-mode minibuffer-inactive-mode) "'" nil :actions nil)
  (sp-local-pair '(minibuffer-mode minibuffer-inactive-mode) "`" nil :actions nil)
  (:autoload sp-pair sp-local-pair sp-with-modes sp-point-in-comment sp-point-in-string)
  (:option sp-highlight-pair-overlay nil
           sp-highlight-wrap-overlay nil
           sp-highlight-wrap-tag-overlay nil))

(setup show-paren
  (show-paren-mode t)
  (setopt
   show-paren-delay 0.1
   show-paren-style 'parenthesis
   ;; show-paren-context-when-offscreen 'overlay
   show-paren-highlight-openparen t
   show-paren-when-point-in-periphery t
   show-paren-when-point-inside-paren t))

(setup (:elpaca mic-paren) (paren-activate))

(setup (:elpaca rainbow-delimiters)
  (add-hook 'prog-mode-hook #'rainbow-delimiters-mode))

(setup (:elpaca indent-bars)
  (:custom indent-bars-face '((t (:height 1.08))))
  (:option
   indent-bars-display-on-blank-lines t
   indent-bars-width-frac 0.2
   indent-bars-zigzag nil
   indent-bars-highlight-current-depth nil
   indent-bars-pattern "⎸"
   indent-bars-prefer-character t
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
  (add-hook 'prog-mode-hook #'indent-bars-mode)
  (add-hook 'yaml-mode-hook #'indent-bars-mode))

(setup hl-line
  (:option  hl-line-sticky-flag nil
	    global-hl-line-sticky-flag nil
	    hl-line-range-function (lambda () (cons (line-end-position)
						    (line-beginning-position 2))))
  (add-hook 'prog-mode-hook #'hl-line-mode))

(setup (:elpaca diff-hl)
  (add-hook 'prog-mode-hook #'diff-hl-mode))

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
  (:custom
   display-time-default-load-average nil
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
  (setopt display-line-numbers-type 'visual)
  (:option display-line-numbers-width 3
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
  (add-hook 'emacs-lisp-mode-hook #'elisp-autofmt-mode))

(setup (:elpaca aggressive-indent :host github :repo "Malabarba/aggressive-indent-mode")
  (add-hook 'emacs-lisp-mode-hook #'aggressive-indent-mode))

(setup (:elpaca colorful-mode)
  (:init (setq-default colorful-use-prefix t))
  (dolist (mode '(html-mode php-mode help-mode helpful-mode))
             (add-to-list 'global-colorful-modes mode))
  (add-hook 'prog-mode-hook #'colorful-mode))

(setup (:elpaca region-occurrences-highlighter)
  (add-hook 'prog-mode-hook #'region-occurrences-highlighter-mode)
  (add-hook 'text-mode-hook #'region-occurrences-highlighter-mode)
  (define-key prog-mode-map (kbd "M-n") 'region-occurrences-highlighter-next)
  (define-key prog-mode-map (kbd "M-p") 'region-occurrences-highlighter-prev))

(setup whitespace
  (whitespace-mode +1)
  (:init
   (setq-default which-func-update-delay 0.2
		 show-trailing-whitespace nil))
  (:option
   whitespace-line-column nil
   whitespace-style '(faces tab-mark missing-newline-at-eof)
   whitespace-display-mappings `((tab-mark ?\t [,(make-glyph-code ?» 'whitespace-tab) ?\t] )))
  (:custom indicate-empty-lines nil)
  (add-hook 'before-save-hook #'delete-trailing-whitespace)
  (add-hook 'prog-mode-hook #'(lambda () (setq-local show-trailing-whitespace t))))

(setup (:elpaca whitespace-cleanup-mode)
  (add-hook 'before-save-hook #'whitespace-cleanup)
  (add-hook 'prog-mode-hook #'(lambda () (setq-local show-trailing-whitespace t))))

(setup (:elpaca dtrt-indent)
  (defmacro space/hide-lighter (mode)
    "Diminish MODE name in mode line to LIGHTER."
    `(eval-after-load 'diminish '(diminish ',mode)))
  (space/hide-lighter dtrt-indent-mode)

  (add-hook 'prog-mode-hook #' (lambda ()
				 (dtrt-indent-mode)
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

(setup ui

  (set-display-table-slot standard-display-table 'truncation (make-glyph-code ?…))
  (set-display-table-slot standard-display-table 'wrap (make-glyph-code ?–))

  (add-hook 'emacs-startup-hook
	    #'(lambda()(with-no-warnings
                    (setopt ;; window.el config
                     split-height-threshold 80
                     split-width-threshold 120
                     pop-up-windows nil))))

  (setopt minibuffer-prompt-properties
          '(read-only t intangible t cursor-intangible t face minibuffer-prompt))
  (add-hook 'minibuffer-setup-hook #'cursor-intangible-mode)

  (setopt enable-recursive-minibuffers t)

  (setopt window-resize-pixelwise t)

  )

(setup feature

  (:init (ffap-bindings))

  (:option delete-selection-mode t
           global-so-long-mode t
           global-subword-mode t
           global-prettify-symbols-mode t)

  (:custom global-text-scale-adjust-resizes-frames nil ;; face-remap
           custom-buffer-done-kill t ;; cus-edit
           tramp-backup-directory-alist backup-directory-alist ;; Tramp
           )

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
					   auto-save-list-file-prefix (file-name-concat user-emacs-directory "var/auto-save-list/.saves-")
					   ;; startup
					   )))

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

  (global-set-key (kbd "C-c e") 'switch-to-enlight-buffer)

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

  (global-set-key (kbd "C-x C-c") 'nano-kill)

  ;; copy by emacs-china

  ;; fronzen emacs situation

  (defun spray-mode-usr2-handler ()
    "Handle case where spray mode timer is left running when the w3m
    buffer it is spraying is killed inadvertently instead of stopping
    spray mode first and won't respond to C-g or other mechanisms.
    This will stop spray mode via an external signal: pkill -USR2
    emacs.
    SRC https://emacs.stackexchange.com/a/70000/37266 ."
    (interactive)
    ;; arbitrary elisp you wish to execute:
    (message "Got USR2 signal")
    (spray-stop))
  (global-set-key [signal usr2] 'spray-mode-usr2-handler)
  ;; ............. ^ here we register the event handler that will
  ;; automatically be called when send-usr2-signal-to-emacs fires

  (defun send-usr2-signal-to-emacs ()
    "Send pkill -USR2 emacs without command line.
    SRC https://emacs.stackexchange.com/a/70000/37266 ."
    (interactive)
    (signal-process (emacs-pid) 'sigusr2))
  (global-set-key (kbd "M-G") 'send-usr2-signal-to-emacs)

  )

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
  (:init
   (set-face-attribute 'default (selected-frame)
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
			    `([,ligature-re 0 font-shape-gstring]))))
  (:option
   face-font-rescale-alist `(("Symbola"             . 1.3)
			     ("Microsoft YaHei"     . 1.2)
			     ("WenQuanYi Zen Hei"   . 1.2)
			     ("Sarasa Term SC Nerd" . 1.2)
			     ("PingFang SC"         . 1.16)
			     ("Lantinghei SC"       . 1.16)
			     ("Kaiti SC"            . 1.16)
			     ("Yuanti SC"           . 1.16)
			     ("Apple Color Emoji"   . 0.91)))

  (:custom
   fixed-pitch '((t (:family "SF Mono")))
   fixed-pitch-serif '((t (:family "SF Mono"))) ;; New York
   variable-pitch '((t (:family "SF Pro"))) ;; Helvetica Neue
   )
  )

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
  (add-hook 'kill-emacs-hook #'fontaine-store-latest-preset)
  (require 'xdg)
  (setq fontaine-latest-state-file (expand-file-name "emacs/fontaine-latest-state.eld" (xdg-cache-home))))

(setup treesit

  (setopt treesit-enabled-modes t
          treesit-font-lock-level 4)

  (save-match-data
    (dolist (sym '(auto-mode-alist interpreter-mode-alist))
      (set sym (cl-loop for (src . fn) in (symbol-value sym)
                        unless (and (functionp fn)
                                    (string-match "-ts-mode\\(?:-maybe\\)?$" (symbol-name fn)))
                        collect (cons src fn)))))

  (:option treesit-language-source-alist
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
             (haskell . ("https://github.com/tree-sitter/tree-sitter-haskell" "v0.23.1"))
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
             (java-mode       . java-ts-mode)
             (js-mode         . js-ts-mode)
             (js-json-mode    . json-ts-mode)
             (python-mode     . python-ts-mode)
             (ruby-mode       . ruby-ts-mode)
             (sh-mode         . bash-ts-mode)
             (typescript-mode . typescript-ts-mode))

           toml-ts-mode-indent-offset 4
           rust-ts-mode-indent-offset 4
           cmake-ts-mode-indent-offset 4
           json-ts-mode-indent-offset 4
           go-ts-mode-indent-offset 4)

  (:custom treesit--indent-verbose t
           treesit--font-lock-verbose nil))

(setup abbrev
  (add-hook 'org-mode-hook      #'abbrev-mode)
  (add-hook 'markdown-mode-hook #'abbrev-mode)
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
  (add-hook 'ielm-mode-hook #'turn-on-eldoc-mode)
  (add-hook 'emacs-lisp-mode-hook #'turn-on-eldoc-mode)
  (add-hook 'lisp-interaction-mode-hook #'turn-on-eldoc-mode)
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
  (define-key emacs-lisp-mode-map (kbd "C-c C-d") #'helpful-at-point)
  (define-key lisp-interaction-mode-map (kbd "C-c C-d") #'helpful-at-point)
  (define-key help-mode-map (kbd "r") #'remove-hook-at-point)
  (add-hook 'helpful-mode-hook #'cursor-sensor-mode))

(setup which-key
  (which-key-mode +1)
  (:option which-key-lighter nil
           which-key-sort-order 'which-key-description-order
           which-key-separator " "
           which-key-prefix-prefix "… "
           which-key-max-display-columns 3
           which-key-idle-delay 3
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

  (:hooks emacs-lisp-mode (lambda ()
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

  (keymap-global-set "C-c h" 'consult-history)
  (keymap-global-set "C-c m" 'consult-mode-command)
  (keymap-global-set "C-c b"  'consult-bookmark)
  (keymap-global-set "C-c k"  'consult-kmacro)
  (keymap-global-set "C-x b"  'consult-buffer)
  (keymap-global-set "M-#"  'consult-register-load)
  (keymap-global-set "M-'"  'consult-register-store)
  (keymap-global-set "C-M-#"  'consult-register)
  (keymap-global-set "M-y"  'consult-yank-pop)
  (keymap-global-set "M-g e"  'consult-compile-error)
  (keymap-global-set "M-g f"  'consult-flymake)
  (keymap-global-set "M-g g"  'consult-goto-line)
  (keymap-global-set "M-g M-g" 'consult-goto-line)
  (keymap-global-set "M-g o"  'consult-outline)
  (keymap-global-set "M-g m"  'consult-mark)
  (keymap-global-set "M-g k"  'consult-global-mark)
  (keymap-global-set "M-g i"  'consult-imenu)
  (keymap-global-set "M-g I"  'consult-imenu-multi)
  (keymap-global-set "M-s f"  'consult-find)
  (keymap-global-set "M-s L"  'consult-locate)
  (keymap-global-set "M-s g"  'consult-grep)
  (keymap-global-set "M-s G"  'consult-git-grep)
  (keymap-global-set "M-s r"  'consult-ripgrep)
  (keymap-global-set "M-s k"  'consult-keep-lines)
  (keymap-global-set "M-s u"  'consult-focus-lines)

  (keymap-global-set "M-s e"  'consult-isearch-history)
  (keymap-global-set "C-s"  'consult-line)

  )

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

  (keymap-global-set "C-." (function embark-act))
  (keymap-global-set "C-;" (function embark-dwim))
  (keymap-global-set "C-h B" (function embark-bindings)))

(setup (:elpaca embark-consult)
  (:hooks embark-collect-mode consult-preview-at-point-mode))

(setup (:elpaca marginalia)
  (:init (marginalia-mode))
  (:custom marginalia-max-relative-age 0
           marginalia-align 'right)
  (:hooks marginalia-mode-hook nerd-icons-completion-marginalia-setup))

(setup ibuffer
  (add-hook 'ibuffer-mode-hook #'hl-line-mode)
  (:custom ibuffer-default-sorting-mode 'recency  ;; can use alphabetic)
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

  (add-hook 'eshell-mode-hook #'completion-preview-mode)

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
  (add-hook 'shell-mode-hook #'ansi-color-for-comint-mode-on))

(setup (:elpaca multi-vterm)
  (:option vterm-shell 'zsh))

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

(setup (:elpaca lsp-mode)
  (setq lsp-completion-provider :none)
  (defun lsp-mode-setup-completion ()
    (setf (alist-get 'styles (alist-get 'lsp-capf completion-category-defaults))
          '(flex))) ;; Configure flex
  (add-hook 'lsp-completion-mode-hook #'lsp-mode-setup-completion))

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
  (add-hook 'lsp-mode-hook #'lsp-ui-mode)
  (add-hook 'lsp-mode-hook #'lsp-ui-set-doc-border))

(setup module
  (:load lang-apl)
  (:load-incremental lang-org))

(setup (:elpaca markdown-mode)
  (add-hook 'markdown-mode-hook #'electric-quote-mode))

(setup (:elpaca hyperbole :host github :repo "emacsmirror/hyperbole")
  (:option hywiki-directory (concat user-emacs-directory "etc/hywiki/"))
  (require 'hpath)
  (require 'hbut)
  (setq hpath:external-display-alist-x
        (list (cons (format "\\.\\(%s\\)$"
                            hpath:external-file-suffixes)
                    "xdg-open"))))
