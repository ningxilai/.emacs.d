;;;   -*- lexical-binding: t; -*-

;;; Code:

;; Dired

(setup dired

  (:init (let ((args (list "-ahl" "-v" "--group-directories-first")))
           (when (featurep :system 'bsd)
             ;; Use GNU ls as `gls' from `coreutils' if available. Add `(setq
             ;; dired-use-ls-dired nil)' to your config to suppress the Dired warning
             ;; when not using GNU ls.
             (if-let* ((gls (executable-find "gls")))
                 (setq insert-directory-program gls)
               ;; BSD ls doesn't support -v or --group-directories-first
               (setq args (list (car args)))))
           (setq dired-listing-switches (string-join args " ")))

         (put 'dired-find-alternate-file 'disabled nil))

  (:option
   ;; Always copy/delete recursively
   dired-recursive-copies  'always
   dired-recursive-deletes 'top
   dired-isearch-filenames 'dwim
   dired-movement-style 'cycle)

  (:custom
   dired-at-point-require-prefix t
   delete-by-moving-to-trash t
   dired-maybe-use-globstar t
   dired-mouse-drag-files t
   dired-kill-when-opening-new-dired-buffer t
   dired-filter-verbose nil
   dired-hide-details-hide-symlink-targets nil
   dired-free-space nil
   dired-dwim-target t ; suggest a target for moving/copying intelligently
   ;; don't prompt to revert, just do it
   dired-auto-revert-buffer (lambda (dirname)
                              (and (not (file-remote-p dirname))
                                   (dired-directory-changed-p dirname))))

  (:with-hook dired-mode-hook (:hook dired-async-mode
                                     dired-omit-mode
                                     dired-hide-details-mode
                                     nerd-icons-dired-mode))

  (:bind "C-c C-r" dirvish-rsync))

(setup image-dired
  (:custom
   image-dired-external-viewer "gimp"
   ;; Where to store image caches
   image-dired-dir (file-name-concat user-emacs-directory "var/image-dired/")
   image-dired-db-file (concat image-dired-dir "db.el")
   image-dired-gallery-dir (concat image-dired-dir "gallery/")
   image-dired-temp-image-file (concat image-dired-dir "temp-image")
   image-dired-temp-rotate-image-file (concat image-dired-dir "temp-rotate-image"))
  (:option
   ;; Screens are larger nowadays, we can afford slightly larger thumbnails
   image-dired-thumb-size 150))

(setup (:elpaca dirvish)
  (dirvish-peek-mode)
  (dirvish-override-dired-mode)
  (dirvish-side-follow-mode)
  (when (or (daemonp) (display-graphic-p))
    (push 'vc-state dirvish-attributes))
  (cl-callf append dirvish-attributes '(nerd-icons subtree-state))
  (advice-add 'dired--find-file :override #'dirvish--find-entry)
  (advice-add 'dired-noselect :around #'dirvish-dired-noselect-a)
  (:option dirvish-override-dired-mode t
           dirvish-reuse-session 'open
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
           '(vc-state file-size git-msg subtree-state collapse file-time)))

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

(provide 'tool-dired)

;; ends here.
