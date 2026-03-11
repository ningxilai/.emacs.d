;;;   -*- lexical-binding: t; -*-

;;; Code:

;; project

(setup project
  (:mode (("\\.\\(ipe\\|qrc\\|svn\\)\\'") . xml-mode))
  (:option project-mode-line t
           project-vc-include-untracked nil

           vc-follow-symlinks t                                   ;; Open links not open
           vc-handled-backends '(Git Hg)                          ;; Only git or mercurial
           vc-display-status nil                                  ;; No info on the modeline.
           vc-ignore-dir-regexp (format "\\(%s\\)\\|\\(%s\\)"     ;; disable vc on remotes
                                        vc-ignore-dir-regexp
                                        tramp-file-name-regexp)))

(setup (:elpaca projectile)
  (:autoload projectile-project-root
             projectile-project-name
             projectile-project-p
             projectile-locate-dominating-file
             projectile-relevant-known-projects)
  (:custom projectile-enable-caching (if noninteractive t 'persistent)
           projectile-globally-ignored-files '(".DS_Store" "TAGS")
           projectile-globally-ignored-file-suffixes '(".elc" ".pyc" ".o")
           projectile-kill-buffers-filter 'kill-only-files
           projectile-ignored-projects '("~/")
           projectile-known-projects-file (file-name-concat user-emacs-directory "var/projects.eld")
           projectile-ignored-project-function t
           projectile-fd-executable 'fd)

  (:bind [remap evil-jump-to-tag] projectile-find-tag
         [remap find-tag]         #'projectile-find-tag)

  (put 'projectile-ag 'disabled "Use +default/search-project instead")
  (put 'projectile-ripgrep 'disabled "Use +default/search-project instead")
  (put 'projectile-grep 'disabled "Use +default/search-project instead")

  (:option projectile-project-root-files '()
           projectile-project-root-files-top-down-recurring '("Makefile")))

(setup (:elpaca majutsu :host github :repo "0WD0/majutsu"))

(setup (:elpaca diff-hl)
  (:hooks prog-mode-hook diff-hl-mode))

(setup ediff
  (:option ediff-keep-variants nil
           ediff-split-window-function 'split-window-horizontally
           ediff-window-setup-function 'ediff-setup-windows-plain))

(provide 'tool-project)

;; ends here.
