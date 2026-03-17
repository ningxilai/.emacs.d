;;;   -*- lexical-binding: t; -*-

;;; Code:

;; project

(setup project
  (:mode (("\\.\\(ipe\\|qrc\\|svn\\)\\'") . xml-mode))
  (:option project-mode-line t
           project-vc-include-untracked nil
           project-list-file (file-name-concat user-emacs-directory "var/projects"))
  (add-to-list 'project-vc-backend-markers-alist '(Jujutsu . ".jj"))
  (add-to-list 'project-vc-extra-root-markers ".jj"))

(setup vc
  (:load vcgit vc-extend :dirs ("site-lisp/tools/vc"))
  (:option vc-follow-symlinks t ;; Open links not open,default '(RCS CVS SVN SCCS Bzr Git Hg Mtn Arch)
           vc-handled-backends '(Git Hg SVN Jj) ;; Only git or mercurial
           vc-display-status nil ;; No info on the modeline.
           vc-ignore-dir-regexp (format "\\(%s\\)\\|\\(%s\\)" ;; disable vc on remotes
                                        vc-ignore-dir-regexp
                                        tramp-file-name-regexp)
           vc-suppress-confirm t ;;自动保存当前buffer后进行操作 除非进行一个危险的操作,如回滚
           vc-command-messages 'log
           vc-find-revision-no-save t
           vc-use-short-revision t
           vc-git-print-log-follow nil ;; 似乎与 vc-log-short-style有冲突 C-x v l 展示异常
           vc-git-revision-complete-only-branches t
           vc--inhibit-async-window nil ;; vc-pull vc-push 不显示window
           vc-log-short-style '(directory file) ;; C-x v l 列出当前文件的历史版本时 不展示详情
           vc-allow-rewriting-published-history 'ask
           vc-annotate-background-mode nil
           vc-dir-hide-up-to-date-on-revert t
           vc-log-show-limit 200
           vc-git-diff-switches '("--ignore-space-at-eol" "--ignore-blank-lines"  "--textconv") ;; --textconv 以支持 gpg 文件的diff，去除 "--ignore-space-change" 否则会导致在 diff-mode 中提交部分hunk的时候失败
           vc-svn-diff-switches '("-x --ignore-eol-style") ;; svn diff --help
           ;; -b (--ignore-space-change): 忽略空白数量的修改。
           ;; -w (--ignore-all-space): 忽略所有的空白。
           ;; --ignore-eol-style: 忽略行尾样式的改变。
           )

  (define-advice vc-next-action (:around (orig-fun &rest args) default-mark-all)
    "Default mark all in *vc-dir*"
    (when (and (eq major-mode 'vc-dir-mode)
               (not (vc-dir-marked-files)))
      (vc-dir-mark-all-files nil))
    (apply orig-fun args))

  (define-advice vc-dir-headers (:around (orig-fun &rest args) progress)
    (interactive)
    (let ((msg (apply orig-fun args)))
      (when (eq vc-dir-backend 'Git)
        (let ((gitdir (vc-git--git-path)))
          (setq msg (string-trim-left msg "VC backend : Git\n")) ;
          ;; (setq msg (string-trim-left msg "Working dir:.+?\n"))
          (setq msg (string-trim-right msg "Stash      : Nothing stashed\n"))))
      msg))
  )

(setup (:elpaca vc-jj)
  (:require vc-jj project-jj))

(setup (:elpaca diff-hl)
  (:hooks prog-mode-hook diff-hl-mode))

(setup ediff
  (:option ediff-keep-variants nil
           ediff-split-window-function 'split-window-horizontally
           ediff-window-setup-function 'ediff-setup-windows-plain))

(setup diff
  (:option diff-font-lock-prettify t
           diff-ignore-whitespace-switches "-w"
           diff-jump-to-old-file t ;; 控制 diff-mode 中RET C-u 的行为，
           diff-switches "-ubB")
  (:with-map diff-mode-shared-map
    (:bind "s" vcgit-stage
           "u" vcgit-unstage))
  (:bind "c" vc-next-action
         "d" outline-cycle
         "x" diff-hunk-kill
         "X" diff-file-kill
         "C-c C-e" toggle-diff-whitespace))

(setup log-view
  (:require vc-dir)
  (:bind "C-c p o" vc-switch-project
         "C-c p x" vcgit-reset
         "C-c p C-i" log-view-diff
         "C-c p i" log-view-toggle-entry-display
         "C-c p g a" log-view-annotate-version
         "C-c l RET" log-view-find-revision)
  (:custom log-edit-hook nil))

  (provide 'tool-project)

;; ends here.
