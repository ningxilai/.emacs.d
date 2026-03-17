;;; init.el --- my emacs init file -*- lexical-binding:t; -*-

;;; Code:

;; init

(let ((gc-cons-threshold most-positive-fixnum)
      (file-name-handler-alist nil))

  (eval-when-compile

    (require 'xdg)

    (defvar elpaca-installer-version 0.12)
    (defvar elpaca-directory (expand-file-name "elpaca/" user-emacs-directory))
    (defvar elpaca-builds-directory (expand-file-name "emacs/elpaca/builds/" (xdg-cache-home)))
    (defvar elpaca-sources-directory (expand-file-name "sources/" elpaca-directory))
    (defvar elpaca-order '(elpaca :repo "https://github.com/progfolio/elpaca.git"
                                  :ref nil :depth 1 :inherit ignore
                                  :files (:defaults "elpaca-test.el" (:exclude "extensions"))
                                  :build (:not elpaca-activate)))
    (let* ((repo  (expand-file-name "elpaca/" elpaca-sources-directory))
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
    (elpaca `(,@elpaca-order)))

  (dolist (dir '("lisp" "site-lisp"))
    (push (expand-file-name dir user-emacs-directory) load-path))

  (require 'setup)
  (require 'setup-load)

  )

(when (boundp 'load-path-filter-function)
  (when (require 'persistent-cached-load-filter nil t)
    (setq persistent-cached-load-filter-assoc-type 'hash+eq)
    (persistent-cached-load-filter-easy-setup)))

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

(setup module
  (:load lang-markdown lang-web lang-org lang-elisp :dirs ("site-lisp/langs/"))
  (:load tool-ui tool-base tool-meow tool-dired tool-prog tool-text tool-rime tool-completion tool-nerd-icons tool-wm tool-formats tool-shr tool-fanyi tool-dired tool-project tool-eshell tool-musicbrainz :dirs ("site-lisp/tools/")))
