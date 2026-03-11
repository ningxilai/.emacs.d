;;;   -*- lexical-binding: t; -*-

;;; Code:

;; Nerd-Icons

(setup (:elpaca nerd-icons-dired)
  (:init (defface nerd-icons-dired-dir-face
           '((t (:inherit 'font-lock-doc-face)))
           "Face for the directory icon."
           :group 'nerd-icons-faces)
         (defun my-nerd-icons-icon-for-dir (dir)
           (nerd-icons-icon-for-dir dir :face 'nerd-icons-dired-dir-face))
         (setq nerd-icons-dired-dir-icon-function #'my-nerd-icons-icon-for-dir)))

(setup (:elpaca nerd-icons-corfu)
  (:init (add-to-list 'corfu-margin-formatters #'nerd-icons-corfu-formatter)))

(setup (:elpaca nerd-icons-ibuffer)
  (nerd-icons-ibuffer-mode)
  (:hooks ibuffer-mode-hook nerd-icons-ibuffer-mode))

(setup (:elpaca nerd-icons-completion)
  (:init (nerd-icons-completion-mode))
  (:hooks elpaca-after-init-hook nerd-icons-completion-mode))

(provide 'tool-nerd-icons)

;; ends here.
