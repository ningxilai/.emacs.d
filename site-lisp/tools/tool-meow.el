;;;   -*- lexical-binding: t; -*-

;;; Code:

;; Meow

(setup (:elpaca paren-face))
(setup (:elpaca rainbow-delimiters))

(setup (:elpaca meow)
  (:require meow)
  (defun meow-setup ()
    (setq meow-cheatsheet-layout meow-cheatsheet-layout-qwerty)
    (meow-motion-define-key
     '("j" . meow-next)
     '("k" . meow-prev)
     '("<escape>" . ignore))
    (meow-leader-define-key
     ;; Use SPC (0-9) for digit arguments.
     '("1" . meow-digit-argument)
     '("2" . meow-digit-argument)
     '("3" . meow-digit-argument)
     '("4" . meow-digit-argument)
     '("5" . meow-digit-argument)
     '("6" . meow-digit-argument)
     '("7" . meow-digit-argument)
     '("8" . meow-digit-argument)
     '("9" . meow-digit-argument)
     '("0" . meow-digit-argument)
     '("/" . meow-keypad-describe-key)
     '("?" . meow-cheatsheet))
    (meow-normal-define-key
     '("0" . meow-expand-0)
     '("9" . meow-expand-9)
     '("8" . meow-expand-8)
     '("7" . meow-expand-7)
     '("6" . meow-expand-6)
     '("5" . meow-expand-5)
     '("4" . meow-expand-4)
     '("3" . meow-expand-3)
     '("2" . meow-expand-2)
     '("1" . meow-expand-1)
     '("-" . negative-argument)
     '(";" . meow-reverse)
     '("," . meow-inner-of-thing)
     '("." . meow-bounds-of-thing)
     '("[" . meow-beginning-of-thing)
     '("]" . meow-end-of-thing)
     '("a" . meow-append)
     '("A" . meow-open-below)
     '("b" . meow-back-word)
     '("B" . meow-back-symbol)
     '("c" . meow-change)
     '("d" . meow-delete)
     '("D" . meow-backward-delete)
     '("e" . meow-next-word)
     '("E" . meow-next-symbol)
     '("f" . meow-find)
     '("g" . meow-cancel-selection)
     '("G" . meow-grab)
     '("h" . meow-left)
     '("H" . meow-left-expand)
     '("i" . meow-insert)
     '("I" . meow-open-above)
     '("j" . meow-next)
     '("J" . meow-next-expand)
     '("k" . meow-prev)
     '("K" . meow-prev-expand)
     '("l" . meow-right)
     '("L" . meow-right-expand)
     '("m" . meow-join)
     '("n" . meow-search)
     '("o" . meow-block)
     '("O" . meow-to-block)
     '("p" . meow-yank)
     '("q" . meow-quit)
     '("Q" . meow-goto-line)
     '("r" . meow-replace)
     '("R" . meow-swap-grab)
     '("s" . meow-kill)
     '("t" . meow-till)
     '("u" . meow-undo)
     '("U" . meow-undo-in-selection)
     '("v" . meow-visit)
     '("w" . meow-mark-word)
     '("W" . meow-mark-symbol)
     '("x" . meow-line)
     '("X" . meow-goto-line)
     '("y" . meow-save)
     '("Y" . meow-sync-grab)
     '("z" . meow-pop-selection)
     '("'" . repeat)
     '("<escape>" . ignore)))
  (meow-setup)
  (meow-global-mode t)
  (:option meow-cursor-type-insert 'hollow
           meow-cursor-type-default 'box)
  (:load meow-pretty-paren :dirs ("site-lisp/tools/meow-pretty-paren"))
  (:require rainbow-delimiters paren-face)
  (meow-setup-pretty-paren))

(setup which-key

  (:option which-key-lighter " WK"
           which-key-sort-order 'which-key-description-order
           which-key-separator " "
           which-key-prefix-prefix "… "
           which-key-max-display-columns 3
           which-key-idle-delay 3.0
           which-key-idle-secondary-delay 0.25
           which-key-add-column-padding 1
           which-key-max-description-length 40))

(setup (:elpaca paredit)
  (defun paredit-enable-for-lisp ()
    "Enable evil for lisp like languages mode."
    (paredit-mode t))
  (dolist (hook '(clojure-mode-hook
                  scheme-mode-hook
                  lisp-mode-hook
                  emacs-lisp-mode-hook
                  racket-mode-hook
                  hy-mode-hook))
    (add-hook hook #'paredit-enable-for-lisp)))

(setup electric

  (defun electric-enable-for-prog ()
    "Enable Electric in Lisp modes."
    (progn
      (electric-pair-mode t)
      (electric-indent-mode t)
      (electric-layout-mode t)))

  (add-hook 'prog-mode-hook #'electric-enable-for-prog)

  (defun electric-disable-for-lisp ()
    "Disable Electric in Lisp modes."
    (progn
      (electric-pair-mode nil)
      (electric-indent-mode nil)
      (electric-layout-mode nil)))

  (dolist (hook '(clojure-mode-hook
                  scheme-mode-hook
                  lisp-mode-hook
                  emacs-lisp-mode-hook
                  racket-mode-hook
                  hy-mode-hook))
    (add-hook hook #'electric-disable-for-lisp))

  (:custom electric-pair-inhibit-predicate 'electric-pair-conservative-inhibit
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

  (defun smartparens-enable-for-prog ()
    "Enable smartparens for programming modes (except Lisp)."
    (smartparens-mode t))

  (add-hook 'prog-mode-hook #'smartparens-enable-for-prog)

  (defun smartparens-disable-for-lisp ()
    "Disable smartparens in Lisp modes."
    (smartparens-mode nil))

  (dolist (hook '(clojure-mode-hook
                  scheme-mode-hook
                  lisp-mode-hook
                  emacs-lisp-mode-hook
                  racket-mode-hook
                  hy-mode-hook))
    (add-hook hook #'smartparens-disable-for-lisp))

  (:require smartparens-config)
  (:option sp-max-prefix-length 25
           sp-max-pair-length 4)
  (add-to-list 'sp-lisp-modes 'sly-mrepl-mode)
  (defadvice hungry-delete-backward
      (before sp-delete-pair-advice activate)
    (save-match-data (sp-delete-pair (ad-get-arg 0))))

  (sp-local-pair '(minibuffer-mode minibuffer-inactive-mode) "'" nil :actions nil)
  (sp-local-pair '(minibuffer-mode minibuffer-inactive-mode) "`" nil :actions nil)
  (:autoload sp-pair sp-local-pair sp-with-modes sp-point-in-comment sp-point-in-string)
  (:option sp-highlight-pair-overlay nil
           sp-highlight-wrap-overlay nil
           sp-highlight-wrap-tag-overlay nil))

(setup show-paren
  (show-paren-mode t)
  (:option show-paren-delay 0.1
           show-paren-style 'parenthesis
           ;; show-paren-context-when-offscreen 'overlay
           show-paren-highlight-openparen t
           show-paren-when-point-in-periphery t
           show-paren-when-point-inside-paren t))

(setup (:elpaca mic-paren) (:hooks prog-mode-hook paren-activate))

(setup (:elpaca macrostep)
  (define-key emacs-lisp-mode-map (kbd "C-c e") #'macrostep-expand)
  (define-key lisp-interaction-mode-map (kbd "C-c e") #'macrostep-expand))

(provide 'tool-meow)

;; ends here.
