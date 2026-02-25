;; -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(setup ruby-ts-mode
  (:option ruby-indent-level 2)
  (add-to-list 'auto-mode-alist '("\\.rjs\\'" . ruby-mode)))

(provide 'lang-ruby)

;; ends here
