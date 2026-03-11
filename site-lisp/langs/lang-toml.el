;; -*- lexical-binding: t; -*-

;;; commentary:

;;; code:

(setup toml-ts-mode
  (add-to-list 'auto-mode-alist
               '("poetry\\.lock\\'" . toml-ts-mode)))

(provide 'lang-toml)

;; ends here
