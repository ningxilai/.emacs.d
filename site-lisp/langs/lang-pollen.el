;; -*- lexical-binding: t; -*-

;;; Pollen

(setup pollen-mode
  (:load pollen-mode :dirs ("site-lisp/langs/pollen-mode"))
  (:mode "\\.\\(pm\\|pmd\\|pp\\|p\\)\\'"))

(provide 'lang-pollen)
;;; lang-pollen.el ends here
