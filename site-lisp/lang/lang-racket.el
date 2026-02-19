;; -*- lexical-binding: t; -*-

;;; Racket

(setup (:elpaca racket-mode)
  (:mode "\\.rkt\\'" "\\.scrbl\\'"  "\\.rhm\\'")
  (:load tool-pollen :dirs ("site-lisp/tool/"))
  (:require tool-pollen)
  (:mode ("\\.html.pm" . racket-mode))
  (:hooks racket-mode-hook pollen-enable-if-pollen-file))

(provide 'lang-racket)
;;; lang-chinese.el ends here
