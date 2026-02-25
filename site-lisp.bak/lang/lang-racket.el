;; -*- lexical-binding: t; -*-

;;; Racket & Pollen

(setup (:elpaca racket-mode)
  (:mode "\\.rkt\\'" "\\.scrbl\\'"  "\\.rhm\\'")
  (:load pollen-mode :dirs ("site-lisp/lang/"))
  (:require pollen-mode)
  (:mode ("\\.html.pm" . racket-mode))
  (:hooks racket-mode-hook pollen-enable-if-pollen-file))

(provide 'lang-racket)
;;; lang-chinese.el ends here
