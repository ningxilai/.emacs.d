;; -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(setup sh-mode
  (setopt sh-indent-after-continuation 'always)
  (:hooks sh-mode (lambda ()(progn (setq-local indent-tabs-mode t
                                          tab-width 4)
                              (defvaralias 'sh-basic-offset 'tab-width)
                              ))))

(provide 'lang-sh)

;; ends here
