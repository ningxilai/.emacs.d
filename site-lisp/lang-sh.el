;; -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(setup sh-mode
  (:option sh-indent-after-continuation 'always)
  (:hooks sh-mode-hook (lambda ()(progn (setq-local indent-tabs-mode t
                                               tab-width 4)
                                   (defvaralias 'sh-basic-offset 'tab-width)
                                   ))))

(provide 'lang-sh)

;; ends here
