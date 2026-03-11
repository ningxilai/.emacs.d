;; -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(setup c-ts-mode
  (:option c-set-style 'linux)
  (:hooks c-mode-hook (lambda ()(progn (setq-local c-default-style '((java-mode . "java")
                                                                (awk-mode . "awk")
                                                                (other . "linux"))
                                              c-ts-mode-indent-style 'linux
                                              c-ts-mode-enable-doxygen nil)
                                  (defvaralias 'c-ts-mode-indent-offset 'tab-width)
                                  ))))

(provide 'lang-c)

;; ends here
