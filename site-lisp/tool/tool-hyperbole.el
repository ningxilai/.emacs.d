;;; Commentary:  -*- lexical-binding: t; -*-

;;; Code:

;; Hyperbole

(setup (:elpaca hyperbole :host github :repo "emacsmirror/hyperbole")
  (:option hywiki-directory (concat user-emacs-directory "etc/hywiki/"))
  (:require hpath hbut)
  (:custom hpath:external-display-alist-x
           (list (cons (format "\\.\\(%s\\)$"
                               hpath:external-file-suffixes)
                       "xdg-open"))))

(provide 'tool-hyperbole)
;; ends here.
