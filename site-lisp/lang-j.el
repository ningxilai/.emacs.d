;; -*- lexical-binding: t; -*-

;;; commentary:

;;; code:

(setup (:elpaca j-mode :host github :repo "LdBeth/j-mode" :files ("*.el"))
  (add-to-list 'auto-mode-alist '("\\.ij[rsp]$" . j-mode))
  (add-to-list 'auto-mode-alist '("\\.ijt$" . j-lab-mode))
  (:option j-console-cmd "jconsole")
  (:hooks inferior-j-mode-hook (lambda () (electric-pair-mode -1)))
  (:custom j-verb-face '((t (:foreground "Red")))
           j-adverb-face '((t (:foreground "Green")))
           j-conjunction-face '((t (:foreground "Blue")))
           j-other-face '((t (:foreground "Black")))))

(provide 'lang-j)
;; ends here
