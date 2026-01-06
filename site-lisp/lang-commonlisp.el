;; -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

;; CommonLisp

(use-package sly
  :ensure t
  :custom
  (inferior-lisp-program "/bin/sbcl") ;; ~/.roswell/impls/x86-64/linux/sbcl-bin/2.5.4/bin/sbcl
  (common-lisp-hyperspec-root "file:///home/iris/.local/share/doc/HyperSpec/")
  (sly-net-coding-system 'utf-8-unix)
  (sly-protocol-version 'ignore)
  (sly-lisp-implementations
   '((sbcl ("sbcl") :coding-system utf-8-unix)
     (qlot ("qlot" "exec" "sbcl") :coding-system utf-8-unix)))
  :config
  (let ((features '(sly-fancy)))
    (sly-setup features))
  :bind (:map sly-mrepl-mode-map
              ("TAB" . indent-for-tab-command)))

(use-package sly-asdf :ensure t)
(use-package sly-quicklisp :ensure t)
(use-package sly-repl-ansi-color :ensure t)
(use-package sly-macrostep :ensure t)

(provide 'lang-commonlisp)
;; ends
