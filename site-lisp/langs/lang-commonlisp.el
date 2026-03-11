;; -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

;; CommonLisp

(setup (:elpaca sly)
  (:init (let ((features '(sly-fancy)))
           (sly-setup features)))
  (:custom inferior-lisp-program "/bin/sbcl" ;; ~/.roswell/impls/x86-64/linux/sbcl-bin/2.5.4/bin/sbcl
           common-lisp-hyperspec-root "file:///home/iris/.local/share/doc/HyperSpec/"
           sly-net-coding-system 'utf-8-unix
           sly-protocol-version 'ignore
           sly-lisp-implementations
           '((sbcl ("sbcl") :coding-system utf-8-unix)
             (qlot ("qlot" "exec" "sbcl") :coding-system utf-8-unix)))
  (:with-map sly-mrepl-mode-map
    (:bind "TAB" indent-for-tab-command)))

(setup (:elpaca sly-asdf))
(setup (:elpaca sly-quicklisp))
(setup (:elpaca sly-repl-ansi-color))
(setup (:elpaca sly-macrostep))

(provide 'lang-commonlisp)
;; ends
