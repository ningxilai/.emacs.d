;; -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(setup (:elpaca parseclj))
(setup (:elpaca parseedn))

(setup (:elpaca clojure-mode)
  (:option clojure-indent-style 'always-indent
           clojure-indent-keyword-style 'always-indent
           clojure-enable-indent-specs nil))

(setup (:elpaca clojure-ts-mode)
  (:init (setq clojure-ts-auto-remap nil))
  (:option clojure-ts-auto-remap nil
           clojure-ts-indent-style 'fixed
           clojure-ts-semantic-indent-rules '(("->" . ((:block 1)))
                                              ("->>" . ((:block 1)))))

  (:hooks clojure-ts-mode-hook cider-mode)

  (cl-callf2 rassq-delete-all 'clojure-ts-clojurescript-mode auto-mode-alist)
  (cl-callf2 rassq-delete-all 'clojure-ts-clojurec-mode auto-mode-alist)
  (cl-callf2 rassq-delete-all 'clojure-ts-clojuredart-mode auto-mode-alist)
  (cl-callf2 rassq-delete-all 'clojure-ts-jank-mode auto-mode-alist)
  (cl-callf2 rassq-delete-all 'clojure-ts-joker-mode auto-mode-alist)
  (:hooks clojure-ts-mode-hook (lambda () (progn (require 'flycheck-clj-kondo) (flycheck-mode 1)))))

(setup (:elpaca cider)
  (:option nrepl-hide-special-buffers t
           nrepl-log-messages nil
           cider-font-lock-dynamically '(macro core function var deprecated)
           cider-overlays-use-font-lock t
           cider-print-options '(("length" 100))
           cider-prompt-for-symbol nil
           cider-repl-history-display-duplicates nil
           cider-repl-history-display-style 'one-line
           cider-repl-history-file (file-name-concat user-emacs-directory "var/cider-repl-history")
           cider-repl-history-highlight-current-entry t
           cider-repl-history-quit-action 'delete-and-restore
           cider-repl-history-highlight-inserted-item t
           cider-repl-history-size 1000
           cider-repl-result-prefix ";; => "
           cider-repl-use-clojure-font-lock t
           cider-repl-use-pretty-printing t
           cider-repl-wrap-history nil
           cider-stacktrace-default-filters '(tooling dup)
           cider-repl-pop-to-buffer-on-connect 'display-only))

(setup (:elpaca clj-refactor)
  (:hooks clojure-mode-hook clj-refactor-mode
          clojure-ts-mode-hook clj-refactor-mode))

(setup (:elpaca hydra))

(setup (:elpaca sesman))
(setup (:elpaca spinner))
(setup (:elpaca queue))

(provide 'lang-clojure)

;; ends here
