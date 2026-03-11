;;;   -*- lexical-binding: t; -*-

;;; Code:

;; lsp-ltex.el & text-mode

(setup (:elpaca lsp-ltex-plus :host github :repo "emacs-languagetool/lsp-ltex-plus")
  (:hooks text-mode (lambda () (progn (require 'lsp-ltex-plus) (lsp-deferred))))
  (:option lsp-ltex-plus-version "18.6.1"
           lsp-ltex-plus-user-rules-path "etc/lsp/lsp-ltex-plus"))

(provide 'tool-ltex-plus)

;; ends here.
