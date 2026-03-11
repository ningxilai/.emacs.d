;; -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(setup (:elpaca haskell-mode)
  (:custom haskell-process-suggest-remove-import-lines t  ; warnings for redundant imports etc
           haskell-process-auto-import-loaded-modules t
           haskell-process-show-overlays t)
  (:hooks haskell-mode-hook haskell-collapse-mode
          haskell-mode-hook interactive-haskell-mode
          haskell-mode-hook (lambda()(setq-local yas-indent-line 'fixed))
          haskell-mode-local-vars lsp-haskell
          haskell-literate-mode-local-vars lsp-haskell))

(setup (:elpaca haskell-ts-mode :repos "https://codeberg.org/pranshu/haskell-ts-mode")
  (:custom haskell-ts-font-lock-level 4
           haskell-ts-use-indent t
           haskell-ts-ghci "ghci"
           haskell-ts-use-indent t))

(setup (:elpaca haskell-snippets)
  (:custom haskell-snippets-dir "elpaca/builds/haskell-snippets/snippets/haskell-mode"))

(setup (:elpaca lsp-haskell :host github :repo "emacs-lsp/lsp-haskell")
  (:custom lsp-haskell-server-path "~/.ghcup/bin/haskell-language-server-wrapper"))

(provide 'lang-haskell)
;;; ends here
