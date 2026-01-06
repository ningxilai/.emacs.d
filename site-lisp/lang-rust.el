;; -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(setup rust-ts-mode
  (:option
   lsp-rust-analyzer-proc-macro-enable t
   lsp-rust-analyzer-experimental-proc-attr-macros t)
  (add-hook 'rust-ts-mode-hook 'cargo-minor-mode)
  (add-hook 'rust-ts-mode-hook 'tree-sitter-hl-mode)
  (:hooks rust-ts-mode (lambda () (setq-local tab-width 4
                                         rust-format-on-save t))))

(setup (:elpaca cargo))
(setup (:elpaca ron-mode))

(provide 'lang-rust)

;; ends here
