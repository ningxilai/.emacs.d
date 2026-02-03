;; -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(setup rust-ts-mode
  (:option
   lsp-rust-analyzer-proc-macro-enable t
   lsp-rust-analyzer-experimental-proc-attr-macros t)
  (:with-hook rust-ts-mode-hook
    (:hook cargo-minor-mode
           tree-sitter-hl-mode
           (lambda () (setq-local tab-width 4
                                  rust-format-on-save t)))))

(setup (:elpaca cargo))
(setup (:elpaca ron-mode))

(provide 'lang-rust)

;; ends here
