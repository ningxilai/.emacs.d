;;;  -*- lexical-binding: t; -*-

;;; Code:

;; Pandoc

(setup (:elpaca websocket))

(setup (:elpaca deno-bridge :host github :repo "manateelazycat/deno-bridge"))

(setup (:elpaca pandoc-preview :host github :repo "ningxilai/PandocPreview" :files (:defaults "pandoc-preview.el" "pandoc-preview.ts" (:exclude "README.md"))))

(provide 'tool-pandoc)

;; ends here.
