;;;  -*- lexical-binding: t; -*-

;;; Code:

;; Reader

(setup (:elpaca reader :host codeberg :repo "MonadicSheep/emacs-reader" :files ("*.el" "render-core.so") :pre-build ("make" "all")))

(setup doc-view
  (:option doc-view-continuous t
           doc-view-resolution 144))

(provide 'tool-reader)

;; ends here.
