;;;  -*- lexical-binding: t; -*-

;;; Code:

;; Reader

(setup (:elpaca reader :host codeberg :repo "MonadicSheep/emacs-reader" :files ("*.el" "render-core.so") :pre-build ("make" "all")))

(provide 'tool-reader)
;; ends here.
