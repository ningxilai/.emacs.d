;;; Commentary:  -*- lexical-binding: t; -*-

;;; Code:

;; Reader

;; (setup (:elpaca reader :host codeberg :repo "MonadicSheep/emacs-reader" :files ("*.el" "render-core.so") :pre-build ("make" "all")))

(setup epub-mode (:elpaca epub :host github :repo "PeteLeng/epub-mode")
       (:require exml-query epub)
       (:mode ".*\\.epub"))

(provide 'tool-reader)
;; ends here.
