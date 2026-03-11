;;;   -*- lexical-binding: t; -*-

;;; Code:

;; Citre

(setup (:elpaca citre :host github :repo "universal-ctags/citre")
  (:require citre citre-config)
  (:global "C-c p" citre-peek)
  (:with-map citre-peek-keymap (:bind "n" citre-peek-next-line
                                      "p" citre-peek-prev-line
                                      "C-g" citre-peek-abort
                                      "j" citre-peek-jump)))

;; ends here.
