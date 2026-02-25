;; -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(setup (:elpaca idris2-mode :host github :repo "idris-community/idris2-mode")
  (:option idris2-interpreter-path "~/.pack/bin/idris2"))

(provide 'lang-idris2)
;; ends here
