;;; Commentary:  -*- lexical-binding: t; -*-

;;; Code:

;; Web

(setup (:elpaca web-mode)
  (:mode "\\.[px]?html?\\'"
         "\\.\\(?:tpl\\|blade\\)\\(?:\\.php\\)?\\'"
         "\\.erb\\'"
         "\\.[lh]?eex\\'"
         "\\.jsp\\'"
         "\\.as[cp]x\\'"
         "\\.ejs\\'"
         "\\.hbs\\'"
         "\\.mustache\\'"
         "\\.svelte\\'"
         "\\.twig\\'"
         "\\.jinja2?\\'"
         "\\.eco\\'"
         "wp-content/themes/.+/.+\\.php\\'"
         "templates/.+\\.php\\'")
  (:require web-mode)
  (:init (dolist (alist web-mode-engines-auto-pairs)
           (setcdr alist
                   (cl-loop for pair in (cdr alist)
                            unless (string-match-p "^[a-z-]" (cdr pair))
                            collect (cons (car pair)
                                          (string-trim-right (cdr pair)
                                                             "\\(?:>\\|]\\|}\\)+\\'"))))))
  (:option web-mode-enable-html-entities-fontification t
           web-mode-auto-close-style 1)
  (:custom web-mode-enable-auto-quoting nil
           web-mode-enable-auto-pairing t))

(provide 'lang-web)
;; ends here.
