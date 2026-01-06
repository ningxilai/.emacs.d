;;; -*- lexical-binding: t; -*-

;;; commentary:

;;; code:

(setup (:elpaca typst-ts-mode)
  (define-key typst-ts-mode-map (kbd "C-c C-c") 'typst-ts-tmenu)
  (add-to-list 'auto-mode-alist '(".*\\.typ$" . "typst"))
  (:custom
   ;; don't add "--open" if you'd like `watch` to be an error detector
   typst-ts-mode-watch-options "--open"
   ;; experimental settings (I'm the main dev, so I enable these)
   typst-ts-mode-enable-raw-blocks-highlight t
   typst-ts-mode-highlight-raw-blocks-at-startup t))

(setup (:elpaca websocket))

(setup (:elpaca typst-preview)
  (define-key typst-ts-mode-map (kbd "C-c C-t p") 'typst-preview-start)
  (:custom typst-preview-executable "tinymist preview"
           typst-preview-browser "default"))

(provide 'lang-typst)
;;; ends here
