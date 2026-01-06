;; -*- lexical-binding: t; -*-

;;; commentary:

;;; code:

(eval-and-compile
  (require 'apl))

(setup (:elpaca gnu-apl-mode)
  (:option gnu-apl-show-tips-on-start nil)
  (:hook gnu-apl-mode (lambda ()
                        (progn
                          (set-input-method "apl-ascii")
                          (buffer-face-set '(:family "APL386 Unicode" :height 125))
                          (buffer-face-mode)
                          ;; https://github.com/abrudz/APL386
                          ;; https://aplwiki.com/wiki/Fonts
                          (electric-pair-mode -1)))
         gnu-apl-interactive-mode . (lambda ()
                                      (progn
                                        (set-input-method "apl-ascii")
                                        (buffer-face-set '(:family "BQN386 Unicode" :height 125))
                                        (buffer-face-mode)
                                        (electric-pair-mode -1)))))

(provide 'lang-apl)

;; ends here
