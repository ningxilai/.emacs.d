;;; Commentary:  -*- lexical-binding: t; -*-

;;; Code:

;; EWW

(setup (:elpaca ekp :host github :repo "Kinneyzhang/emacs-kp" :file (:default "*.el" "dictionaries"))
  (:require ekp-utils ekp-hyphen ekp)
  (ekp-c-module-load)
  (:custom ekp-latin-lang "en_US")
  (:option ekp-line-penalty 10
           ekp-hyphen-penalty 50
           ekp-adjacent-fitness-penalty 100

           ;; pixels

           ekp-lws-ideal-pixel 8
           ekp-lws-stretch-pixel 4
           ekp-lws-shrink-pixel 3

           ekp-mws-ideal-pixel 7
           ekp-mws-stretch-pixel 3
           ekp-mws-shrink-pixel 2

           ekp-cws-ideal-pixel 0
           ekp-cws-stretch-pixel 2
           ekp-cws-shrink-pixel 0)

  (:load eww-kp-direct :dirs ("site-lisp/tool/eww-kp-direct"))
  (:require eww-kp-direct)
  (:init
   ;; Enable KP algorithm for EWW
   (setopt eww-kp-use-kp t
           eww-kp-line-width 65)))

(setup (:elpaca shrface :host github :repo "chenyanming/shrface")
  (:require shrface))

(setup (:elpaca shr-tag-pre-highlight :host github :repo "xuchunyang/shr-tag-pre-highlight.el")
  (:require shr-tag-pre-highlight)
  (:init (add-to-list 'shr-external-rendering-functions
                      '(pre . shr-tag-pre-highlight))
         (with-eval-after-load 'eww
           (advice-add 'eww-display-html :around
                       'eww-display-html--override-shr-external-rendering-functions))))

(provide 'tool-eww)
;; ends here.
