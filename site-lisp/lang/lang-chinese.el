;; -*- lexical-binding: t; -*-

;;; Rime

(setup (:elpaca rime :host github :repo "DogLooksGood/emacs-rime")
  (:option default-input-method "rime"

           rime-show-candidate 'minibuffer
           rime-librime-root "~/.local/src/librime/build/"
           rime-user-data-dir "~/.config/rime")
  (:custom rime-disable-predicates '(rime-predicate-after-alphabet-char-p
                                     rime-predicate-current-uppercase-letter-p
                                     rime-predicate-prog-in-code-p)

           rime-inline-predicates '(rime-predicate-space-after-cc-p
                                    rime-predicate-current-uppercase-letter-p
                                    rime-predicate-punctuation-after-ascii-p)

           rime-posframe-properties '( :background-color "#333333"
                                       :foreground-color "#c4cdd3"
                                       :font "Zhuque Fangsong (technical preview)"
                                       :internal-border-width 10))
  (:hooks post-command-hook
          (lambda () (progn (defvar input-method-cursor-color "Orange"
                         "Default cursor color if using an input method.")

                       (defvar default-cursor-color (frame-parameter nil 'cursor-color)
                         "Default text cursor color.")

                       (defun change-cursor-color-on-input-method ()
                         "Set cursor color depending on whether an input method is used or not."
                         (interactive)
                         (set-cursor-color (if current-input-method
                                               input-method-cursor-color
                                             default-cursor-color)))))))

(provide 'lang-chinese)
;;; lang-chinese.el ends here
