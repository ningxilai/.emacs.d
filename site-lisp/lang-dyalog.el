;; -*- lexical-binding: t; -*-

;;; commentary:

;;; code:

(elpaca (:elpaca dyalog-mode)
  (add-to-list 'auto-mode-alist '("\\.apl[afno]" . dyalog-mode))
  (add-to-list 'interpreter-mode-alist '("dyalogscript\\(\\.bash\\)?" . dyalog-mode))
  (:init (setq dyalog-fix-whitespace-before-save t
               dyalog-leading-spaces 0))
  (:hooks dyalog-mode-hook (lambda () (progn (electric-pair-mode -1)
                                        (buffer-face-set '(:family "BQN386 Unicode" :height 125))
                                        (buffer-face-mode))))
  (:option modify-syntax-entry ?# ". 1" dyalog-mode-syntax-table
           modify-syntax-entry ?! ". 2<" dyalog-mode-syntax-table)
  (:custom dyalog-keyword-chars "×≤≥≠∨∧÷∊⍴↑↓⍳○←→⌈⌊∘⍎⍕⊂⊃⊆⊇∩∪⊥⊤⍨⍒⍋⌽⍉⊖⍟⍱⍲⍬⌹≡≢⍪⌿⍀⍺⍵⎕⍞⋄⍷⍸⌷⍣⊣⊢⌶⌺⍥⍠⌸⍤"))

(provide 'lang-dyalog)

;; ends here
