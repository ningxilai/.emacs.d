;;;  -*- lexical-binding: t; -*-

;;; Code:

;; Theme

(setup ui

  (defun nano-modeline-setup ()
    "Setup header line format.
This function should be called after theme is loaded."
    (set-face-attribute 'header-line nil
                        :background 'unspecified
                        :underline nil
                        :box nil
                        :inherit nil)

    (setopt header-line-format
            '(:eval
              (let* ((read-only-p buffer-read-only)
                     (modified-p (buffer-modified-p))
                     (line-color (cond (read-only-p "#81A1C1") ; Frost 2 - blue
                                       (modified-p "#D08770") ; Aurora 1 - orange
                                       (t "#677691"))) ; Default - grey
                     ;; Get cursor position using format-mode-line (like nano-modeline)
                     (coords-raw (format-mode-line " %l/%c "))
                     ;; Parse line and column numbers from the raw format
                     (coords-parts (split-string coords-raw "/"))
                     ;; (line-num (string-to-number (car coords-parts))) ; 1-base
                     (line-num (1- (string-to-number (car coords-parts)))) ; 0-base
                     (col-num (string-to-number (cadr coords-parts)))
                     ;; Calculate the width needed for coordinates display
                     ;; Use the same number of digits for both line and column
                     (max-width (max (length (number-to-string line-num))
                                     (length (number-to-string col-num))))
                     ;; Ensure at least 2 digits to avoid single-digit display
                     (max-width (if (= max-width 1) 2 max-width))
                     (format-str (format "%%0%dd/%%0%dd" max-width max-width))
                     (coords-inner (format format-str line-num col-num))
                     (coords (format " %s " coords-inner))
                     (coords-length (length coords))
                     (window-width (window-width))
                     (half-width (/ window-width 2))
                     (half-coords (/ coords-length 2))
                     (left-width (max 0 (- half-width half-coords)))
                     (right-width (max 0 (- window-width left-width coords-length)))
                     (line-char ?─) ; Unicode horizontal line character
                     (default-foreground (face-foreground 'default nil 'default)))
                (list
                 ;; Left line segment using page-break-line style
                 (propertize (make-string left-width line-char)
                             'face `(:foreground ,line-color :height 1.0))
                 ;; Coordinates - use default foreground color
                 (propertize coords 'face `(:foreground ,default-foreground))
                 ;; Right line segment using page-break-line style
                 (propertize (make-string right-width line-char)
                             'face `(:foreground ,line-color :height 1.0)))))))

  ;; Use enable-theme-functions (a hook run after a theme is enabled)
  (when (boundp 'enable-theme-functions)
    (add-hook 'enable-theme-functions (lambda (_) (nano-modeline-setup))))

  (set-display-table-slot standard-display-table 'truncation (make-glyph-code ?…))
  (set-display-table-slot standard-display-table 'wrap (make-glyph-code ?–))

  (setopt ;; window.el config
   split-height-threshold 80
   split-width-threshold 120
   pop-up-windows nil)

  (setopt minibuffer-prompt-properties
          '(read-only t intangible t cursor-intangible t face minibuffer-prompt))
  (add-hook 'minibuffer-setup-hook #'cursor-intangible-mode)

  (setopt enable-recursive-minibuffers t)

  (setopt window-resize-pixelwise t))

(setup (:elpaca doom-themes)

  (defun +nanolize (&rest args)
    (interactive)
    (let* ((background
            (plist-get (custom-face-attributes-get 'default nil)
                       :background))
           (mode-line
            (plist-get (custom-face-attributes-get 'mode-line nil)
                       :background))
           (mode-line-active
            (or (plist-get (custom-face-attributes-get 'mode-line-active nil)
                           :background)
                mode-line))
           (mode-line-inactive
            (or (plist-get (custom-face-attributes-get 'mode-line-inactive nil)
                           :background)
                mode-line)))
      (dolist (face '(window-divider
                      window-divider-first-pixel
                      window-divider-last-pixel))
        (set-face-attribute face nil :foreground background))
      (set-face-attribute 'mode-line-active nil
                          :box `(:line-width 1 :color ,mode-line-active :style nil))
      (set-face-attribute 'mode-line-inactive nil
                          :box `(:line-width 1 :color ,mode-line-inactive :style nil))))

  (+nanolize)
  (advice-add 'enable-theme :after '+nanolize)

  (:init (load-theme 'doom-nord t nil)))

(setup (:elpaca doom-modeline)
  (doom-modeline-mode +1)
  (:require advance-words-count)
  (:option doom-modeline-bar-width 3
           doom-modeline-github nil
           doom-modeline-mu4e nil
           doom-modeline-persp-name nil
           doom-modeline-minor-modes nil
           doom-modeline-major-mode-icon nil
           doom-modeline-buffer-file-name-style 'relative-from-project
           doom-modeline-buffer-encoding 'nondefault
           doom-modeline-default-eol-type (if (featurep :system 'windows) 1 0)
           doom-modeline-enable-buffer-position nil)

  (doom-modeline-def-segment word-count-advance ()
    (let* ((start (point-min))
           (end (point-max))
           (list (advance-words-count start end))
           (cjk (car list))
           (ascii (car (last list)))
           (wc (+ cjk ascii)))
      (propertize (format " CJK:%d Total:%d" cjk wc)
                  'face (doom-modeline-face))))

  (doom-modeline-def-modeline 'main
    '(eldoc bar window-state workspace-name window-number modals matches follow buffer-info remote-host buffer-position word-count-advance parrot selection-info)
    '(compilation objed-state misc-info project-name persp-name battery grip irc mu4e gnus github debug repl lsp minor-modes input-method indent-info buffer-encoding major-mode process vcs check time)))

(setup (:elpaca advance-words-count :host github :repo "LdBeth/advance-words-count.el"))

(setup (:elpaca enlight)

  (setopt initial-buffer-choice #'enlight)

  ;; copy by ldbeth

  (defun switch-to-scratch-buffer (&optional arg)
    "Switch to the `*scratch*' buffer, creating it first if needed.
    if prefix argument ARG is given, switch to it in an other, possibly new window."
    (interactive "P")
    (let ((exists (get-buffer "*enlight*")))
      (if arg
          (switch-to-buffer-other-window (get-buffer-create "*enlight*"))
        (switch-to-buffer (get-buffer-create "*enlight*")))
      (unless (or exists
                  (eq major-mode initial-major-mode))
        (funcall initial-major-mode))))

  (global-set-key (kbd "C-c s") 'switch-to-scratch-buffer)

  (:custom enlight-content
           (concat
            (propertize "MENU" 'face 'highlight)
            "\n\n"
            (enlight-menu
             '(("\nOrg Mode"
                ("Org-Agenda (current day)" (org-agenda nil "a") "a"))
               ("\nFolder"
                ("Hypr folder" (dired "~/.config/hypr/") "h")
                ("Documents folder" (dired "~/Documents/") "d"))
               ("\nInit"
                ("init.el" (dired "~/.config/emacs/") "i"))
               ("\nOther"
                ("Projects" project-switch-project "p")))))))

;; Font

(setup (:elpaca indent-bars)
  (:custom indent-bars-face '((t (:height 1.08))))
  (:option indent-bars-display-on-blank-lines t
           indent-bars-width-frac 0.1
           indent-bars-zigzag nil
           indent-bars-highlight-current-depth nil
           indent-bars-pattern "│"
           indent-bars-prefer-character nil
           indent-bars-color '(highlight :face-bg t :blend 0.225)
           indent-bars-no-descend-string t)
  (require 'indent-bars-ts)
  (setopt indent-bars-treesit-ignore-blank-lines-types '("module")
          indent-bars-treesit-scope '((python
                                       function_definition
                                       class_definition
                                       for_statement
                                       if_statement
                                       with_statement
                                       while_statement)))
  (:hooks prog-mode-hook indent-bars-mode
          yaml-mode-hook indent-bars-mode))

(setup (:elpaca page-break-lines)
  (:init (global-page-break-lines-mode t))
  (set-fontset-font "fontset-default"
                    (cons page-break-lines-char page-break-lines-char)
                    (face-attribute 'default :family)))

(setup fontset
  (:init (set-face-attribute 'default (selected-frame)
                             :height 110
                             :weight 'light :family "Lilex Nerd Font") ;; SF Mono
         (set-face-attribute 'bold (selected-frame)
                             :weight 'regular)
         (set-face-attribute 'bold-italic (selected-frame)
                             :weight 'regular))

  (set-char-table-range composition-function-table ?f '(["\\(?:ff?[fijlt]\\)" 0 font-shape-gstring]))
  (set-char-table-range composition-function-table ?T '(["\\(?:Th\\)" 0 font-shape-gstring]))

  (set-fontset-font t 'unicode (font-spec :family "Unifont" :weight 'normal :slant 'normal))
  (set-fontset-font t 'latin (font-spec :family "IBM Plex Mono" :weight 'light :slant 'normal))
  (set-fontset-font t 'greek (font-spec :family "Catrinity" :weight 'normal :slant 'normal))

  (set-fontset-font t 'emoji
                    (cond
                     ((member "Noto Emoji" (font-family-list)) "Noto Emoji")
                     ((member "Symbola" (font-family-list)) "Symbola")
                     ((member "Apple Color Emoji" (font-family-list)) "Apple Color Emoji")
                     ((member "Noto Color Emoji" (font-family-list)) "Noto Color Emoji")
                     ((member "Segoe UI Emoji" (font-family-list)) "Segoe UI Emoji")))


  (set-fontset-font t 'han
                    (cond
                     ((member "LXGW WenKai" (font-family-list)) "LXGW WenKai")
                     ((member "Zhuque Fangsong (technical preview)" (font-family-list)) "Zhuque Fangsong (technical preview)")
                     ((member "PingFang SC" (font-family-list)) "PingFang SC")
                     ((member "方正柳公权楷书 简繁" (font-family-list)) "方正柳公权楷书 简繁")
                     ((member "Sarasa Term SC Nerd" (font-family-list)) "Sarasa Term SC Nerd")
                     ((member "WenQuanYi Micro Hei" (font-family-list)) "WenQuanYi Micro Hei")
                     ((member "WenQuanYi Zen Hei" (font-family-list)) "WenQuanYi Zen Hei")
                     ((member "Microsoft YaHei" (font-family-list)) "Microsoft YaHei")))


  (set-fontset-font t 'cjk-misc
                    (cond
                     ((member "Noto Serif CJK SC" (font-family-list)) "Noto Serif CJK SC")
                     ((member "Sarasa UI J" (font-family-list)) "Sarasa UI J")))

  (set-fontset-font t 'kana (font-spec :family "Sarasa Gothic" :weight 'normal :slant 'normal))
  (set-fontset-font t 'bopomofo (font-spec :family "Symbola" :weight 'normal :slant 'normal))

  (dolist (char/ligature-re
           `((?-  . ,(rx (or (or "-->" "-<<" "->>" "-|" "-~" "-<" "->") (+ "-"))))
             (?/  . ,(rx (or (or "/==" "/=" "/>" "/**" "/*") (+ "/"))))
             (?*  . ,(rx (or (or "*>" "*/") (+ "*"))))
             (?<  . ,(rx (or (or "<<=" "<<-" "<|||" "<==>" "<!--" "<=>" "<||" "<|>" "<-<" "<==" "<=<" "<-|" "<~>" "<=|" "<~~" "<$>" "<+>" "</>" "<*>" "<->" "<=" "<|" "<:" "<>"  "<$" "<-" "<~" "<+" "</" "<*")
                             (+ "<"))))
             (?:  . ,(rx (or (or ":?>" "::=" ":>" ":<" ":?" ":=") (+ ":"))))
             (?=  . ,(rx (or (or "=>>" "==>" "=/=" "=!=" "=>" "=:=") (+ "="))))
             (?!  . ,(rx (or (or "!==" "!=") (+ "!"))))
             (?>  . ,(rx (or (or ">>-" ">>=" ">=>" ">]" ">:" ">-" ">=") (+ ">"))))
             (?&  . ,(rx (+ "&")))
             (?|  . ,(rx (or (or "|->" "|||>" "||>" "|=>" "||-" "||=" "|-" "|>" "|]" "|}" "|=")
                             (+ "|"))))
             (?.  . ,(rx (or (or ".?" ".=" ".-" "..<") (+ "."))))
             (?+  . ,(rx (or "+>" (+ "+"))))
             (?\[ . ,(rx (or "[<" "[|")))
             (?\{ . ,(rx "{|"))
             (?\? . ,(rx (or (or "?." "?=" "?:") (+ "?"))))
             (?#  . ,(rx (or (or "#_(" "#[" "#{" "#=" "#!" "#:" "#_" "#?" "#(")
                             (+ "#"))))
             (?\; . ,(rx (+ ";")))
             (?_  . ,(rx (or "_|_" "__")))
             (?~  . ,(rx (or "~~>" "~~" "~>" "~-" "~@")))
             (?$  . ,(rx "$>"))
             (?^  . ,(rx "^="))
             (?\] . ,(rx "]#"))))
    (let ((char (car char/ligature-re))
          (ligature-re (cdr char/ligature-re)))
      (set-char-table-range composition-function-table char
                            `([,ligature-re 0 font-shape-gstring]))))

  (:option face-font-rescale-alist `(("Symbola"             . 1.3)
                                     ("Microsoft YaHei"     . 1.2)
                                     ("WenQuanYi Zen Hei"   . 1.2)
                                     ("Sarasa Term SC Nerd" . 1.2)
                                     ("PingFang SC"         . 1.16)
                                     ("Lantinghei SC"       . 1.16)
                                     ("Kaiti SC"            . 1.16)
                                     ("Yuanti SC"           . 1.16)
                                     ("Apple Color Emoji"   . 0.91)))

  (:custom-face fixed-pitch  ((t((:family "SF Mono"))))
                fixed-pitch-serif ((t((:family "SF Mono")))) ;; New York
                variable-pitch ((t((:family "SF Pro")))))) ;; Helvetica Neue

(setup (:elpaca fontaine)
  (:option fontaine-presets
           '((regular
              :default-height 120
              :default-weight regular
              :fixed-pitch-height 1.0
              :variable-pitch-height 1.0)
             (large
              :default-height 140
              :default-weight normal
              :fixed-pitch-height 1.0
              :variable-pitch-height 1.05)
             (t
              :default-family "Lilex Nerd Font Mono" ;; CaskaydiaCove Nerd Font Mono
              :fixed-pitch-family "Lilex Nerd Font Mono" ;; CaskaydiaCove Nerd Font Mono
              :variable-pitch-family "Lilex Nerd Font Mono" ;; CaskaydiaCove Nerd Font Mono
              :italic-family "IBM Plex Mono" ;; CaskaydiaCove Nerd Font Mono Italic
              :blod-family "IBM Plex Serif" ;; CaskaydiaCove Nerd Font Mono Bold
              :variable-pitch-weight normal
              :bold-weight bold
              :italic-slant italic
              :line-spacing 0.1));; Core package
           fontaine-set-preset 'regular)
  (:hooks kill-emacs-hook fontaine-store-latest-preset)
  (require 'xdg)
  (setq fontaine-latest-state-file (expand-file-name "emacs/fontaine-latest-state.eld" (xdg-cache-home))))

(provide 'tool-ui)

;; ends here.
