;;;   -*- lexical-binding: t; -*-

;;; Code:

;; Prog

(setup (:elpaca lsp-mode)
  (:option lsp-completion-provider :none)
  (defun lsp-mode-setup-completion ()
    (setf (alist-get 'styles (alist-get 'lsp-capf completion-category-defaults))
          '(flex))) ;; Configure flex

  (:hooks lsp-completion-mode-hook lsp-mode-setup-completion))

(setup (:elpaca lsp-ui)
  (:init (setq lsp-ui-sideline-show-diagnostics nil
               lsp-ui-sideline-ignore-duplicate t
               lsp-ui-doc-delay 0.1
               lsp-ui-doc-show-with-cursor t
               lsp-ui-imenu-auto-refresh 'after-save
               lsp-ui-imenu-colors `(,(face-foreground 'font-lock-keyword-face)
                                     ,(face-foreground 'font-lock-string-face)
                                     ,(face-foreground 'font-lock-constant-face)
                                     ,(face-foreground 'font-lock-variable-name-face))))
  (:bind [remap xref-find-definitions] lsp-ui-peek-find-definitions
         [remap xref-find-references] lsp-ui-peek-find-references)
  (:option lsp-ui-set-doc-border "#167167")
  (:hooks lsp-mode-hook lsp-ui-mode))

(setup (:elpaca dtrt-indent)
  (defmacro space/hide-lighter (mode)
    "Diminish MODE name in mode line to LIGHTER."
    `(eval-after-load 'diminish '(diminish ',mode)))
  (space/hide-lighter dtrt-indent-mode)

  (:hooks prog-mode-hook (lambda () (dtrt-indent-mode)
                           (dtrt-indent-adapt))))

(setup treesit

  (:option treesit-enabled-modes t
           treesit-font-lock-level 4
           treesit--font-lock-verbose nil

           treesit--indent-verbose t

           toml-ts-mode-indent-offset 4
           rust-ts-mode-indent-offset 4
           cmake-ts-mode-indent-offset 4
           json-ts-mode-indent-offset 4
           go-ts-mode-indent-offset 4)

  (save-match-data
    (dolist (sym '(auto-mode-alist interpreter-mode-alist))
      (set sym (cl-loop for (src . fn) in (symbol-value sym)
                        unless (and (functionp fn)
                                    (string-match "-ts-mode\\(?:-maybe\\)?$" (symbol-name fn)))
                        collect (cons src fn)))))

  (:custom treesit-language-source-alist
           '((awk . ("https://github.com/Beaglefoot/tree-sitter-awk.git"))
             (bash       . ("https://github.com/tree-sitter/tree-sitter-bash.git"))
             (bibtex . ("https://github.com/latex-lsp/tree-sitter-bibtex.git"))
             (blueprint . ("https://github.com/huanie/tree-sitter-blueprint.git"))
             (commonlisp . ("https://github.com/tree-sitter-grammars/tree-sitter-commonlisp"))
             (c          . ("https://github.com/tree-sitter/tree-sitter-c.git"))
             (cmake      . ("https://github.com/uyha/tree-sitter-cmake.git"))
             (cpp        . ("https://github.com/tree-sitter/tree-sitter-cpp.git"))
             (csharp     . ("https://github.com/tree-sitter/tree-sitter-c-sharp.git"))
             (css        . ("https://github.com/tree-sitter/tree-sitter-css.git"))
             (clojure    . ("https://github.com/sogaiu/tree-sitter-clojure.git"))
             (dockerfile . ("https://github.com/camdencheek/tree-sitter-dockerfile.git"))
             (go         . ("https://github.com/tree-sitter/tree-sitter-go.git"))
             (gomod      . ("https://github.com/camdencheek/tree-sitter-go-mod.git"))
             (html       . ("https://github.com/tree-sitter/tree-sitter-html.git"))
             (haskell . ("https://github.com/tree-sitter/tree-sitter-haskell.git"))
             (java       . ("https://github.com/tree-sitter/tree-sitter-java.git"))
             (javascript . ("https://github.com/tree-sitter/tree-sitter-javascript.git"))
             (json       . ("https://github.com/tree-sitter/tree-sitter-json.git"))
             (latex . ("https://github.com/latex-lsp/tree-sitter-latex.git"))
             (make . ("https://github.com/tree-sitter-grammars/tree-sitter-make.git"))
             (nu . ("https://github.com/nushell/tree-sitter-nu.git"))
             (org . ("https://github.com/milisims/tree-sitter-org.git"))
             (markdown   . ("https://github.com/tree-sitter-grammars/tree-sitter-markdown.git" nil "tree-sitter-markdown/src"))
             (markdown-inline . ("https://github.com/tree-sitter-grammars/tree-sitter-markdown.git" nil "tree-sitter-markdown-inline/src"))
             (perl . ("https://github.com/ganezdragon/tree-sitter-perl.git"))
             (proto . ("https://github.com/mitchellh/tree-sitter-proto.git"))
             (python     . ("https://github.com/tree-sitter/tree-sitter-python.git"))
             (r . ("https://github.com/r-lib/tree-sitter-r.git"))
             (ruby       . ("https://github.com/tree-sitter/tree-sitter-ruby.git"))
             (rust       . ("https://github.com/tree-sitter/tree-sitter-rust.git"))
             (sql . ("https://github.com/DerekStride/tree-sitter-sql.git" "gh-page"))
             (surface . ("https://github.com/connorlay/tree-sitter-surface.git"))
             (toml       . ("https://github.com/tree-sitter/tree-sitter-toml.git"))
             (tsx        . ("https://github.com/tree-sitter/tree-sitter-typescript.git" nil "tsx/src"))
             (typst      . ("https://github.com/uben0/tree-sitter-typst.git"))
             (typescript . ("https://github.com/tree-sitter/tree-sitter-typescript.git" nil "typescript/src"))
             (verilog . ("https://github.com/gmlarumbe/tree-sitter-verilog.git"))
             (vhdl . ("https://github.com/alemuller/tree-sitter-vhdl.git"))
             (vue . ("https://github.com/tree-sitter-grammars/tree-sitter-vue.git"))
             (wast . ("https://github.com/wasm-lsp/tree-sitter-wasm.git" nil "wast/src"))
             (wat . ("https://github.com/wasm-lsp/tree-sitter-wasm.git" nil "wat/src"))
             (wgsl . ("https://github.com/mehmetoguzderin/tree-sitter-wgsl.git"))
             (yaml       . ("https://github.com/ikatyang/tree-sitter-yaml.git")))

           major-mode-remap-alist
           '((c-mode          . c-ts-mode)
             (c++-mode        . c++-ts-mode)
             (c-or-c++-mode   . c-or-c++-ts-mode)
             (cmake-mode      . cmake-ts-mode)
             (clojure-mode    . clojure-ts-mode)
             (conf-toml-mode  . toml-ts-mode)
             (csharp-mode     . csharp-ts-mode)
             (css-mode        . css-ts-mode)
             (html-mode       . html-ts-mode)
             (java-mode       . java-ts-mode)
             (js-mode         . js-ts-mode)
             (json-mode       . json-ts-mode)
             (mhtml-mode      . mhtml-ts-mode)
             (python-mode     . python-ts-mode)
             (ruby-mode       . ruby-ts-mode)
             (sh-mode         . bash-ts-mode)
             (typescript-mode . typescript-ts-mode))))

(setup (:elpaca colorful-mode)
  (:init (setq-default colorful-use-prefix t))
  (dolist (mode '(html-mode php-mode help-mode helpful-mode))
    (add-to-list 'global-colorful-modes mode))
  (:hooks prog-mode-hook colorful-mode))

(setup (:elpaca region-occurrences-highlighter)
  (:hooks prog-mode-hook region-occurrences-highlighter-mode
          text-mode-hook region-occurrences-highlighter-mode)
  (:with-map prog-mode-map (:bind "M-n" region-occurrences-highlighter-next
                                  "M-p" region-occurrences-highlighter-prev)))

(setup (:elpaca hsluv)
  (:require color)
  (:require hsluv)
  (:require cl-lib)
  (:init (require 'color)
         (require 'hsluv)
         (require 'cl-lib)

         (defun perfect-palette (p q &optional hue-start s l luv)
           "Calculate a perfect palette of P colors with step Q. Return HEX color strings."
           (let ((hue-start (or hue-start 0))
                 (s (or s 0))
                 (l (or l 0))
                 (omega (/ (float q) p)))
             (mapcar
              (lambda (x)
                (let ((hue (mod (+ hue-start (* x omega)) 1.0)))
                  (if luv
                      (hsluv-hpluv-to-hex (list (* 360 hue) s l))
                    (apply #'color-rgb-to-hex
                           (append (color-hsl-to-rgb hue (/ s 100.0) (/ l 100.0))
                                   '(2))))))
              (number-sequence 0 (1- p) 1))))

         (defvar perfect-palette-override--faces
           '(font-lock-builtin-face
             font-lock-comment-face
             font-lock-constant-face
             font-lock-function-name-face
             font-lock-keyword-face
             font-lock-string-face
             font-lock-type-face
             font-lock-variable-name-face
             font-lock-warning-face)
           "Faces to override with perfect-palette colors.")

         (defvar perfect-palette-override--saved nil
           "Saved face specs before overriding.")

         (defun perfect-palette-override--apply (palette)
           "Override font-lock faces with generated PALETTE from `perfect-palette'."
           (setq perfect-palette-override--saved nil)
           (cl-loop for face in perfect-palette-override--faces
                    for col in palette
                    do (push (list face (face-attribute face :foreground nil 'default)) perfect-palette-override--saved)
                    do (set-face-foreground face col)))

         (defun perfect-palette-override-enable
             (&optional p q hue-start s l luv)
           "Enable perfect-palette based face override for font-lock faces."
           (interactive)
           (let* ((p (or p 9))
                  (q (or q 4))
                  (hue-start (or hue-start 0.58))
                  (s (or s 45))
                  (l (or l 65))
                  (luv (or luv t))
                  (palette (perfect-palette p q hue-start s l luv)))
             (perfect-palette-override--apply
              ;; 颜色数量可能比 faces 少/多，补齐或截断
              (cl-subseq (append palette (make-list (length perfect-palette-override--faces) "#81a1c1"))
                         0 (length perfect-palette-override--faces)))))

         (defun perfect-palette-override-disable ()
           "Restore original font-lock face colors."
           (interactive)
           (when perfect-palette-override--saved
             (dolist (elem perfect-palette-override--saved)
               (let ((face (nth 0 elem))
                     (color (nth 1 elem)))
                 (set-face-foreground face color)))
             (setq perfect-palette-override--saved nil))))

  (:hooks prog-mode-hook perfect-palette-override-enable))

(setup whitespace
  (whitespace-mode +1)
  (:init
   (setq-default which-func-update-delay 0.2
                 show-trailing-whitespace nil))
  (:option whitespace-line-column nil)
  (:custom indicate-empty-lines nil
           whitespace-style '(faces
                              tab-mark
                              missing-newline-at-eof
                              trailing
                              space-before-tab
                              indentation
                              empty
                              space-after-tab))
  (:custom-face whitespace-display-mappings `((tab-mark ?\t [,(make-glyph-code ?» 'whitespace-tab) ?\t])))
  (:hooks before-save-hook delete-trailing-whitespace-mode
          prog-mode-hook (lambda () (setq-local show-trailing-whitespace t))))

(setup (:elpaca whitespace-cleanup-mode)
  (:hooks before-save-hook whitespace-cleanup))

(provide 'tool-prog)

;; ends here.
