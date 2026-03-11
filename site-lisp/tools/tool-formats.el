;;;   -*- lexical-binding: t; -*-

;;; Code:

;; Apheleia

(setup (:elpaca apheleia)
  (:require apheleia)
  (:init (add-to-list 'apheleia-mode-alist '(sh-mode . shfmt))
         (add-to-list 'apheleia-mode-alist '(cuda-mode . clang-format))
         (add-to-list 'apheleia-mode-alist '(cuda-ts-mode . clang-format))
         (add-to-list 'apheleia-mode-alist '(protobuf-mode . clang-format))

         (add-to-list 'apheleia-formatters-mode-extension-assoc '(cuda-mode . ".cu"))
         (add-to-list 'apheleia-formatters-mode-extension-assoc '(cuda-ts-mode . ".cu"))
         (add-to-list 'apheleia-formatters-mode-extension-assoc '(glsl-ts-mode . ".glsl"))
         (add-to-list 'apheleia-formatters-mode-extension-assoc '(protobuf-mode . ".proto"))

         ;; Apheleia's default clang-format config doesn't respect `c-basic-offset', so
         ;; force it to in the absence of a .clang-format file.
         (setf (alist-get 'clang-format apheleia-formatters)
               `("clang-format"
                 "-assume-filename"
                 (or (apheleia-formatters-local-buffer-file-name)
                     (apheleia-formatters-mode-extension)
                     ".c")
                 (when apheleia-formatters-respect-indent-level
                   (unless (locate-dominating-file default-directory ".clang-format")
                     (format "--style={IndentWidth: %d}" c-basic-offset)))))

         ;; Apheleia's default config for prettier passes an explicit --tab-width N to
         ;; all prettier formatters, respecting your indent settings in Emacs, but
         ;; overriding any indent settings in your prettier config files. This changes
         ;; it to omit indent switches if any configuration for prettier is present in
         ;; the current project.
         (dolist (formatter '(prettier prettier-css prettier-html prettier-javascript
                                       prettier-json prettier-scss prettier-svelte
                                       prettier-typescript prettier-yaml))
           (setf (alist-get formatter apheleia-formatters)
                 (append (delete '(apheleia-formatters-js-indent "--use-tabs" "--tab-width")
                                 (alist-get formatter apheleia-formatters))
                         '((when apheleia-formatters-respect-indent-level
                             (unless (or (cl-loop for file
                                                  in '(".prettierrc"
                                                       ".prettierrc.json"
                                                       ".prettierrc.yml"
                                                       ".prettierrc.yaml"
                                                       ".prettierrc.json5"
                                                       ".prettierrc.js" "prettier.config.js"
                                                       ".prettierrc.mjs" "prettier.config.mjs"
                                                       ".prettierrc.cjs" "prettier.config.cjs"
                                                       ".prettierrc.toml")
                                                  if (locate-dominating-file default-directory file)
                                                  return t)
                                         (when-let ((pkg (locate-dominating-file default-directory "package.json")))
                                           (require 'json)
                                           (let ((json-key-type 'alist))
                                             (assq 'prettier
                                                   (json-read-file (expand-file-name "package.json" pkg))))))
                               (apheleia-formatters-indent "--use-tabs" "--tab-width")))))))))

(provide 'tool-formats)

;; ends here.
