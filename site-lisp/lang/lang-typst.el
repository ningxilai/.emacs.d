;;; -*- lexical-binding: t; -*-

;;; commentary:

;;; code:

(setup (:elpaca typst-ts-mode)
  (add-to-list 'auto-mode-alist '(".*\\.typ$" . typst-ts-mode))
  (:custom typst-ts-indent-offset 2
           ;; don't add "--open" if you'd like `watch` to be an error detector
           typst-ts-mode-watch-options "--open"
           ;; experimental settings (I'm the main dev, so I enable these)
           typst-ts-mode-enable-raw-blocks-highlight t
           typst-ts-mode-highlight-raw-blocks-at-startup t)
  (:bind "C-c C-c" typst-ts-tmenu))

(setup (:elpaca websocket))

(setup typst-preview-mode (:elpaca typst-preview)

       (defun +typst-in-math-p ()
         "如果在 typst-ts-mode 的 math 环境中则返回 non-nil。"
         (interactive)
         (when (derived-mode-p 'typst-ts-mode)
           (let ((node (treesit-node-at (point))))
             ;; 向上查找父节点，直到找到类型为 "math" 的节点
             (treesit-parent-until
              node
              (lambda (n)
                (member (treesit-node-type n) '("math")))))))
       (defun +typst-preview--connect-browser (browser hostname)
         "Open browser `BROWSER' at websocket URL `HOSTNAME'."
         (let ((full-url (concat "http://" hostname)))
           (pcase browser
             ("default" (browse-url full-url))
             ("miniwindow"
              (let* ((browse-url-generic-program (executable-find "chromium"))
                     (browse-url-browser-function 'browse-url-generic)
                     (browse-url-generic-args `("--new-window" ,(concat "--app=" full-url))))
                (browse-url full-url)))
             (_
              (let* ((browse-url-generic-program (executable-find browser))
                     (browse-url-browser-function 'browse-url-generic))
                (browse-url full-url))))))
       (defun +typst-preview-open-browser ()
         "Open typst-preview browser interactively."
         (interactive)
         (let* ((browser-list '("miniwindow" "default" "google chrome"))
                (browser (completing-read "Browser: " browser-list nil nil)))
           (+typst-preview--connect-browser browser (typst-preview--master-static-host typst-preview--local-master))))

       (:advice typst-preview--connect-browser :override +typst-preview--connect-browser
                typst-preview-open-browser :override +typst-preview-open-browser)
       (:bind "C-c C-j" typst-preview-send-position)
       (:custom typst-preview-browser "miniwindow"
                typst-preview-invert-colors "auto"
                typst-preview-executable "tinymist"
                typst-preview-cmd-options '("--features" "html")
                typst-preview-partial-rendering t))

(provide 'lang-typst)
;;; ends here
