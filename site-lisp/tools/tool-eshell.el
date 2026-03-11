;;;   -*- lexical-binding: t; -*-

;;; Code:

;; eshell/vterm

(setup (:elpaca vterm)
  (:option vterm-shell "zsh"
           ansi-color-for-comint-mode t
           comint-prompt-read-only t
           comint-buffer-maximum-size 4096)
  (:init (defun shell-comint-input-sender-hook ()
           "Check certain shell commands.
 Executes the appropriate behavior for certain commands."
           (setq comint-input-sender
                 (lambda (proc command)
                   (cond
                    ;; Check for clear command and execute it.
                    ((string-match "^[ \t]*clear[ \t]*$" command)
                     (comint-send-string proc "\n")
                     (let ((inhibit-read-only  t))
                       (erase-buffer)))
                    ;; Check for man command and execute it.
                    ((string-match "^[ \t]*man[ \t]*" command)
                     (comint-send-string proc "\n")
                     (setq command (replace-regexp-in-string
                                    "^[ \t]*man[ \t]*" "" command))
                     (setq command (replace-regexp-in-string
                                    "[ \t]+$" "" command))
                     (funcall 'man command))
                    ;; Send other commands to the default handler.
                    (t (comint-simple-send proc command)))))))
  (add-hook 'shell-mode-hook 'shell-comint-input-sender-hook)
  (add-to-list 'comint-output-filter-functions 'ansi-color-process-output)
  (defun vterm--rename-buffer-as-title (title)
    (rename-buffer (format "vterm @ %s" title) t))
  (setq vterm-set-title-functions 'vterm-set-title-functions)
  (:hooks shell-mode-hook ansi-color-for-comint-mode-on))

(setup (:elpaca multi-vterm))

(setup (:elpaca eshell-vterm)
  (eshell-vterm-mode t))

(setup (:elpaca vterm-toggle)
  (add-to-list 'display-buffer-alist
               '((display-buffer-reuse-window display-buffer-at-bottom)
                 (display-buffer-reuse-window display-buffer-in-direction)
                 ;;display-buffer-in-direction/direction/dedicated is added in emacs27
                 (direction . bottom)
                 (dedicated . t) ;dedicated is supported in emacs27
                 (reusable-frames . visible)
                 (window-height . 0.3)))
  (:option vterm-toggle-fullscreen-p nil))

(setup eshell

  (defalias 'open 'find-file-other-window)

  (defun eshell/sudo-open (filename)
    "Open a file as root in Eshell."
    (let ((qual-filename (if (string-match "^/" filename)
                             filename
                           (concat (expand-file-name (eshell/pwd)) "/" filename))))
      (switch-to-buffer
       (find-file-noselect
        (concat "/sudo::" qual-filename)))))

  (defun eshell-other-window ()
    "Create or visit an eshell buffer."
    (interactive)
    (if (not (get-buffer "*eshell*"))
        (progn
          (split-window-sensibly (selected-window))
          (other-window 1)
          (eshell))
      (switch-to-buffer-other-window "*eshell*")))

  (defun eshell-browser-history ()
    "Insert command from eshell history at point."
    (interactive)
    (require 'em-hist)
    (let* ((start-pos (save-excursion (eshell-bol) (point)))
           (end-pos (point))
           (input (buffer-substring-no-properties start-pos end-pos))
           (history (when (> (ring-size eshell-history-ring) 0)
                      (ring-elements eshell-history-ring)))
           (command (completing-read "Command: " history nil t input)))
      (delete-region start-pos end-pos)
      (insert command)))

  (:require em-smart)
  (:option eshell-where-to-jump 'begin
           eshell-review-quick-commands nil
           eshell-smart-space-goes-to-end t)

  (:require em-term)
  (mapc (lambda (x) (add-to-list 'eshell-visual-commands x))
        '("el" "elinks" "htop" "less" "ssh" "tmux" "top"))

  (add-hook 'eshell-output-filter-functions #'eshell-truncate-buffer)

  (:hooks eshell-mode-hook completion-preview-mode)

  ;; default clear command clears the buffer
  ;; using `eshell/clear-scrollback', aka `(eshell/clear t)' by default

  (defun enhance.eshell:clear-advice (args)
    (if (null args) '(t) args))
  (advice-add 'eshell/clear :filter-args #'enhance.eshell:clear-advice)

  (defalias 'clean 'eshell/clear-scrollback)

  ;; Copy by Li-yiyang

  ;; get and list eshell buffer
  ;; inspired from https://emacs-china.org/t/eshell-util/24432/3

  (defun enhance.eshell:eshell-buffers ()
    "Return a list of eshell buffers"
    (let ((ebuff '()))
      (cl-dolist (buff (buffer-list))
        (with-current-buffer buff
          (when (derived-mode-p 'eshell-mode)
            (push buff ebuff))))
      ebuff))

  ;; default open new eshell buffer when calling `eshell'
  ;; see https://www.emacswiki.org/emacs/EshellMultipleEshellBuffers
  ;; advice function wrap around for interactive call

  (defun enhance.eshell:eshell-advice (fn &rest args)
    (if args (funcall fn args)
      ;; by default open new eshell on *different* dir
      (let* ((cwd default-directory)
             (buff (cl-find-if
                    #'(lambda (buff)
                        (string= cwd (buffer-local-value 'default-directory buff)))
                    (enhance.eshell:eshell-buffers))))
        (if buff
            (switch-to-buffer buff)
          (funcall fn '(N))))))
  (advice-add 'eshell :around #'enhance.eshell:eshell-advice)

  ;; eshls: list Eshell buffer
  (defun eshell/eshls (&rest args)
    "List all eshell buffer names.
Click to switch to eshell buffer. "
    (cl-loop for buffer in (buffer-list)
             if (eq (buffer-local-value 'major-mode buffer) 'eshell-mode)
             do (eshell-printn
                 (propertize
                  (buffer-name buffer)
                  'category 'default-button
                  'button   (buffer-name buffer)
                  'action   #'(lambda (&rest args)
                                (interactive)
                                (let ((buffer (get-text-property (point) 'button)))
                                  (message buffer)
                                  (switch-to-buffer buffer)))
                  'help-echo   (concat "Switch to eshell buffer: `"
                                       (buffer-name buffer) "'. ")
                  'follow-link t))))

  (cl-defmacro push-plist (key val plist)
    `(setq ,plist (cons ,key (cons ,val ,plist))))

  ;; imgcat: show image in eshell
  ;; see https://emacs-china.org/t/imgcat-eshell/3439
  (defun eshell/imgcat (&rest args)
    "Display `images' in eshell. "
    (if eshell-in-pipeline-p
        (error "Elisp function does not support piped input. ")
      (eshell-eval-using-options
       "imgcat" args
       '((nil "width"      t width      "width of image(s)")
         (nil "height"     t height     "height of image(s)")
         (nil "max-width"  t max-width  "max width of image(s) [default 400]")
         (nil "max-height" t max-height "max height of image(s)")
         (?h  "help"   nil nil "show this usage screen")
         :show-usage
         :usage "[OPTION] IMAGE...
Show IMAGE(s) file in eshell. ")
       (let ((property ()))
         (push-plist :max-width (string-to-number (or max-width "400")) property)
         (when max-height (push-plist :max-height (string-to-number max-height) property))
         (when width      (push-plist :width      (string-to-number width)      property))
         (when height     (push-plist :height     (string-to-number height)     property))
         (if (endp args)
             (eshell-show-usage "image" nil)
           (dolist (img (eshell-flatten-list args))
             (eshell-printn
              (propertize img 'display (apply #'create-image (expand-file-name img)
                                              nil nil property)))))))))

  (:option eshell-highlight-prompt nil
           eshell-cmpl-cycle-completions nil
           eshell-buffer-maximum-lines 20000
           eshell-destroy-buffer-when-process-dies t
           eshell-history-size 350
           eshell-hist-ignoredups t
           eshell-plain-echo-behavior t
           eshell-prompt-regexp "^[^αλ\n]*[αλ] "
           eshell-prompt-function (lambda nil
                                    (let ((prompt (concat
                                                   (if (string= (eshell/pwd) (getenv "HOME"))
                                                       (propertize "~" 'face `(:foreground "#99CCFF"))
                                                     (replace-regexp-in-string
                                                      (getenv "HOME")
                                                      (propertize "~" 'face `(:foreground "#99CCFF"))
                                                      (propertize (eshell/pwd) 'face `(:foreground "#99CCFF"))))
                                                   (if (= (user-uid) 0)
                                                       (propertize " α " 'face `(:foreground "#FF6666"))
                                                     (propertize " λ " 'face `(:foreground "#A6E22E"))))))
                                      (add-text-properties 0 (length prompt)
                                                           '(read-only t
                                                                       front-sticky (read-only)
                                                                       rear-nonsticky (read-only))
                                                           prompt)
                                      prompt))))

(setup (:elpaca pcmpl-args)
  (:require pcmpl-args))

(provide 'tool-eshell)

;; ends here.
