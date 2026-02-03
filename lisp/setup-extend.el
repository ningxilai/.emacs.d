;;; setup-extend.el -*- lexical-binding: t -*-

(require 'setup)
(require 'cl-lib)

;; Exactly the same definition as a snippet available at
;; https://www.emacswiki.org/emacs/SetupEl#h5o-4 but renamed
(defmacro define-setup-macro (name signature &rest body)
  "Shorthand for `setup-define'.
  NAME is the name of the local macro.  SIGNATURE is used as the
  argument list for FN.  If BODY starts with a string, use this as
  the value for :documentation.  Any following keywords are passed
  as OPTS to `setup-define'."
  (declare (debug defun))
  (let (opts)
    (when (stringp (car body))
      (setq opts (nconc (list :documentation (pop body))
			opts)))
    (while (keywordp (car body))
      (let* ((prop (pop body))
	     (val `',(pop body)))
	(setq opts (nconc (list prop val) opts))))
    `(setup-define ,name
       (cl-function (lambda ,signature ,@body))
       ,@opts)))

(define-setup-macro
 :silence (&rest body)
 "Evaluate BODY but keep the echo era clean."
 :debug (setup)
 (cl-letf (((symbol-function 'message) #'format))
   ,(macroexp-progn body)))

(define-setup-macro
 :delay (time &rest body)
 "Delay loading BODY until a certain amount of idle time has passed."
 :indent 1
 `(run-with-idle-timer ,time nil (lambda nil ,@body)))

;; (define-setup-macro
;;  :global* (key command)
;;  "Use `keymap-global-set' to define global keybindings."
;;  :repeatable t
;;  `(keymap-global-set ,key #',command))
;;
;; (define-setup-macro
;;  :local (key command)
;;  "Use `keymap-set' to define keybindings."
;;  :after-loaded t
;;  :repeatable t
;;  `(keymap-set ,(setup-get 'map) ,key #',command))

(provide 'setup-extend)
;;; setup-extend.el ends here
