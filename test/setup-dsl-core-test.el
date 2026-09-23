;;; setup-dsl-core-test.el --- minimal primitive core tests -*- lexical-binding: t; -*-

(require 'ert)
(require 'setup-dsl-backend)

(ert-deftest setup-core-rejects-sugar ()
  (should-error (setup-dsl-compile '(:option answer 1))
                :type 'setup-dsl-rewrite-error))

(ert-deftest setup-core-checks-semantic-symbols ()
  (should (equal (setup-dsl-compile '(:require foo))
                 '(require 'foo nil t)))
  (should-error (setup-dsl-compile '(:require "foo"))
                :type 'setup-value-error))

(ert-deftest setup-core-checks-code-and-key ()
  (should (equal (setup-dsl-compile '(:bind (current-global-map) "C-c x" foo))
                 '(if (vectorp "C-c x")
                      (define-key (current-global-map) "C-c x" foo)
                    (keymap-set (current-global-map) "C-c x" foo))))
  (should-error (setup-dsl-compile '(:bind global-map foo command))
                :type 'setup-value-error))

(ert-deftest setup-core-extension-is-explicit ()
  (setup-dsl-register-extension
   :option (lambda (form) `(:set ,(nth 1 form) ,(nth 2 form) :default)))
  (unwind-protect
      (should (equal (setup-dsl-compile '(:option answer 1))
                     '(set-default-toplevel-value 'answer 1)))
    (setf (alist-get :option setup-dsl-extensions) nil)))

(provide 'setup-dsl-core-test)
;;; setup-dsl-core-test.el ends here
