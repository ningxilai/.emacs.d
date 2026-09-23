;;; setup-value-test.el --- explicit setup value tests -*- lexical-binding: t; -*-

(require 'ert)
(require 'setup-value)
(require 'setup-dsl-rewrite)

(ert-deftest setup-value-symbol-is-explicit ()
  (should (eq (setup-value-symbol 'feature 'foo) 'foo))
  (should-error (setup-value-symbol 'feature "foo")
                :type 'setup-value-error))

(ert-deftest setup-value-key-allows-text ()
  (should (equal (setup-value-key 'key "C-c x") "C-c x"))
  (should-error (setup-value-key 'key 'x)
                :type 'setup-value-error))

(ert-deftest setup-dsl-option-requires-symbol-variable ()
  (should (equal (setup-dsl-rewrite '(:option answer 42))
                 '(:set answer 42 :default)))
  (should-error (setup-dsl-rewrite '(:option "answer" 42))
                :type 'setup-value-error))

(ert-deftest setup-dsl-after-requires-symbol-feature ()
  (should-error (setup-dsl-rewrite '(:after "foo" (:eval t)))
                :type 'setup-value-error))

(provide 'setup-value-test)
;;; setup-value-test.el ends here
