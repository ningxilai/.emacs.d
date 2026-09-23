;;; setup-dsl-test.el --- tests for the setup DSL -*- lexical-binding: t; -*-

(require 'ert)
(require 'setup-dsl)

(ert-deftest setup-dsl-surface-read ()
  (should (equal (setup-dsl-read-string "; comment\n(:option answer 42)")
                 '((:option answer 42)))))

(ert-deftest setup-dsl-option-rewrite ()
  (should (equal (setup-dsl-rewrite '(:option answer 42))
                 '(:set answer 42 :default))))

(ert-deftest setup-dsl-elisp-backend ()
  (should (equal (setup-dsl-compile '(:option answer 42))
                 '(set-default-toplevel-value 'answer 42))))

(ert-deftest setup-dsl-setup-backend ()
  (should (equal (setup-dsl-compile '(:option answer 42) :setup)
                 '(:option* answer 42))))

(ert-deftest setup-dsl-global-backend ()
  (should (equal (setup-dsl-compile '(:global "C-c x" foo))
                 '(if (vectorp "C-c x")
                      (define-key (current-global-map) "C-c x" foo)
                    (keymap-set (current-global-map) "C-c x" foo)))))

(provide 'setup-dsl-test)
;;; setup-dsl-test.el ends here
