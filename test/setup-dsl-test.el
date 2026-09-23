;;; setup-dsl-test.el --- tests for the setup DSL -*- lexical-binding: t; -*-

(require 'ert)
(require 'setup-dsl)

(ert-deftest setup-dsl-option-rewrite ()
  (should (equal (setup-dsl-rewrite '(:option answer 42))
                 '(:set answer 42 :default))))

(ert-deftest setup-dsl-option-elisp ()
  (should (equal (setup-dsl-compile '(:option answer 42))
                 '(set-default-toplevel-value 'answer 42))))

(ert-deftest setup-dsl-option-setup ()
  (should (equal (setup-dsl-compile '(:option answer 42) :setup)
                 '(:option* answer 42))))

(ert-deftest setup-dsl-hooks-require-pairs ()
  (should-error (setup-dsl-rewrite '(:hooks hook function hook-only))))

(ert-deftest setup-dsl-opaque-eval ()
  (should (equal (setup-dsl-rewrite '(:eval (:option not-a-dsl-term)))
                 '(:eval (:option not-a-dsl-term)))))

(provide 'setup-dsl-test)
;;; setup-dsl-test.el ends here
