;;; setup-dsl-test.el --- tests for setup-dsl -*- lexical-binding: t; -*-

(require 'ert)
(require 'setup-dsl)

(ert-deftest setup-dsl-parse-surface ()
  (should (equal
           (setup-dsl-parse-string "(:option foo 1 :default)")
           '((:option foo 1 :default)))))

(ert-deftest setup-dsl-rewrite-option ()
  (should (equal
           (setup-dsl-rewrite '(:option foo 1))
           '(:set foo 1 :default))))

(ert-deftest setup-dsl-compile-global ()
  (should (equal
           (setup-dsl-compile '(:global "C-c x" foo))
           '(keymap-set (current-global-map) "C-c x" #'foo))))

(provide 'setup-dsl-test)
;;; setup-dsl-test.el ends here
