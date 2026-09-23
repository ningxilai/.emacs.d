;;; setup-dsl-rewrite-test.el --- typed primitive boundary tests -*- lexical-binding: t; -*-

(require 'ert)
(require 'setup-dsl-rewrite)

(ert-deftest setup-dsl-symbol-and-text-classes ()
  (should (equal (setup-dsl-rewrite '(:require foo)) '(:require foo)))
  (should-error (setup-dsl-rewrite '(:require "foo")) :type 'setup-value-error)
  (should-error (setup-dsl-rewrite '(:mode "foo-mode" "\\.foo\\'") )
                :type 'setup-value-error))

(ert-deftest setup-dsl-code-and-opaque-classes ()
  (should (equal (setup-dsl-rewrite '(:bind map "C-c x" foo))
                 '(:bind map "C-c x" foo)))
  (should (equal (setup-dsl-rewrite '(:eval (:require "not-a-feature")))
                 '(:eval (:require "not-a-feature")))))

(ert-deftest setup-dsl-typed-primitives-are-registered ()
  (dolist (name '(:seq :set :bind :hook :require :autoload :load-after
                  :mode :interpreter :eval :raw))
    (should (setup-dsl-primitive name))))

(provide 'setup-dsl-rewrite-test)
;;; setup-dsl-rewrite-test.el ends here
