;;; setup-peg-test.el --- tests for PEG infrastructure -*- lexical-binding: t; -*-

(require 'ert)
(require 'setup-peg)

(ert-deftest setup-peg-reads-data-without-evaluation ()
  (should (equal (setup-peg-read-string "; comment\n(:option answer 42)")
                 '((:option answer 42)))))

(ert-deftest setup-peg-rejects-unterminated-string ()
  (should-error (setup-peg-read-string "(:eval \"unterminated)"
                                       :type 'setup-peg-error)))

(provide 'setup-peg-test)
;;; setup-peg-test.el ends here
