;;; kaomel-test.el --- Tests for kaomel -*- lexical-binding: t; -*-

;;; Commentary:
;; Tests for the Kaomel kaomoji picker package

;;; Code:

(require 'ert)
(require 'kaomel)

(ert-deftest kaomel-test-retrieve-kaomojis-from-path ()
  "Test that kaomel--retrieve-kaomojis-from-path correctly parses JSON."
  (let ((parsed-data (kaomel--retrieve-kaomojis-from-path kaomel-path)))
    ;; Should return a vector (JSON array becomes vector)
    (should (vectorp parsed-data))
    ;; Should have at least one entry
    (should (> (length parsed-data) 0))
    ;; Each entry should be a hash table
    (should (hash-table-p (aref parsed-data 0)))))

(provide 'kaomel-test)
;;; kaomel-test.el ends here