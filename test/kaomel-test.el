;;; kaomel-test.el --- Tests for kaomel -*- lexical-binding: t; -*-

;;; Commentary:
;; Tests for the Kaomel kaomoji picker package

;;; Code:

(require 'ert)
(require 'kaomel)
(require 'kaomel-data)
(require 'kaomel-utils)

(ert-deftest kaomel-test-retrieve-kaomojis-from-path ()
  "Test that vectorized data matches JSON loading."
  (let* ((json-path (expand-file-name "kaomel-data.json" default-directory))
         (json-data (kaomel-dev-retrieve-kaomojis-from-json json-path))
         (vectorized-data kaomel-data))
    ;; Both should be vectors
    (should (vectorp json-data))
    (should (vectorp vectorized-data))
    ;; Should have same length
    (should (= (length json-data) (length vectorized-data)))
    ;; Should have at least one entry
    (should (> (length vectorized-data) 0))
    ;; Each entry should be a hash table
    (should (hash-table-p (aref vectorized-data 0)))
    ;; First entry should have same structure
    (let ((json-first (aref json-data 0))
          (vec-first (aref vectorized-data 0)))
      (should (equal (gethash "yan" json-first) (gethash "yan" vec-first)))
      (should (vectorp (gethash "tag" json-first)))
      (should (vectorp (gethash "tag" vec-first))))))

(ert-deftest kaomel-test-tag-filtering-basic ()
  "Test basic tag filtering functionality."
  ;; Create a mock tag cluster similar to the JSON structure
  (let* ((mock-tag-cluster
          (let ((ht (make-hash-table :test 'equal)))
            (puthash "tag"
                     (vector
                      (let ((tag-ht (make-hash-table :test 'equal)))
                        (puthash "orig" "laugh" tag-ht)
                        (puthash "en" "laugh" tag-ht)
                        (puthash "it" "ridere" tag-ht)
                        tag-ht)
                      (let ((tag-ht (make-hash-table :test 'equal)))
                        (puthash "orig" "笑" tag-ht)
                        (puthash "en" "smile" tag-ht)
                        (puthash "it" "sorriso" tag-ht)
                        tag-ht))
                     ht)
            ht))
         ;; Test with default language preferences
         (kaomel-tag-langs '("orig" "en"))
         (result (kaomel--tag-cluster-to-filtered-string mock-tag-cluster)))
    ;; Should return a string
    (should (stringp result))
    ;; Should contain expected tags
    (should (string-match-p "laugh" result))
    (should (string-match-p "笑" result))
    (should (string-match-p "smile" result))))

(ert-deftest kaomel-test-tag-filtering-ascii-only ()
  "Test ASCII-only tag filtering."
  (let* ((mock-tag-cluster
          (let ((ht (make-hash-table :test 'equal)))
            (puthash "tag"
                     (vector
                      (let ((tag-ht (make-hash-table :test 'equal)))
                        (puthash "orig" "笑い" tag-ht)  ; Japanese characters
                        (puthash "en" "laugh" tag-ht)   ; ASCII
                        tag-ht))
                     ht)
            ht))
         ;; Enable ASCII-only filtering
         (kaomel-only-ascii-tags t)
         (kaomel-tag-langs '("orig" "en"))
         (result (kaomel--tag-cluster-to-filtered-string mock-tag-cluster)))
    ;; Should contain ASCII tags
    (should (string-match-p "laugh" result))
    ;; Should NOT contain Japanese characters (filtered out)
    (should-not (string-match-p "笑" result))))

(provide 'kaomel-test)
;;; kaomel-test.el ends here
