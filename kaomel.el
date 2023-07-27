;;; kaomel.el --- A snappy kaomoji picker for Emacs -*- lexical-binding: t; -*-
;;
;; (ɔ) Copyleft 2023 Giovanni Crisalfi (gicrisf)
;;
;; Author: Giovanni Crisalfi <giovanni.crisalfi@protonmail.com>
;; Maintainer: Giovanni Crisalfi <giovanni.crisalfi@protonmail.com>
;; Created: luglio 19, 2023
;; Modified: luglio 27, 2023
;; Version: 0.0.3
;; Keywords: convenience extensions faces tools
;; Homepage: https://github.com/gicrisf/kaomel
;; Package-Requires: ((emacs "27.1"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  Description
;;
;;; Code:

;; (require 'helm-core)
(require 'json)

(defgroup kaomel nil
  "A snappy kaomoji picker for Emacs."
  :prefix "kaomel-"
  :group 'comm)

(defcustom kaomel-path (expand-file-name
                        "kaomoji.json"
                        (file-truename
                         (if load-file-name
                             (file-name-directory load-file-name)
                           default-directory)))
  "Variable to store the source of kaomojis."
  :group 'kaomel
  :type 'string)

(defcustom kaomel-helm nil
  "To use or not to use helm as completion system.
Default is NIL, which means we use completion-read instead;
completion-read can be coupled with Vertico/Selectrum or similar packages."
  :group 'kaomel
  :type 'boolean)

(defcustom kaomel-separator "=>"
  "To use or not to use helm as completion system.
Default is NIL, which means we use completion-read instead;
completion-read can be coupled with Vertico/Selectrum or similar packages."
  :group 'kaomel
  :type 'string)

(defun kaomel--retrieve-kaomojis-from-path (path)
  "Retrieve kaomojis stored as json in the specified PATH.
Then, it returns them as a parsed object."
  (let ((parsed (with-temp-buffer
                  (set-buffer-multibyte t)
                  (insert-file-contents path)
                  (goto-char (point-min))
                  (let ((json-object-type 'hash-table))
                    (json-read)))))
    parsed))

(defun kaomel--get ()
  "Prompt the user to select a kaomoji using 'completing-read'."
  (let ((completions (kaomel--get-candidates)))
    (cdr (assoc (completing-read "Pick a kaomoji: " completions)
                 completions))))

(defun kaomel--get-candidates ()
  "Get candidates as alist.
\(DISPLAY . REAL\)"
  (mapcar (lambda (moji-el)
            (cons (concat (car moji-el)
                          " "
                          kaomel-separator
                          " "
                          (cdr moji-el))
                  (cdr moji-el)))
          (kaomel--build-general-seq)))

(defun kaomel-insert ()
  "Pick your kaomoji and insert it in the buffer."
  (interactive)
  (insert (if kaomel-helm (kaomel--get-through-helm)
            (kaomel--get))))

(defun kaomel--get-through-helm ()
  "Prompt the user to select a kaomoji using Helm."
  (helm :sources
        (helm-build-sync-source "Pick a kaomoji:"
          :candidates (kaomel--get-candidates)
          :candidate-number-limit 1000)))

(defun kaomel-to-clipboard ()
  "Pick your kaomoji and store it in your clipboard."
  (interactive)
  (kill-new (if kaomel-helm (kaomel--get-through-helm)
            (kaomel--get))))

(defcustom kaomel-tag-langs '("en" "hepburn")
  "Preferred languages in showed tags;
You can choose between:
- Original (orig)
- Hiragana (hira)
- Katakana (kana)
- English (en)
- Italian (it)."
  :group 'kaomel
  :type 'list)

(defun kaomel--build-general-seq ()
  "Build a generic sequence of kaomojis."
  (let ((agnostic-kaomoji-seq '())
        (parsed-json-vec (kaomel--retrieve-kaomojis-from-path kaomel-path)))
    (mapc (lambda (kaomoji-tagged-cluster)
            (let ((tag (mapconcat
                        'identity
                        (delete-dups
                         (flatten-tree
                          (mapcar
                           (lambda (el-tag)
                             (mapcar
                              (lambda (lang)
                                (let ((word (gethash lang el-tag)))
                                  (when (not (string-match-p "^[[:space:]]*$" word))
                                    (car (split-string
                                          (string-trim
                                           (replace-regexp-in-string "[[:nonascii:]]" "" word)))))))
                              kaomel-tag-langs))
                           (gethash "tag" kaomoji-tagged-cluster))))
                        " "))
                  (yan (gethash "yan" kaomoji-tagged-cluster)))
              (setq agnostic-kaomoji-seq
                    (append (mapcar (lambda (moji) (cons tag moji)) yan)
                            agnostic-kaomoji-seq))))
          parsed-json-vec)
    agnostic-kaomoji-seq))

(provide 'kaomel)
;;; kaomel.el ends here
