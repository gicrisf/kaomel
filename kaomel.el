;;; kaomel.el --- A snappy kaomoji picker for Emacs -*- lexical-binding: t; -*-
;;
;; (ɔ) Copyleft 2023 Giovanni Crisalfi (gicrisf)
;;
;; Author: Giovanni Crisalfi <giovanni.crisalfi@protonmail.com>
;; Maintainer: Giovanni Crisalfi <giovanni.crisalfi@protonmail.com>
;; Created: luglio 19, 2023
;; Modified: luglio 19, 2023
;; Version: 0.0.1
;; Keywords: convenience extensions faces tools
;; Homepage: https://github.com/gicrisf/kaomel
;; Package-Requires: ((emacs "24.3"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  Description
;;
;;; Code:

(require 'helm-core)
(require 'json)

(defvar kaomel-path (let ((current-file (symbol-file 'kaomel--build-general-seq)))
                      (expand-file-name "kaomoji.json" (file-name-directory current-file)))
  "Variable to store the source of kaomojis.")

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

(defun kaomel--build-general-seq ()
  "Build a generic sequence of kaomojis."
(let ((agnostic-kaomoji-seq '())
      (parsed-json-vec (kaomel--retrieve-kaomojis-from-path kaomel-path)))
  (mapc (lambda (kaomoji-tagged-cluster)
          (let ((tag (gethash "tag" kaomoji-tagged-cluster))
                (yan (gethash "yan" kaomoji-tagged-cluster)))
            (setq agnostic-kaomoji-seq
                  (append (mapcar (lambda (moji) (cons tag moji)) yan)
                          agnostic-kaomoji-seq))))
        parsed-json-vec)
  agnostic-kaomoji-seq))

(defun kaomel--get-through-helm ()
  "Prompt the user to select a kaomoji using Helm"
  (helm :sources
        (helm-build-sync-source "Pick your kaomoji"
          :candidates
          ;; (DISPLAY . REAL)
          (mapcar (lambda (moji-el)
                    (cons (concat (car moji-el) " => " (cdr moji-el)) (cdr moji-el)))
                  (kaomel--build-general-seq))
          :candidate-number-limit 1000)))

(defun kaomel-insert ()
  "Pick your kaomoji!"
  (interactive)
  (insert (kaomel--get-through-helm)))

(defun kaomel-to-clipboard ()
  "Pick your kaomoji and store it in your clipboard!"
  (interactive)
  (kill-new (kaomel--get-through-helm)))

(provide 'kaomel)
;;; kaomel.el ends here
