;;; kaomel.el --- A snappy kaomoji picker -*- lexical-binding: t; -*-
;;
;; (ɔ) Copyleft 2023 Giovanni Crisalfi (gicrisf)
;;
;; Author: Giovanni Crisalfi <giovanni.crisalfi@protonmail.com>
;; Maintainer: Giovanni Crisalfi <giovanni.crisalfi@protonmail.com>
;; Created: 2023-07-19
;; Modified: 2025-08-12
;; Version: 1.0.0
;; Keywords: convenience extensions faces tools
;; Homepage: https://github.com/gicrisf/kaomel
;; Package-Requires: ((emacs "27.1"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;; Kaomel is an interactive kaomoji picker for Emacs that provides a
;; convenient way to insert kaomojis into your
;; buffers or copy them to the clipboard.
;;
;; The package includes a curated dataset of approximately 1000 kaomojis
;; with multilingual tags in various languages including Japanese
;; (hiragana/katakana), English, and Italian, with multiple romanization
;; formats for enhanced searchability.
;;
;; Main commands:
;;   M-x kaomel-insert      - Insert selected kaomoji at point
;;   M-x kaomel-to-clipboard - Copy selected kaomoji to clipboard
;;
;; The package automatically detects and integrates with popular
;; completion frameworks:
;;   - Uses Helm interface when helm-core is available
;;   - Falls back to completing-read (works with Vertico, Ivy, etc.)
;;   - Configurable via kaomel-force-completing-read to force completing-read
;;
;; Customization options include tag language preferences, filtering
;; settings, visual separators, and display formatting to tailor the
;; picker experience to your workflow.
;;
;; Ensure your Emacs font supports Unicode characters for proper
;; kaomoji rendering.
;;
;;; Code:

(require 'kaomel-data)

(defgroup kaomel nil
  "A snappy kaomoji picker."
  :prefix "kaomel-"
  :group 'comm)


(defcustom kaomel-force-completing-read nil
  "Force use of \\='completing-read\\=' even if Helm is present.
Normally, Helm users prefer using Helm for all tasks, but a user might
have both completion systems and want to specify which one to use with
Kaomel.  The default value is NIL, which means we use Helm if it is
available and \\='completing-read\\=' if it is not."
  :group 'kaomel
  :type 'boolean)

(defvar kaomel--getter
  (if kaomel-force-completing-read
      #'kaomel--get-through-cr
    (if (require 'helm-core nil t)
        #'kaomel--get-through-helm
      #'kaomel--get-through-cr))
  "The getter function actually called.")

;;;###autoload
(defun kaomel-insert ()
  "Pick your kaomoji and insert it in the buffer."
  (interactive)
  (insert (funcall kaomel--getter)))

;;;###autoload
(defun kaomel-to-clipboard ()
  "Pick your kaomoji and store it in your clipboard."
  (interactive)
  (kill-new (funcall kaomel--getter)))

(defcustom kaomel-tag-langs '("orig" "hepburn" "en")
  "Preferred languages in showed tags;
You can choose between:
- Original (orig)
- Hiragana (hira)
- Katakana (kana)
- English (en)
- Italian (it)."
  :group 'kaomel
  :type 'list)

(defcustom kaomel-only-ascii-tags nil
  "If t, kaomel filters out every non-ascii character from tags."
  :group 'kaomel
  :type 'boolean)

(defcustom kaomel-heavy-trim-tags nil
  "If t, kaomel shows only the first word for every tag token.
Useful to make the tag strings shorter when you choose many
languages."
  :group 'kaomel
  :type 'boolean)

(defcustom kaomel-tag-val-separator " "
  "Separator that is put between the tags and the value."
  :group 'kaomel
  :type 'string)

(defcustom kaomel-tag-tag-separator " "
  "Separator that is put between a tag the tag next to it."
  :group 'kaomel
  :type 'string)

(defcustom kaomel-prompt "Pick a Kaomoji:"
  "Displayed when prompting the user to pick a Kaomoji.

Example usage:
\\(setq kaomel-prompt \\\"Select a Kaomoji:\\\"\\)

Changing the value of this variable will update the prompt line in all
relevant interactions within the `kaomel` package."
  :group 'kaomel
  :type 'string)

(defcustom kaomel-candidate-number-limit nil
  "Maximum number of candidates to display in Helm interface.
If nil, no limit is applied (show all candidates)."
  :group 'kaomel
  :type '(choice (const nil) integer))

(defun kaomel--normalize-widths (max-width units)
  "Pad UNITS'tag strings with spaces to match MAX-WIDTH."
  (mapcar
   (lambda (el)
     (let* ((tag (car el))
            (delta-width (- max-width (string-width tag)))
            (tag (if (> delta-width 0)
                     (concat
                      tag
                      (mapconcat #'identity
                                 (make-list delta-width " ") ""))
                   tag)))
       (cons tag (cdr el))))
   units))

(defun kaomel--tag-cluster-to-filtered-string (tag-cluster)
  "Build string from TAG-CLUSTER based on options."
  (mapconcat
   'identity
   ;; no duplicated elements
   (delete-dups
    ;; no nil elements
    (flatten-tree
     (mapcar
      (lambda (el-tag)
        ;; only elements from the chosen language
        (mapcar
         (lambda (lang)
           (let ((word (gethash lang el-tag)))
             ;; only non-white elements
             (when (not (string-match-p "^[[:space:]]*$" word))
               (let* ((filtered-word
                       (if kaomel-only-ascii-tags
                           ;; only ascii (filter out asian words)
                           (replace-regexp-in-string "[[:nonascii:]]" "" word)
                         word))
                      ;; no white characters around the words
                      (filtered-word (string-trim filtered-word))
                      (filtered-word
                       (if kaomel-heavy-trim-tags
                           ;; only 1st words of each translation
                           (car (split-string filtered-word))
                         filtered-word)))
                 filtered-word))))
         ;; list of languages
         kaomel-tag-langs))
      (gethash "tag" tag-cluster))))
   kaomel-tag-tag-separator))

(defun kaomel--build-general-seq ()
  "Build a generic sequence of kaomojis."
  (let ((agnostic-kaomoji-seq '())
        (max-length 0))
    (mapc (lambda (tag-cluster)
            (let ((tag (kaomel--tag-cluster-to-filtered-string tag-cluster))
                  (yan (gethash "yan" tag-cluster)))
              ;; keep in memory the max lengths reached
              (let ((this-length (length tag)))
                (when (> this-length max-length)
                  (setq max-length this-length)))
              ;; update the sequence
              (setq agnostic-kaomoji-seq
                    (append (mapcar (lambda (moji) (cons tag moji)) yan)
                            agnostic-kaomoji-seq))))
          kaomel-data)
    (kaomel--normalize-widths
     max-length agnostic-kaomoji-seq)))

(defun kaomel--get-candidates ()
  "Get candidates as alist.
\(DISPLAY . REAL\)"
  (mapcar (lambda (moji-el)
            (cons (concat (car moji-el)
                          " "
                          kaomel-tag-val-separator
                          " "
                          (cdr moji-el))
                  (cdr moji-el)))
          (kaomel--build-general-seq)))

(defun kaomel--get-through-cr ()
  "Prompt the user to select a kaomoji using \\='completing-read\\='."
  (let ((completions (kaomel--get-candidates)))
    (cdr (assoc (completing-read
                 (concat kaomel-prompt " ") completions)
                completions))))

(defun kaomel--get-through-helm ()
  "Prompt the user to select a kaomoji using Helm."
  (with-no-warnings
    ;; Warning suppressed because I perform a runtime
    ;; check on helm. This internal function should be
    ;; called only if helm exists.
    (helm :sources
          (helm-build-sync-source
           kaomel-prompt
           :candidates (kaomel--get-candidates)
           :candidate-number-limit kaomel-candidate-number-limit))))

(provide 'kaomel)
;;; kaomel.el ends here
