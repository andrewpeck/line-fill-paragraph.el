;;; line-fill.el --- Functions for semantic line fill -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2023-2026 Andrew Peck

;; Author: Andrew Peck <peckandrew@gmail.com>
;; URL: https://github.com/andrewpeck/line-fill.el
;; Version: 0.0.1
;; Package-Requires: ((emacs "29.1"))
;; Keywords: text writing

;; This file is not part of GNU Emacs.
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>

;;; Commentary:
;;
;; Provides `line-fill-paragraph' and `line-fill-buffer', which reformat prose
;; so that each sentence occupies its own line.  Useful e.g. in LaTeX documents
;; or other version controlled prose, since a change to one sentence produces a
;; minimal diff rather than reflowing an entire paragraph.
;;
;; Note that sentence boundaries are found with `forward-sentence', so the
;; value of `sentence-end-double-space' decides what counts as a sentence.
;; With its default value of t, single-spaced prose is deliberately left alone.
;;
;;; Code:

(defgroup line-fill nil
  "Fill paragraphs with one sentence per line."
  :group 'fill
  :prefix "line-fill-")

(defcustom line-fill-non-separators
  (append
   (list "n.b." "i.e." "e.g." "c.f." "viz." "eg." "ie."
         "Mr." "Mrs." "Ms." "Dr." "Prof." "vs." "etc." "cf." "al."
         "St." "No." "Vol." "Fig." "Eq." "Jr." "Sr." "Inc." "Ltd.")
   ;; single letter initials such as A. B. C.
   (mapcar (lambda (x) (concat (upcase (char-to-string x)) ".")) (number-sequence ?a ?z)))
  "Words ending in a period that do not end a sentence.

A line break is never inserted after one of these, so abbreviations
such as \"e.g.\" and initials such as \"A.\" keep the text that
follows them on the same line."
  :type '(repeat string)
  :group 'line-fill)

(defconst line-fill--closing-punct-regexp
  "[!?][\"')\u201d\u2019\u00bb]+\\'"
  "Regexp matching sentence punctuation closed by a quote or paren.

Used to avoid breaking after constructs like \"(or more!)\" or
\"\\\"Stop!\\\"\", which `forward-sentence' treats as sentence ends.")

(defsubst line-fill--abbrev-regexp ()
  "Generate a regular expression matching a token that ends in an abbreviation.

The abbreviation must finish the token and be preceded either by the
start of the token or by a non-alphanumeric character.  This matches
\"e.g.\", \"U.S.\" and \"\\author{A.\" while leaving acronyms such as
\"NASA.\" alone, since there the final \"A.\" follows a letter."
  (concat "\\(?:\\`\\|[^[:alnum:]]\\)\\(?:"
          (string-join
           (mapcar #'regexp-quote line-fill-non-separators)
           "\\|")
          "\\)\\'"))

(defsubst line-fill--token-at-point ()
  "Return the whitespace-delimited token ending at point."
  (buffer-substring-no-properties
   (save-excursion (skip-chars-backward "^ \t\n") (point))
   (point)))

(defun line-fill--sentence-limit ()
  "Return a marker at the start of the current paragraph's last sentence.

Sentence breaks are only inserted before this position, so that no
break is added after the paragraph's final sentence."
  (save-excursion
    (forward-paragraph 1)
    (backward-sentence)
    (point-marker)))

(defun line-fill--scan-breaks (limit fn)
  "Walk sentences forward from point to LIMIT, calling FN at each line break.

FN is called with point at a position where a line break belongs; it
may modify the buffer.  When FN is nil nothing is modified, which makes
this a dry run.  Return the number of break positions found.

Sentence ends whose preceding token is in `line-fill-non-separators',
or which match `line-fill--closing-punct-regexp', are not counted."
  (let ((abbrev-regexp (line-fill--abbrev-regexp))
        (count 0)
        (prev -1))
    (catch 'line-fill-done
      (while t
        ;; advance one sentence; exit cleanly at end of buffer
        (condition-case nil
            (forward-sentence)
          (end-of-buffer (throw 'line-fill-done nil)))
        ;; stop past the final sentence, and defensively if `forward-sentence'
        ;; ever stops making progress
        (when (or (> (point) (marker-position limit))
                  (<= (point) prev))
          (throw 'line-fill-done nil))
        (let ((token (line-fill--token-at-point)))
          (unless (or (string-match abbrev-regexp token)
                      (string-match line-fill--closing-punct-regexp token))
            (setq count (1+ count))
            (when fn (funcall fn))))
        (setq prev (point))))
    count))

(defun line-fill--break ()
  "Replace the spaces before point with a line break.
Uses `default-indent-new-line' so that a comment prefix or
`fill-prefix' is carried onto the new line."
  (just-one-space)              ;; leaves only one space, point is after it
  (delete-char -1)              ;; delete the space
  (default-indent-new-line))    ;; and break the line, keeping any prefix

;;;###autoload
(defun line-fill-buffer (&optional P)
  "Fill every paragraph in the buffer with one sentence per line.

When called with prefix argument P, call `fill-paragraph' on each paragraph."
  (interactive "P")
  (save-excursion
    (goto-char (point-min))
    (while (not (eobp))
      (line-fill-paragraph P)
      (forward-paragraph 1))))

;;;###autoload
(defun line-fill-paragraph (&optional P)
  "Fill paragraph with one sentence per line.

When called with prefix argument P call `fill-paragraph'.
Otherwise split the current paragraph into one sentence per line."
  (interactive "P")
  ;; ordinary fill paragraph when prefix arg is set
  (if P (fill-paragraph P)
    (save-excursion
      ;; `fill-paragraph' is used to normalize the paragraph onto a single line
      ;; before it is re-split, so that sentences already spread over several
      ;; lines are rejoined first.  This relies on dynamic binding.
      (let* ((fill-column most-positive-fixnum)
             (para-text (save-excursion
                          (let ((end (progn (forward-paragraph 1) (point))))
                            (backward-paragraph 1)
                            (buffer-substring-no-properties (point) end)))))
        ;; Joining the paragraph is destructive: if nothing is split back out
        ;; the original line structure is lost for good.  So two guards run
        ;; before `fill-paragraph' is allowed to touch anything.
        ;;
        ;; First, the paragraph has to look like prose at all.  This keeps
        ;; `line-fill-buffer' away from things like LaTeX preambles and tabular
        ;; environments, where a period is preceded by markup rather than text.
        (when (string-match "[[:alnum:])\"'][.!?]\\s-" para-text)
          ;; Second, dry-run the scan and require at least one real break.  A
          ;; paragraph whose every sentence end is an abbreviation would
          ;; otherwise be joined and never split again.  Filling only rewrites
          ;; whitespace, so the sentences seen here are the ones seen below.
          (when (> (save-excursion
                     (let ((limit (line-fill--sentence-limit)))
                       (forward-paragraph 1)
                       (backward-paragraph 1)
                       (line-fill--scan-breaks limit nil)))
                   0)
            (fill-paragraph)
            (let ((limit (line-fill--sentence-limit)))
              (beginning-of-line)
              (line-fill--scan-breaks limit #'line-fill--break))))))))

(provide 'line-fill)
;;; line-fill.el ends here
