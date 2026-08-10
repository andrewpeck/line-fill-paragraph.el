;;; tests/test-line-fill.el --- ERT tests for line-fill  -*- lexical-binding: t; -*-

;; Copyright (C) 2023-2026 Andrew Peck

;; This file is not part of GNU Emacs.

;;; Commentary:
;; ERT tests for line-fill.el

;;; Code:

(require 'ert)
(require 'cl-lib)

(defvar lfp-test-dir (file-name-directory (or load-file-name buffer-file-name)))

(require 'line-fill)

;;; Helpers

(defmacro lfp-with-buffer (content &rest body)
  "Run BODY in a temp buffer pre-filled with CONTENT, point at start.

`sentence-end-double-space' is bound to nil so that single-spaced
prose is treated as separate sentences; see
`lfp-test-respects-sentence-end-double-space' for the other setting."
  (declare (indent 1))
  `(with-temp-buffer
     (let ((sentence-end-double-space nil))
       (insert ,content)
       (goto-char (point-min))
       ,@body)))

(defmacro lfp-with-mode-buffer (mode content &rest body)
  "Run BODY in a temp buffer in MODE, pre-filled with CONTENT."
  (declare (indent 2))
  `(with-temp-buffer
     (funcall ,mode)
     (let ((sentence-end-double-space nil))
       (insert ,content)
       (goto-char (point-min))
       ,@body)))

(defun lfp-fill (content)
  "Return CONTENT after `line-fill-paragraph' has run on it."
  (lfp-with-buffer content
    (line-fill-paragraph)
    (buffer-string)))

;;; Basic splitting

(ert-deftest lfp-test-two-sentences ()
  "Two sentences in one line become two lines."
  (should (equal (lfp-fill "Hello world. Goodbye world.")
                 "Hello world.\nGoodbye world.")))

(ert-deftest lfp-test-three-sentences ()
  "Three sentences in one line become three lines."
  (should (equal (lfp-fill "First sentence here. Second sentence here. Third sentence here.")
                 "First sentence here.\nSecond sentence here.\nThird sentence here.")))

(ert-deftest lfp-test-single-sentence ()
  "A single sentence is left unchanged."
  (should (equal (lfp-fill "Just one sentence here.")
                 "Just one sentence here.")))

(ert-deftest lfp-test-already-split ()
  "Sentences already on separate lines are left unchanged."
  (should (equal (lfp-fill "First sentence.\nSecond sentence.")
                 "First sentence.\nSecond sentence.")))

(ert-deftest lfp-test-rejoins-mid-sentence-breaks ()
  "A sentence broken across lines is rejoined before being re-split."
  (should (equal (lfp-fill "First sentence\nhere. Second sentence\nhere.")
                 "First sentence here.\nSecond sentence here.")))

(ert-deftest lfp-test-multiple-spaces-collapsed ()
  "Multiple spaces between sentences are collapsed when splitting."
  (should (equal (lfp-fill "First sentence.  Second sentence.")
                 "First sentence.\nSecond sentence.")))

(ert-deftest lfp-test-question-mark-sentence ()
  "Sentences ending with question marks are split correctly."
  (should (equal (lfp-fill "Is this working? Yes it is.")
                 "Is this working?\nYes it is.")))

(ert-deftest lfp-test-exclamation-mark-sentence ()
  "Sentences ending with exclamation marks are split correctly."
  (should (equal (lfp-fill "Wow it works! Great news.")
                 "Wow it works!\nGreat news.")))

(ert-deftest lfp-test-idempotent ()
  "Filling twice gives the same result as filling once."
  (let ((once (lfp-fill "First one here. Second one here. Third one here.")))
    (should (equal (lfp-fill once) once))))

;;; Abbreviation handling

(ert-deftest lfp-test-no-break-after-abbreviation ()
  "Sentences containing e.g. are not split at the abbreviation."
  (should (equal (lfp-fill "Rodents are mammals. All rodents have continuously growing incisors. Many species are kept as pets, e.g. guinea pigs and hamsters, which are popular due to their docile nature (and we e.g. often see them in schools and homes).")
                 "Rodents are mammals.\nAll rodents have continuously growing incisors.\nMany species are kept as pets, e.g. guinea pigs and hamsters, which are popular due to their docile nature (and we e.g. often see them in schools and homes).")))

(ert-deftest lfp-test-abbreviation-does-not-swallow-next-break ()
  "A skipped abbreviation must not consume the following sentence break.

Regression: the scan used to advance an extra sentence after an
abbreviation, so the next real boundary was never considered."
  (should (equal (lfp-fill "Foo bar e.g. baz. Next sentence here. Third one here.")
                 "Foo bar e.g. baz.\nNext sentence here.\nThird one here.")))

(ert-deftest lfp-test-abbreviation-at-line-start ()
  "An abbreviation with no preceding whitespace is still recognized.

Regression: the previous token search looked backwards for a literal
space, which fails at the beginning of a line."
  (should (equal (lfp-fill "e.g. this thing here. Second one. Third one.")
                 "e.g. this thing here.\nSecond one.\nThird one.")))

(ert-deftest lfp-test-multi-initial-abbreviation ()
  "Abbreviations made of several initials, such as U.S., do not break."
  (should (equal (lfp-fill "The U.S. is large. It has states. Many of them.")
                 "The U.S. is large.\nIt has states.\nMany of them.")))

(ert-deftest lfp-test-acronym-is-not-an-abbreviation ()
  "A trailing initial inside an acronym does not suppress the break.

\"NASA.\" ends in \"A.\" but that follows a letter, so it really does
end the sentence."
  (should (equal (lfp-fill "Visit NASA. Then visit NATO. Now go home.")
                 "Visit NASA.\nThen visit NATO.\nNow go home."))
  (should (equal (lfp-fill "The lab is at MIT. The other is at CERN. We go there.")
                 "The lab is at MIT.\nThe other is at CERN.\nWe go there.")))

(ert-deftest lfp-test-titles-do-not-break ()
  "Common titles such as Mr. and Dr. do not end a sentence."
  (should (equal (lfp-fill "Mr. Smith arrived here. He was late. Very late.")
                 "Mr. Smith arrived here.\nHe was late.\nVery late."))
  (should (equal (lfp-fill "Dr. Jones spoke here. We ate apples, etc. and left. All done.")
                 "Dr. Jones spoke here.\nWe ate apples, etc. and left.\nAll done.")))

(ert-deftest lfp-test-single-letter-initials-do-not-break ()
  "Single letter initials keep the following text on the same line."
  (should (equal (lfp-fill "The variable is X. The next one is Y. Done here.")
                 "The variable is X. The next one is Y. Done here.")))

(ert-deftest lfp-test-non-separators-is-customizable ()
  "Adding an entry to `line-fill-non-separators' suppresses that break."
  (should (equal (lfp-fill "See Sec. 4 for details. Then read on.")
                 "See Sec.\n4 for details.\nThen read on."))
  (let ((line-fill-non-separators (cons "Sec." line-fill-non-separators)))
    (should (equal (lfp-fill "See Sec. 4 for details. Then read on.")
                   "See Sec. 4 for details.\nThen read on."))))

;;; Punctuation closed by quotes or parens

(ert-deftest lfp-test-no-break-after-paren-exclamation ()
  "Sentences are not split after ! or ? inside parentheses."
  (should (equal (lfp-fill "Beavers are the largest rodents in North America. Their dams can raise water levels by a surprising amount (or more!) than most people expect.")
                 "Beavers are the largest rodents in North America.\nTheir dams can raise water levels by a surprising amount (or more!) than most people expect.")))

(ert-deftest lfp-test-quoted-exclamation-does-not-swallow-next-break ()
  "A quoted exclamation is skipped without eating the following break."
  (should (equal (lfp-fill "He said \"Stop!\" Then he left. All was quiet.")
                 "He said \"Stop!\" Then he left.\nAll was quiet.")))

(ert-deftest lfp-test-curly-quoted-exclamation ()
  "Typographic closing quotes are treated like ASCII ones."
  (should (equal (lfp-fill "She said “Stop!” Then left. All quiet.")
                 "She said “Stop!” Then left.\nAll quiet.")))

;;; Paragraphs that must be left alone

(ert-deftest lfp-test-no-sentence-punctuation-untouched ()
  "A paragraph with no sentence punctuation is never joined."
  (should (equal (lfp-fill "just a bunch of words\nwith no punctuation\nat all here")
                 "just a bunch of words\nwith no punctuation\nat all here")))

(ert-deftest lfp-test-all-boundaries-abbreviations-untouched ()
  "A paragraph is not joined when no real break would be inserted.

Filling is destructive, so it must not happen unless at least one line
break is actually produced."
  (should (equal (lfp-fill "Some text i.e.\nmore text.")
                 "Some text i.e.\nmore text.")))

(ert-deftest lfp-test-latex-preamble-untouched ()
  "Markup-only lines are not treated as prose and joined together."
  (should (equal (lfp-fill "\\title{Foo Bar}\n\\author{A. Researcher}\n\\date{\\today}\n")
                 "\\title{Foo Bar}\n\\author{A. Researcher}\n\\date{\\today}\n")))

(ert-deftest lfp-test-respects-sentence-end-double-space ()
  "Sentence boundaries follow `sentence-end-double-space'.

With Emacs' default value of t, single-spaced prose is not considered
to contain sentence breaks and is deliberately left alone."
  (with-temp-buffer
    (let ((sentence-end-double-space t))
      (insert "One here. Two here. Three here.")
      (goto-char (point-min))
      (line-fill-paragraph)
      (should (equal (buffer-string) "One here. Two here. Three here."))))
  (with-temp-buffer
    (let ((sentence-end-double-space t))
      (insert "One here.  Two here.  Three here.")
      (goto-char (point-min))
      (line-fill-paragraph)
      (should (equal (buffer-string) "One here.\nTwo here.\nThree here.")))))

;;; Prefixes and indentation

(ert-deftest lfp-test-comment-prefix-preserved ()
  "Splitting a comment keeps the comment prefix on every line."
  (lfp-with-mode-buffer #'emacs-lisp-mode ";; First sentence here. Second sentence here. Third one.\n"
    (line-fill-paragraph)
    (should (equal (buffer-string)
                   ";; First sentence here.\n;; Second sentence here.\n;; Third one.\n"))))

(ert-deftest lfp-test-indentation-preserved ()
  "An indented paragraph keeps its indentation on every line."
  (should (equal (lfp-fill "    Indented one. Indented two. Indented three.")
                 "    Indented one.\n    Indented two.\n    Indented three.")))

;;; Interface

(ert-deftest lfp-test-prefix-arg-calls-fill-paragraph ()
  "When called with a prefix argument, `fill-paragraph' is invoked instead."
  (lfp-with-buffer "First sentence. Second sentence."
    (let ((fill-column 20)
          (fill-paragraph-called nil))
      (cl-letf (((symbol-function 'fill-paragraph)
                 (lambda (&rest _args) (setq fill-paragraph-called t))))
        (line-fill-paragraph '(4))
        (should fill-paragraph-called)))))

(ert-deftest lfp-test-point-position-preserved ()
  "The function uses `save-excursion', so point is not moved."
  (lfp-with-buffer "First sentence. Second sentence."
    (line-fill-paragraph)
    (should (= (point) (point-min)))))

(ert-deftest lfp-test-point-preserved-mid-paragraph ()
  "Point is preserved when it starts inside the paragraph."
  (lfp-with-buffer "One here. Two here. Three here."
    (goto-char 15)
    (line-fill-paragraph)
    (should (= (point) 15))))

;;; line-fill-buffer

(ert-deftest lfp-test-buffer-fills-every-paragraph ()
  "`line-fill-buffer' splits each paragraph and keeps them separated."
  (lfp-with-buffer "One here. Two here.\n\nThree here. Four here.\n\nFive here. Six here.\n"
    (line-fill-buffer)
    (should (equal (buffer-string)
                   "One here.\nTwo here.\n\nThree here.\nFour here.\n\nFive here.\nSix here.\n"))))

(ert-deftest lfp-test-buffer-without-trailing-newline ()
  "`line-fill-buffer' terminates and fills a final paragraph at eob."
  (lfp-with-buffer "One here. Two here.\n\nThree here. Four here."
    (line-fill-buffer)
    (should (equal (buffer-string)
                   "One here.\nTwo here.\n\nThree here.\nFour here."))))

(ert-deftest lfp-test-buffer-unchanged ()
  "Running `line-fill-buffer' on line-fill-buffer.tex leaves it unchanged.

The fixture is already one sentence per line, so this pins down that
`line-fill-buffer' does not disturb LaTeX markup such as preambles and
tabular environments."
  (let* ((file (expand-file-name "line-fill-buffer.tex" lfp-test-dir))
         (original (with-temp-buffer
                     (insert-file-contents file)
                     (buffer-string))))
    (dolist (mode (list #'latex-mode #'fundamental-mode #'text-mode))
      (lfp-with-mode-buffer mode original
        (line-fill-buffer)
        (should (equal (buffer-substring-no-properties (point-min) (point-max))
                       original))))))

(provide 'test-line-fill)
;;; tests/test-line-fill.el ends here
