;;; test-gptel-agent-trace.el --- Tests for gptel-agent-trace  -*- lexical-binding: t; -*-

;; Copyright (C) 2025  Free Software Foundation, Inc.

;; This file is part of GNU Emacs.

;; GNU Emacs is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Tests for the pure functions in gptel-agent-trace.el.
;;
;; These tests do NOT load gptel or gptel-agent — only the trace module
;; itself, with external variables stubbed as needed.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'org)

;; Stub external variables that gptel-agent-trace.el declares with `defvar'
(defvar gptel-org-agent-tool-confirm-keywords '("PENDING" "ALLOWED" "DENIED"))

;; Now load the module under test
(require 'gptel-agent-trace)


;;; ---------------------------------------------------------------
;;; Helpers
;;; ---------------------------------------------------------------

(defun test-trace--count-matches (regexp string)
  "Count occurrences of REGEXP in STRING."
  (let ((count 0)
        (start 0))
    (while (string-match regexp string start)
      (cl-incf count)
      (setq start (match-end 0)))
    count))

(defun test-trace--make-org-buffer (content)
  "Create an org-mode buffer with CONTENT.
If CONTENT contains #+TODO: lines, they will be picked up by
`org-set-regexps-and-options'.  Returns the buffer; caller must kill it."
  (let ((buf (generate-new-buffer " *test-trace*")))
    (with-current-buffer buf
      (org-mode)
      (insert content)
      (org-set-regexps-and-options)
      (goto-char (point-min)))
    buf))


;;; ---------------------------------------------------------------
;;; 1. gptel-agent-trace--summarize-tool-block
;;; ---------------------------------------------------------------

(ert-deftest gptel-agent-trace-test-summarize-bash ()
  "S-expression format: (Bash :command \"ls -la\") → \"Bash: ls -la\"."
  (should (equal (gptel-agent-trace--summarize-tool-block
                  "(Bash :command \"ls -la\")")
                 "Bash: ls -la")))

(ert-deftest gptel-agent-trace-test-summarize-agent ()
  "S-expression Agent block extracts subagent_type value."
  (should (equal (gptel-agent-trace--summarize-tool-block
                  "(Agent :subagent_type \"researcher\" :description \"Study hooks\")")
                 "Agent: researcher")))

(ert-deftest gptel-agent-trace-test-summarize-truncation ()
  "Long argument values are truncated to 60 chars (57 + \"...\")."
  (let* ((long-arg (make-string 80 ?x))
         (block (format "(Bash :command \"%s\")" long-arg))
         (summary (gptel-agent-trace--summarize-tool-block block)))
    (should (string-prefix-p "Bash: " summary))
    ;; "Bash: " is 6 chars, arg part should be 60 chars (57 + "...")
    (should (<= (length summary) (+ 6 60)))
    (should (string-suffix-p "..." summary))))

(ert-deftest gptel-agent-trace-test-summarize-no-args ()
  "S-expression with no quoted argument returns just the tool name."
  (should (equal (gptel-agent-trace--summarize-tool-block
                  "(MyTool :flag t)")
                 "MyTool")))

(ert-deftest gptel-agent-trace-test-summarize-empty-string ()
  "Empty/unknown text returns fallback word."
  ;; Completely empty → "tool" (the default)
  (should (equal (gptel-agent-trace--summarize-tool-block "")
                 "tool"))
  ;; Only whitespace
  (should (equal (gptel-agent-trace--summarize-tool-block "   ")
                 "tool")))

(ert-deftest gptel-agent-trace-test-summarize-fallback ()
  "Non-sexp first word is used as tool name."
  (should (equal (gptel-agent-trace--summarize-tool-block
                  "SomeWeirdFormat more stuff")
                 "SomeWeirdFormat")))


;;; ---------------------------------------------------------------
;;; 2. gptel-agent-trace--trace-file-name
;;; ---------------------------------------------------------------

(ert-deftest gptel-agent-trace-test-trace-file-name-basic ()
  "Buffer visiting \"test-ai.org\" produces \"test-ai-trace.org\"."
  (let* ((tmp (make-temp-file "test-ai" nil ".org"))
         (buf (find-file-noselect tmp)))
    (unwind-protect
        (let ((gptel-agent-trace-file-suffix "-trace"))
          (let ((trace-name (gptel-agent-trace--trace-file-name buf)))
            (should trace-name)
            (should (string-match-p "-trace\\.org\\'" trace-name))
            ;; The base name should be preserved
            (let ((base (file-name-sans-extension
                         (file-name-nondirectory tmp))))
              (should (string-prefix-p
                       base
                       (file-name-sans-extension
                        (file-name-nondirectory trace-name)))))))
      (kill-buffer buf)
      (delete-file tmp))))

(ert-deftest gptel-agent-trace-test-trace-file-name-no-file ()
  "Buffer not visiting a file returns nil."
  (with-temp-buffer
    (should (null (gptel-agent-trace--trace-file-name (current-buffer))))))

(ert-deftest gptel-agent-trace-test-trace-file-name-custom-suffix ()
  "Custom suffix is applied correctly."
  (let* ((tmp (make-temp-file "myproject" nil ".org"))
         (buf (find-file-noselect tmp)))
    (unwind-protect
        (let ((gptel-agent-trace-file-suffix "-debug"))
          (let ((trace-name (gptel-agent-trace--trace-file-name buf)))
            (should (string-match-p "-debug\\.org\\'" trace-name))))
      (kill-buffer buf)
      (delete-file tmp))))


;;; ---------------------------------------------------------------
;;; 3. gptel-agent-trace--collect-elements
;;; ---------------------------------------------------------------

(ert-deftest gptel-agent-trace-test-collect-elements ()
  "Collect all four element types from an org buffer.
The test content places state changes under the agent heading (not
inside the confirmation heading subtree) to avoid overlapping regions."
  (let* ((content (concat
                   ;; org-mode needs #+TODO to recognize custom keywords
                   "#+TODO: PENDING ALLOWED DENIED | DONE\n"
                   "* Project\n"
                   "** Agent heading\n"
                   "Some text.\n"
                   "#+begin_src gptel-tool\n"
                   "(Bash :command \"ls -la\")\n"
                   "#+end_src\n"
                   "More text.\n"
                   "#+begin_src gptel-reasoning\n"
                   "The user wants to list files.\n"
                   "#+end_src\n"
                   "- State \"AI-DOING\"   from \"AI-DO\"      [2026-04-02 Thu 10:22]\n"
                   "- State \"AI-DO\"      from \"\"           [2026-04-02 Thu 10:00]\n"
                   "*** PENDING Requesting permission to run: Bash\n"
                   "(Bash :command \"ls\")\n"
                   "** Next section\n"
                   "End of content.\n"))
         (buf (test-trace--make-org-buffer content))
         (gptel-agent-trace-elements
          '(tool-blocks reasoning-blocks confirm-headings state-changes))
         (gptel-org-agent-tool-confirm-keywords
          '("PENDING" "ALLOWED" "DENIED")))
    (unwind-protect
        (with-current-buffer buf
          ;; Agent heading is at "** Agent heading"
          (goto-char (point-min))
          (re-search-forward "^\\*\\* Agent heading")
          (beginning-of-line)
          (let* ((agent-pos (point))
                 (subtree-end (save-excursion
                                (org-end-of-subtree t) (point)))
                 (elements (gptel-agent-trace--collect-elements
                            agent-pos buf subtree-end)))
            ;; Should have 4 elements
            (should (= (length elements) 4))
            ;; Check types in order (sorted by position)
            (should (eq (plist-get (nth 0 elements) :type) 'tool-block))
            (should (eq (plist-get (nth 1 elements) :type) 'reasoning-block))
            (should (eq (plist-get (nth 2 elements) :type) 'state-change))
            (should (eq (plist-get (nth 3 elements) :type) 'confirm-heading))
            ;; Tool block text should contain the src block
            (should (string-match-p "gptel-tool"
                                    (plist-get (nth 0 elements) :text)))
            ;; Reasoning block text
            (should (string-match-p "gptel-reasoning"
                                    (plist-get (nth 1 elements) :text)))
            ;; State change text
            (should (string-match-p "State \"AI-DOING\""
                                    (plist-get (nth 2 elements) :text)))
            ;; Confirm heading text
            (should (string-match-p "Requesting permission"
                                    (plist-get (nth 3 elements) :text)))
            ;; Positions should be ascending
            (should (< (plist-get (nth 0 elements) :beg)
                       (plist-get (nth 1 elements) :beg)))
            (should (< (plist-get (nth 1 elements) :beg)
                       (plist-get (nth 2 elements) :beg)))
            (should (< (plist-get (nth 2 elements) :beg)
                       (plist-get (nth 3 elements) :beg)))))
      (kill-buffer buf))))

(ert-deftest gptel-agent-trace-test-collect-elements-partial ()
  "Only enabled element types are collected."
  (let* ((content (concat
                   "* Agent\n"
                   "#+begin_src gptel-tool\n"
                   "(Bash :command \"ls\")\n"
                   "#+end_src\n"
                   "#+begin_src gptel-reasoning\n"
                   "thinking...\n"
                   "#+end_src\n"
                   "Trailing content.\n"))
         (buf (test-trace--make-org-buffer content))
         ;; Only tool-blocks enabled
         (gptel-agent-trace-elements '(tool-blocks)))
    (unwind-protect
        (with-current-buffer buf
          (goto-char (point-min))
          (let* ((agent-pos (point))
                 (subtree-end (save-excursion
                                (org-end-of-subtree t) (point)))
                 (elements (gptel-agent-trace--collect-elements
                            agent-pos buf subtree-end)))
            (should (= (length elements) 1))
            (should (eq (plist-get (nth 0 elements) :type) 'tool-block))))
      (kill-buffer buf))))


;;; ---------------------------------------------------------------
;;; 4. gptel-agent-trace--heading-path-at
;;; ---------------------------------------------------------------

(ert-deftest gptel-agent-trace-test-heading-path-nested ()
  "Heading path returns full path from root to current heading."
  (let ((buf (test-trace--make-org-buffer
              (concat "* Project\n"
                      "** Task heading\n"
                      "*** Agent heading\n"
                      "Some content here.\n"))))
    (unwind-protect
        (with-current-buffer buf
          ;; Position inside the *** Agent heading section
          (goto-char (point-min))
          (re-search-forward "^\\*\\*\\* Agent heading")
          (forward-line 1)  ; on "Some content here."
          (let ((path (gptel-agent-trace--heading-path-at
                       (point) buf)))
            (should (equal path '("Project" "Task heading" "Agent heading")))))
      (kill-buffer buf))))

(ert-deftest gptel-agent-trace-test-heading-path-at-heading ()
  "Heading path works when point is on a heading line itself."
  (let ((buf (test-trace--make-org-buffer
              (concat "* Top\n"
                      "** Middle\n"
                      "*** Deep\n"))))
    (unwind-protect
        (with-current-buffer buf
          (goto-char (point-min))
          (re-search-forward "^\\*\\* Middle")
          (beginning-of-line)
          (let ((path (gptel-agent-trace--heading-path-at (point) buf)))
            (should (equal path '("Top" "Middle")))))
      (kill-buffer buf))))

(ert-deftest gptel-agent-trace-test-heading-path-top-level ()
  "Heading path at top level returns single-element list."
  (let ((buf (test-trace--make-org-buffer "* Only heading\nBody.\n")))
    (unwind-protect
        (with-current-buffer buf
          (goto-char (point-min))
          (let ((path (gptel-agent-trace--heading-path-at (point) buf)))
            (should (equal path '("Only heading")))))
      (kill-buffer buf))))


;;; ---------------------------------------------------------------
;;; 5. gptel-agent-trace--ensure-heading-path
;;; ---------------------------------------------------------------

(ert-deftest gptel-agent-trace-test-ensure-heading-path-creates ()
  "Creating a 3-level heading path in a buffer with a header."
  (let ((buf (generate-new-buffer " *test-trace-ensure*")))
    (unwind-protect
        (progn
          (with-current-buffer buf
            (org-mode)
            ;; Add a title like the real trace buffer would have
            (insert "#+TITLE: Trace\n"))
          (gptel-agent-trace--ensure-heading-path
           buf '("Project" "Task" "Agent"))
          (with-current-buffer buf
            (let ((text (buffer-string)))
              (should (string-match-p "^\\* Project" text))
              (should (string-match-p "^\\*\\* Task" text))
              (should (string-match-p "^\\*\\*\\* Agent" text)))))
      (kill-buffer buf))))

(ert-deftest gptel-agent-trace-test-ensure-heading-path-no-duplicates ()
  "Calling ensure-heading-path twice with same path doesn't duplicate."
  (let ((buf (generate-new-buffer " *test-trace-ensure2*")))
    (unwind-protect
        (progn
          (with-current-buffer buf
            (org-mode)
            (insert "#+TITLE: Trace\n"))
          (gptel-agent-trace--ensure-heading-path
           buf '("Project" "Task" "Agent"))
          (gptel-agent-trace--ensure-heading-path
           buf '("Project" "Task" "Agent"))
          (with-current-buffer buf
            (let ((text (buffer-string)))
              ;; Each heading should appear exactly once
              (should (= 1 (test-trace--count-matches "^\\* Project" text)))
              (should (= 1 (test-trace--count-matches "^\\*\\* Task" text)))
              (should (= 1 (test-trace--count-matches "^\\*\\*\\* Agent" text))))))
      (kill-buffer buf))))

(ert-deftest gptel-agent-trace-test-ensure-heading-path-sibling ()
  "Different sub-paths create sibling headings."
  (let ((buf (generate-new-buffer " *test-trace-ensure3*")))
    (unwind-protect
        (progn
          (with-current-buffer buf
            (org-mode)
            (insert "#+TITLE: Trace\n"))
          (gptel-agent-trace--ensure-heading-path
           buf '("Project" "Agent-A"))
          (gptel-agent-trace--ensure-heading-path
           buf '("Project" "Agent-B"))
          (with-current-buffer buf
            (let ((text (buffer-string)))
              (should (= 1 (test-trace--count-matches "^\\* Project" text)))
              (should (= 1 (test-trace--count-matches "^\\*\\* Agent-A" text)))
              (should (= 1 (test-trace--count-matches "^\\*\\* Agent-B" text))))))
      (kill-buffer buf))))


;;; ---------------------------------------------------------------
;;; 6. gptel-agent-trace--extract-subtree (integration)
;;; ---------------------------------------------------------------

(ert-deftest gptel-agent-trace-test-extract-subtree-integration ()
  "Integration: extract-subtree moves elements to trace, leaves summaries.

The test content is structured so that:
- State change lines appear under the Agent heading directly (not inside
  the ALLOWED confirmation sub-heading), avoiding overlapping regions.
- The ALLOWED heading is the last child, followed by a sibling section
  to give org-end-of-subtree a clear boundary.
- #+TODO keywords are declared in-buffer for org-get-todo-state to work."
  (let* ((main-file (make-temp-file "test-main" nil ".org"))
         (gptel-agent-trace-file-suffix "-trace")
         (trace-file (let ((dir (file-name-directory main-file))
                           (base (file-name-sans-extension
                                  (file-name-nondirectory main-file))))
                       (expand-file-name (concat base "-trace.org") dir)))
         (gptel-agent-trace-elements
          '(tool-blocks reasoning-blocks confirm-headings state-changes))
         (gptel-org-agent-tool-confirm-keywords
          '("PENDING" "ALLOWED" "DENIED"))
         (main-content
          (concat
           "#+TODO: PENDING ALLOWED DENIED | DONE\n"
           "* Project\n"
           "** Task heading\n"
           "*** Agent heading\n"
           "Some response text here.\n"
           "#+begin_src gptel-tool\n"
           "(Bash :command \"ls -la\")\n"
           "(:name \"Bash\" :args (:command \"ls -la\"))\n"
           "\n"
           "total 42\n"
           "drwxr-xr-x 2 user user 4096 Jan 1 00:00 .\n"
           "#+end_src\n"
           "\n"
           "More response text.\n"
           "\n"
           "#+begin_src gptel-reasoning\n"
           "The user wants to list files. I should use Bash.\n"
           "#+end_src\n"
           "\n"
           "- State \"AI-DOING\"   from \"AI-DO\"      [2026-04-02 Thu 10:22]\n"
           "- State \"AI-DO\"      from \"\"           [2026-04-02 Thu 10:00]\n"
           "\n"
           "Final result text.\n"
           "**** ALLOWED Requesting permission to run: Bash\n"
           "(Bash :command \"ls -la\")\n"
           "\n"
           "** Other section\n"
           "Unrelated content.\n"))
         main-buf trace-buf)
    (unwind-protect
        (progn
          ;; Write main file content and open it
          (with-temp-file main-file
            (insert main-content))
          (setq main-buf (find-file-noselect main-file))
          (with-current-buffer main-buf
            (org-mode)
            (org-set-regexps-and-options)

            ;; Find the agent heading position
            (goto-char (point-min))
            (re-search-forward "^\\*\\*\\* Agent heading")
            (beginning-of-line)
            (let ((agent-pos (point)))
              ;; Run the extraction
              (gptel-agent-trace--extract-subtree agent-pos main-buf)))

          ;; --- Verify main buffer ---
          (with-current-buffer main-buf
            (let ((text (buffer-string)))
              ;; Should still have plain response text
              (should (string-match-p "Some response text here\\." text))
              (should (string-match-p "More response text\\." text))
              (should (string-match-p "Final result text\\." text))

              ;; Should have a summary link for the tool block
              (should (string-match-p
                       (regexp-quote
                        (format "[[file:%s::*Agent heading][Bash: ls -la]]"
                                (file-name-nondirectory trace-file)))
                       text))

              ;; Should NOT have the raw tool block
              (should-not (string-match-p "#\\+begin_src gptel-tool" text))

              ;; Should NOT have reasoning block
              (should-not (string-match-p "#\\+begin_src gptel-reasoning" text))
              (should-not (string-match-p "I should use Bash" text))

              ;; Should NOT have the ALLOWED confirmation heading
              (should-not (string-match-p "ALLOWED Requesting permission" text))

              ;; Should NOT have state change lines
              (should-not (string-match-p "State \"AI-DOING\"" text))
              (should-not (string-match-p "State \"AI-DO\"" text))

              ;; Should still have the unrelated section
              (should (string-match-p "Other section" text))))

          ;; --- Verify trace file ---
          (setq trace-buf (find-file-noselect trace-file))
          (with-current-buffer trace-buf
            (let ((text (buffer-string)))
              ;; Should have the tool block content
              (should (string-match-p "#\\+begin_src gptel-tool" text))
              (should (string-match-p "(Bash :command \"ls -la\")" text))

              ;; Should have reasoning block
              (should (string-match-p "#\\+begin_src gptel-reasoning" text))
              (should (string-match-p "I should use Bash" text))

              ;; Should have the confirmation heading content
              (should (string-match-p "Requesting permission to run" text))

              ;; Should have state changes
              (should (string-match-p "State \"AI-DOING\"" text))

              ;; Should have the heading hierarchy mirrored
              (should (string-match-p "^\\* Project" text))
              (should (string-match-p "^\\*\\* Task heading" text))
              (should (string-match-p "^\\*\\*\\* Agent heading" text)))))

      ;; Cleanup
      (when (and main-buf (buffer-live-p main-buf))
        (with-current-buffer main-buf
          (set-buffer-modified-p nil))
        (kill-buffer main-buf))
      (when (and trace-buf (buffer-live-p trace-buf))
        (with-current-buffer trace-buf
          (set-buffer-modified-p nil))
        (kill-buffer trace-buf))
      (when (file-exists-p main-file) (delete-file main-file))
      (when (file-exists-p trace-file) (delete-file trace-file)))))


(provide 'test-gptel-agent-trace)
;;; test-gptel-agent-trace.el ends here
