;;; gptel-agent-trace.el --- Trace export for gptel-agent  -*- lexical-binding: t; -*-

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

;; Export AI trace data (tool calls, reasoning blocks, state change logs,
;; tool confirmation headings) from the main org document to a separate
;; trace file, keeping the main document clean for version control.
;;
;; This module hooks into `gptel-post-response-functions' at priority 93
;; (after block folding at 90, before user-heading insertion at 95).
;; At hook time we are in an agent's indirect buffer narrowed to the agent
;; subtree, with the agent heading at `(point-min)'.
;;
;; Trace elements are copied to `<basename>-trace.org' under a heading
;; hierarchy that mirrors the source document.  Each extracted element is
;; either replaced with a compact summary line (tool blocks) or removed
;; entirely (reasoning blocks, confirmation headings, state change logs).
;;
;; Usage:
;;   (require 'gptel-agent-trace)
;;   (gptel-agent-trace-mode 1)
;;
;; Or add to init.el:
;;   (add-hook 'gptel-mode-hook (lambda () (gptel-agent-trace-mode 1)))

;;; Code:

(require 'cl-lib)
(require 'org)

;; Forward declarations -- gptel-org.el
(declare-function gptel-org--debug "gptel-org")
(declare-function gptel-org--in-agent-indirect-buffer-p "gptel-org")

;; Forward declarations -- gptel-org-agent.el
(declare-function gptel-org-agent--current-agent-tag "gptel-org-agent")

;; External variables
(defvar gptel-org-agent-tool-confirm-keywords)


;;; Customization

(defgroup gptel-agent-trace nil
  "Trace export for gptel-agent."
  :group 'gptel)

(defcustom gptel-agent-trace-auto-export t
  "When non-nil, automatically export trace on response completion.
Set to nil to disable the post-response hook; you can still call
`gptel-agent-trace-export-subtree' manually."
  :type 'boolean
  :group 'gptel-agent-trace)

(defcustom gptel-agent-trace-file-suffix "-trace"
  "Suffix appended before the .org extension for trace files.
For example, with the default value, \"emacs-ai.org\" produces
\"emacs-ai-trace.org\"."
  :type 'string
  :group 'gptel-agent-trace)

(defcustom gptel-agent-trace-elements
  '(tool-blocks reasoning-blocks confirm-headings state-changes)
  "Element types to export to the trace file.
Each symbol enables extraction of one category:

  `tool-blocks'       — #+begin_src gptel-tool blocks
  `reasoning-blocks'  — #+begin_src gptel-reasoning blocks
  `confirm-headings'  — PENDING/ALLOWED/DENIED confirmation headings
  `state-changes'     — State change log lines"
  :type '(set (const :tag "Tool call blocks" tool-blocks)
              (const :tag "Reasoning blocks" reasoning-blocks)
              (const :tag "Tool confirmation headings" confirm-headings)
              (const :tag "State change log lines" state-changes))
  :group 'gptel-agent-trace)


;;; Trace file management

(defun gptel-agent-trace--trace-file-name (base-buffer)
  "Derive the trace file path from BASE-BUFFER's file name.
Returns nil if BASE-BUFFER is not visiting a file."
  (when-let* ((file (buffer-file-name base-buffer)))
    (let ((dir (file-name-directory file))
          (base (file-name-sans-extension (file-name-nondirectory file)))
          (ext (file-name-extension file t)))
      (expand-file-name (concat base gptel-agent-trace-file-suffix ext) dir))))

(defun gptel-agent-trace--get-trace-buffer (base-buffer)
  "Get or create the trace buffer for BASE-BUFFER.
Creates the trace file with a minimal header if it does not exist.
Returns the trace buffer, or nil if BASE-BUFFER has no file."
  (when-let* ((trace-file (gptel-agent-trace--trace-file-name base-buffer))
              (main-name (file-name-nondirectory
                          (buffer-file-name base-buffer))))
    (let ((buf (find-file-noselect trace-file)))
      (when (zerop (buffer-size buf))
        (with-current-buffer buf
          (insert (format "#+TITLE: Trace: %s\n" main-name))
          (unless (derived-mode-p 'org-mode) (org-mode))))
      buf)))


;;; Heading path utilities

(defun gptel-agent-trace--heading-path-at (pos base-buffer)
  "Return the heading path from root to POS in BASE-BUFFER.
The result is a list of heading title strings, outermost first.
For example: (\"gptel\" \"Study trace splitting\" \"Gathering info\")."
  (with-current-buffer base-buffer
    (save-excursion
      (goto-char pos)
      (unless (org-at-heading-p)
        (org-back-to-heading t))
      (let ((path nil))
        (push (org-get-heading t t t t) path)
        (while (org-up-heading-safe)
          (push (org-get-heading t t t t) path))
        path))))

(defun gptel-agent-trace--ensure-heading-path (trace-buf heading-path)
  "Create or find HEADING-PATH hierarchy in TRACE-BUF.
HEADING-PATH is a list of strings, e.g. (\"Project\" \"Agent\" \"Sub\").
Returns the position at the end of the deepest heading's section
\(before any children), where new content should be inserted."
  (with-current-buffer trace-buf
    (save-excursion
      (goto-char (point-min))
      (let ((level 0)
            (scope-end (point-max)))
        (dolist (title heading-path)
          (cl-incf level)
          (let ((found nil)
                (search-start (point)))
            ;; Search for existing heading at this level with this title
            (save-excursion
              (goto-char search-start)
              (while (and (not found)
                          (<= (point) scope-end)
                          (re-search-forward org-heading-regexp scope-end t))
                (beginning-of-line)
                (when (and (org-at-heading-p)
                           (= (org-current-level) level)
                           (string= (org-get-heading t t t t) title))
                  (setq found (point)))
                (forward-line 1)))
            (if found
                (progn
                  (goto-char found)
                  (setq scope-end (save-excursion
                                    (org-end-of-subtree t) (point))))
              ;; Create the heading at end of current scope
              (goto-char scope-end)
              (unless (bolp) (insert "\n"))
              (let ((stars (make-string level ?*)))
                (insert (format "%s %s\n" stars title)))
              (setq scope-end (point)))))
        ;; Return insertion point: after deepest heading, skip body text
        (when (save-excursion (beginning-of-line) (org-at-heading-p))
          (forward-line 1))
        (let ((limit scope-end))
          (while (and (< (point) limit)
                      (not (org-at-heading-p))
                      (not (eobp)))
            (forward-line 1)))
        (point)))))


;;; Tool block summarization

(defun gptel-agent-trace--summarize-tool-block (block-text)
  "Generate a one-line summary from a gptel-tool BLOCK-TEXT.
Returns a string like \"Bash: ls -la\" or \"Agent: researcher\"."
  (let ((tool-name "tool")
        (arg-summary ""))
    (cond
     ;; S-expression format: (ToolName :key value ...)
     ((string-match "^[ \t]*(\\([A-Za-z_-]+\\)" block-text)
      (setq tool-name (match-string 1 block-text))
      (when (string-match ":\\([a-z_-]+\\)[ \t]+\"\\([^\"\n]*\\)\"" block-text)
        (setq arg-summary (match-string 2 block-text))))
     ;; Fallback: first word on first non-empty line
     ((string-match "\\`[ \t]*\\([A-Za-z_-]+\\)" block-text)
      (setq tool-name (match-string 1 block-text))))
    (when (> (length arg-summary) 60)
      (setq arg-summary (concat (substring arg-summary 0 57) "...")))
    (if (string-empty-p arg-summary)
        tool-name
      (format "%s: %s" tool-name arg-summary))))


;;; Trace element collection

(defun gptel-agent-trace--collect-elements (agent-pos base-buffer subtree-end)
  "Collect trace elements in BASE-BUFFER between AGENT-POS and SUBTREE-END.
Returns a list of plists, each with keys:
  :type        — symbol: `tool-block', `reasoning-block',
                 `confirm-heading', or `state-change'
  :beg         — start position in BASE-BUFFER
  :end         — end position in BASE-BUFFER
  :text        — the extracted text
  :heading-pos — position of the enclosing heading

The list is sorted by :beg in ascending order."
  (let ((elements nil)
        (confirm-kws (and (boundp 'gptel-org-agent-tool-confirm-keywords)
                          gptel-org-agent-tool-confirm-keywords)))
    (with-current-buffer base-buffer
      (save-excursion

        ;; 1. Collect src blocks (tool and reasoning)
        (when (or (memq 'tool-blocks gptel-agent-trace-elements)
                  (memq 'reasoning-blocks gptel-agent-trace-elements))
          (goto-char agent-pos)
          (while (and (<= (point) subtree-end)
                      (re-search-forward
                       "^[ \t]*#\\+begin_src gptel-\\(tool\\|reasoning\\)"
                       subtree-end t))
            (let* ((block-type (match-string 1))
                   (sym (if (string= block-type "tool")
                            'tool-block 'reasoning-block))
                   (enabled (memq (if (eq sym 'tool-block)
                                      'tool-blocks 'reasoning-blocks)
                                  gptel-agent-trace-elements)))
              (when enabled
                (beginning-of-line)
                (let ((beg (point)))
                  (if (re-search-forward
                       "^[ \t]*#\\+end_src" subtree-end t)
                      (forward-line 1)
                    (goto-char subtree-end))
                  (let ((end (point))
                        (heading-pos
                         (save-excursion
                           (goto-char beg)
                           (org-back-to-heading t)
                           (point))))
                    (push (list :type sym
                                :beg beg :end end
                                :text (buffer-substring-no-properties beg end)
                                :heading-pos heading-pos)
                          elements)))))))

        ;; 2. Collect confirmation headings (PENDING/ALLOWED/DENIED)
        (when (and (memq 'confirm-headings gptel-agent-trace-elements)
                   confirm-kws)
          (goto-char agent-pos)
          (while (and (<= (point) subtree-end)
                      (re-search-forward org-heading-regexp subtree-end t))
            (beginning-of-line)
            (when (org-at-heading-p)
              (let ((todo (org-get-todo-state)))
                (if (and todo
                         (member todo confirm-kws)
                         (string-match-p
                          "Requesting permission to run:"
                          (org-get-heading t t t t)))
                    ;; Found confirmation heading — collect subtree, skip past
                    (let* ((beg (point))
                           (end (save-excursion
                                  (org-end-of-subtree t)
                                  (when (and (not (eobp))
                                             (looking-at-p "^[ \t]*$"))
                                    (forward-line 1))
                                  (point)))
                           (parent-pos
                            (save-excursion
                              (org-up-heading-safe) (point))))
                      (push (list :type 'confirm-heading
                                  :beg beg :end end
                                  :text (buffer-substring-no-properties
                                         beg end)
                                  :heading-pos parent-pos)
                            elements)
                      ;; Skip past this entire subtree
                      (goto-char end))
                  ;; Not a confirmation heading — move past
                  (forward-line 1))))))

        ;; 3. Collect state change log lines
        (when (memq 'state-changes gptel-agent-trace-elements)
          (goto-char agent-pos)
          (let ((state-re
                 "^[ \t]*- State \"[^\"]*\"[ \t]+from[ \t]+\"?[^\"]*\"?[ \t]+\\[.*\\][ \t]*$"))
            (while (and (<= (point) subtree-end)
                        (re-search-forward state-re subtree-end t))
              (let ((line-beg (line-beginning-position)))
                (forward-line 1)
                ;; Group consecutive state change lines
                (while (and (< (point) subtree-end)
                            (looking-at state-re))
                  (forward-line 1))
                (let ((line-end (point))
                      (heading-pos
                       (save-excursion
                         (goto-char line-beg)
                         (org-back-to-heading t)
                         (point))))
                  (push (list :type 'state-change
                              :beg line-beg :end line-end
                              :text (buffer-substring-no-properties
                                     line-beg line-end)
                              :heading-pos heading-pos)
                        elements))))))))

    ;; Return sorted ascending by position
    (sort elements (lambda (a b) (< (plist-get a :beg) (plist-get b :beg))))))


;;; Core extraction

(defun gptel-agent-trace--extract-subtree (agent-pos base-buffer)
  "Walk agent subtree at AGENT-POS in BASE-BUFFER, extract trace elements.
Copies trace elements to the trace file and replaces/removes them
in the main buffer.  Tool blocks are replaced with summary link lines;
all other trace elements are removed entirely."
  (let* ((subtree-end
          (with-current-buffer base-buffer
            (save-excursion
              (goto-char agent-pos)
              (org-end-of-subtree t)
              (point))))
         (elements (gptel-agent-trace--collect-elements
                    agent-pos base-buffer subtree-end)))
    (when elements
      (let ((trace-buffer (gptel-agent-trace--get-trace-buffer base-buffer)))
        (when trace-buffer
          (let ((inhibit-read-only t)
                (trace-file-name
                 (file-name-nondirectory
                  (gptel-agent-trace--trace-file-name base-buffer))))
            ;; Process in REVERSE order to preserve buffer positions
            (dolist (elem (nreverse elements))
              (let* ((type (plist-get elem :type))
                     (beg  (plist-get elem :beg))
                     (end  (plist-get elem :end))
                     (text (plist-get elem :text))
                     (heading-pos (plist-get elem :heading-pos))
                     (heading-path (gptel-agent-trace--heading-path-at
                                    heading-pos base-buffer)))

                ;; 1. Copy to trace file under mirrored heading path
                (let ((insert-pos
                       (gptel-agent-trace--ensure-heading-path
                        trace-buffer heading-path)))
                  (with-current-buffer trace-buffer
                    (save-excursion
                      (goto-char insert-pos)
                      (insert text)
                      (unless (bolp) (insert "\n")))))

                ;; 2. Replace or remove in main buffer
                (with-current-buffer base-buffer
                  (save-excursion
                    (goto-char beg)
                    (pcase type
                      ('tool-block
                       ;; Replace with a one-line summary link
                       (let* ((block-body
                               (if (string-match
                                    "#\\+begin_src gptel-tool\n\\(\\(?:.\\|\n\\)*?\\)\n?#\\+end_src"
                                    text)
                                   (match-string 1 text)
                                 text))
                              (summary
                               (gptel-agent-trace--summarize-tool-block
                                block-body))
                              (trace-heading (car (last heading-path)))
                              (link (format "- [[file:%s::*%s][%s]]\n"
                                            trace-file-name
                                            trace-heading
                                            summary)))
                         (delete-region beg end)
                         ;; Remove trailing blank line left over
                         (when (looking-at "^[ \t]*$")
                           (delete-region
                            (point)
                            (save-excursion (forward-line 1) (point))))
                         (insert link)))

                      ;; Reasoning, confirm-headings, state-changes: remove
                      (_
                       (delete-region beg end)
                       (when (looking-at "^[ \t]*$")
                         (delete-region
                          (point)
                          (save-excursion
                            (forward-line 1) (point))))))))))

            ;; Save trace buffer
            (with-current-buffer trace-buffer
              (save-buffer))))))))


;;; Interactive command

;;;###autoload
(defun gptel-agent-trace-export-subtree ()
  "Interactively export trace from the agent subtree at point.
Finds the nearest agent heading (tagged with *@agent) and
extracts trace elements to the trace file."
  (interactive)
  (unless (derived-mode-p 'org-mode)
    (user-error "Not in an org-mode buffer"))
  (save-excursion
    (unless (org-at-heading-p) (org-back-to-heading t))
    ;; Walk up to find an agent-tagged heading
    (let ((found nil))
      (while (and (not found) (org-at-heading-p))
        (let ((tags (org-get-tags nil t)))
          (if (cl-some (lambda (tag)
                         (and (stringp tag)
                              (string-suffix-p "@agent" tag)))
                       tags)
              (setq found (point))
            (unless (org-up-heading-safe)
              (user-error "No agent heading found in parent chain")))))
      (when found
        (gptel-agent-trace--extract-subtree found (current-buffer))
        (message "Trace exported to %s"
                 (file-name-nondirectory
                  (gptel-agent-trace--trace-file-name
                   (current-buffer))))))))


;;; Hook function

(defun gptel-agent-trace--export (_beg _end)
  "Export trace elements from the agent subtree to a trace file.
Added to `gptel-post-response-functions' at priority 93.

Checks that we are in a top-level agent indirect buffer (not a
sub-agent), then walks the agent subtree to find and extract trace
elements."
  (when gptel-agent-trace-auto-export
    (condition-case err
        (when (and (fboundp 'gptel-org--in-agent-indirect-buffer-p)
                   (gptel-org--in-agent-indirect-buffer-p)
                   (fboundp 'gptel-org-agent--current-agent-tag))
          (let ((agent-tag (gptel-org-agent--current-agent-tag)))
            (when (and agent-tag
                       ;; Top-level agents: one '@' (e.g. "main@agent")
                       ;; Sub-agents: 2+ '@' (e.g. "researcher@main@agent")
                       (not (string-match-p "@.*@" agent-tag)))
              (let ((base-buffer (buffer-base-buffer (current-buffer))))
                (when (and base-buffer (buffer-file-name base-buffer))
                  (let ((agent-pos
                         (save-excursion
                           (goto-char (point-min))
                           (when (org-at-heading-p) (point)))))
                    (when agent-pos
                      (when (fboundp 'gptel-org--debug)
                        (gptel-org--debug
                         "[gptel-trace] Exporting trace for %s in %s"
                         agent-tag (buffer-name base-buffer)))
                      (gptel-agent-trace--extract-subtree
                       agent-pos base-buffer))))))))
      (error
       (when (fboundp 'gptel-org--debug)
         (gptel-org--debug
          "[gptel-trace] Error during trace export: %S" err))))))


;;; Minor mode

;;;###autoload
(define-minor-mode gptel-agent-trace-mode
  "Toggle automatic trace export for gptel-agent responses.
When enabled, trace elements (tool calls, reasoning blocks,
confirmation headings, state changes) are automatically exported
to a separate trace file after each agent response completes."
  :global t
  :group 'gptel-agent-trace
  (if gptel-agent-trace-mode
      (add-hook 'gptel-post-response-functions
                #'gptel-agent-trace--export 93)
    (remove-hook 'gptel-post-response-functions
                 #'gptel-agent-trace--export)))

(provide 'gptel-agent-trace)
;;; gptel-agent-trace.el ends here
