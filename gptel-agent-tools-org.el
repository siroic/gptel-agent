;;; gptel-agent-tools-org.el --- Org-mode tools for gptel-agent -*- lexical-binding: t; -*-

;; Copyright (C) 2025

;; Keywords: comm, tools

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Org-mode structural navigation tools for gptel-agent.
;;
;; Provides tools for token-efficient exploration of org documents:
;; get an overview first, then drill down into specific headings.
;;
;; Tools:
;; - "OrgOutline"   : Get the heading structure of an org file
;; - "OrgHeading"   : Retrieve the content of a specific heading
;; - "ReadOrgLink"  : Resolve an org-mode file link and return content

;;; Code:

(require 'gptel)
(require 'org-element)
(eval-when-compile (require 'cl-lib))

(declare-function gptel-agent--read-file-lines "gptel-agent-tools")

;;; OrgOutline — Get the heading structure of an org file

(defun gptel-agent--org-outline (file-path &optional depth)
  "Return the heading outline of org FILE-PATH up to DEPTH levels.
If DEPTH is nil or 0, return all levels."
  (unless (file-readable-p file-path)
    (error "Error: File %s is not readable" file-path))
  (when (file-directory-p file-path)
    (error "Error: Cannot read directory %s as file" file-path))
  (let ((buf (find-file-noselect file-path t))
        (depth (if (and depth (> depth 0)) depth nil))
        result)
    (unwind-protect
        (with-current-buffer buf
          ;; file-path is an .org file; find-file-noselect already set the mode.
          ;; Do NOT call (org-mode) here — it re-fires mode hooks and triggers LSP.
          (unless (derived-mode-p 'org-mode) (org-mode))
          (let ((tree (org-element-parse-buffer 'headline)))
            (org-element-map tree 'headline
              (lambda (hl)
                (let ((level (org-element-property :level hl))
                      (title (org-element-property :raw-value hl))
                      (todo (org-element-property :todo-keyword hl))
                      (tags (org-element-property :tags hl))
                      (line (line-number-at-pos
                             (org-element-property :begin hl))))
                  (when (or (null depth) (<= level depth))
                    (push
                     (concat
                      ;; Line number prefix for later drill-down
                      (format "L%-5d " line)
                      ;; Org heading with proper stars
                      (make-string level ?*)
                      " "
                      (if todo (concat todo " ") "")
                      title
                      (if tags
                          (concat (make-string
                                   (max 1 (- 60 level
                                              (length (or todo ""))
                                              (if todo 1 0)
                                              (length title)))
                                   ?\s)
                                  ":" (mapconcat #'identity tags ":") ":")
                        ""))
                     result))))
              ;; When depth=1, no need to recurse into sub-headings
              nil nil (when (eql depth 1) 'headline))))
      (unless (buffer-modified-p buf) (kill-buffer buf)))
    (if result
        (let ((lines (nreverse result)))
          (format "File: %s (%d headings%s)\n\n%s"
                  file-path
                  (length lines)
                  (if depth (format ", depth %d" depth) ", all levels")
                  (mapconcat #'identity lines "\n")))
      (format "File: %s (no headings found)" file-path))))

;;; OrgHeading — Retrieve the content of a specific heading

(defun gptel-agent--org-heading (file-path &optional heading line scope)
  "Return content from an org heading in FILE-PATH.
Find the heading by HEADING title or LINE number.
SCOPE controls what to return: \"subtree\" (default), \"section\", or \"children\"."
  (unless (file-readable-p file-path)
    (error "Error: File %s is not readable" file-path))
  (unless (or heading line)
    (error "Error: Either heading or line must be provided"))
  (let ((buf (find-file-noselect file-path t))
        (scope (or scope "subtree"))
        target-line result heading-title)
    (unwind-protect
        (with-current-buffer buf
          ;; Do NOT call (org-mode) unconditionally — it re-fires mode hooks and
          ;; triggers LSP warnings.  find-file-noselect sets the mode already.
          (unless (derived-mode-p 'org-mode) (org-mode))
          (goto-char (point-min))
          (cond
           ;; Find by line number
           (line
            (forward-line (1- line))
            (unless (org-at-heading-p)
              (error "Error: Line %d is not a heading in %s" line file-path)))
           ;; Find by heading title
           (heading
            (let ((found nil)
                  (case-fold-search t))
              (while (and (not found)
                          (re-search-forward org-complex-heading-regexp nil t))
                (when (string-match-p
                       (regexp-quote heading)
                       (or (match-string 4) ""))
                  (setq found t)))
              (unless found
                (error "Error: Heading matching %S not found in %s"
                       heading file-path))
              (beginning-of-line))))
          (setq target-line (line-number-at-pos))
          (setq heading-title (nth 4 (org-heading-components)))
          (pcase scope
            ("subtree"
             (let ((beg (point)))
               (org-end-of-subtree t t)
               (setq result (buffer-substring-no-properties beg (point)))))
            ("section"
             (let ((beg (line-end-position)))
               (outline-next-heading)
               ;; If next heading is a child, stop there;
               ;; if same/higher level, include all
               (setq result (string-trim
                             (buffer-substring-no-properties beg (point))))))
            ("children"
             (let (children)
               (save-excursion
                 (when (org-goto-first-child)
                   (push (format "L%-5d %s"
                                 (line-number-at-pos)
                                 (buffer-substring-no-properties
                                  (line-beginning-position)
                                  (line-end-position)))
                         children)
                   (while (org-get-next-sibling)
                     (push (format "L%-5d %s"
                                   (line-number-at-pos)
                                   (buffer-substring-no-properties
                                    (line-beginning-position)
                                    (line-end-position)))
                           children))))
               (setq result
                     (if children
                         (mapconcat #'identity (nreverse children) "\n")
                       "(no child headings)"))))))
      (unless (buffer-modified-p buf) (kill-buffer buf)))
    (format "File: %s (heading \"%s\", line %d, scope: %s)\n\n%s"
            file-path heading-title target-line scope result)))

;;; ReadOrgLink — Resolve an org-mode file link

(defun gptel-agent--resolve-org-link (link &optional context-lines)
  "Resolve an org-mode file LINK and return a snippet around the target.

LINK is an org link string like \"[[file:path/to/file::42]]\" or
\"file:path/to/file::42\".  Supports file links with line numbers,
heading searches (*Heading), custom-id (#id), and text search strings.

CONTEXT-LINES is the number of lines to show before and after the
target (default 10).  Returns the snippet with line numbers."
  (require 'org-element)
  (let* ((context-lines (or context-lines 10))
         parsed link-type path search-option)
    ;; Parse the link using org-element
    ;; Note: org-element-link-parser works without org-mode active — do NOT
    ;; call (org-mode) here as it fires org-mode-hook which triggers LSP warnings.
    (with-temp-buffer
      ;; Wrap bare links in brackets for the parser
      (insert (if (string-prefix-p "[[" link) link
                (concat "[[" link "]]")))
      (goto-char (point-min))
      (setq parsed (org-element-link-parser)))
    (unless parsed
      (error "Error: Could not parse org link: %s" link))
    (setq link-type (org-element-property :type parsed))
    (setq path (org-element-property :path parsed))
    (setq search-option (org-element-property :search-option parsed))
    ;; Only file links supported
    (unless (string= link-type "file")
      (error "Error: Only file links are supported, got type: %s" link-type))
    (unless (file-readable-p path)
      (error "Error: File not readable: %s" path))
    (when (file-directory-p path)
      (error "Error: Path is a directory: %s" path))
    (cond
     ;; Line number: pure digits
     ((and search-option (string-match-p "\\`[0-9]+\\'" search-option))
      (let* ((target-line (string-to-number search-option))
             (start (max 1 (- target-line context-lines)))
             (end (+ target-line context-lines)))
        (format "File: %s (line %d, showing %d-%d):\n\n%s"
                path target-line start end
                (gptel-agent--read-file-lines path start end))))
     ;; Heading search: *Heading
     ((and search-option (string-prefix-p "*" search-option))
      (let* ((heading (substring search-option 1))
             (buf (find-file-noselect path t))
             result target-line)
        (unwind-protect
            (with-current-buffer buf
              (unless (derived-mode-p 'org-mode) (org-mode))
              (goto-char (point-min))
              (if (re-search-forward
                   (format org-complex-heading-regexp-format
                           (regexp-quote heading))
                   nil t)
                  (let* ((elem (org-element-at-point))
                         (beg (org-element-property :begin elem))
                         (end (org-element-property :end elem)))
                    (setq target-line (line-number-at-pos beg))
                    (setq result
                          (buffer-substring-no-properties beg end)))
                (error "Error: Heading '*%s' not found in %s"
                       heading path)))
          (unless (buffer-modified-p buf)
            (kill-buffer buf)))
        (format "File: %s (heading \"%s\", line %d):\n\n%s"
                path heading target-line result)))
     ;; Custom ID: #custom-id
     ((and search-option (string-prefix-p "#" search-option))
      (let* ((custom-id (substring search-option 1))
             (buf (find-file-noselect path t))
             result target-line)
        (unwind-protect
            (with-current-buffer buf
              (unless (derived-mode-p 'org-mode) (org-mode))
              (goto-char (point-min))
              (if (re-search-forward
                   (format "^[ \t]*:CUSTOM_ID:[ \t]+%s[ \t]*$"
                           (regexp-quote custom-id))
                   nil t)
                  (let* ((elem (org-element-at-point))
                         ;; Navigate to the heading owning this property
                         (heading (org-element-lineage elem '(headline) t))
                         (beg (org-element-property :begin
                                                    (or heading elem)))
                         (end (org-element-property :end
                                                    (or heading elem))))
                    (setq target-line (line-number-at-pos beg))
                    (setq result
                          (buffer-substring-no-properties beg end)))
                (error "Error: Custom ID '#%s' not found in %s"
                       custom-id path)))
          (unless (buffer-modified-p buf)
            (kill-buffer buf)))
        (format "File: %s (custom-id \"%s\", line %d):\n\n%s"
                path custom-id target-line result)))
     ;; Text search: any other search option string
     (search-option
      (let* ((buf (find-file-noselect path t))
             target-line)
        (unwind-protect
            (with-current-buffer buf
              (goto-char (point-min))
              (if (search-forward search-option nil t)
                  (setq target-line (line-number-at-pos
                                     (match-beginning 0)))
                (error "Error: Search string %S not found in %s"
                       search-option path)))
          (unless (buffer-modified-p buf)
            (kill-buffer buf)))
        (let* ((start (max 1 (- target-line context-lines)))
               (end (+ target-line context-lines)))
          (format "File: %s (search %S, line %d, showing %d-%d):\n\n%s"
                  path search-option target-line start end
                  (gptel-agent--read-file-lines path start end)))))
     ;; No search option: return file head, capped by context-lines
     (t
      (let* ((max-lines (* 2 context-lines))
             (content (gptel-agent--read-file-lines path 1 max-lines))
             (total-lines (with-temp-buffer
                            (insert-file-contents path)
                            (count-lines (point-min) (point-max)))))
        (if (> total-lines max-lines)
            (format "File: %s (showing first %d of %d lines):\n\n%s"
                    path max-lines total-lines content)
          (format "File: %s:\n\n%s" path content)))))))

;;; Tool declarations

(gptel-make-tool
 :name "OrgOutline"
 :function #'gptel-agent--org-outline
 :description "Get the heading structure of an org-mode file as a table of contents.

Returns headings with their line numbers, TODO keywords, and tags.
Use this for token-efficient exploration of org documents before
retrieving specific sections with OrgHeading.

Controls heading depth like org-mode's global cycling:
- depth=1: OVERVIEW — top-level headings only
- depth=N: headings to N levels deep
- depth omitted: CONTENTS — all headings at all levels

Each heading is prefixed with its line number (e.g. \"L42\") for use
with the Read tool or OrgHeading's line parameter."
 :args '((:name "file_path"
          :type string
          :description "The path to the org file.")
         (:name "depth"
          :type integer
          :description "Max heading depth to include. 1 = top-level only, 2 = two levels, etc. Omit for all levels."
          :optional t
          :minimum 1))
 :category "org"
 :include t)

(gptel-make-tool
 :name "OrgHeading"
 :function #'gptel-agent--org-heading
 :description "Retrieve the content of a specific heading from an org-mode file.

Use after OrgOutline to drill down into a specific section.
Find the heading by title (substring match) or line number.

Scope controls what is returned:
- \"subtree\" (default): the full heading with all its children and content
- \"section\": only the body text of this heading, before any child headings
- \"children\": just the direct child heading lines with their line numbers

Exactly one of heading or line must be provided."
 :args '((:name "file_path"
          :type string
          :description "The path to the org file.")
         (:name "heading"
          :type string
          :description "Heading title to search for (substring match, case-insensitive)."
          :optional t)
         (:name "line"
          :type integer
          :description "Line number of the heading, as returned by OrgOutline."
          :optional t)
         (:name "scope"
          :type string
          :enum ["subtree" "section" "children"]
          :description "What to return: \"subtree\" (full heading + children), \"section\" (body only, no children), or \"children\" (direct child heading lines only). Default: \"subtree\"."
          :optional t))
 :category "org"
 :include t)

(gptel-make-tool
 :name "ReadOrgLink"
 :function #'gptel-agent--resolve-org-link
 :description "Resolve an org-mode file link and return content around the target location.

Parses org link syntax and returns a snippet. Supports:
- Line numbers: [[file:path::42]] → lines around line 42
- Heading search: [[file:path::*Heading]] → that subtree
- Custom ID: [[file:path::#my-id]] → that section
- Text search: [[file:path::search text]] → lines around first match
- Plain file: [[file:path]] → first 2×context_lines lines (default 20)

Use this when you encounter org-mode links in context and need to see what they point to."
 :args '((:name "link"
          :type string
          :description "The org link to resolve, e.g. '[[file:path/to/file::42]]' or 'file:path::*Heading'")
         (:name "context_lines"
          :type integer
          :description "Lines of context before/after target line (default 10). Only applies to line-number links and plain file links."
          :optional t
          :minimum 1
          :maximum 50))
 :category "org"
 :include t)

(provide 'gptel-agent-tools-org)
;;; gptel-agent-tools-org.el ends here

;; Local Variables:
;; elisp-flymake-byte-compile-load-path: ("~/.local/share/git/elpaca/repos/gptel/" "~/.local/share/git/elpaca/repos/transient/lisp" "~/.local/share/git/elpaca/repos/compat/")
;; End:
