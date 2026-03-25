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
;; - "OrgOutline"  : Get the heading structure of an org file
;; - "OrgHeading"  : Retrieve the content of a specific heading

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
          (org-mode)
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
          (org-mode)
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

(provide 'gptel-agent-tools-org)
;;; gptel-agent-tools-org.el ends here

;; Local Variables:
;; elisp-flymake-byte-compile-load-path: ("~/.local/share/git/elpaca/repos/gptel/" "~/.local/share/git/elpaca/repos/transient/lisp" "~/.local/share/git/elpaca/repos/compat/")
;; End:
