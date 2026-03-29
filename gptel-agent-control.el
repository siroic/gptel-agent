;;; gptel-agent-control.el --- Centralized control buffer for gptel agent tool requests  -*- lexical-binding: t; -*-

;; Copyright (C) 2025  Karthik Chikmagalur

;; Author: Karthik Chikmagalur <karthikchikmagalur@gmail.com>

;; SPDX-License-Identifier: GPL-3.0-or-later

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

;; This file is NOT part of GNU Emacs.

;;; Commentary:

;; Provides a centralized control buffer (`*gptel-control*') for managing tool
;; call approvals across all active gptel agents.
;;
;; When agents make tool calls that require user confirmation, those requests
;; are collected in the control buffer.  The user can then accept, reject, or
;; inspect individual requests, or bulk-accept/reject all pending requests.
;;
;; Usage:
;;
;; 1.  Enable the control buffer by adding
;;     `gptel-agent-control-display-tool-calls' to the agent's tool display
;;     pipeline, or call `gptel-agent-control--add-request' directly when a
;;     tool call needs confirmation.
;;
;; 2.  The control buffer opens automatically (in a side window by default)
;;     when the first request arrives.
;;
;; 3.  Use the following keys in the control buffer:
;;
;;     a   - Accept the tool request at point
;;     r   - Reject the tool request at point
;;     A   - Accept all pending requests
;;     R   - Reject all pending requests
;;     RET - Jump to the agent's buffer
;;     n/p - Navigate between entries
;;     g   - Refresh the display
;;     q   - Quit the control buffer window

;;; Code:

(require 'cl-lib)

;; Forward declarations -- gptel.el
(declare-function gptel--accept-tool-calls "gptel")
(declare-function gptel--reject-tool-calls "gptel")
(declare-function gptel--clean-tool-overlay "gptel")
(declare-function gptel--format-tool-call "gptel")
(declare-function gptel--map-tool-args "gptel")
(declare-function gptel-tool-name "gptel")
(declare-function gptel-tool-description "gptel")

;; Forward declarations -- gptel-agent-tools.el
(declare-function gptel-agent--confirm-overlay "gptel-agent-tools")

;; Variables from gptel
(defvar gptel-org-agent-subtrees)
(defvar gptel-tool-call-actions-map)


;;; Customization

(defgroup gptel-agent-control nil
  "Centralized control buffer for gptel agent tool requests."
  :group 'gptel)

(defcustom gptel-agent-control-display-action
  '(display-buffer-in-side-window
    (side . bottom)
    (slot . 0)
    (window-height . 0.3))
  "Display action for the gptel agent control buffer.

This is passed to `display-buffer' when the control buffer is first
shown.  See Info node `(elisp) Displaying Buffers' for the format."
  :type 'sexp
  :group 'gptel-agent-control)

(defcustom gptel-agent-control-auto-close nil
  "When non-nil, close the control buffer window when all requests are handled."
  :type 'boolean
  :group 'gptel-agent-control)


;;; Data model

(defvar gptel-agent-control--id-counter 0
  "Monotonically increasing counter for generating unique entry IDs.")

(defvar-local gptel-agent-control--entries nil
  "List of pending tool request entries in the control buffer.

Each entry is a plist with keys:
  :id         - unique integer identifier
  :tool-calls - list of (tool-spec arg-plist callback) triplets
  :info       - the FSM info plist
  :overlay    - the original overlay in the agent buffer (if any)
  :agent-type - string identifying the agent (e.g. \"researcher@main@agent\")
  :buffer     - the agent's buffer (indirect or direct)
  :timestamp  - float-time when the request was added
  :region     - cons (start . end) of this entry's region in the control buffer")

(defconst gptel-agent-control--buffer-name "*gptel-control*"
  "Name of the centralized tool control buffer.")

(defconst gptel-agent-control--separator
  (propertize (concat (make-string 60 ?─) "\n")
              'face 'shadow
              'font-lock-face 'shadow)
  "Visual separator between entries in the control buffer.")


;;; Mode definition

(defvar-keymap gptel-agent-control-mode-map
  :doc "Keymap for gptel agent control buffer."
  :parent special-mode-map
  "a"   #'gptel-agent-control--accept-at-point
  "r"   #'gptel-agent-control--reject-at-point
  "A"   #'gptel-agent-control--accept-all
  "R"   #'gptel-agent-control--reject-all
  "RET" #'gptel-agent-control--jump-to-buffer
  "n"   #'gptel-agent-control--next-entry
  "p"   #'gptel-agent-control--prev-entry
  "g"   #'gptel-agent-control--refresh
  "q"   #'quit-window)

(define-derived-mode gptel-agent-control-mode special-mode "gptel-control"
  "Major mode for the gptel agent tool request control buffer.

Displays pending tool call requests from all active gptel agents.
Each request can be individually accepted or rejected, or all
requests can be handled in bulk.

\\{gptel-agent-control-mode-map}"
  :group 'gptel-agent-control
  (setq-local gptel-agent-control--entries nil)
  (setq truncate-lines t)
  (setq-local revert-buffer-function #'gptel-agent-control--revert))


;;; Buffer management

(defun gptel-agent-control--get-buffer ()
  "Get or create the control buffer.

Returns the `*gptel-control*' buffer, creating it and setting it to
`gptel-agent-control-mode' if it does not already exist."
  (let ((buf (get-buffer-create gptel-agent-control--buffer-name)))
    (unless (eq (buffer-local-value 'major-mode buf)
                'gptel-agent-control-mode)
      (with-current-buffer buf
        (gptel-agent-control-mode)))
    buf))

(defun gptel-agent-control--show-buffer ()
  "Display the control buffer using `gptel-agent-control-display-action'.

Only displays the buffer if it is not already visible in some window."
  (let ((buf (gptel-agent-control--get-buffer)))
    (unless (get-buffer-window buf t)
      (display-buffer buf gptel-agent-control-display-action))
    buf))

(defun gptel-agent-control--maybe-hide-buffer ()
  "Hide the control buffer window if `gptel-agent-control-auto-close' is set.

Only hides when the entries list is empty."
  (when (and gptel-agent-control-auto-close
             (null gptel-agent-control--entries))
    (when-let* ((buf (get-buffer gptel-agent-control--buffer-name))
                (win (get-buffer-window buf t)))
      (quit-window nil win))))


;;; Entry formatting

(defun gptel-agent-control--format-tool-summary (tool-calls)
  "Format a one-line summary of TOOL-CALLS for display.

TOOL-CALLS is a list of (tool-spec arg-plist callback) triplets.
Returns a propertized string like:
  Tool: Bash \"git status\"
  Tools: Read \"init.el\", Write \"output.el\""
  (let* ((len (length tool-calls))
         (label (if (> len 1) "Tools" "Tool"))
         (summaries
          (mapcar
           (lambda (tool-call)
             (let* ((tool-spec (car tool-call))
                    (arg-plist (cadr tool-call))
                    (name (gptel-tool-name tool-spec))
                    (arg-values (gptel--map-tool-args tool-spec arg-plist))
                    (first-arg (car arg-values)))
               (concat
                (propertize name 'face 'font-lock-keyword-face)
                (when first-arg
                  (concat
                   " "
                   (propertize
                    (prin1-to-string
                     (truncate-string-to-width
                      (if (stringp first-arg)
                          (replace-regexp-in-string "\n" "⮐" first-arg)
                        (format "%S" first-arg))
                      50 nil nil t))
                    'face 'font-lock-constant-face))))))
           tool-calls)))
    (concat (propertize (concat label ": ") 'face 'font-lock-type-face)
            (mapconcat #'identity summaries ", "))))

(defun gptel-agent-control--insert-entry (entry)
  "Insert a formatted display of ENTRY into the current buffer.

ENTRY is a plist from `gptel-agent-control--entries'.
Records the buffer region in the entry's :region slot.
Point should be at the insertion position."
  (let ((start (point))
        (agent-type (plist-get entry :agent-type))
        (tool-calls (plist-get entry :tool-calls))
        (buf (plist-get entry :buffer))
        (id (plist-get entry :id))
        (timestamp (plist-get entry :timestamp)))
    ;; Header line with agent type
    (insert (propertize "─── " 'face 'shadow))
    (insert (propertize (concat "Agent: "
                                (or agent-type "unknown"))
                        'face '(:inherit font-lock-function-name-face
                                         :weight bold)))
    (when (and buf (buffer-live-p buf))
      (insert (propertize (concat " in " (buffer-name buf))
                          'face 'shadow)))
    (insert " ")
    (insert (propertize (make-string (max 1 (- 60 (current-column))) ?─)
                        'face 'shadow))
    (insert "\n")
    ;; Tool call summary
    (insert (gptel-agent-control--format-tool-summary tool-calls))
    (insert "\n")
    ;; Timestamp
    (when timestamp
      (insert (propertize (format-time-string "  [%H:%M:%S]" timestamp)
                          'face 'shadow)
              "\n"))
    ;; Action buttons
    (insert "  ")
    (insert-text-button "[Accept]"
                        'action (lambda (_btn)
                                  (gptel-agent-control--accept-entry id))
                        'face 'success
                        'follow-link t
                        'help-echo "Accept this tool request")
    (insert " ")
    (insert-text-button "[Reject]"
                        'action (lambda (_btn)
                                  (gptel-agent-control--reject-entry id))
                        'face 'error
                        'follow-link t
                        'help-echo "Reject this tool request")
    (insert " ")
    (insert-text-button "[Jump]"
                        'action (lambda (_btn)
                                  (gptel-agent-control--jump-to-entry id))
                        'face 'link
                        'follow-link t
                        'help-echo "Jump to the agent's buffer")
    (insert "\n\n")
    ;; Record region and set text properties for entry identification
    (let ((end (point)))
      (plist-put entry :region (cons start end))
      (put-text-property start end 'gptel-control-id id))))

(defun gptel-agent-control--insert-footer ()
  "Insert the footer with key bindings help."
  (insert (propertize
           "Keys: a=accept  r=reject  A=accept-all  R=reject-all  RET=jump  n/p=navigate  q=quit\n"
           'face 'font-lock-comment-face)))

(defun gptel-agent-control--render ()
  "Render the full control buffer contents from `gptel-agent-control--entries'."
  (let ((inhibit-read-only t))
    (erase-buffer)
    (if (null gptel-agent-control--entries)
        (insert (propertize "No pending tool requests.\n" 'face 'shadow))
      (dolist (entry gptel-agent-control--entries)
        (gptel-agent-control--insert-entry entry)))
    (gptel-agent-control--insert-footer)
    (goto-char (point-min))))


;;; Entry management

(defun gptel-agent-control--generate-id ()
  "Generate a unique entry ID."
  (cl-incf gptel-agent-control--id-counter))

(defun gptel-agent-control--add-request (tool-calls info &optional agent-type)
  "Add a tool request to the control buffer.

TOOL-CALLS is a list of (tool-spec arg-plist callback) triplets,
the same format used by `gptel--display-tool-calls'.

INFO is the FSM info plist for the request.

AGENT-TYPE is an optional string identifying the agent (e.g.,
\"researcher@main@agent\").  When nil, it is inferred from the
buffer name.

Creates an entry in `gptel-agent-control--entries', inserts a
formatted section in the control buffer, and auto-shows the control
buffer when the first request arrives."
  (let* ((id (gptel-agent-control--generate-id))
         (buffer (plist-get info :buffer))
         (entry (list :id id
                      :tool-calls tool-calls
                      :info info
                      :overlay nil
                      :agent-type (or agent-type
                                      (and buffer (buffer-live-p buffer)
                                           (buffer-name buffer))
                                      "unknown")
                      :buffer buffer
                      :timestamp (float-time)
                      :region nil)))
    (with-current-buffer (gptel-agent-control--get-buffer)
      ;; Append entry (newest last)
      (setq gptel-agent-control--entries
            (nconc gptel-agent-control--entries (list entry)))
      ;; Re-render the buffer
      (gptel-agent-control--render))
    ;; Show the buffer
    (gptel-agent-control--show-buffer)
    id))

(defun gptel-agent-control--find-entry (id)
  "Find the entry with ID in the control buffer's entries list.

Returns the entry plist, or nil if not found."
  (let ((buf (get-buffer gptel-agent-control--buffer-name)))
    (when (buffer-live-p buf)
      (with-current-buffer buf
        (cl-find id gptel-agent-control--entries
                 :key (lambda (e) (plist-get e :id)))))))

(defun gptel-agent-control--remove-entry (id)
  "Remove the entry with ID from the control buffer.

Updates the display after removal."
  (let ((buf (get-buffer gptel-agent-control--buffer-name)))
    (when (buffer-live-p buf)
      (with-current-buffer buf
        (setq gptel-agent-control--entries
              (cl-remove id gptel-agent-control--entries
                         :key (lambda (e) (plist-get e :id))))
        (gptel-agent-control--render)
        (gptel-agent-control--maybe-hide-buffer)))))

(defun gptel-agent-control--entry-at-point ()
  "Return the entry plist for the tool request at point, or nil."
  (when-let* ((id (get-text-property (point) 'gptel-control-id)))
    (cl-find id gptel-agent-control--entries
             :key (lambda (e) (plist-get e :id)))))


;;; Actions

(defun gptel-agent-control--accept-entry (id)
  "Accept the tool request identified by ID.

Calls `gptel--accept-tool-calls' with the stored tool-calls and
overlay, then removes the entry from the control buffer."
  (when-let* ((entry (gptel-agent-control--find-entry id)))
    (let ((tool-calls (plist-get entry :tool-calls))
          (ov (plist-get entry :overlay)))
      (gptel--accept-tool-calls tool-calls ov)
      (gptel-agent-control--remove-entry id)
      (message "Tool request accepted."))))

(defun gptel-agent-control--reject-entry (id)
  "Reject the tool request identified by ID.

Calls `gptel--reject-tool-calls' with the stored tool-calls and
overlay, then removes the entry from the control buffer."
  (when-let* ((entry (gptel-agent-control--find-entry id)))
    (let ((tool-calls (plist-get entry :tool-calls))
          (ov (plist-get entry :overlay)))
      (gptel--reject-tool-calls tool-calls ov)
      (gptel-agent-control--remove-entry id)
      (message "Tool request rejected."))))

(defun gptel-agent-control--jump-to-entry (id)
  "Jump to the agent buffer for the entry identified by ID."
  (when-let* ((entry (gptel-agent-control--find-entry id))
              (buf (plist-get entry :buffer)))
    (if (buffer-live-p buf)
        (pop-to-buffer buf)
      (message "Agent buffer is no longer available."))))

(defun gptel-agent-control--accept-at-point ()
  "Accept the tool request at point."
  (interactive)
  (if-let* ((entry (gptel-agent-control--entry-at-point)))
      (gptel-agent-control--accept-entry (plist-get entry :id))
    (user-error "No tool request at point")))

(defun gptel-agent-control--reject-at-point ()
  "Reject the tool request at point."
  (interactive)
  (if-let* ((entry (gptel-agent-control--entry-at-point)))
      (gptel-agent-control--reject-entry (plist-get entry :id))
    (user-error "No tool request at point")))


(defun gptel-agent-control--jump-to-buffer ()
  "Jump to the agent's buffer for the tool request at point."
  (interactive)
  (if-let* ((entry (gptel-agent-control--entry-at-point)))
      (gptel-agent-control--jump-to-entry (plist-get entry :id))
    (user-error "No tool request at point")))

(defun gptel-agent-control--accept-all ()
  "Accept all pending tool requests."
  (interactive)
  (let ((buf (get-buffer gptel-agent-control--buffer-name)))
    (unless (and buf (buffer-live-p buf))
      (user-error "No control buffer"))
    (with-current-buffer buf
      (let ((entries (copy-sequence gptel-agent-control--entries)))
        (if (null entries)
            (message "No pending tool requests.")
          (when (yes-or-no-p
                 (format "Accept all %d pending tool request(s)? "
                         (length entries)))
            (dolist (entry entries)
              (let ((tool-calls (plist-get entry :tool-calls))
                    (ov (plist-get entry :overlay)))
                (gptel--accept-tool-calls tool-calls ov)))
            (setq gptel-agent-control--entries nil)
            (gptel-agent-control--render)
            (gptel-agent-control--maybe-hide-buffer)
            (message "All tool requests accepted.")))))))

(defun gptel-agent-control--reject-all ()
  "Reject all pending tool requests."
  (interactive)
  (let ((buf (get-buffer gptel-agent-control--buffer-name)))
    (unless (and buf (buffer-live-p buf))
      (user-error "No control buffer"))
    (with-current-buffer buf
      (let ((entries (copy-sequence gptel-agent-control--entries)))
        (if (null entries)
            (message "No pending tool requests.")
          (when (yes-or-no-p
                 (format "Reject all %d pending tool request(s)? "
                         (length entries)))
            (dolist (entry entries)
              (let ((tool-calls (plist-get entry :tool-calls))
                    (ov (plist-get entry :overlay)))
                (gptel--reject-tool-calls tool-calls ov)))
            (setq gptel-agent-control--entries nil)
            (gptel-agent-control--render)
            (gptel-agent-control--maybe-hide-buffer)
            (message "All tool requests rejected.")))))))


;;; Navigation

(defun gptel-agent-control--next-entry ()
  "Move point to the next tool request entry."
  (interactive)
  (let ((current-id (get-text-property (point) 'gptel-control-id))
        (next-pos nil))
    ;; If we're inside an entry, first move past it
    (when current-id
      (setq next-pos (next-single-property-change (point) 'gptel-control-id)))
    (unless next-pos
      (setq next-pos (point)))
    ;; Now find the next region that has a gptel-control-id
    (let ((pos next-pos))
      (while (and (< pos (point-max))
                  (null (get-text-property pos 'gptel-control-id)))
        (setq pos (or (next-single-property-change pos 'gptel-control-id)
                      (point-max))))
      (if (< pos (point-max))
          (goto-char pos)
        (message "No more entries.")))))

(defun gptel-agent-control--prev-entry ()
  "Move point to the previous tool request entry."
  (interactive)
  (let ((current-id (get-text-property (point) 'gptel-control-id))
        (prev-pos nil))
    ;; If we're inside an entry, go to its start
    (when current-id
      (setq prev-pos (previous-single-property-change (point) 'gptel-control-id)))
    (unless prev-pos
      (setq prev-pos (point)))
    ;; Now search backward for a region with a different gptel-control-id
    (let ((pos (max (point-min) (1- prev-pos))))
      (while (and (> pos (point-min))
                  (null (get-text-property pos 'gptel-control-id)))
        (setq pos (or (previous-single-property-change pos 'gptel-control-id)
                      (point-min))))
      ;; Go to the beginning of that entry
      (when (get-text-property pos 'gptel-control-id)
        (let ((start (or (previous-single-property-change pos 'gptel-control-id)
                         (point-min))))
          ;; If start itself doesn't have the property, move forward
          (if (get-text-property start 'gptel-control-id)
              (goto-char start)
            (goto-char (or (next-single-property-change start 'gptel-control-id)
                           pos))))))
    (when (= (point) (point-min))
      (message "No previous entries."))))


;;; Refresh

(defun gptel-agent-control--refresh ()
  "Refresh the control buffer display.

Re-renders the buffer from the entries list.  Stale entries whose
agent buffer is no longer live are removed."
  (interactive)
  (let ((buf (get-buffer gptel-agent-control--buffer-name)))
    (when (buffer-live-p buf)
      (with-current-buffer buf
        ;; Remove stale entries whose buffer has been killed
        (setq gptel-agent-control--entries
              (cl-remove-if
               (lambda (entry)
                 (let ((agent-buf (plist-get entry :buffer)))
                   (and agent-buf (not (buffer-live-p agent-buf)))))
               gptel-agent-control--entries))
        (gptel-agent-control--render)
        (gptel-agent-control--maybe-hide-buffer)))))

(defun gptel-agent-control--revert (_ignore-auto _noconfirm)
  "Revert function for the control buffer.

Calls `gptel-agent-control--refresh'."
  (gptel-agent-control--refresh))


;;; Cleanup

(defun gptel-agent-control--cleanup-for-buffer (buffer)
  "Remove all entries associated with BUFFER from the control buffer.

This should be called when an agent is aborted or its buffer is killed,
to prevent stale entries from accumulating.  Pending tool calls for the
removed entries are NOT accepted or rejected -- they are silently
discarded.

BUFFER can be a buffer object or buffer name."
  (let ((buf (get-buffer gptel-agent-control--buffer-name))
        (target (if (bufferp buffer) buffer (get-buffer buffer))))
    (when (and buf (buffer-live-p buf) target)
      (with-current-buffer buf
        (let ((before-count (length gptel-agent-control--entries)))
          (setq gptel-agent-control--entries
                (cl-remove-if
                 (lambda (entry)
                   (eq (plist-get entry :buffer) target))
                 gptel-agent-control--entries))
          (when (/= before-count (length gptel-agent-control--entries))
            (gptel-agent-control--render)
            (gptel-agent-control--maybe-hide-buffer)))))))


;;; Integration -- alternative tool call display

(defun gptel-agent-control-display-tool-calls (tool-calls info)
  "Display TOOL-CALLS in the centralized control buffer.

This function mirrors the interface of `gptel--display-tool-calls'
and can be used as an alternative display function for tool call
confirmation when agents are running.

TOOL-CALLS is a list of (tool-spec arg-plist callback) triplets.
INFO is the FSM info plist for the request.

The agent type is inferred from the buffer context: if the buffer
is an agent indirect buffer (with a name like `*gptel:...*'), the
agent tag is extracted.  Otherwise the buffer name is used.

This function also creates the standard inline overlay in the agent
buffer (for visual feedback there) and stores its reference in the
control entry."
  (let* ((buffer (plist-get info :buffer))
         ;; Infer agent type from buffer name or agent tag
         (agent-type
          (when (and buffer (buffer-live-p buffer))
            (with-current-buffer buffer
              (or (and (fboundp 'gptel-org-agent--current-agent-tag)
                       (gptel-org-agent--current-agent-tag))
                  (buffer-name buffer)))))
         ;; Create inline overlay in the agent buffer for visual context
         (ov (gptel-agent-control--create-inline-overlay tool-calls info))
         ;; Add to control buffer
         (id (gptel-agent-control--add-request tool-calls info agent-type)))
    ;; Store the overlay reference in the entry
    (when-let* ((entry (gptel-agent-control--find-entry id)))
      (plist-put entry :overlay ov))
    id))

(defun gptel-agent-control--create-inline-overlay (tool-calls info)
  "Create a minimal inline overlay in the agent buffer for TOOL-CALLS.

INFO is the FSM info plist.  The overlay serves as visual feedback
in the agent buffer and is needed by `gptel--accept-tool-calls' and
`gptel--reject-tool-calls' for cleanup.

Returns the overlay, or nil if the buffer is not available."
  (let* ((buffer (plist-get info :buffer))
         (start-marker (plist-get info :position))
         (tracking-marker (plist-get info :tracking-marker)))
    (when (and buffer (buffer-live-p buffer))
      (with-current-buffer buffer
        (let* ((ov-start (save-excursion
                           (goto-char start-marker)
                           (text-property-search-backward 'gptel 'response)
                           (point)))
               (ov-end (or tracking-marker start-marker))
               (ov (or (cdr-safe (get-char-property-and-overlay
                                  start-marker 'gptel-tool))
                       (make-overlay ov-start ov-end nil nil nil))))
          (overlay-put ov 'gptel-tool tool-calls)
          (overlay-put ov 'gptel-info info)
          (overlay-put ov 'keymap gptel-tool-call-actions-map)
          (overlay-put ov 'help-echo "Tool call pending -- see *gptel-control* buffer")
          ov)))))


;;; Interactive entry point

;;;###autoload
(defun gptel-agent-control ()
  "Open the gptel agent control buffer.

If the buffer does not exist, create it.  If it exists, switch to it."
  (interactive)
  (let ((buf (gptel-agent-control--get-buffer)))
    (with-current-buffer buf
      (gptel-agent-control--render))
    (pop-to-buffer buf)))

(provide 'gptel-agent-control)
;;; gptel-agent-control.el ends here
