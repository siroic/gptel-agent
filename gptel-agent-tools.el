;;; gptel-agent-tools.el --- LLM tools for gptel-agent     -*- lexical-binding: t; -*-

;; Copyright (C) 2025  Karthik Chikmagalur

;; Author: Karthik Chikmagalur <karthikchikmagalur@gmail.com>
;; Keywords:

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

;; Adds the following gptel tools.
;; System:
;; - "Bash"           : Execute a Bash command.
;;
;; Web:
;; - "WebSearch"             : Search the web for the first five results to a query.
;; - "Read"               : Fetch and read the contents of a URL.
;; - "YouTube"       : Find the description and video transcript for a youtube video.
;;
;; Filesystem:
;; - "Mkdir"  : Create a new directory.
;; - "Glob"      : Find files matching a glob pattern
;; - "Grep"      : Grep for text in file(s).
;; - "Read" : Read a specific line range from a file.
;; - "Insert"  : Insert text at a specific line number in a file.
;; - "Edit"      : Replace text in file(s) using string match or unified diff.
;; - "Write"      : Create a new file with content.

;;; Code:



(require 'gptel)
(require 'eww)
(require 'url-http)
(eval-when-compile (require 'cl-lib))

(declare-function org-escape-code-in-region "org-src")
(declare-function gptel-agent-read-file "gptel-agent")

;; gptel-org-agent.el (Phase 2 sub-agent subtrees)
(declare-function gptel-org-agent--setup-task-subtree "gptel-org-agent")
(declare-function gptel-org-agent--extract-final-text "gptel-org-agent")
(declare-function gptel-org-agent--close-indirect-buffer "gptel-org-agent")
(declare-function gptel-org-agent--transform-org-instructions "gptel-org-agent")
;; gptel-org-agent.el (Phase 4 TodoWrite org integration)
(declare-function gptel-org-agent--write-todo-org "gptel-org-agent")
(defvar gptel-org-agent-subtrees)

(defvar url-http-end-of-headers)
(defvar gptel-agent--agents)
(defvar gptel-agent--skills)
(defconst gptel-agent--hrule
  (propertize "\n" 'face '(:inherit shadow :underline t :extend t)))

;;; Tool use preview
(defun gptel-agent--confirm-overlay (from to &optional no-hide)
  "Set up tool call preview overlay FROM TO.

If NO-HIDE is non-nil, don't hide the overlay body by default."
  (let ((ov (make-overlay from to nil t)))
    (overlay-put ov 'evaporate t)
    (overlay-put ov 'gptel-agent-tool t)
    (overlay-put ov 'priority 10)
    (overlay-put ov 'keymap
                 (make-composed-keymap
                  (define-keymap
                    "n"     'gptel-agent--next-overlay
                    "p"     'gptel-agent--previous-overlay
                    "q"     'gptel--reject-tool-calls
                    "<tab>" 'gptel-agent--cycle-overlay
                    "TAB"   'gptel-agent--cycle-overlay)
                  gptel-tool-call-actions-map))
    (unless no-hide
      (gptel-agent--cycle-overlay ov))
    ov))

(defun gptel-agent--cycle-overlay (ov)
  "Cycle tool call preview overlay OV at point."
  (interactive (list (cdr (get-char-property-and-overlay
                           (point) 'gptel-agent-tool))))
  (save-excursion
    (goto-char (overlay-start ov))
    (let ((line-end (line-end-position))
          (end      (overlay-end ov)))
      (pcase-let ((`(,value . ,hide-ov)
                   (get-char-property-and-overlay line-end 'invisible)))
        (if (and hide-ov (eq value t))
            (delete-overlay hide-ov)
          (unless hide-ov (setq hide-ov (make-overlay line-end (1- end) nil t)))
          (overlay-put hide-ov 'evaporate t)
          (overlay-put hide-ov 'invisible t)
          (overlay-put hide-ov 'before-string " ▼"))))))

(defun gptel-agent--next-overlay ()
  "Jump to the next `gptel-agent' tool overlay."
  (interactive)
  (when-let* ((ov (cdr (get-char-property-and-overlay
                        (point) 'gptel-agent-tool)))
              (end (overlay-end ov)))
    (when (get-char-property end 'gptel-tool)
      (goto-char end))))

(defun gptel-agent--previous-overlay ()
  "Jump to the previous `gptel-agent' tool overlay."
  (interactive)
  (when-let* ((ov (cdr (get-char-property-and-overlay
                        (1- (point)) 'gptel-agent-tool))))
    (goto-char (overlay-start ov))))

(defsubst gptel-agent--block-bg ()
  "Return a background face suitable for displaying code."
  (cond
   ((derived-mode-p 'org-mode) 'org-block)
   ((derived-mode-p 'markdown-mode) 'markdown-code-face)
   (t `( :background ,(face-attribute 'mode-line-inactive :background)
         :extend t))))

(defun gptel-agent--fontify-block (path-or-mode start end)
  "Fontify region from START to END.

Fontification is assuming it is the contents of file PATH-OR-MODE (if it
is a string), or major-mode (if it is a symbol).  Applied font-lock-face
properties persist through refontification."
  (let ((lang-mode)                     ; (org-src-get-lang-mode lang)
        (org-buffer (current-buffer)))
    (with-temp-buffer
      (insert-buffer-substring-no-properties org-buffer start end)
      (insert " ")                      ; Add space to ensure property change
      (delay-mode-hooks
        (if (symbolp path-or-mode)
            (setq lang-mode path-or-mode)
          (let ((buffer-file-name path-or-mode))
            (setq lang-mode
                  (or (cdr (assoc-string
                            (concat
                             "\\." (file-name-extension path-or-mode) "\\'")
                            auto-mode-alist))
                      (progn (set-auto-mode t) major-mode)))))
        (funcall lang-mode))
      (font-lock-ensure)
      (let ((pos (point-min)))
        (while (< pos (1- (point-max))) ; Skip the added space
          (let* ((next (next-property-change pos nil (1- (point-max))))
                 (face-prop (get-text-property pos 'face)))
            (when face-prop
              (put-text-property
               (+ start (- pos (point-min)))
               (+ start (- (or next (1- (point-max))) (point-min)))
               'font-lock-face face-prop org-buffer))
            (setq pos (or next (1- (point-max))))))))))

;;; System tools
;; "Execute Bash commands to inspect files and system state.

;; This tool provides access to a Bash shell with GNU coreutils (or equivalents)
;; available. You can use any standard Linux commands including: cd, ls, file, cat,
;; grep, awk, sed, head, tail, wc, find, sort, uniq, cut, tr, and more.

;; PURPOSE:
;; - Efficiently inspect files and system state WITHOUT consuming excessive
;; tokens. This is preferred over reading entire large files.
;; - Modify files or system state as appropriate, using cp, mv, rm, patch,
;; git subcommands (git log, commit, branch and more) and so on.

;; BEST PRACTICES:
;; - Use pipes to combine commands: 'cat file.log | grep ERROR | tail -20'
;; - For large files, use head/tail: 'head -50 file.txt' or 'tail -100 file.log'
;; - Use grep with context: 'grep -A 5 -B 5 pattern file.txt'
;; - Check file sizes first: 'wc -l file.txt' before reading
;; - Use file command to identify file types: 'file *'
;; - Combine with other tools: 'find . -name \"*.el\" | head -10'

;; EXAMPLES:
;; - List files with details: 'ls -lah /path/to/dir'
;; - Print lines 25-35 of a long file/stream: 'sed -n \"25,35p\" app.log'
;; - Find recent errors: 'grep -i error /var/log/app.log | tail -20'
;; - Check file type: 'file document.pdf'
;; - Count lines: 'wc -l *.txt'
;; - Search with context: 'grep -A 3 \"function foo\" script.sh'

;; The command will be executed in the current working directory. Output is
;; returned as a string. Long outputs should be filtered/limited using pipes."

;; - Can run commands in background with `run_in_background: true`
;; - Default timeout is 2 minutes (120000ms), max is 10 minutes

(defun gptel-agent--eval-elisp-preview-setup (arg-values _info)
  "Setup preview overlay for Elisp evaluation tool call.

ARG-VALUES is the list of arguments for the tool call."
  (let ((expr (car arg-values))
        (from (point)) (inner-from))
    (insert
     "(" (propertize "Eval" 'font-lock-face 'font-lock-keyword-face)
     ")\n")
    (setq inner-from (point))
    (insert expr)
    (gptel-agent--fontify-block 'emacs-lisp-mode inner-from (point))
    ;; (add-text-properties inner-from (point) '(line-prefix "  " wrap-prefix "  "))
    (insert "\n\n")
    (font-lock-append-text-property
     inner-from (1- (point)) 'font-lock-face (gptel-agent--block-bg))
    (gptel-agent--confirm-overlay from (point) t)))

(defun gptel-agent--execute-bash-preview-setup (arg-values _info)
  "Setup preview overlay for Bash command execution tool call.

ARG-VALUES is the list of arguments for the tool call."
  (pcase-let ((from (point))
              (`(,command ,sudo) arg-values))
    (let ((sudo (and sudo (not (eq sudo :json-false)))))
      (insert
       "(" (propertize "Bash" 'font-lock-face 'font-lock-keyword-face)
       (if sudo
           (propertize " [sudo]" 'font-lock-face 'font-lock-warning-face)
         "")
       ")\n")
      (let ((inner-from (point)))
        (insert command)
        (gptel-agent--fontify-block 'sh-mode inner-from (point))
        (insert "\n\n")
        (font-lock-append-text-property
         inner-from (1- (point)) 'font-lock-face (gptel-agent--block-bg))
        (gptel-agent--confirm-overlay from (point) t)))))

(defvar gptel-agent--bash-file-op-patterns
  (list
   ;; Patterns match the first command in a pipeline or command chain.
   ;; Each pattern is anchored to match at the start of a command.
   (cons (rx bos (* space)
             (or "grep" "egrep" "fgrep" "rg" "ripgrep" "ag" "ack")
             (or space eol))
         "Use the `Grep` tool instead of shell grep/rg/ag.")
   (cons (rx bos (* space)
             "find" (or space eol))
         "Use the `Glob` tool instead of shell find.")
   (cons (rx bos (* space)
             (or "ls" "dir" "tree")
             (or space eol))
         "Use the `Glob` tool (with pattern \"*\") instead of ls/dir/tree.")
   (cons (rx bos (* space)
             (or "cat" "head" "tail" "less" "more" "bat")
             (or space eol))
         "Use the `Read` tool instead of cat/head/tail.")
   (cons (rx bos (* space)
             (or "sed" "awk" "perl -pe" "perl -ne" "perl -i")
             (or space eol))
         "Use the `Edit` tool instead of sed/awk.")
   (cons (rx bos (* space)
             (or "wc" "nl")
             (or space eol))
         "Use the `Read` or `Grep` tool instead of wc/nl."))
  "Alist of (REGEXP . MESSAGE) for shell commands that should use native tools.
Each regexp is matched against the command string.")

(defun gptel-agent--check-bash-file-ops (command)
  "Check if COMMAND uses shell commands that should use native tools.
Returns an error message string if a file operation is detected, nil otherwise."
  (let ((result nil))
    (catch 'found
      (dolist (pattern gptel-agent--bash-file-op-patterns)
        (when (string-match-p (car pattern) command)
          (setq result
                (format "ERROR: %s\n\nDo NOT use Bash for file operations. \
The command `%s` should not be run via Bash.\n%s"
                        (cdr pattern)
                        (car (split-string command " " t))
                        (cdr pattern)))
          (throw 'found t))))
    result))

(defun gptel-agent--execute-bash (callback command &optional sudo)
  "Execute COMMAND in bash and call CALLBACK with output.

CALLBACK is called with the command output string when the process finishes.
COMMAND is the bash command string to execute.
SUDO if non-nil, execute the command as root via TRAMP sudo."
  (let ((file-op-error (gptel-agent--check-bash-file-ops command))
        (sudo (and sudo (not (eq sudo :json-false)))))
    (if file-op-error
        (funcall callback file-op-error)
      (if sudo
        ;; Sudo: use TRAMP sudo path with process-file (synchronous)
        (let ((default-directory "/sudo::/"))
          (condition-case err
              (with-temp-buffer
                (let ((exit-code
                       (process-file "/bin/sh" nil (current-buffer) nil
                                     "-c" command)))
                  (funcall callback
                           (if (zerop exit-code)
                               (buffer-string)
                             (format "Command failed with exit code %d:\nSTDOUT+STDERR:\n%s"
                                     exit-code (buffer-string))))))
            (error
             (funcall callback
                      (format "Error executing sudo command: %S" err)))))
      ;; No sudo: use make-process (truly async)
      (let* ((output-buffer (generate-new-buffer " *gptel-agent-bash*"))
             (proc (make-process
                    :name "gptel-agent-bash"
                    :buffer output-buffer
                    :command (list "bash" "-c" command)
                    :connection-type 'pipe
                    :sentinel
                    (lambda (process _event)
                      (when (memq (process-status process) '(exit signal))
                        (let* ((exit-code (process-exit-status process))
                               (output (with-current-buffer (process-buffer process)
                                         (buffer-string))))
                          (kill-buffer (process-buffer process))
                          (funcall callback
                                   (if (zerop exit-code)
                                       output
                                     (format "Command failed with exit code %d:\nSTDOUT+STDERR:\n%s"
                                             exit-code output)))))))))
        proc)))))

;;; Web tools

(defun gptel-agent--fetch-with-timeout (url url-cb tool-cb failed-msg &rest args)
  "Fetch URL and call URL-CB in the result buffer.

Call TOOL-CB if there is an error or a timeout.  TOOL-CB and ARGS are
passed to URL-CB.  FAILED-MSG is a fragment used for messaging.  Handles
cleanup."
  (let* ((timeout 30) timer done
         (inherit-process-coding-system t)
         (proc-buffer
          (url-retrieve
           url (lambda (status)
                 (setq done t)
                 (when timer (cancel-timer timer))
                 (if-let* ((err (plist-get status :error)))
                     (funcall tool-cb
                              (format "Error: %s failed with error: %S" failed-msg err))
                   (apply url-cb tool-cb args))
                 (kill-buffer (current-buffer)))
           args 'silent)))
    (setq timer
          (run-at-time
           timeout nil
           (lambda (buf cb)
             (unless done
               (setq done t)
               (let ((kill-buffer-query-functions)) (kill-buffer buf))
               (funcall
                cb (format "Error: %s timed out after %d seconds."
                           failed-msg timeout))))
           proc-buffer tool-cb))
    proc-buffer))

;;;; Web search
(defun gptel-agent--shr-next-link ()
  "Jump to the next SHR link in the buffer.  Return jump position."
  (let ((current-prop (get-char-property (point) 'shr-url))
        (next-pos (point)))
    (while (and (not (eobp))
                (setq next-pos
                      (or (next-single-property-change (point) 'shr-url)
                          (point-max)))
                (let ((next-prop (get-char-property next-pos 'shr-url)))
                  (or (equal next-prop current-prop)
                      (equal next-prop nil))))
      (goto-char next-pos))
    (goto-char next-pos)))

(defvar gptel-agent--web-search-active nil)

(defun gptel-agent--web-search-eww (tool-cb query &optional count)
  "Search the web using eww's default search engine (usually DuckDuckGo).

Call TOOL-CB with the results as a string.  QUERY is the search string.
COUNT is the number of results to return (default 5)."
  ;; No more than two active searches at one time
  (setq gptel-agent--web-search-active
        (cl-delete-if-not
         (lambda (buf) (and (buffer-live-p buf)
                       (process-live-p (get-buffer-process buf))))
         gptel-agent--web-search-active))
  (if (>= (length gptel-agent--web-search-active) 2)
      (progn (message "Web search: waiting for turn")
             (run-at-time 5 nil #'gptel-agent--web-search-eww
                          tool-cb query count))
    (push (gptel-agent--fetch-with-timeout
           (concat eww-search-prefix (url-hexify-string query))
           #'gptel-agent--web-search-eww-callback
           tool-cb (format "Web search for \"%s\"" query))
          gptel-agent--web-search-active)))

(defun gptel-agent--web-fix-unreadable ()
  "Replace invalid characters from point to end in current buffer."
  (while (and (skip-chars-forward "\0-\x3fff7f")
              (not (eobp)))
    (display-warning
     '(gptel gptel-agent-tools)
     (format "Invalid character in buffer \"%s\"" (buffer-name)))
    (delete-char 1) (insert "?")))

(defun gptel-agent--web-search-eww-callback (cb)
  "Extract website text and run callback CB with it."
  (let* ((count 5) (results))
    (goto-char (point-min))
    (goto-char url-http-end-of-headers)
    ;; (gptel-agent--web-fix-unreadable)
    (let* ((dom (libxml-parse-html-region (point) (point-max)))
           (result-count 0))
      (eww-score-readability dom)
      ;; (erase-buffer) (buffer-disable-undo)
      (with-temp-buffer
        (shr-insert-document (eww-highest-readability dom))
        (goto-char (point-min))
        (while (and (not (eobp)) (< result-count count))
          (let ((pos (point))
                (url (get-char-property (point) 'shr-url))
                (next-pos (gptel-agent--shr-next-link)))
            (when-let* (((stringp url))
                        (idx (string-search "http" url))
                        (url-fmt (url-unhex-string (substring url idx))))
              (cl-incf result-count)
              (push (concat url-fmt "\n\n"
                            (string-trim
                             (buffer-substring-no-properties pos next-pos))
                            "\n\n----\n")
                    results))))))
    (funcall cb (apply #'concat (nreverse results)))))

;;;; Read URLs
(defun gptel-agent--read-url (tool-cb url)
  "Fetch URL text and call TOOL-CB with it."
  (gptel-agent--fetch-with-timeout
   url
   (lambda (cb)
     (goto-char (point-min)) (forward-paragraph)
     (condition-case errdata
         (let ((dom (libxml-parse-html-region (point) (point-max))))
           (with-temp-buffer
             (eww-score-readability dom)
             (shr-insert-document (eww-highest-readability dom))
             (decode-coding-region (point-min) (point-max) 'utf-8)
             (funcall
              cb (buffer-substring-no-properties
                  (point-min) (point-max)))))
       (error (funcall cb (format "Error: Request failed with error data:\n%S"
                                  errdata)))))
   tool-cb (format "Fetch for \"%s\"" url)))

;;;; Fetch youtube transcript
(defun gptel-agent--yt-parse-captions (xml-string)
  "Parse YouTube caption XML-STRING and return DOM."
  (with-temp-buffer
    (insert xml-string)
    (set-buffer-multibyte t)
    (decode-coding-region (point-min) (point-max) 'utf-8)
    (goto-char (point-min))
    ;; Clean up the XML
    (dolist (reps '(("\n" . " ")
                    ("&amp;" . "&")
                    ("&quot;" . "\"")
                    ("&#39;" . "'")
                    ("&lt;" . "<")
                    ("&gt;" . ">")))
      (save-excursion
        (while (search-forward (car reps) nil t)
          (replace-match (cdr reps) nil t))))
    (libxml-parse-xml-region (point-min) (point-max))))

(defun gptel-agent--yt-format-captions (caption-dom &optional chunk-time)
  "Format CAPTION-DOM as paragraphs with timestamps.

CHUNK-TIME is the number of seconds per paragraph (default 30)."
  (when (and (listp caption-dom)
             (eq (car-safe caption-dom) 'transcript))
    (let ((chunk-time (or chunk-time 30))
          (result "")
          (current-para "")
          (para-start-time 0))
      (dolist (elem (cddr caption-dom)) ;; Process each text element
        (when (and (listp elem) (eq (car elem) 'text))
          (let* ((attrs (cadr elem))
                 (text (caddr elem))
                 (start (string-to-number (cdr (assoc 'start attrs))))
                 ;; Check if we've crossed into a new chunk-time boundary
                 (should-chunk (and (> (abs (- start para-start-time)) 3)
                                    (not (= (floor para-start-time chunk-time)
                                            (floor start chunk-time))))))
            (when (and should-chunk (> (length current-para) 0))
              ;; Add completed paragraph
              (setq result (concat result
                                   (format "[%d:%02d]\n%s\n\n"
                                           (floor para-start-time 60)
                                           (mod para-start-time 60)
                                           (string-trim current-para))))
              (setq current-para "")
              (setq para-start-time start))

            (when text
              (setq current-para (concat current-para " " text))))))

      ;; Add final paragraph
      (when (> (length current-para) 0)
        (setq result (concat result
                             (format "[%d:%02d]\n%s\n\n"
                                     (floor para-start-time 60)
                                     (mod para-start-time 60)
                                     (string-trim current-para)))))
      result)))

(defun gptel-agent--yt-fetch-watch-page (callback video-id)
  "Step 1: Fetch YouTube watch page for VIDEO-ID.

Call CALLBACK with error or proceeds to fetch InnerTube data."
  (url-retrieve
   (format "https://youtube.com/watch?v=%s" video-id)
   (lambda (status callback video-id)
     (if-let ((error (plist-get status :error)))
         (funcall callback (format "Error fetching page: %s" error))
       (goto-char (point-min))
       (search-forward "\n\n" nil t)
       (let* ((html (buffer-substring (point) (point-max)))
              (api-key (and (string-match
                             "\"INNERTUBE_API_KEY\":\"\\([a-zA-Z0-9_-]+\\)"
                             html)
                            (match-string 1 html))))
         (if api-key
             (gptel-agent--yt--fetch-innertube callback video-id api-key)
           (funcall callback "Error: Could not extract API key")))))
   (list callback video-id)))

(defun gptel-agent--yt--fetch-innertube (callback video-id api-key)
  "Step 2: Fetch VIDEO-ID metadata from YouTube InnerTube API.

Call CALLBACK with error or proceeds to fetch captions."
  (let ((url-request-method "POST")
        (url-request-extra-headers
         '(("Content-Type" . "application/json")
           ("Accept-Language" . "en-US")))
        (url-request-data
         (encode-coding-string
          (json-encode
           `((context . ((client . ((clientName . "ANDROID")
                                    (clientVersion . "20.10.38")))))
             (videoId . ,video-id)))
          'utf-8)))
    (url-retrieve
     (format "https://www.youtube.com/youtubei/v1/player?key=%s" api-key)
     (lambda (status callback)
       (if-let ((error (plist-get status :error)))
           (funcall callback (format "Error fetching metadata: %s" error))
         (goto-char (point-min))
         (search-forward "\n\n" nil t)
         (let* ((json-data (ignore-errors
                             (json-parse-buffer :object-type 'plist)))
                (video-details (plist-get json-data :videoDetails))
                (description (plist-get video-details :shortDescription))
                (caption-tracks (map-nested-elt
                                 json-data
                                 '(:captions
                                   :playerCaptionsTracklistRenderer
                                   :captionTracks))))
           (gptel-agent--yt-fetch-captions callback description caption-tracks))))
     (list callback))))

(defun gptel-agent--yt-fetch-captions (callback description caption-tracks)
  "Step 3: Find and fetch English captions for CAPTION-TRACKS.

Call CALLBACK with formatted result containing DESCRIPTION and transcript."
  (if (not caption-tracks)
      (funcall callback
               (format "# Description\n\n%s\n\n# Transcript\n\nNo transcript available."
                       (or description "No description available.")))
    (let ((en-caption
           (cl-find-if
            (lambda (track)
              (string-match-p "^en" (or (plist-get track :languageCode) "")))
            caption-tracks)))
      (if (not en-caption)
          (funcall callback
                   (format "# Description\n\n%s\n\n# Transcript\n\nNo English transcript available."
                           (or description "No description available.")))
        (let ((base-url (replace-regexp-in-string
                         "&fmt=srv3" ""
                         (plist-get en-caption :baseUrl))))
          (url-retrieve
           base-url
           (lambda (status callback description)
             (if-let ((error (plist-get status :error)))
                 (funcall callback
                          (format "# Description\n\n%s\n\n# Transcript\n\nError fetching transcript: %s"
                                  (or description "No description available.")
                                  error))
               (goto-char (point-min))
               (search-forward "\n\n" nil t)
               (let* ((xml-string (buffer-substring (point) (point-max)))
                      (caption-dom (gptel-agent--yt-parse-captions xml-string))
                      (formatted-transcript
                       (gptel-agent--yt-format-captions caption-dom 30)))
                 (funcall callback
                          (format "# Description\n\n%s\n\n# Transcript\n\n%s"
                                  (or description "No description available.")
                                  (or formatted-transcript "Error parsing transcript."))))))
           (list callback description)))))))

(defun gptel-agent--yt-read-url (callback url)
  "Fetch YouTube metadata and transcript for URL, calling CALLBACK with result.
CALLBACK is called with a markdown-formatted string containing the video
description and transcript formatted as timestamped paragraphs."
  (if-let* ((video-id
             (and (string-match
                   (rx bol (opt "http" (opt "s") "://")
                       (opt "www.") "youtu" (or ".be" "be.com") "/"
                       (opt "watch?v=")
                       (group (one-or-more (not (any "?&")))))
                   url)
                  (match-string 1 url))))
      (gptel-agent--yt-fetch-watch-page callback video-id)
    (funcall callback "Error: Invalid YouTube URL")))

;;; Code tools
;;;; Diagnostics from flymake
(declare-function flymake--project-diagnostics "flymake")
(declare-function flymake--diag-beg "flymake")
(declare-function flymake--diag-type "flymake")
(declare-function flymake--diag-text "flymake")
(declare-function flymake-diagnostic-buffer "flymake")

(defun gptel-agent--flymake-diagnostics (&optional all)
  "Collect flymake errors across all open buffers in the current project.

Errors with low severity are not collected.  With ALL, collect all
diagnostics."
  (let ((project (project-current)))
    (unless project
      (error "Not in a project.  Cannot collect flymake diagnostics"))
    (require 'flymake)
    (let ((results '()))
      (dolist (diag (flymake--project-diagnostics project))
        (let ((severity (flymake--diag-type diag)))
          (when (memq severity `(:error :warning ,@(and all '(:note))))
            (with-current-buffer (flymake-diagnostic-buffer diag)
              (let* ((beg (flymake--diag-beg diag))
                     (line-num (line-number-at-pos beg))
                     (line-text (buffer-substring-no-properties
                                 (line-beginning-position) (line-end-position))))
                (push (format "File: %s:%d\nSeverity: %s\nMessage: %s\n---\n%s"
                              (buffer-file-name)
                              line-num
                              severity
                              (flymake--diag-text diag)
                              line-text)
                      results))))))
      (string-join (nreverse results) "\n\n"))))

;;; Filesystem tools
;;;; Make directories
;;;; Writing to files
(defun gptel-agent--edit-files-preview-setup (arg-values _info)
  "Insert tool call preview for ARG-VALUES for \"Edit\" tool."
  (pcase-let ((from (point)) (files-affected) (description)
              (`(,path ,old-str ,new-str-or-diff ,diffp) arg-values))

    (if (and diffp (not (eq diffp :json-false)))
        (progn                          ;Patch
          (insert new-str-or-diff)
          (save-excursion
            (while (re-search-backward "^\\+\\+\\+ \\(.*\\)$" from t)
              (push (match-string 1) files-affected))
            (goto-char from)
            (when (looking-at "^ *```\\(diff\\|patch\\)\\s-*\n")
              (delete-region (match-beginning 0) (match-end 0))))
          (skip-chars-backward " \t\r\n")
          (when (looking-back "^ *```\\s-*\\'" (line-beginning-position))
            (delete-region (line-beginning-position) (line-end-position)))
          (setq description "Patch")
          (require 'diff-mode)
          (gptel-agent--fontify-block 'diff-mode from (point)))
      (when old-str                     ;Text replacement
        (push path files-affected)
        (setq description "ReplaceIn")
        (insert
         (propertize old-str 'font-lock-face 'diff-removed
                     'line-prefix (propertize "-" 'face 'diff-removed))
         "\n" (propertize new-str-or-diff 'font-lock-face 'diff-added
                          'line-prefix (propertize "+" 'face 'diff-added))
         "\n")))
    (insert "\n")
    (font-lock-append-text-property
     from (1- (point)) 'font-lock-face (gptel-agent--block-bg))
    (when (derived-mode-p 'org-mode)
      (org-escape-code-in-region from (1- (point))))
    (save-excursion
      (goto-char from)
      (insert
       "(" (propertize description 'font-lock-face 'font-lock-keyword-face)
       " " (mapconcat (lambda (f) (propertize (concat "\"" f "\"")
                                         'font-lock-face 'font-lock-constant-face))
                      files-affected " ")
       ")\n"))
    (gptel-agent--confirm-overlay from (point) t)))

(defun gptel-agent--fix-patch-headers ()
  "Fix line numbers in hunks in diff at point."
  ;; Find and process each hunk header
  (while (re-search-forward "^@@ -\\([0-9]+\\),\\([0-9]+\\) +\\+\\([0-9]+\\),\\([0-9]+\\) @@" nil t)
    (let ((hunk-start (line-beginning-position))
          (orig-line (string-to-number (match-string 1)))
          (new-line (string-to-number (match-string 3)))
          (orig-count 0)
          (new-count 0))

      ;; Count lines in this hunk until we hit the next @@ or EOF
      (goto-char hunk-start)
      (forward-line 1)
      (save-match-data
        (while (and (not (eobp))
                    (not (looking-at-p "^@@")))
          (cond
           ;; Removed lines (not ---)
           ((looking-at-p "^-[^-]")
            (cl-incf orig-count))
           ;; Added lines (not +++)
           ((looking-at-p "^\\+[^+]")
            (cl-incf new-count))
           ;; Context lines (space at start)
           ((looking-at-p "^ ")
            (cl-incf orig-count)
            (cl-incf new-count)))
          (forward-line 1)))

      ;; Replace the hunk header with corrected counts
      (goto-char hunk-start)
      (delete-line)
      (insert (format "@@ -%d,%d +%d,%d @@\n"
                      orig-line orig-count new-line new-count)))))

;;;; Create a directory
(defun gptel-agent--make-directory (parent name)
  "Create a directory NAME in PARENT directory.

Creates the directory and any missing parent directories.  If the
directory already exists, this is a no-op and returns success.

PARENT is the parent directory path,NAME is the name of the new
directory to create."
  (condition-case errdata
      (progn
        (make-directory (expand-file-name name parent) t)
        (format "Directory %s created/verified in %s" name parent))
    (error "Error creating directory %s in %s:\n%S" name parent errdata)))

(defun gptel-agent--edit-files (path &optional old-str new-str-or-diff diffp)
  "Replace text in file(s) at PATH using either string matching or unified diff.

This function supports two distinct modes of operation:

1. STRING REPLACEMENT MODE (DIFFP is nil or :json-false):
   - Searches for OLD-STR in the file at PATH
   - Replaces it with NEW-STR-OR-DIFF
   - Requires OLD-STR to match exactly once (uniquely) in the file
   - Only works on single files, not directories

2. DIFF/PATCH MODE (when DIFFP is non-nil and not :json-false):
   - Applies NEW-STR-OR-DIFF as a unified diff using the `patch` command
   - Works on both single files and directories
   - OLD-STR is ignored in this mode
   - NEW-STR-OR-DIFF can contain the diff in fenced code blocks
     (=diff or =patch)
   - Uses the -N (--forward) option to ignore already-applied patches

PATH - File or directory path to modify (must be readable)
OLD-STR - (String mode only) Exact text to find and replace
NEW-STR-OR-DIFF - Replacement text (string mode) or unified diff (diff mode)
DIFFP - If non-nil (and not :json-false), use diff/patch mode

Error Conditions:
  - PATH not readable
  - (String mode) PATH is a directory
  - (String mode) OLD-STR not found in file
  - (String mode) OLD-STR matches multiple times (ambiguous)
  - (Diff mode) patch command fails (exit status non-zero)

Returns:
  Success message string describing what was changed

Signals:
  error - On any failure condition (caught and displayed by gptel)"
  (unless (file-readable-p path)
    (error "Error: File or directory %s is not readable" path))

  (unless new-str-or-diff
    (error "Required argument `new_str' missing"))

  (if (or (eq diffp :json-false) old-str)
      ;; Replacement by Text
      (progn
        (when (file-directory-p path)
          (error "Error: String replacement is intended for single files, not directories (%s)"
                 path))
        (with-temp-buffer
          (insert-file-contents path)
          (if (search-forward old-str nil t)
              (if (save-excursion (search-forward old-str nil t))
                  (error "Error: Match is not unique.\
Consider providing more context for the replacement, or a unified diff")
                ;; TODO: More robust backspace escaping
                (replace-match (string-replace  "\\" "\\\\" new-str-or-diff))
                (write-region nil nil path)
                (format "Successfully replaced %s (truncated) with %s (truncated)"
                        (truncate-string-to-width old-str 20 nil nil t)
                        (truncate-string-to-width new-str-or-diff 20 nil nil t)))
            (error "Error: Could not find old_str \"%s\" in file %s"
                   (truncate-string-to-width old-str 20) path))))
    ;; Replacement by Diff
    (unless (executable-find "patch")
      (error "Error: Command \"patch\" not available, cannot apply diffs.\
Use string replacement instead"))
    (let* ((out-buf-name (generate-new-buffer-name "*patch-stdout*"))
           ;; (err-buf-name (generate-new-buffer-name "*patch-stderr*"))
           (target-file (expand-file-name path))
           (exit-status -1)             ; Initialize to a known non-zero value
           (result-output "")
           ;; (result-error "")
           )
      (unwind-protect
          (let ((default-directory (file-name-directory (expand-file-name path)))
                (patch-options    '("--forward" "--verbose")))

            (with-temp-message
                (format "Applying diff to: `%s` with options: %s"
                        target-file patch-options)
              (with-temp-buffer
                (insert new-str-or-diff)
                ;; Insert trailing newline, required by patch
                (unless (eq (char-before (point-max)) ?\n)
                  (goto-char (point-max))
                  (insert "\n"))
                (goto-char (point-min))
                ;; Remove code fences, if present
                (when (looking-at-p "^ *```diff\n")
                  (save-excursion
                    (delete-line)
                    (goto-char (point-max))
                    (forward-line -1)   ;guaranteed to be at a blank newline
                    (when (looking-at-p "^ *```") (delete-line))))
                ;; Fix line numbers in hunk headers
                (gptel-agent--fix-patch-headers)

                (setq exit-status
                      (apply #'call-process-region (point-min) (point-max)
                             "patch" nil (list out-buf-name t) ; stdout/stderr buffer names
                             nil patch-options))))

            ;; Retrieve content from buffers using their names
            (when-let* ((stdout-buf (get-buffer out-buf-name)))
              (when (buffer-live-p stdout-buf)
                (with-current-buffer stdout-buf
                  (setq result-output (buffer-string)))))

            (if (= exit-status 0)
                (format "Diff successfully applied to %s.
Patch command options: %s
Patch STDOUT:\n%s"
                        target-file patch-options result-output)
              ;; Signal an Elisp error, which gptel will catch and display.
              ;; The arguments to 'error' become the error message.
              (error "Error: Failed to apply diff to %s (exit status %s).
Patch command options: %s
Patch STDOUT:\n%s"
                     target-file exit-status patch-options
                     result-output)))
        (let ((stdout-buf-obj (get-buffer out-buf-name))) ;Clean up
          (when (buffer-live-p stdout-buf-obj) (kill-buffer stdout-buf-obj)))))))

(defun gptel-agent--insert-in-file-preview-setup (arg-values _info)
  "Preview setup for Insert.
INFO is the tool call info plist.
ARG-VALUES is a list: (path line-number new-str)"
  (let ((from (point)) (line-offset)
        (face-bg (gptel-agent--block-bg))
        (cb (current-buffer)))
    (pcase-let ((`(,path ,line-number ,new-str) arg-values))
      (insert "("
              (propertize "insert_into_file " 'font-lock-face 'font-lock-keyword-face)
              (propertize (concat "\"" path "\"")
                          'font-lock-face 'font-lock-constant-face)
              ")\n")
      (if (file-readable-p path)
          (insert
           (with-temp-buffer       ;NEW-STR with context lines, styled as a diff
             (insert-file-contents path)
             (pcase line-number
               (-1 (goto-char (point-max)))
               (_ (forward-line line-number)))
             (save-excursion
               (forward-line -6)
               (setq line-offset (line-number-at-pos))
               (delete-region (point-min) (point))
               (dotimes (_ 12)
                 (put-text-property
                  (line-beginning-position) (line-end-position)
                  'line-prefix (propertize (format "%4d " line-offset) 'face
                                           `(:inherit ,face-bg :inherit line-number)))
                 (forward-line 1) (when (eolp) (insert " "))
                 (cl-incf line-offset)))
             (insert (propertize new-str 'font-lock-face 'diff-added
                                 'fontified t 'font-lock-multiline t
                                 'line-prefix (propertize "   + " 'face 'diff-added)))
             (save-excursion
               (forward-line 6)
               (delete-region (point) (point-max)))
             (font-lock-append-text-property
              (point-min) (point-max) 'font-lock-face face-bg)
             (when (provided-mode-derived-p
                    (buffer-local-value 'major-mode cb) 'org-mode)
               (org-escape-code-in-region (point-min) (point-max)))
             (buffer-string)) "\n")
        (insert (propertize "[File not readable]\n\n" 'font-lock-face 'error)))
      (gptel-agent--confirm-overlay from (point)))))

(defun gptel-agent--insert-in-file (path line-number new-str)
  "Insert NEW-STR at LINE-NUMBER in file at PATH.

LINE-NUMBER conventions:
- 0 inserts at the beginning of the file
- -1 inserts at the end of the file
- N > 1 inserts before line N"
  (unless (file-readable-p path)
    (error "Error: File %s is not readable" path))

  (when (file-directory-p path)
    (error "Error: Cannot insert into directory %s" path))

  (with-temp-buffer
    (insert-file-contents path)

    (pcase line-number
      (0 (goto-char (point-min)))       ; Insert at the beginning
      (-1 (goto-char (point-max)))      ; Insert at the end
      (_ (goto-char (point-min))
         (forward-line line-number)))   ; Insert before line N

    ;; Insert the new string
    (insert new-str)

    ;; Ensure there's a newline after the inserted text if not already present
    (unless (or (string-suffix-p "\n" new-str) (eobp))
      (insert "\n"))

    ;; Write the modified content back to the file
    (write-region nil nil path)

    (format "Successfully inserted text at line %d in %s" line-number path)))

(defun gptel-agent--write-file-preview-setup (arg-values _info)
  "Setup preview overlay for Write file tool call.

ARG-VALUES is the list of arguments for the tool call."
  (pcase-let ((from (point))
              (`(,path ,filename ,content) arg-values))
    (insert
     "(" (propertize "Write " 'font-lock-face 'font-lock-keyword-face)
     (propertize (prin1-to-string path) 'font-lock-face 'font-lock-constant-face) " "
     (propertize (prin1-to-string filename) 'font-lock-face 'font-lock-constant-face)
     ")\n")
    (let ((inner-from (point)))
      (insert content)
      (gptel-agent--fontify-block filename inner-from (point))
      (insert "\n\n")
      (font-lock-append-text-property
       inner-from (1- (point)) 'font-lock-face (gptel-agent--block-bg))
      (when (derived-mode-p 'org-mode)
        (org-escape-code-in-region inner-from (1- (point)))))
    (gptel-agent--confirm-overlay from (point))))

;;;; Write content to a file
(defun gptel-agent--write-file (path filename content)
  "Write CONTENT to FILENAME in PATH.

PATH and FILENAME are expanded to create the full path.  CONTENT is
written to the file.  Returns a success message string, or signals an
error if writing fails.

PATH, FILENAME, and CONTENT must all be strings."
  (unless (and (stringp path) (stringp filename) (stringp content))
    (error "PATH, FILENAME or CONTENT is not a string, cancelling action"))
  (let ((full-path (expand-file-name filename path)))
    (condition-case errdata
        (with-temp-buffer
          (insert content)
          ;; Use write-region instead of write-file: write-file calls
          ;; set-visited-file-name which triggers set-auto-mode and
          ;; find-file-hook, causing LSP to activate for the file's mode.
          (write-region (point-min) (point-max) full-path)
          (format "Created file %s in %s" filename path))
      (error "Error: Could not write file %s:\n%S" path errdata))))

;;;; Find files using regexes
(defun gptel-agent--glob (pattern &optional path depth)
  "Find files matching PATTERN using the `tree' command.

PATTERN is a case-insensitive regex pattern to match filenames against.
PATH is the optional directory to search (defaults to current directory).
DEPTH limits recursion depth when provided (non-negative integer).

Returns a string listing matching files with full paths, sorted by
modification time.  If the output is too large (>20000 chars), it writes
the full results to a temporary file and returns a truncated version with
instructions to use `Read' for the full contents.

Raises an error if PATTERN is empty, PATH is not readable, or the
`tree' executable is not found."
  (when (string-empty-p pattern)
    (error "Error: pattern must not be empty"))
  (if path
      (unless (and (file-readable-p path) (file-directory-p path))
        (error "Error: path %s is not readable" path))
    (setq path "."))
  (unless (executable-find "tree")
    (error "Error: Executable `tree` not found.  This tool cannot be used"))
  (let ((full-path (expand-file-name path)))
    (with-temp-buffer
      (let* ((args (list "-l" "-f" "-i" "-I" ".git"
                         "--sort=mtime" "--ignore-case"
                         "--prune" "-P" pattern full-path))
             (args (if (natnump depth)
                       (nconc args (list "-L" (number-to-string depth)))
                     args))
             (exit-code (apply #'call-process "tree" nil t nil args)))
        (when (/= exit-code 0)
          (goto-char (point-min))
          (insert (format "Glob failed with exit code %d\n.STDOUT:\n\n"
                          exit-code))))
      (when (> (buffer-size) 20000)
        ;; Too large - save to temp file and return truncated info
        (let* ((temp-dir (expand-file-name "gptel-agent-temp"
                                           (temporary-file-directory)))
               (temp-file (expand-file-name
                           (format "glob-%s-%s.txt"
                                   (format-time-string "%Y%m%d-%H%M%S")
                                   (random 10000))
                           temp-dir)))
          (unless (file-directory-p temp-dir) (make-directory temp-dir t))
          (write-region nil nil temp-file)
          (let ((max-lines 50)
                (orig-size (buffer-size))
                (orig-lines (line-number-at-pos (point-max))))
            ;; Insert header
            (goto-char (point-min))
            (insert (format "Glob results too large (%d chars, %d lines)\
 for context window.\nStored in: %s\n\nFirst %d lines:\n\n"
                            orig-size orig-lines temp-file max-lines))
            ;; Truncate to first max-lines lines
            (forward-line max-lines)
            (delete-region (point) (point-max))
            ;; Insert footer
            (goto-char (point-max))
            (insert (format "\n\n[Use Read tool with file_path=\"%s\" to view full results]"
                            temp-file)))))
      (buffer-string))))

;;;; Read files or directories
(defun gptel-agent--read-file-lines (filename start-line end-line)
  "Return lines START-LINE to END-LINE fom FILENAME."
  (unless (file-readable-p filename)
    (error "Error: File %s is not readable" filename))

  (when (file-directory-p filename)
    (error "Error: Cannot read directory %s as file" filename))

  (when (file-symlink-p filename)
    (setq filename (file-truename filename)))

  (if (and (not start-line) (not end-line)) ;read full file
      (if (> (file-attribute-size (file-attributes filename))
             (* 512 1024))
          (error "Error: File is too large (> 512 KB).\
Please specify a line range to read")
        (with-temp-buffer
          (insert-file-contents filename)
          (buffer-string)))
    ;; TODO: Handle nil start-line OR nil end-line
    (cl-decf start-line)
    (let* ((file-size (nth 7 (file-attributes filename)))
           (chunk-size (min file-size (* 512 1024)))
           (byte-offset 0) (line-offset (- end-line start-line)))
      (with-temp-buffer
        ;; Go to start-line
        (while (and (> start-line 0)
                    (< byte-offset file-size))
          (insert-file-contents
           filename nil byte-offset (+ byte-offset chunk-size))
          (setq byte-offset (+ byte-offset chunk-size))
          (setq start-line (forward-line start-line))
          (when (eobp)
            (if (/= (line-beginning-position) (line-end-position))
                ;; forward-line counted 1 extra line
                (cl-incf start-line))
            (delete-region (point-min) (line-beginning-position))))

        (delete-region (point-min) (point))

        ;; Go to end-line, forward by line-offset
        (cl-block nil
          (while (> line-offset 0)
            (setq line-offset (forward-line line-offset))
            (when (and (eobp) (/= (line-beginning-position) (line-end-position)))
              ;; forward-line counted 1 extra line
              (cl-incf line-offset))
            (if (= line-offset 0)
                (delete-region (point) (point-max))
              (if (>= byte-offset file-size)
                  (cl-return)
                (insert-file-contents
                 filename nil byte-offset (+ byte-offset chunk-size))
                (setq byte-offset (+ byte-offset chunk-size))))))

        (buffer-string)))))

(defun gptel-agent--grep (regex path &optional glob context-lines)
  "Search for REGEX in file or directory at PATH using ripgrep.

REGEX is a PCRE-format regular expression to search for.
PATH can be a file or directory to search in.

Optional arguments:
GLOB restricts the search to files matching the glob pattern.
  Examples: \"*.el\", \"*.md\", \"*.rs\"
CONTEXT-LINES specifies the number of lines of context to show
  around each match (0-15 inclusive, defaults to 0).

Returns a string containing matches grouped by file, with line numbers
and optional context.  Results are limited to 1000 or fewer matches per
file.  Results are sorted by modification time."
  (unless (file-readable-p path)
    (error "Error: File or directory %s is not readable" path))
  (let ((grepper (or (executable-find "rg") (executable-find "grep"))))
    (unless grepper
      (error "Error: ripgrep/grep not available, this tool cannot be used"))
    (with-temp-buffer
      (let* ((cmd (file-name-sans-extension (file-name-nondirectory grepper)))
             (args
              (cond
               ((string= "rg" cmd)
                (delq nil (list "--sort=modified"
                                (and (natnump context-lines)
                                     (format "--context=%d" context-lines))
                                (and glob (format "--glob=%s" glob))
                                ;; "--files-with-matches"
                                "--max-count=1000"
                                "--heading" "--line-number" "-e" regex
                                (expand-file-name (substitute-in-file-name path)))))
               ((string= "grep" cmd)
                (delq nil (list "--recursive"
                                (and (natnump context-lines)
                                     (format "--context=%d" context-lines))
                                (and glob (format "--include=%s" glob))
                                "--max-count=1000"
                                "--line-number" "--regexp" regex
                                (expand-file-name (substitute-in-file-name path)))))
               (t (error "Error: failed to identify grepper"))))
             (exit-code (apply #'call-process grepper nil '(t t) nil args)))
        (when (/= exit-code 0)
          (goto-char (point-min))
          (insert (format "Error: search failed with exit-code %d.  Tool output:\n\n" exit-code)))
        (buffer-string)))))

;;; Todo-write tool (task tracking)
(defvar-local gptel-agent--todos nil)

(defun gptel-agent-toggle-todos ()
  "Toggle the display of the gptel agent todo list."
  (interactive)
  (pcase-let ((`(,prop-value . ,ov)
               (or (get-char-property-and-overlay (point) 'gptel-agent--todos)
                   (get-char-property-and-overlay
                    (previous-single-char-property-change
                     (point) 'gptel-agent--todos nil (point-min))
                    'gptel-agent--todos))))
    (if-let* ((fmt (overlay-get ov 'after-string)))
        (progn (overlay-put ov 'gptel-agent--todos fmt)
               (overlay-put ov 'after-string nil))
      (overlay-put ov 'after-string
                   (and (stringp prop-value) prop-value))
      (overlay-put ov 'gptel-agent--todos t))))

(defun gptel-agent--write-todo (todos)
  "Display a formatted task list in the buffer.

TODOS is a list of plists with keys :content, :activeForm, and :status.
Completed items are displayed with strikethrough and shadow face.
Exactly one item should have status \"in_progress\".

When `gptel-org-agent-subtrees' is enabled and we're in an org-mode
buffer, delegates to `gptel-org-agent--write-todo-org' which creates
org TODO headings instead of overlays."
  (setq gptel-agent--todos todos)
  ;; Dispatch: org headings when subtrees are active, overlays otherwise
  (message "[gptel-todo] write-todo dispatch: subtrees=%S org-mode=%S write-fn=%S buf=%S"
           (bound-and-true-p gptel-org-agent-subtrees)
           (derived-mode-p 'org-mode)
           (fboundp 'gptel-org-agent--write-todo-org)
           (buffer-name))
  (if (and (bound-and-true-p gptel-org-agent-subtrees)
           (derived-mode-p 'org-mode)
           (fboundp 'gptel-org-agent--write-todo-org))
      (gptel-org-agent--write-todo-org todos)
    ;; Fallback: overlay-based display for non-org or non-subtree buffers
    (let* ((info (gptel-fsm-info gptel--fsm-last))
           (pos (plist-get info :position))
           (where-from
            (and pos (previous-single-property-change
                      pos 'gptel nil (point-min))))
           (where-to pos))
      (unless (or (null where-from) (null where-to) (= where-from where-to))
        (pcase-let ((`(,_ . ,todo-ov)
                     (get-char-property-and-overlay where-from 'gptel-agent--todos)))
          (if todo-ov
              ;; Move if reusing an old overlay and the text has changed.
              (move-overlay todo-ov where-from where-to)
            (setq todo-ov (make-overlay where-from where-to nil t))
            (overlay-put todo-ov 'gptel-agent--todos t)
            (overlay-put todo-ov 'evaporate t)
            (overlay-put todo-ov 'priority -40)
            (overlay-put todo-ov 'keymap (define-keymap
                                           "C-c g t" #'gptel-agent-toggle-todos))
            (plist-put
             info :post            ; Don't use push, see note in gptel-anthropic
             (cons (lambda (&rest _)    ; Clean up header line and todo overlay after tasks are done
                     (when (and gptel-mode gptel-use-header-line header-line-format)
                       (setf (nth 2 header-line-format) gptel--header-line-info))
                     (when (overlayp todo-ov) (delete-overlay todo-ov)))
                   (plist-get info :post))))
          (let* ((formatted-todos       ; Format the todo list
                  (mapconcat
                   (lambda (todo)
                     (pcase (plist-get todo :status)
                       ("completed"
                        (concat "✓ " (propertize (plist-get todo :content)
                                                 'face '(:inherit shadow :strike-through t))))
                       ("in_progress"
                        (concat "● " (propertize (plist-get todo :activeForm)
                                                 'face '(:inherit bold :inherit warning))))
                       (_ (concat "○ " (plist-get todo :content)))))
                   todos "\n"))
                 (in-progress
                  (cl-loop for todo across todos
                           when (equal (plist-get todo :status) "in_progress")
                           return (plist-get todo :activeForm)))
                 (todo-display
                  (concat
                   (unless (= (char-before (overlay-end todo-ov)) 10) "\n")
                   gptel-agent--hrule
                   (propertize "Task list: [ "
                               'face '(:inherit font-lock-comment-face :inherit bold))
                   (save-excursion
                     (goto-char (1- (overlay-end todo-ov)))
                     (propertize (substitute-command-keys "\\[gptel-agent-toggle-todos]")
                                 'face 'help-key-binding))
                   (propertize " to toggle display ]\n" 'face 'font-lock-comment-face)
                   formatted-todos "\n"
                   gptel-agent--hrule)))
            (overlay-put todo-ov 'after-string todo-display)
            (when (and gptel-mode gptel-use-header-line in-progress header-line-format)
              (setf (nth 2 header-line-format)
                    (concat (propertize
                             " " 'display
                             `(space :align-to (- right ,(+ 5 (length in-progress)))))
                            (propertize (concat "Task: " in-progress)
                                        'face 'font-lock-escape-face))))))))
    t))

;;; Skill tool
(defun gptel-agent--get-skill (skill &optional _args)
  "Return the details of the SKILL.

This loads the body of the corresponding SKILL.  When using this as a
tool in gptel, make sure the known skills are added to the context
window.  `gptel-agent--skills-system-message' can be used to generate
the known skills as string ready to be included to the context."
  (let ((skill-dir
         (car-safe
          (alist-get skill gptel-agent--skills nil nil #'string-equal))))
    (if (not skill-dir)
        (format "Error: skill %s not found." skill)
      (let* ((skill-dir-expanded (expand-file-name skill-dir))
             (skill-files
              (mapcar
               (lambda (full-path)
                 (cons (file-relative-name full-path skill-dir-expanded)
                       full-path))
               (directory-files-recursively skill-dir-expanded ".*")))
             (body (plist-get
                    (cdr (gptel-agent-read-file
                          (expand-file-name "SKILL.md" skill-dir)))
                    :system)))
        (if body
            (let (start)
              (with-temp-buffer
                (insert "## Skill: " skill
                        "\n- base dir: " skill-dir-expanded "\n")
                (setq start (point))
                (insert body)
                (pcase-dolist (`(,rel-path . ,full-path) skill-files)
                  (unless (string-match-p "SKILL\\.md" rel-path)
                    (goto-char start)
                    (while (search-forward-regexp (regexp-quote rel-path) nil t)
                      (replace-match full-path t t))))
                (buffer-string)))
          (format "Could not load body of skill %s" skill))))))

;;; Task tool (sub-agent)
(defvar gptel-agent-request--handlers
  `((WAIT ,#'gptel-agent--indicate-wait
          ,#'gptel--handle-wait)
    (TPRE ,#'gptel--handle-pre-tool ,#'gptel--fsm-transition)
    (TOOL ,#'gptel-agent--indicate-tool-call
          ,#'gptel--handle-tool-use)
    (TRET ,#'gptel--handle-post-tool
          ,#'gptel--handle-tool-result))
  "See `gptel-request--handlers'.")

;;; Sub-agent subtree handlers (Phase 2)
;;
;; When `gptel-org-agent-subtrees' is enabled, sub-agent tasks use
;; buffer-writing (like `gptel-send') instead of callback-based string
;; accumulation.  The LLM response is streamed directly into an indirect
;; buffer narrowed to the agent's subtree.
;;
;; On completion (DONE/ERRS/ABRT), the final text is extracted from the
;; indirect buffer, the buffer is closed, and the parent's main-cb is
;; called with the result.
;;
;; Extra keys stored in the FSM info plist:
;;   :agent-main-cb         - callback to return result to parent agent
;;   :agent-indirect-buffer - the indirect buffer for this sub-agent
;;   :agent-type            - agent type string (e.g., "researcher")
;;   :agent-description     - short task description

(defun gptel-agent-subtree--cleanup (fsm)
  "Clean up subtree resources for FSM and return the main-cb.

Extracts the final response text from the indirect buffer, closes the
indirect buffer (folding the subtree), deletes the task overlay from
the parent buffer, and returns a cons cell (MAIN-CB . RESULT-TEXT).

RESULT-TEXT is either the extracted response or nil on failure."
  (let* ((info (gptel-fsm-info fsm))
         (main-cb (plist-get info :agent-main-cb))
         (indirect-buf (plist-get info :agent-indirect-buffer))
         (ov (plist-get info :context))
         (agent-type (plist-get info :agent-type))
         (description (plist-get info :agent-description))
         (result-text
          (when (and indirect-buf (buffer-live-p indirect-buf))
            (gptel-org-agent--extract-final-text indirect-buf))))
    ;; Prepend the agent type header to the result
    (when result-text
      (setq result-text
            (format "%s result for task: %s\n\n%s"
                    (capitalize agent-type) description result-text)))
    ;; Close the indirect buffer, folding the subtree in the base buffer
    (when (and indirect-buf (buffer-live-p indirect-buf))
      (gptel-org-agent--close-indirect-buffer indirect-buf t))
    ;; Clean up the task overlay in the parent buffer
    (when (and ov (overlayp ov) (overlay-buffer ov))
      (delete-overlay ov))
    (cons main-cb result-text)))

(defun gptel-agent-subtree--handle-done (fsm)
  "Handle successful completion of a sub-agent subtree task.

Extract the final response from the indirect buffer, clean up
resources, and call main-cb with the result.  FSM is the sub-agent's
state machine."
  (let* ((cleanup (gptel-agent-subtree--cleanup fsm))
         (main-cb (car cleanup))
         (result (cdr cleanup)))
    (when main-cb
      (funcall main-cb
               (or result
                   (format "Error: %s sub-agent completed but produced no output."
                           (plist-get (gptel-fsm-info fsm) :agent-type)))))))

(defun gptel-agent-subtree--handle-error (fsm)
  "Handle error in a sub-agent subtree task.

Clean up resources and call main-cb with an error message.
FSM is the sub-agent's state machine."
  (let* ((info (gptel-fsm-info fsm))
         (cleanup (gptel-agent-subtree--cleanup fsm))
         (main-cb (car cleanup))
         (agent-type (plist-get info :agent-type))
         (description (plist-get info :agent-description))
         (error-data (plist-get info :error)))
    (when main-cb
      (funcall main-cb
               (format "Error: Task %s could not finish task \"%s\". Error details: %S"
                       agent-type description error-data)))))

(defun gptel-agent-subtree--handle-abort (fsm)
  "Handle user abort of a sub-agent subtree task.

Clean up resources and call main-cb with an abort message.
FSM is the sub-agent's state machine."
  (let* ((info (gptel-fsm-info fsm))
         (cleanup (gptel-agent-subtree--cleanup fsm))
         (main-cb (car cleanup))
         (agent-type (plist-get info :agent-type))
         (description (plist-get info :agent-description)))
    (when main-cb
      (funcall main-cb
               (format "Error: Task \"%s\" was aborted by the user. %s could not finish."
                       description agent-type)))))

(defvar gptel-agent-subtree--handlers
  `((WAIT ,#'gptel-agent--indicate-wait
          ,#'gptel--handle-wait ,#'gptel--update-wait)
    (TYPE ,#'gptel--handle-pre-insert)
    (ERRS ,#'gptel-agent-subtree--handle-error ,#'gptel--fsm-last)
    (TPRE ,#'gptel--handle-pre-tool ,#'gptel--fsm-transition)
    (TOOL ,#'gptel-agent--indicate-tool-call
          ,#'gptel--handle-tool-use ,#'gptel--update-tool-ask)
    (TRET ,#'gptel--handle-post-tool ,#'gptel--handle-tool-result)
    (DONE ,#'gptel--handle-post-insert
          ,#'gptel-agent-subtree--handle-done ,#'gptel--fsm-last)
    (ABRT ,#'gptel-agent-subtree--handle-abort))
  "Handler table for sub-agent tasks using buffer-writing subtrees.

Combines `gptel-send--handlers' (buffer insertion via TYPE/DONE) with
`gptel-agent-request--handlers' (agent indicator overlays for WAIT/TOOL),
plus custom DONE/ERRS/ABRT handlers that extract text from the indirect
buffer and call the parent agent's main-cb.")

(defun gptel-agent--task-preview-setup (arg-values _info)
  "Preview setup for Agent.
INFO is the tool call info plist.
ARG-VALUES is a list: (type description prompt)"
  (pcase-let ((from (point))
              (`(,type ,desc ,prompt) arg-values))
    (insert "("
            (propertize "Agent " 'font-lock-face 'font-lock-keyword-face)
            (propertize (prin1-to-string type)
                        'font-lock-face 'font-lock-escape-face)
            " " (propertize (prin1-to-string desc)
                            'font-lock-face
                            '(:inherit font-lock-constant-face :inherit bold))
            "\n" (propertize (prin1-to-string prompt)
                             'line-prefix "  "
                             'wrap-prefix "  "
                             'font-lock-face 'font-lock-constant-face)
            ")\n\n")
    (gptel-agent--confirm-overlay from (point) t)))

(defun gptel-agent--indicate-wait (fsm)
  "Display waiting indicator for agent task FSM."
  (when-let* ((info (gptel-fsm-info fsm))
              (info-ov (plist-get info :context))
              (count (overlay-get info-ov 'count)))
    (run-at-time
     1.5 nil
     (lambda (ov count)
       (when (and (overlay-buffer ov)
                  (eql (overlay-get ov 'count) count))
         (let* ((task-msg (overlay-get ov 'msg))
                (new-info-msg
                 (concat task-msg
                         (concat
                          (propertize "Waiting... " 'face 'warning) "\n"
                          (propertize "\n" 'face
                                      '(:inherit shadow :underline t :extend t))))))
           (overlay-put ov 'after-string new-info-msg))))
     info-ov count)))

(defun gptel-agent--indicate-tool-call (fsm)
  "Display tool call indicator for agent task FSM."
  (when-let* ((info (gptel-fsm-info fsm))
              (tool-use (plist-get info :tool-use))
              (ov (plist-get info :context)))
    ;; Update overlay with tool calls
    (when (overlay-buffer ov)
      (let* ((task-msg (overlay-get ov 'msg))
             (info-count (overlay-get ov 'count))
             (new-info-msg))
        (setq new-info-msg
              (concat task-msg
                      (concat
                       (propertize "Calling Tools... " 'face 'mode-line-emphasis)
                       (if (= info-count 0) "\n" (format "(+%d)\n" info-count))
                       (mapconcat (lambda (call)
                                    (gptel--format-tool-call
                                     (plist-get call :name)
                                     (map-values (plist-get call :args))))
                                  tool-use)
                       "\n" gptel-agent--hrule)))
        (overlay-put ov 'count (+ info-count (length tool-use)))
        (overlay-put ov 'after-string new-info-msg)))))

(defun gptel-agent--task-overlay (where &optional agent-type description)
  "Create overlay for agent task at WHERE with AGENT-TYPE and DESCRIPTION."
  (let* ((bounds                  ;where to place the overlay, handle edge cases
          (save-excursion
            (goto-char where)
            (when (bobp) (insert "\n"))
            (if (and (bolp) (eolp))
                (cons (1- (point)) (point))
              (cons (line-beginning-position) (line-end-position)))))
         (ov (make-overlay (car bounds) (cdr bounds) nil t))
         (msg (concat
               (unless (eq (char-after (car bounds)) 10) "\n")
               "\n" gptel-agent--hrule
               (propertize (concat (capitalize agent-type) " Task: ")
                           'face 'font-lock-escape-face)
               (propertize description 'face 'font-lock-doc-face)
               (propertize (format " [%s/%s]"
                                   (gptel-backend-name gptel-backend)
                                   (gptel--model-name gptel-model))
                           'face 'shadow)
               "\n")))
    (prog1 ov
      (overlay-put ov 'gptel-agent t)
      (overlay-put ov 'count 0)
      (overlay-put ov 'msg msg)
      (overlay-put ov 'line-prefix "")
      (overlay-put
       ov 'after-string
       (concat msg (propertize "Waiting..." 'face 'warning) "\n"
               gptel-agent--hrule)))))

(defun gptel-agent--resolve-model (model-name)
  "Resolve MODEL-NAME to a backend and model.
MODEL-NAME is a string like \"haiku\", \"claude-sonnet-4-6\", or
\"Ollama/hippo_fast\" (Backend/model format).

When MODEL-NAME contains a \"/\", the part before the slash is used as
the backend name and the part after as the model name, allowing
unambiguous resolution.

Returns a plist (:backend BACKEND :model MODEL) if found, nil otherwise."
  (if-let* ((slash-pos (string-search "/" model-name))
            (backend-name (substring model-name 0 slash-pos))
            (model-part (substring model-name (1+ slash-pos)))
            (backend (alist-get backend-name gptel--known-backends
                                nil nil #'equal)))
      ;; Backend/model format: resolve within the specified backend
      (let ((model-sym (intern (downcase model-part))))
        (or
         ;; Check model alias within the specified backend
         (when (and (get model-sym :model-id)
                    (memq model-sym (gptel-backend-models backend)))
           (list :backend backend :model model-sym))
         ;; Check by model name within the specified backend
         (cl-loop
          for model in (gptel-backend-models backend)
          when (string-equal-ignore-case
                model-part (gptel--model-name model))
          return (list :backend backend :model model))))
    ;; No slash: search all backends (original behavior)
    (let ((model-sym (intern (downcase model-name))))
      (or
       ;; Check if it's a model alias (has :model-id property)
       (when (get model-sym :model-id)
         (cl-loop for (_name . backend) in gptel--known-backends
                  when (memq model-sym (gptel-backend-models backend))
                  return (list :backend backend :model model-sym)))
       ;; Check if it matches a model name in any backend
       (cl-loop
        for (_name . backend) in gptel--known-backends
        thereis (cl-loop
                 for model in (gptel-backend-models backend)
                 when (string-equal-ignore-case
                       model-name (gptel--model-name model))
                 return (list :backend backend :model model)))))))

(defun gptel-agent--get-model-override (agent-type)
  "Get model override for AGENT-TYPE from org heading tags or properties.

Checks (in priority order):
1. Tags on the current org heading matching AGENT@MODEL pattern
   (e.g. \"gatherer@haiku\", \"researcher@opus\")
2. The GPTEL_AGENT_MODELS org property (inherited), with
   space-separated agent@model entries

Returns a plist (:backend BACKEND :model MODEL) if an override is
found, nil otherwise."
  (when (derived-mode-p 'org-mode)
    (save-excursion
      (ignore-errors (org-back-to-heading t))
      (let ((prefix (concat agent-type "@")))
        ;; 1. Check tags for agent@model pattern
        (or (cl-loop
             for tag in (org-get-tags)
             for tag-down = (downcase tag)
             when (string-prefix-p prefix tag-down)
             thereis (gptel-agent--resolve-model
                      (substring tag-down (length prefix))))
            ;; 2. Check GPTEL_AGENT_MODELS property (inherited)
            (when-let* ((prop (org-entry-get nil "GPTEL_AGENT_MODELS" t))
                        (entries (split-string prop))
                        (match (cl-find-if
                                (lambda (entry)
                                  (string-prefix-p prefix (downcase entry)))
                                entries)))
              (gptel-agent--resolve-model
               (substring (downcase match) (length prefix)))))))))

(defun gptel-agent--get-parent-subagent-model (agent-type)
  "Get model override for AGENT-TYPE from parent agent's subagent-models.

Looks up the currently active preset (the parent/calling agent) in
`gptel-agent--agents' and checks its `:subagent-models' property for
an entry matching AGENT-TYPE.

Returns a plist (:backend BACKEND :model MODEL) if found, nil otherwise."
  (when gptel-log-level
    (gptel--log
     (format "get-parent-subagent-model: agent-type=%s gptel--preset=%s (bound=%s)"
             agent-type
             (and (boundp 'gptel--preset) gptel--preset)
             (boundp 'gptel--preset))
     "preset-debug" t))
  (let ((result
         (when-let* ((parent-name (and (boundp 'gptel--preset) gptel--preset
                                       (symbol-name gptel--preset)))
                     (parent-plist (cdr (assoc parent-name gptel-agent--agents)))
                     (subagent-models (plist-get parent-plist :subagent-models))
                     (agent-key (intern (concat ":" agent-type)))
                     (model-name (plist-get subagent-models agent-key)))
           (gptel-agent--resolve-model model-name))))
    (when gptel-log-level
      (gptel--log
       (format "get-parent-subagent-model: result=%s (parent=%s, subagent-models=%s)"
               result
               (and (boundp 'gptel--preset) gptel--preset
                    (symbol-name gptel--preset))
               (and (boundp 'gptel--preset) gptel--preset
                    (plist-get (cdr (assoc (symbol-name gptel--preset)
                                           gptel-agent--agents))
                               :subagent-models)))
       "preset-debug" t))
    result))

(defun gptel-agent--task (main-cb agent-type description prompt)
  "Call a gptel agent to do specific compound tasks.

MAIN-CB is the main callback to return a value to the main loop.
AGENT-TYPE is the name of the agent.
DESCRIPTION is a short description of the task.
PROMPT is the detailed prompt instructing the agent on what is required.

When `gptel-org-agent-subtrees' is enabled and we're in an org-mode
buffer, the sub-agent's conversation is written into a dedicated
child subtree via an indirect buffer (subtree mode).  Otherwise, the
legacy callback-based string accumulation is used."
  (when gptel-log-level
    (gptel--log
     (format "agent--task entry: agent-type=%s gptel--preset=%s backend=%s model=%s"
             agent-type gptel--preset
             (and gptel-backend (gptel-backend-name gptel-backend))
             gptel-model)
     "preset-debug" t))
  (let ((model-override (gptel-agent--get-model-override agent-type))
        (parent-model (gptel-agent--get-parent-subagent-model agent-type))
        (work-dir default-directory))
    (when gptel-log-level
      (gptel--log
       (format "agent--task overrides: agent-type=%s model-override=%s parent-model=%s"
               agent-type model-override parent-model)
       "preset-debug" t))
    (gptel-with-preset
        (append (list :include-reasoning nil
                      :use-tools t
                      :context nil      ;Can be overriden by agent
                      ;; Always include :backend and :model so that
                      ;; gptel--preset-syms adds gptel-backend and
                      ;; gptel-model to the let-binding list.  Without
                      ;; this, sub-agents that don't specify their own
                      ;; backend/model inherit stale values from
                      ;; previous requests in the same buffer.
                      :backend (gptel-backend-name gptel-backend)
                      :model gptel-model)
                ;; Parent's subagent-models: applied BEFORE agent's own
                ;; plist so agent's own backend/model wins if specified
                (when parent-model
                  (list :backend (gptel-backend-name
                                  (plist-get parent-model :backend))
                        :model (plist-get parent-model :model)))
                (cdr (assoc agent-type gptel-agent--agents))
                ;; Model override from tags/properties: must be LAST
                ;; because map-do in gptel--apply-preset processes all
                ;; keys left-to-right with last-writer-wins semantics
                (when model-override
                  (list :backend (gptel-backend-name
                                  (plist-get model-override :backend))
                        :model (plist-get model-override :model))))
    (when gptel-log-level
      (gptel--log
       (format "agent--task after gptel-with-preset: agent-type=%s gptel--preset=%s backend=%s model=%s"
               agent-type gptel--preset
               (and gptel-backend (gptel-backend-name gptel-backend))
               gptel-model)
       "preset-debug" t))
    (let* ((info (gptel-fsm-info gptel--fsm-last))
           (where (or (plist-get info :tracking-marker)
                      (plist-get info :position)))
           ;; Try to set up subtree mode for the sub-agent
           (subtree-info
            (and (bound-and-true-p gptel-org-agent-subtrees)
                 (fboundp 'gptel-org-agent--setup-task-subtree)
                 (gptel-org-agent--setup-task-subtree agent-type description)))
           (prompt-with-dir
            (format "Current working directory: %s\nAll relative paths in tool calls are relative to this directory. Do NOT change directory or resolve the working directory.\n\n%s"
                    work-dir prompt)))
      (gptel--update-status " Calling Agent..." 'font-lock-escape-face)
      (if subtree-info
          ;; ---- SUBTREE MODE ----
          ;; Use buffer-writing FSM with indirect buffer.  The LLM response is
          ;; streamed directly into the indirect buffer.  On completion, the
          ;; DONE handler extracts the final text and calls main-cb.
          (let* ((indirect-buf (plist-get subtree-info :indirect-buffer))
                 (pos-marker (plist-get subtree-info :position-marker))
                 (task-ov (gptel-agent--task-overlay where agent-type description))
                 (sub-fsm
                  (gptel-request prompt-with-dir
                    :position pos-marker
                    :buffer indirect-buf
                    :stream gptel-stream
                    :context task-ov
                    :fsm (gptel-make-fsm :table gptel-send--transitions
                                         :handlers gptel-agent-subtree--handlers)
                    :transforms (list #'gptel--transform-add-context
                                      #'gptel-org-agent--transform-org-instructions))))
            ;; Store sub-agent metadata in the FSM info for the DONE/ERRS/ABRT
            ;; handlers.  This is safe because Emacs is single-threaded and the
            ;; async callback hasn't fired yet.
            (let ((sub-info (gptel-fsm-info sub-fsm)))
              (plist-put sub-info :agent-main-cb main-cb)
              (plist-put sub-info :agent-indirect-buffer indirect-buf)
              (plist-put sub-info :agent-type agent-type)
              (plist-put sub-info :agent-description description)))
        ;; ---- LEGACY MODE ----
        ;; Callback-based string accumulation (original behavior).
        ;; No indirect buffer; the agent's response is accumulated in a
        ;; local string and returned to main-cb when complete.
        (let ((partial (format "%s result for task: %s\n\n"
                               (capitalize agent-type) description)))
          (gptel-request prompt-with-dir
            :context (gptel-agent--task-overlay where agent-type description)
            :fsm (gptel-make-fsm :table gptel-send--transitions
                                 :handlers gptel-agent-request--handlers)
            :transforms (list #'gptel--transform-add-context
                              #'gptel-org-agent--transform-org-instructions)
            :callback
            (lambda (resp info)
              (let ((ov (plist-get info :context)))
                (pcase resp
                  ('nil
                   (delete-overlay ov)
                   (funcall main-cb
                            (format "Error: Task %s could not finish task \"%s\". \

Error details: %S"
                                    agent-type description (plist-get info :error))))
                  (`(tool-call . ,calls)
                   (unless (plist-get info :tracking-marker)
                     (plist-put info :tracking-marker where))
                   (gptel--display-tool-calls calls info))
                  ((pred stringp)
                   (setq partial (concat partial resp))
                   ;; If tool use is pending, the agent isn't done, so we just
                   ;; accumulate output without printing it.  We print at the end.
                   (unless (plist-get info :tool-use)
                     (delete-overlay ov)
                     (when-let* ((transformer (plist-get info :transformer)))
                       (setq partial (funcall transformer partial)))
                     (funcall main-cb partial)))
                  ('abort
                   (delete-overlay ov)
                   (funcall main-cb
                            (format "Error: Task \"%s\" was aborted by the user. \
%s could not finish."
                                    description agent-type)))))))))))))

;;; Register tool call preview functions

(pcase-dolist (`(,tool-name . ,setup-fn)
               `(("Write"     ,#'gptel-agent--write-file-preview-setup)
                 ("Eval"     ,#'gptel-agent--eval-elisp-preview-setup)
                 ("Bash"   ,#'gptel-agent--execute-bash-preview-setup)
                 ("Edit"     ,#'gptel-agent--edit-files-preview-setup)
                 ("Insert" ,#'gptel-agent--insert-in-file-preview-setup)
                 ("Agent"     ,#'gptel-agent--task-preview-setup)))
  (setf (alist-get tool-name gptel--tool-preview-alist
                   nil nil #'equal)
        setup-fn))

;;; All tool declarations

(gptel-make-tool
 :name "Bash"
 :function #'gptel-agent--execute-bash
 :description "Execute Bash commands.

This tool provides access to a Bash shell with GNU coreutils (or equivalents) available.
Use this to inspect system state, run builds, tests or other development or system administration tasks.

Do NOT use this for file operations, finding, reading or editing files.
Use the provided file tools instead: `Read`, `Write`, `Edit`, \
`Glob`, `Grep`

- Quote file paths with spaces using double quotes.
- Chain dependent commands with && (or ; if failures are OK)
- Use absolute paths instead of cd when possible
- For parallel commands, make multiple `Bash` calls in one message
- Run tests, check your work or otherwise close the loop to verify changes you make.
- Set `sudo` to true when the command needs root privileges (e.g. apt install, \
systemctl, editing /etc files).

EXAMPLES:
- Run tests: 'npm test' or 'cargo test'
- Build project: 'make all'
- Check git status: 'git status' or 'git log --oneline -10'
- Check file type: 'file document.pdf'
- Install a package: command='apt install -y nginx', sudo=true

DO NOT USE for: grep, find, ls, cat, head, tail, wc, sed, awk, or any \
file search/read/edit commands.  Use `Grep`, `Glob`, `Read`, `Edit` instead.

The command will be executed in the current working directory.  Output is
returned as a string.  Long outputs should be filtered/limited using pipes."
 :args '(( :name "command"
           :type string
           :description "The Bash command to execute.  \
Can include pipes and standard shell operators.
Do NOT use for file operations: no grep, find, ls, cat, head, \
tail, wc, sed, awk.  Use the Grep, Glob, Read tools instead.
Example: 'git log --oneline -10' or 'npm test'")
         ( :name "sudo"
           :type boolean
           :description "If true, execute the command as root via sudo.  \
Use this when the command requires elevated privileges, e.g. installing \
packages, editing system files, or managing services."
           :optional t))
 :category "gptel-agent"
 :confirm t
 :include t
 :async t)

(gptel-make-tool
 :name "Eval"
 :function
 (lambda (expression)
   (let ((standard-output (generate-new-buffer " *gptel-agent-eval-elisp*"))
         (result nil) (output nil))
     (unwind-protect
         (condition-case err
             (progn
               (setq result (eval (read expression) t))
               (when (> (buffer-size standard-output) 0)
                 (setq output (with-current-buffer standard-output (buffer-string))))
               (concat
                (format "Result:\n%S" result)
                (and output (format "\n\nSTDOUT:\n%s" output))))
           ((error user-error)
            (concat
             (format "Error: eval failed with error %S: %S"
                     (car err) (cdr err))
             (and output (format "\n\nSTDOUT:\n%s" output)))))
       (kill-buffer standard-output))))
 :description "Evaluate Elisp EXPRESSION and return result and any printed output.

EXPRESSION can be anything to evaluate.  It can be a function call, a
variable, a quasi-quoted expression.  The only requirement is that only
the first sexp will be read and evaluated, so if you need to evaluate
multiple expressions, make one call per expression.  Do not combine
expressions using progn etc.  Just go expression by expression and try
to make standalone single expressions.

Instead of saying \"I can't calculate that\" etc, use this tool to
evaluate the result.

The return value is formated to a string using %S, so a string will be
returned as an escaped embedded string and literal forms will be
compatible with `read' where possible.  Some forms have no printed
representation that can be read and will be represented with
#<hash-notation> instead.

Output from `print`, `prin1`, and `princ` is captured and returned as STDOUT.
Use `print` for diagnostic output, not `message` (which goes to *Messages* buffer
and is not captured).

You can use this to quickly change a user setting, check a variable, or
demonstrate something to the user."
 :args '(( :name "expression"
           :type string
           :description "A single elisp sexp to evaluate."))
 :category "gptel-agent"
 :confirm t
 :include t)

(gptel-make-tool
 :name "WebSearch"
 :function 'gptel-agent--web-search-eww
 :description "Search the web for the first five results to a query.  The query can be an arbitrary string.  Returns the top five results from the search engine as a list of plists.  Each object has the keys `:url` and `:excerpt` for the corresponding search result.

This tool uses the Emacs web browser (eww) with its default search engine (typically DuckDuckGo) to perform searches. No API key is required.

If required, consider using the url as the input to the `Read` tool to get the contents of the url.  Note that this might not work as the `Read` tool does not handle javascript-enabled pages."
 :args '((:name "query"
                :type string
                :description "The natural language search query, can be multiple words.")
         (:name "count"
                :type integer
                :description "Number of results to return (default 5)"
                :optional t))
 :include t
 :async t
 :category "gptel-agent")

(gptel-make-tool
 :function #'gptel-agent--read-url
 :name "WebFetch"
 :description "Fetch and read the contents of a URL.

- Returns the text of the URL (not HTML) formatted for reading.
- Request times out after 30 seconds."
 :args '(( :name "url"
           :type "string"
           :description "The URL to read"))
 :async t
 :include t
 :category "gptel-agent")

(gptel-make-tool
 :name "YouTube"
 :function #'gptel-agent--yt-read-url
 :description "Find the description and video transcript for a youtube video.  Returns a markdown formatted string containing two sections:

\"description\": The video description added by the uploader
\"transcript\": The video transcript in SRT format"
 :args '((:name "url"
                :description "The youtube video URL, for example \"https://www.youtube.com/watch?v=H2qJRnV8ZGA\""
                :type "string"))
 :category "gptel-agent"
 :async t
 :include t)

(gptel-make-tool
 :name "Diagnostics"
 :description "Collect all code diagnostics with severity high/medium \
across all open buffers in the current project.

With optional argument `all`, collect notes and low-severity diagnostics
too."
 :function #'gptel-agent--flymake-diagnostics
 :args (list '( :name "all"
                :type boolean
                :description
                "Whether low-severity diagnostics (notes) should also be collected."
                :optional t))
 :category "gptel-agent"
 :include t)

(gptel-make-tool
 :name "Mkdir"
 :description "Create a new directory with the given name in the specified parent directory"
 :function #'gptel-agent--make-directory
 :args (list '( :name "parent"
                :type "string"
                :description "The parent directory where the new directory should be created, e.g. /tmp")
             '( :name "name"
                :type "string"
                :description "The name of the new directory to create, e.g. testdir"))
 :category "gptel-agent"
 :confirm t
 :include t)

(gptel-make-tool
 :name "Edit"
 :description
 "Replace text in one or more files.

To edit a single file, provide the file `path`.

For the replacement, there are two methods:
- Short replacements: Provide both `old_str` and `new_str`, in which case `old_str` \
needs to exactly match one unique section of the original file, including any whitespace.  \
Make sure to include enough context that the match is not ambiguous.  \
The entire original string will be replaced with `new str`.
- Long or involved replacements: set the `diff` parameter to true and provide a unified diff \
in `new_str`. `old_str` can be ignored.

To edit multiple files,
- provide the directory path,
- set the `diff` parameter to true
- and provide a unified diff in `new_str`.

Diff instructions:

- The diff must be provided within fenced code blocks (=diff or =patch) and be in unified format.
- The LLM should generate the diff such that the file paths within the diff \
  (e.g., '--- a/filename' '+++ b/filename') are appropriate for the 'path'.

To simply insert text at some line, use the \"Insert\" instead."
 :function #'gptel-agent--edit-files
 :args '(( :name "path"
           :description "File path or directory to edit"
           :type string)
         ( :name "old_str"
           :description "Original string to replace.  If providing a unified diff, this should be false"
           :type string
           :optional t)
         ( :name "new_str"
           :description "Replacement string OR unified diff text"
           :type string)
         ( :name "diff"
           :description "Whether the replacement is a string or a diff.  `true` for a diff, `false` otherwise."
           :type boolean))
 :category "gptel-agent"
 :confirm t
 :include t)

(gptel-make-tool
 :name "Insert"
 :description "Insert `new_str` after `line_number` in file at `path`.

Use this tool for purely additive actions: adding text to a file at a \
specific location with no changes to the surrounding context."
 :function #'gptel-agent--insert-in-file
 :args '(( :name "path"
           :description "Path of file to edit."
           :type string)
         ( :name "line_number"
           :description "The line number at which to insert `new_str`, with
- 0 to insert at the beginning, and
- -1 to insert at the end."
           :type integer)
         ( :name "new_str"
           :description "String to insert at `line_number`."
           :type string))
 :category "gptel-agent"
 :confirm t
 :include t)

(gptel-make-tool
 :name "Write"
 :description "Create a new file with the specified content.
Overwrites an existing file, so use with care!
Consider using the more granular tools \"Insert\" or \"Edit\" first."
 :function #'gptel-agent--write-file
 :args (list '( :name "path"
                :type "string"
                :description "The directory where to create the file, \".\" is the current directory.")
             '( :name "filename"
                :type "string"
                :description "The name of the file to create.")
             '( :name "content"
                :type "string"
                :description "The content to write to the file"))
 :category "gptel-agent"
 :confirm t)

(gptel-make-tool
 :name "Glob"
 :description "Recursively find files matching a provided glob pattern.

- Supports glob patterns like \"*.md\" or \"*test*.py\".
  The glob applies to the basename of the file (with extension).
- Does not support double wildcard \"**/*\".
- Returns matching file paths at all depths sorted by modification time.
  Limit the depth of the search by providing the `depth` argument.
- When you are doing an open ended search that may require multiple rounds
  of globbing and grepping, use the \"task\" tool instead
- You can call multiple tools in a single response.  It is always better to
  speculatively perform multiple searches in parallel if they are potentially useful."
 :function #'gptel-agent--glob
 :args '(( :name "pattern"
           :type string
           :description "Glob pattern to match, for example \"*.el\". Must not be empty.
Use \"*\" to list all files in a directory.")
         ( :name "path"
           :type string
           :description "Directory to search in.  Supports relative paths and defaults to \".\""
           :optional t)
         ( :name "depth"
           :description "Limit directory depth of search, 1 or higher. Defaults to no limit."
           :type integer
           :optional t))
 :category "gptel-agent"
 :include t)

(gptel-make-tool
 :name "Read"
 :description "Read file contents between specified line numbers `start_line` and `end_line`,
with both ends included.

Consider using the \"Grep\" tool to find the right range to read first.

Reads the whole file if the line range is not provided.

Files over 512 KB in size can only be read by specifying a line range."
 :function #'gptel-agent--read-file-lines
 :args '(( :name "file_path"
           :type string
           :description "The path to the file to be read.")
         ( :name "start_line"
           :type integer
           :description "The line to start reading from, defaults to the start of the file"
           :optional t)
         ( :name "end_line"
           :type integer
           :description "The line up to which to read, defaults to the end of the file."
           :optional t))
 :category "gptel-agent"
 :include t)

(gptel-make-tool
 :name "Grep"
 :description "Search for text in file(s) at `path`.

Use this tool to find relevant parts of files to read.

Returns a list of matches prefixed by the line number, and grouped by file.  Can search an individual file (if providing a file path) or a directory.  Consider using this tool to find the right line range for the \"Read\" tool.

When searching directories, optionally restrict the types of files in the search with a `glob`.  Can request context lines around each match using the `context_lines` parameters."
 :function #'gptel-agent--grep
 :args '(( :name "regex"
           :description "Regular expression to search for in file contents."
           :type string)
         ( :name "path"
           :description "File or directory to search in."
           :type string)
         ( :name "glob"
           :description "Optional glob to restrict file types to search for.
Only required when path is a directory.
Examples: *.md, *.rs"
           :type string
           :optional t)
         ( :name "context_lines"
           :description "Number of lines of context to retrieve around each match (0-15 inclusive).
Optional, defaults to 0."
           :optional t
           :type integer
           :maximum 15))
 :category "gptel-agent"
 :include t)

(gptel-make-tool
 :name "TodoWrite"
 :description "Create and manage a structured task list for your current session.  \
Helps track progress and organize complex tasks. Use proactively for multi-step work.

Only one todo can be `in_progress` at a time."
 :function #'gptel-agent--write-todo
 :args
 '(( :name "todos"
     :type array
     :items
     ( :type object
       :properties
       (:content
        ( :type string :minLength 1
          :description "Imperative form describing what needs to be done (e.g., 'Run tests')")
        :status
        ( :type string
          :enum ["pending" "in_progress" "completed"]
          :description "Task status: pending, in_progress (exactly one), or completed")
        :activeForm
        ( :type string :minLength 1
          :description "Present continuous form shown during execution (e.g., 'Running tests')")))))
 :category "gptel-agent")

(gptel-make-tool
 :name "Skill"
 :description "Load a skill into the current conversation.

Each skill provides guidance on how to execute a specific task.
You can invoke a skill with optional args, the args are for your future reference only.

When to use:
- When a skill is relevant, you must invoke this tool IMMEDIATELY
- This is a BLOCKING REQUIREMENT: invoke the relevant Skill tool before generating any other response about the task
- Only use skills listed in your prompt
- Do not invoke a skill that is already loaded.

How to use:
- Invoke with the skill name and optional args.  The args are for your reference only
- Examples:
    - `skill: \"pdf\"` - invoke the pdf skill
    - `skill: \"commit\", args: \"-m 'Fix bug'\"` - invoke with arguments
    - `skill: \"review-pr\", args: \"123\"` - invoke with arguments"
 :function #'gptel-agent--get-skill
 :args '(( :name "skill"
           :type string
           :description "Name of the skill, chosen from the list of available skills")
         ( :name "args"
           :type string
           :optional t
           :description "Args relevant to the skill, for your future reference"))
 :category "gptel-agent"
 :include t)

(gptel-make-tool
 :name "Agent"
 :description "Launch a specialized agent to handle complex, multi-step tasks autonomously.  \
Agents run independently and return results in one message.  \
Use for open-ended searches, complex research, or when uncertain about finding results in first few tries."
 :function #'gptel-agent--task
 :args '(( :name "subagent_type"
           :type string
           :enum ["researcher" "introspector"]
           :description "The type of specialized agent to use for this task")
         ( :name "description"
           :type string
           :description "A short (3-5 word) description of the task")
         ( :name "prompt"
           :type "string"
           :description "The detailed task for the agent to perform autonomously.  \
Should include exactly what information the agent should return."))
 :category "gptel-agent"
 :async t
 :confirm t
 :include t)

(provide 'gptel-agent-tools)
;;; gptel-agent-tools.el ends here

;; Local Variables:
;; elisp-flymake-byte-compile-load-path: ("~/.local/share/git/elpaca/repos/gptel/" "~/.local/share/git/elpaca/repos/transient/lisp" "~/.local/share/git/elpaca/repos/compat/")
;; End:
