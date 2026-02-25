;;; gptel-agent-tools-remote.el --- Remote server management tools for gptel-agent -*- lexical-binding: t; -*-

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

;; Remote server management tools for gptel-agent using TRAMP.
;;
;; Provides tools for executing commands, reading/writing files, and
;; managing services on remote servers via SSH, with optional sudo
;; escalation.
;;
;; All remote operations use Emacs TRAMP, which handles connection
;; pooling, authentication, and multi-hop transparently.
;;
;; Tools:
;; - "RemoteBash"       : Execute a command on a remote host
;; - "RemoteRead"       : Read a file on a remote host
;; - "RemoteWrite"      : Write a file on a remote host
;; - "RemoteEdit"       : Edit a file on a remote host
;; - "RemoteGlob"       : Find files on a remote host
;; - "RemoteGrep"       : Search file contents on a remote host
;; - "ServiceStatus"    : Check systemd service status
;; - "ServiceControl"   : Start/stop/restart/enable/disable a service

;;; Code:

(require 'gptel)
(require 'tramp)
(eval-when-compile (require 'cl-lib))

;;; Helpers

(defun gptel-agent--remote-path (host user sudo path)
  "Construct a TRAMP path for HOST, USER, SUDO flag, and remote PATH.

HOST is the remote hostname or IP.
USER is the SSH username.
SUDO if non-nil and not :json-false, chain through sudo to root.
PATH is the remote filesystem path."
  (let ((sudo (and sudo (not (eq sudo :json-false)))))
    (if sudo
        (format "/ssh:%s@%s|sudo::/%s"
                user host (string-remove-prefix "/" path))
      (format "/ssh:%s@%s:%s" user host path))))

(defun gptel-agent--remote-default-directory (host user sudo)
  "Return a TRAMP default-directory for HOST, USER, and SUDO flag."
  (gptel-agent--remote-path host user sudo "/"))

(defun gptel-agent--remote-preview-header (tool-name host user sudo)
  "Insert preview header for TOOL-NAME connecting to HOST as USER.
SUDO indicates privilege escalation."
  (let ((sudo (and sudo (not (eq sudo :json-false)))))
    (insert
     "(" (propertize tool-name 'font-lock-face 'font-lock-keyword-face)
     " " (propertize (concat user "@" host)
                     'font-lock-face 'font-lock-constant-face)
     (if sudo
         (propertize " [sudo]" 'font-lock-face 'font-lock-warning-face)
       "")
     ")\n")))

;;; RemoteBash — Execute command on remote host

(defun gptel-agent--remote-bash-preview-setup (arg-values _info)
  "Setup preview overlay for RemoteBash tool call.
ARG-VALUES is the list of arguments for the tool call."
  (pcase-let ((from (point))
              (`(,host ,user ,command ,sudo) arg-values))
    (gptel-agent--remote-preview-header "RemoteBash" host user sudo)
    (let ((inner-from (point)))
      (insert command)
      (gptel-agent--fontify-block 'sh-mode inner-from (point))
      (insert "\n\n")
      (font-lock-append-text-property
       inner-from (1- (point)) 'font-lock-face (gptel-agent--block-bg))
      (gptel-agent--confirm-overlay from (point) t))))

(defun gptel-agent--remote-bash (callback host user command &optional sudo)
  "Execute COMMAND on remote HOST as USER via SSH.

CALLBACK is called with the command output string when finished.
HOST is the remote hostname or IP address.
USER is the SSH username.
COMMAND is the shell command string to execute.
SUDO if non-nil, escalate to root via sudo."
  (let ((default-directory (gptel-agent--remote-default-directory host user sudo)))
    (condition-case err
        (with-temp-buffer
          (let ((exit-code
                 (process-file "/bin/sh" nil (current-buffer) nil "-c" command)))
            (funcall callback
                     (if (zerop exit-code)
                         (buffer-string)
                       (format "Command failed with exit code %d:\n%s"
                               exit-code (buffer-string))))))
      (error
       (funcall callback
                (format "Error connecting to %s@%s: %S" user host err))))))

;;; RemoteRead — Read file on remote host

(defun gptel-agent--remote-read-file (host user file-path &optional sudo start-line end-line)
  "Read FILE-PATH on remote HOST as USER.

SUDO if non-nil, read as root.
START-LINE and END-LINE optionally limit the range."
  (let ((tramp-path (gptel-agent--remote-path host user sudo file-path)))
    (unless (file-readable-p tramp-path)
      (error "Error: File %s on %s is not readable" file-path host))
    (when (file-directory-p tramp-path)
      (error "Error: %s on %s is a directory, not a file" file-path host))
    (if (and (not start-line) (not end-line))
        (if (> (file-attribute-size (file-attributes tramp-path))
               (* 512 1024))
            (error "Error: File is too large (> 512 KB). Specify a line range")
          (with-temp-buffer
            (insert-file-contents tramp-path)
            (buffer-string)))
      ;; Line range reading
      (with-temp-buffer
        (insert-file-contents tramp-path)
        (let ((total-lines (count-lines (point-min) (point-max))))
          (when start-line
            (goto-char (point-min))
            (forward-line (1- (max 1 start-line)))
            (delete-region (point-min) (point)))
          (when end-line
            (goto-char (point-min))
            (let ((lines-to-keep (1+ (- (min end-line total-lines)
                                        (or start-line 1)))))
              (forward-line lines-to-keep)
              (delete-region (point) (point-max))))
          (buffer-string))))))

;;; RemoteWrite — Write file on remote host

(defun gptel-agent--remote-write-file-preview-setup (arg-values _info)
  "Setup preview overlay for RemoteWrite tool call.
ARG-VALUES is the list of arguments for the tool call."
  (pcase-let ((from (point))
              (`(,host ,user ,file-path ,content ,sudo) arg-values))
    (gptel-agent--remote-preview-header "RemoteWrite" host user sudo)
    (insert (propertize (concat " → " file-path) 'font-lock-face 'font-lock-doc-face) "\n")
    (let ((inner-from (point)))
      (insert content)
      (gptel-agent--fontify-block file-path inner-from (point))
      (insert "\n\n")
      (font-lock-append-text-property
       inner-from (1- (point)) 'font-lock-face (gptel-agent--block-bg)))
    (gptel-agent--confirm-overlay from (point))))

(defun gptel-agent--remote-write-file (host user file-path content &optional sudo)
  "Write CONTENT to FILE-PATH on remote HOST as USER.

SUDO if non-nil, write as root."
  (let ((tramp-path (gptel-agent--remote-path host user sudo file-path)))
    (condition-case err
        (progn
          (let ((dir (file-name-directory tramp-path)))
            (unless (file-directory-p dir)
              (make-directory dir t)))
          (with-temp-buffer
            (insert content)
            (write-region nil nil tramp-path))
          (format "Successfully wrote %s on %s" file-path host))
      (error (format "Error writing %s on %s: %S" file-path host err)))))

;;; RemoteEdit — Edit file on remote host

(defun gptel-agent--remote-edit-preview-setup (arg-values _info)
  "Setup preview overlay for RemoteEdit tool call.
ARG-VALUES is the list of arguments for the tool call."
  (pcase-let ((from (point))
              (`(,host ,user ,file-path ,old-str ,new-str ,sudo) arg-values))
    (gptel-agent--remote-preview-header "RemoteEdit" host user sudo)
    (insert (propertize (concat " → " file-path) 'font-lock-face 'font-lock-doc-face) "\n")
    (when old-str
      (insert
       (propertize old-str 'font-lock-face 'diff-removed
                   'line-prefix (propertize "-" 'face 'diff-removed))
       "\n"
       (propertize new-str 'font-lock-face 'diff-added
                   'line-prefix (propertize "+" 'face 'diff-added))
       "\n\n")
      (font-lock-append-text-property
       from (1- (point)) 'font-lock-face (gptel-agent--block-bg)))
    (gptel-agent--confirm-overlay from (point) t)))

(defun gptel-agent--remote-edit (host user file-path old-str new-str &optional sudo)
  "Replace OLD-STR with NEW-STR in FILE-PATH on remote HOST as USER.

SUDO if non-nil, edit as root.  OLD-STR must match exactly once."
  (let ((tramp-path (gptel-agent--remote-path host user sudo file-path)))
    (unless (file-readable-p tramp-path)
      (error "Error: File %s on %s is not readable" file-path host))
    (with-temp-buffer
      (insert-file-contents tramp-path)
      (if (search-forward old-str nil t)
          (if (save-excursion (search-forward old-str nil t))
              (error "Error: Match is not unique in %s on %s. Provide more context"
                     file-path host)
            (replace-match (string-replace "\\" "\\\\" new-str))
            (write-region nil nil tramp-path)
            (format "Successfully edited %s on %s" file-path host))
        (error "Error: Could not find old_str in %s on %s" file-path host)))))

;;; RemoteGlob — Find files on remote host

(defun gptel-agent--remote-glob (host user pattern &optional path sudo depth)
  "Find files matching PATTERN on remote HOST as USER.

PATH is the remote directory to search (defaults to home).
SUDO if non-nil, search as root.
DEPTH limits recursion depth."
  (let ((default-directory
         (gptel-agent--remote-path host user sudo (or path "/"))))
    (when (string-empty-p pattern)
      (error "Error: pattern must not be empty"))
    (unless (file-directory-p default-directory)
      (error "Error: path %s on %s is not a directory" (or path "/") host))
    ;; Use process-file with find since tree may not be on remote
    (with-temp-buffer
      (let* ((search-path (file-local-name
                           (gptel-agent--remote-path host user sudo (or path "/"))))
             (args (delq nil
                         (list search-path
                               (when (natnump depth)
                                 "-maxdepth")
                               (when (natnump depth)
                                 (number-to-string depth))
                               "-name" pattern
                               "-not" "-path" "*/.git/*"
                               "-printf" "%T@ %p\\n")))
             (exit-code (apply #'process-file "find" nil (current-buffer) nil args)))
        (when (/= exit-code 0)
          (goto-char (point-min))
          (insert (format "find failed with exit code %d\n" exit-code)))
        ;; Sort by mtime (descending) and strip timestamps
        (let ((lines (split-string (buffer-string) "\n" t)))
          (setq lines (sort lines (lambda (a b) (string> a b))))
          (mapconcat (lambda (line)
                       (if (string-match "^[0-9.]+ \\(.*\\)$" line)
                           (match-string 1 line)
                         line))
                     lines "\n"))))))

;;; RemoteGrep — Search file contents on remote host

(defun gptel-agent--remote-grep (host user regex path &optional sudo glob context-lines)
  "Search for REGEX in PATH on remote HOST as USER.

SUDO if non-nil, search as root.
GLOB restricts file types.
CONTEXT-LINES specifies surrounding lines to show."
  (let ((default-directory
         (gptel-agent--remote-default-directory host user sudo)))
    (unless (file-readable-p (gptel-agent--remote-path host user sudo path))
      (error "Error: %s on %s is not readable" path host))
    ;; Use grep since ripgrep may not be on remote
    (with-temp-buffer
      (let* ((remote-path (file-local-name
                           (gptel-agent--remote-path host user sudo path)))
             (args (delq nil
                         (list "--recursive"
                               (when (natnump context-lines)
                                 (format "--context=%d" context-lines))
                               (when glob
                                 (format "--include=%s" glob))
                               "--max-count=1000"
                               "--line-number"
                               "--regexp" regex
                               remote-path)))
             (exit-code (apply #'process-file "grep" nil (current-buffer) nil args)))
        (when (and (/= exit-code 0) (/= exit-code 1))
          (goto-char (point-min))
          (insert (format "grep failed with exit code %d\n" exit-code)))
        (buffer-string)))))

;;; ServiceStatus — Check systemd service status

(defun gptel-agent--service-status (host user service &optional sudo)
  "Check the status of SERVICE on remote HOST as USER.

SUDO if non-nil, check as root (needed for some service details).
Returns the output of systemctl status."
  (let ((default-directory
         (gptel-agent--remote-default-directory host user sudo)))
    (condition-case err
        (with-temp-buffer
          (let ((exit-code
                 (process-file "systemctl" nil (current-buffer) nil
                               "status" service)))
            ;; systemctl status returns 3 for inactive, 4 for not found
            (cond
             ((= exit-code 4)
              (format "Service '%s' not found on %s" service host))
             (t (buffer-string)))))
      (error (format "Error checking service %s on %s: %S"
                     service host err)))))

;;; ServiceControl — Start/stop/restart/enable/disable a service

(defun gptel-agent--service-control-preview-setup (arg-values _info)
  "Setup preview overlay for ServiceControl tool call.
ARG-VALUES is the list of arguments for the tool call."
  (pcase-let ((from (point))
              (`(,host ,user ,service ,action) arg-values))
    (insert
     "(" (propertize "ServiceControl" 'font-lock-face 'font-lock-keyword-face)
     " " (propertize (concat user "@" host)
                     'font-lock-face 'font-lock-constant-face)
     (propertize " [sudo]" 'font-lock-face 'font-lock-warning-face)
     ")\n"
     (propertize (format "systemctl %s %s" action service)
                 'font-lock-face 'font-lock-warning-face)
     "\n\n")
    (gptel-agent--confirm-overlay from (point) t)))

(defun gptel-agent--service-control (host user service action)
  "Control SERVICE on remote HOST as USER via systemctl.

ACTION is one of: start, stop, restart, reload, enable, disable.
Always uses sudo since service control requires root."
  (unless (member action '("start" "stop" "restart" "reload" "enable" "disable"))
    (error "Error: Invalid action '%s'. Must be one of: start, stop, restart, reload, enable, disable"
           action))
  (let ((default-directory
         (gptel-agent--remote-default-directory host user t)))
    (condition-case err
        (with-temp-buffer
          (let ((exit-code
                 (process-file "systemctl" nil (current-buffer) nil
                               action service)))
            (if (zerop exit-code)
                (format "Successfully ran 'systemctl %s %s' on %s"
                        action service host)
              (format "Failed to %s service %s on %s (exit code %d):\n%s"
                      action service host exit-code (buffer-string)))))
      (error (format "Error controlling service %s on %s: %S"
                     service host err)))))

;;; Register tool call preview functions

(declare-function gptel-agent--confirm-overlay "gptel-agent-tools")
(declare-function gptel-agent--fontify-block "gptel-agent-tools")
(declare-function gptel-agent--block-bg "gptel-agent-tools")

(pcase-dolist (`(,tool-name . ,setup-fn)
               `(("RemoteBash"     ,#'gptel-agent--remote-bash-preview-setup)
                 ("RemoteWrite"    ,#'gptel-agent--remote-write-file-preview-setup)
                 ("RemoteEdit"     ,#'gptel-agent--remote-edit-preview-setup)
                 ("ServiceControl" ,#'gptel-agent--service-control-preview-setup)))
  (setf (alist-get tool-name gptel--tool-preview-alist nil nil #'equal)
        setup-fn))

;;; Tool declarations

(gptel-make-tool
 :name "RemoteBash"
 :function #'gptel-agent--remote-bash
 :description "Execute a shell command on a remote server via SSH.

Uses TRAMP to connect to the remote host. The command runs in /bin/sh.
Set the sudo parameter to true when root access is needed — this uses
TRAMP multi-hop (ssh|sudo) for privilege escalation.

IMPORTANT: Do NOT prefix commands with 'sudo'. Use the sudo parameter instead.
  WRONG: command='sudo journalctl -u nginx'
  RIGHT: command='journalctl -u nginx', sudo=true

EXAMPLES:
- Check uptime: command='uptime'
- View logs as root: command='journalctl -u nginx --no-pager -n 50', sudo=true
- Check disk: command='df -h'
- List processes: command='ps aux | grep nginx'

Do NOT use for local operations. Use the regular Bash tool for local commands."
 :args '((:name "host"
          :type string
          :description "Remote hostname or IP address, e.g. 'server1.example.com' or '192.168.1.10'")
         (:name "user"
          :type string
          :description "SSH username to connect as, e.g. 'admin' or 'deploy'")
         (:name "command"
          :type string
          :description "Shell command to execute on the remote host. Never include 'sudo' here — use the sudo parameter instead.")
         (:name "sudo"
          :type boolean
          :description "If true, execute the command as root via sudo"
          :optional t))
 :category "remote"
 :confirm t
 :include t
 :async t)

(gptel-make-tool
 :name "RemoteRead"
 :function #'gptel-agent--remote-read-file
 :description "Read a file on a remote server via SSH.

Returns the file contents as a string. With sudo, can read root-owned files.
Files over 512 KB require specifying a line range."
 :args '((:name "host"
          :type string
          :description "Remote hostname or IP address")
         (:name "user"
          :type string
          :description "SSH username")
         (:name "file_path"
          :type string
          :description "Absolute path to the file on the remote host, e.g. '/etc/nginx/nginx.conf'")
         (:name "sudo"
          :type boolean
          :description "If true, read as root (needed for files like /etc/shadow)"
          :optional t)
         (:name "start_line"
          :type integer
          :description "Line to start reading from (1-based)"
          :optional t)
         (:name "end_line"
          :type integer
          :description "Line to stop reading at (inclusive)"
          :optional t))
 :category "remote"
 :include t)

(gptel-make-tool
 :name "RemoteWrite"
 :function #'gptel-agent--remote-write-file
 :description "Write content to a file on a remote server via SSH.

Creates the file and any missing parent directories. With sudo, can write
to root-owned locations like /etc/. Overwrites existing files."
 :args '((:name "host"
          :type string
          :description "Remote hostname or IP address")
         (:name "user"
          :type string
          :description "SSH username")
         (:name "file_path"
          :type string
          :description "Absolute path for the file on the remote host")
         (:name "content"
          :type string
          :description "Content to write to the file")
         (:name "sudo"
          :type boolean
          :description "If true, write as root"
          :optional t))
 :category "remote"
 :confirm t
 :include t)

(gptel-make-tool
 :name "RemoteEdit"
 :function #'gptel-agent--remote-edit
 :description "Edit a file on a remote server by replacing a unique string.

Finds OLD-STR in the file and replaces it with NEW-STR. OLD-STR must
match exactly once (be unique). Use RemoteRead first to see the file."
 :args '((:name "host"
          :type string
          :description "Remote hostname or IP address")
         (:name "user"
          :type string
          :description "SSH username")
         (:name "file_path"
          :type string
          :description "Absolute path to the file on the remote host")
         (:name "old_str"
          :type string
          :description "Exact text to find and replace (must be unique in the file)")
         (:name "new_str"
          :type string
          :description "Replacement text")
         (:name "sudo"
          :type boolean
          :description "If true, edit as root"
          :optional t))
 :category "remote"
 :confirm t
 :include t)

(gptel-make-tool
 :name "RemoteGlob"
 :function #'gptel-agent--remote-glob
 :description "Find files matching a pattern on a remote server.

Uses `find` on the remote host to locate files by name pattern.
Results are sorted by modification time (newest first)."
 :args '((:name "host"
          :type string
          :description "Remote hostname or IP address")
         (:name "user"
          :type string
          :description "SSH username")
         (:name "pattern"
          :type string
          :description "Filename pattern (glob), e.g. '*.conf' or '*.log'")
         (:name "path"
          :type string
          :description "Remote directory to search in, e.g. '/etc/nginx/'"
          :optional t)
         (:name "sudo"
          :type boolean
          :description "If true, search as root (access protected directories)"
          :optional t)
         (:name "depth"
          :type integer
          :description "Maximum directory depth to search"
          :optional t))
 :category "remote"
 :include t)

(gptel-make-tool
 :name "RemoteGrep"
 :function #'gptel-agent--remote-grep
 :description "Search file contents on a remote server using grep.

Returns matches with line numbers, grouped by file. Uses grep (not ripgrep)
since the remote host may not have ripgrep installed."
 :args '((:name "host"
          :type string
          :description "Remote hostname or IP address")
         (:name "user"
          :type string
          :description "SSH username")
         (:name "regex"
          :type string
          :description "Regular expression to search for")
         (:name "path"
          :type string
          :description "File or directory path on the remote host")
         (:name "sudo"
          :type boolean
          :description "If true, search as root"
          :optional t)
         (:name "glob"
          :type string
          :description "Restrict search to files matching this glob, e.g. '*.conf'"
          :optional t)
         (:name "context_lines"
          :type integer
          :description "Lines of context around each match (0-15)"
          :optional t
          :maximum 15))
 :category "remote"
 :include t)

(gptel-make-tool
 :name "ServiceStatus"
 :function #'gptel-agent--service-status
 :description "Check the status of a systemd service on a remote server.

Returns the output of `systemctl status <service>`. Includes active state,
logs, and process information."
 :args '((:name "host"
          :type string
          :description "Remote hostname or IP address")
         (:name "user"
          :type string
          :description "SSH username")
         (:name "service"
          :type string
          :description "Service name, e.g. 'nginx', 'postgresql', 'docker'")
         (:name "sudo"
          :type boolean
          :description "If true, check as root (shows more details for some services)"
          :optional t))
 :category "remote"
 :include t)

(gptel-make-tool
 :name "ServiceControl"
 :function #'gptel-agent--service-control
 :description "Control a systemd service on a remote server.

Runs `systemctl <action> <service>` as root (always uses sudo).
Available actions: start, stop, restart, reload, enable, disable.

Always check ServiceStatus first to understand the current state."
 :args '((:name "host"
          :type string
          :description "Remote hostname or IP address")
         (:name "user"
          :type string
          :description "SSH username")
         (:name "service"
          :type string
          :description "Service name, e.g. 'nginx', 'postgresql'")
         (:name "action"
          :type string
          :enum ["start" "stop" "restart" "reload" "enable" "disable"]
          :description "The systemctl action to perform"))
 :category "remote"
 :confirm t
 :include t)

(provide 'gptel-agent-tools-remote)
;;; gptel-agent-tools-remote.el ends here
