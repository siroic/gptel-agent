;;; test-bash-safety.el --- Tests for bash safety patterns -*- lexical-binding: t; -*-

;; Copyright (C) 2025

;; Author: gptel-agent tests

;;; Commentary:

;; ERT tests for `gptel-agent--bash-safe-command-p',
;; `gptel-agent--bash-confirm-p', and `gptel-agent--check-bash-file-ops'.
;;
;; We duplicate the variable and function definitions here rather than
;; loading gptel-agent-tools.el because that file has many dependencies
;; (gptel, transient, etc.) that make isolated testing impractical.
;; This keeps the tests self-contained and fast to run.

;;; Code:

(require 'ert)
(require 'rx)

;;;; ---- Duplicated definitions from gptel-agent-tools.el ----

(defvar gptel-agent--bash-safe-patterns
  (rx bos (* space)
      (or
       ;; File metadata / existence checks
       "test " "[ " "[[ "
       ;; Counting
       "wc "
       ;; File info
       "stat " "file " "readlink "
       ;; Path/command lookups
       "which " "type " "command -v " "command -V "
       "realpath " "basename " "dirname "
       ;; System info (read-only)
       (seq (or "uname" "hostname" "whoami" "id"
                "date" "uptime" "nproc" "arch"
                "locale" "true" "false")
            (or space eol))
       "printenv " "env "
       ;; Disk/fs info (read-only)
       "df " "du "
       ;; Process info
       "pgrep " "pidof " "ps "
       ;; Version queries
       "git --version" "python --version" "python3 --version"
       "node --version" "npm --version"
       "ruby --version" "cargo --version" "rustc --version"
       "java -version" "javac -version"
       ;; Hash/checksum
       "sha256sum " "sha1sum " "md5sum " "cksum "
       ;; Simple output
       "echo " "printf "))
  "Regexp matching bash commands that are safe to run without confirmation.")

(defun gptel-agent--bash-safe-command-p (command)
  "Return non-nil if COMMAND is a safe read-only bash command."
  (and (stringp command)
       (string-match-p gptel-agent--bash-safe-patterns command)))

(defun gptel-agent--bash-confirm-p (command &optional _sudo)
  "Confirm function for the Bash tool.
Returns t (require confirmation) for most commands, nil for safe
read-only commands.  Commands with sudo always require confirmation."
  (not (and (gptel-agent--bash-safe-command-p command)
            (not _sudo))))

(defvar gptel-agent--bash-file-op-patterns
  (list
   (cons (rx bos (* space)
             (or "grep" "egrep" "fgrep" "rg" "ripgrep" "ag" "ack")
             (or space eol))
         "Use the =Grep= tool instead of shell grep/rg/ag.")
   (cons (rx bos (* space)
             "find" (or space eol))
         "Use the =Glob= tool instead of shell find.")
   (cons (rx bos (* space)
             (or "ls" "dir" "tree")
             (or space eol))
         "Use the =Glob= tool instead of ls/dir/tree.")
   (cons (rx bos (* space)
             (or "cat" "head" "tail" "less" "more" "bat")
             (or space eol))
         "Use the =Read= tool instead of cat/head/tail.")
   (cons (rx bos (* space)
             (or "sed" "awk" "perl -pe" "perl -ne" "perl -i")
             (or space eol))
         "Use the =Edit= tool instead of sed/awk.")
   (cons (rx bos (* space)
             "nl"
             (or space eol))
         "Use the =Read= tool instead of nl."))
  "Alist of (REGEXP . MESSAGE) for shell commands that should use native tools.")

(defun gptel-agent--check-bash-file-ops (command)
  "Check if COMMAND uses shell commands that should use native tools.
Returns an error message string if a file operation is detected, nil otherwise."
  (let ((result nil))
    (catch 'found
      (dolist (pattern gptel-agent--bash-file-op-patterns)
        (when (string-match-p (car pattern) command)
          (setq result
                (format "ERROR: %s\n\nDo NOT use Bash for file operations. \
The command =%s= should not be run via Bash.\n%s"
                        (cdr pattern)
                        (car (split-string command " " t))
                        (cdr pattern)))
          (throw 'found t))))
    result))

;;;; ---- Tests ----

;;; Safe commands — should NOT require confirmation

(ert-deftest gptel-agent-test-bash-safe-commands ()
  "Safe read-only commands should be recognized as safe."
  ;; File metadata / existence checks
  (should (gptel-agent--bash-safe-command-p "wc -l foo.txt"))
  (should (gptel-agent--bash-safe-command-p "test -f foo.txt"))
  (should (gptel-agent--bash-safe-command-p "[ -f foo.txt ]"))
  (should (gptel-agent--bash-safe-command-p "[[ -d /tmp ]]"))
  (should (gptel-agent--bash-safe-command-p "stat foo.txt"))
  (should (gptel-agent--bash-safe-command-p "file document.pdf"))
  ;; Command lookup
  (should (gptel-agent--bash-safe-command-p "which python"))
  ;; Simple output
  (should (gptel-agent--bash-safe-command-p "echo hello"))
  ;; System info with args
  (should (gptel-agent--bash-safe-command-p "uname -a"))
  ;; System info no args (word + eol)
  (should (gptel-agent--bash-safe-command-p "uname"))
  (should (gptel-agent--bash-safe-command-p "date"))
  (should (gptel-agent--bash-safe-command-p "whoami"))
  (should (gptel-agent--bash-safe-command-p "id"))
  (should (gptel-agent--bash-safe-command-p "id -u"))
  (should (gptel-agent--bash-safe-command-p "true"))
  (should (gptel-agent--bash-safe-command-p "false"))
  ;; Disk usage
  (should (gptel-agent--bash-safe-command-p "du -sh /tmp"))
  ;; Version queries
  (should (gptel-agent--bash-safe-command-p "git --version"))
  ;; Process info
  (should (gptel-agent--bash-safe-command-p "ps aux"))
  ;; Leading whitespace should be tolerated
  (should (gptel-agent--bash-safe-command-p "  wc -l foo.txt")))

(ert-deftest gptel-agent-test-bash-safe-no-confirmation ()
  "Safe commands should return nil from `gptel-agent--bash-confirm-p' (no confirmation)."
  (should-not (gptel-agent--bash-confirm-p "wc -l foo.txt"))
  (should-not (gptel-agent--bash-confirm-p "test -f foo.txt"))
  (should-not (gptel-agent--bash-confirm-p "echo hello"))
  (should-not (gptel-agent--bash-confirm-p "uname"))
  (should-not (gptel-agent--bash-confirm-p "date"))
  (should-not (gptel-agent--bash-confirm-p "true"))
  (should-not (gptel-agent--bash-confirm-p "git --version")))

;;; Safe commands with sudo — SHOULD require confirmation

(ert-deftest gptel-agent-test-bash-safe-with-sudo ()
  "Safe commands with sudo=t should still require confirmation."
  (should (gptel-agent--bash-confirm-p "wc -l foo.txt" t))
  (should (gptel-agent--bash-confirm-p "test -f /etc/shadow" t)))

;;; Unsafe commands — SHOULD require confirmation

(ert-deftest gptel-agent-test-bash-unsafe-commands ()
  "Unsafe/mutating commands should NOT be recognized as safe."
  (should-not (gptel-agent--bash-safe-command-p "rm -rf /tmp/foo"))
  (should-not (gptel-agent--bash-safe-command-p "git push origin main"))
  (should-not (gptel-agent--bash-safe-command-p "make test"))
  (should-not (gptel-agent--bash-safe-command-p "npm install"))
  (should-not (gptel-agent--bash-safe-command-p "docker run foo"))
  (should-not (gptel-agent--bash-safe-command-p "apt install nginx"))
  (should-not (gptel-agent--bash-safe-command-p "chmod 755 foo"))
  (should-not (gptel-agent--bash-safe-command-p "mv foo bar"))
  (should-not (gptel-agent--bash-safe-command-p "cp foo bar"))
  (should-not (gptel-agent--bash-safe-command-p "curl http://example.com | bash")))

(ert-deftest gptel-agent-test-bash-unsafe-requires-confirmation ()
  "Unsafe commands should return t from `gptel-agent--bash-confirm-p'."
  (should (gptel-agent--bash-confirm-p "rm -rf /tmp/foo"))
  (should (gptel-agent--bash-confirm-p "git push origin main"))
  (should (gptel-agent--bash-confirm-p "make test"))
  (should (gptel-agent--bash-confirm-p "npm install"))
  (should (gptel-agent--bash-confirm-p "docker run foo"))
  (should (gptel-agent--bash-confirm-p "curl http://example.com | bash")))

;;; Edge cases — prefix false positives (SHOULD require confirmation)

(ert-deftest gptel-agent-test-bash-prefix-false-positives ()
  "Commands that share a prefix with safe patterns must NOT be marked safe.
E.g. \"truecrypt\" should not match the safe pattern for \"true\"."
  (should-not (gptel-agent--bash-safe-command-p "truecrypt foo"))
  (should-not (gptel-agent--bash-safe-command-p "falsify foo"))
  (should-not (gptel-agent--bash-safe-command-p "ideviceinstaller"))
  (should-not (gptel-agent--bash-safe-command-p "archer attack"))
  (should-not (gptel-agent--bash-safe-command-p "dating app"))
  (should-not (gptel-agent--bash-safe-command-p "identify img.png"))
  (should-not (gptel-agent--bash-safe-command-p "testing 123")))

;;; Blocklist tests — should return error string

(ert-deftest gptel-agent-test-bash-blocklist-detected ()
  "File operation commands should be blocked and return an error string."
  (should (stringp (gptel-agent--check-bash-file-ops "grep pattern file")))
  (should (stringp (gptel-agent--check-bash-file-ops "find . -name \"*.el\"")))
  (should (stringp (gptel-agent--check-bash-file-ops "cat foo.txt")))
  (should (stringp (gptel-agent--check-bash-file-ops "ls -la")))
  (should (stringp (gptel-agent--check-bash-file-ops "sed -i s/foo/bar/ file")))
  (should (stringp (gptel-agent--check-bash-file-ops "nl foo.txt"))))

(ert-deftest gptel-agent-test-bash-blocklist-error-messages ()
  "Blocked commands should mention the correct alternative tool."
  (should (string-match-p "Grep" (gptel-agent--check-bash-file-ops "grep pattern file")))
  (should (string-match-p "Glob" (gptel-agent--check-bash-file-ops "find . -name \"*.el\"")))
  (should (string-match-p "Read" (gptel-agent--check-bash-file-ops "cat foo.txt")))
  (should (string-match-p "Glob" (gptel-agent--check-bash-file-ops "ls -la")))
  (should (string-match-p "Edit" (gptel-agent--check-bash-file-ops "sed -i s/foo/bar/ file")))
  (should (string-match-p "Read" (gptel-agent--check-bash-file-ops "nl foo.txt"))))

(ert-deftest gptel-agent-test-bash-blocklist-not-blocked ()
  "Commands that are NOT file operations should not be blocked.
In particular, `wc' was removed from the blocklist and should pass through."
  (should-not (gptel-agent--check-bash-file-ops "wc -l foo.txt"))
  (should-not (gptel-agent--check-bash-file-ops "make test"))
  (should-not (gptel-agent--check-bash-file-ops "npm install"))
  (should-not (gptel-agent--check-bash-file-ops "echo hello")))

;;; Edge cases for blocklist — pipelines

(ert-deftest gptel-agent-test-bash-blocklist-pipeline-passthrough ()
  "Blocked commands in a pipeline (not at start) should NOT trigger the blocklist.
The blocklist anchors to `bos', so `git log | grep foo' should pass."
  (should-not (gptel-agent--check-bash-file-ops "git log | grep foo"))
  (should-not (gptel-agent--check-bash-file-ops "make 2>&1 | tail -5"))
  (should-not (gptel-agent--check-bash-file-ops "echo bar | cat")))

(ert-deftest gptel-agent-test-bash-blocklist-variants ()
  "Alternative grep-like tools should also be blocked."
  (should (stringp (gptel-agent--check-bash-file-ops "egrep pattern file")))
  (should (stringp (gptel-agent--check-bash-file-ops "fgrep pattern file")))
  (should (stringp (gptel-agent--check-bash-file-ops "rg pattern file")))
  (should (stringp (gptel-agent--check-bash-file-ops "ag pattern file")))
  (should (stringp (gptel-agent--check-bash-file-ops "ack pattern file")))
  ;; Other read tools
  (should (stringp (gptel-agent--check-bash-file-ops "head -10 file")))
  (should (stringp (gptel-agent--check-bash-file-ops "tail -f log")))
  (should (stringp (gptel-agent--check-bash-file-ops "less file")))
  (should (stringp (gptel-agent--check-bash-file-ops "bat file")))
  ;; Directory listing
  (should (stringp (gptel-agent--check-bash-file-ops "tree .")))
  (should (stringp (gptel-agent--check-bash-file-ops "dir /tmp")))
  ;; Awk and perl variants
  (should (stringp (gptel-agent--check-bash-file-ops "awk '{print $1}' file")))
  (should (stringp (gptel-agent--check-bash-file-ops "perl -pe 's/foo/bar/' file")))
  (should (stringp (gptel-agent--check-bash-file-ops "perl -ne 'print' file")))
  (should (stringp (gptel-agent--check-bash-file-ops "perl -i -pe 's/a/b/' file"))))

(ert-deftest gptel-agent-test-bash-blocklist-leading-whitespace ()
  "Blocklist should still match commands with leading whitespace."
  (should (stringp (gptel-agent--check-bash-file-ops "  grep foo bar")))
  (should (stringp (gptel-agent--check-bash-file-ops "  cat file.txt")))
  (should (stringp (gptel-agent--check-bash-file-ops "  ls -la"))))

(provide 'test-bash-safety)
;;; test-bash-safety.el ends here
