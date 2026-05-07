;;; gptel-agent-test.el --- Tests for gptel-agent loader  -*- lexical-binding: t; -*-

;; Copyright (C) 2025

;; Author: gptel-agent tests

;;; Commentary:

;; ERT tests for `gptel-agent-parse-org-properties' and related
;; loader behaviour.
;;
;; Run from the repo root:
;;
;;   cd test && make test

;;; Code:

(require 'ert)
(require 'gptel)
(require 'gptel-org)
(require 'gptel-agent)
(require 'gptel-org-agent)
(require 'gptel-agent-tools)

(defconst gptel-agent-test--agents-dir
  (expand-file-name
   "agents"
   (file-name-directory
    (directory-file-name
     (file-name-directory (or load-file-name buffer-file-name))))))

(ert-deftest agent-plist-includes-state-words ()
  "Loader parses :state-words: triad and defaults when absent."
  ;; Canonical agent with a known triad.
  (let* ((gatherer (expand-file-name "gatherer.org"
                                     gptel-agent-test--agents-dir))
         (plist (gptel-agent-parse-org-properties gatherer nil nil t)))
    (should (equal (plist-get plist :state-words)
                   '("GATHER" "GATHERING" "GATHERED"))))

  ;; Default case: a property block without :state-words:.
  (let ((tmp (make-temp-file "gptel-agent-test-" nil ".org")))
    (unwind-protect
        (progn
          (with-temp-file tmp
            (insert ":PROPERTIES:\n"
                    ":name: dummy\n"
                    ":description: no state words here\n"
                    ":END:\n"
                    "Body.\n"))
          (let ((plist (gptel-agent-parse-org-properties tmp nil nil t)))
            (should (equal (plist-get plist :state-words)
                           '("PENDING" "RUNNING" "DONE")))))
      (delete-file tmp)))

  ;; Malformed triad: two tokens -> user-error naming the file.
  (let ((tmp (make-temp-file "gptel-agent-test-" nil ".org")))
    (unwind-protect
        (progn
          (with-temp-file tmp
            (insert ":PROPERTIES:\n"
                    ":name: bad\n"
                    ":state-words: ONLY TWO\n"
                    ":END:\n"))
          (let ((err (should-error
                      (gptel-agent-parse-org-properties tmp nil nil t)
                      :type 'user-error)))
            ;; Error message should mention the file path.
            (should (string-match-p (regexp-quote tmp)
                                    (error-message-string err)))))
      (delete-file tmp))))

(ert-deftest state-words-fallback-returns-default ()
  "`gptel-agent-state-words' returns the default triad for unknown agents.

When AGENT-NAME is not a key in `gptel-agent--agents', or when its
plist carries no :state-words, the accessor must fall back to
\(\"PENDING\" \"RUNNING\" \"DONE\") so call sites always receive a
valid three-token triad."
  ;; Unknown agent: empty registry.
  (let ((gptel-agent--agents nil))
    (should (equal '("PENDING" "RUNNING" "DONE")
                   (gptel-agent-state-words "no-such-agent"))))
  ;; Known agent without :state-words property: still defaults.
  (let ((gptel-agent--agents '(("legacy" :description "no triad here"))))
    (should (equal '("PENDING" "RUNNING" "DONE")
                   (gptel-agent-state-words "legacy"))))
  ;; Known agent with :state-words: returns the stored triad.
  (let ((gptel-agent--agents
         '(("gatherer" :state-words ("GATHER" "GATHERING" "GATHERED")))))
    (should (equal '("GATHER" "GATHERING" "GATHERED")
                   (gptel-agent-state-words "gatherer"))))
  ;; Case-sensitive lookup: a mismatched-case name falls back.
  (let ((gptel-agent--agents
         '(("gatherer" :state-words ("GATHER" "GATHERING" "GATHERED")))))
    (should (equal '("PENDING" "RUNNING" "DONE")
                   (gptel-agent-state-words "Gatherer")))))

(ert-deftest scaffold-includes-state-words ()
  "`gptel-agent-scaffold-agent' writes a well-formed agent file.

Verifies that the generated file contains :name:, :description:,
:tools:, and a :state-words: triad of three uppercase tokens whose
first token is the upcased agent name.  Also verifies that the file
has non-trivial body content beyond the property drawer."
  (let ((temp-dir (make-temp-file "gptel-agent-scaffold-" 'dir)))
    (unwind-protect
        (let* ((path (gptel-agent-scaffold-agent "test-agent" temp-dir))
               (content (with-temp-buffer
                          (insert-file-contents path)
                          (buffer-string))))
          ;; Return value is the path.
          (should (equal path (expand-file-name "test-agent.org" temp-dir)))
          (should (file-exists-p path))

          ;; :name: matches.
          (should (string-match-p "^:name: test-agent$" content))

          ;; :description: present (may contain TODO placeholder).
          (should (string-match-p "^:description: .+" content))

          ;; :tools: present.
          (should (string-match-p "^:tools: .+" content))

          ;; :state-words: triad of three whitespace-separated uppercase
          ;; tokens, first token = TEST-AGENT.
          (should (string-match
                   "^:state-words:[ \t]+\\([A-Z0-9-]+\\)[ \t]+\\([A-Z0-9-]+\\)[ \t]+\\([A-Z0-9-]+\\)[ \t]*$"
                   content))
          (should (equal "TEST-AGENT" (match-string 1 content)))
          ;; Doing and done should also be uppercase, non-empty, and
          ;; distinct from the REQUEST token.
          (let ((doing (match-string 2 content))
                (done  (match-string 3 content)))
            (should (and doing (> (length doing) 0)))
            (should (and done  (> (length done)  0)))
            (should (not (equal doing "TEST-AGENT")))
            (should (not (equal done  "TEST-AGENT"))))

          ;; Reparse via the loader: the triad must round-trip.
          (let ((plist (gptel-agent-parse-org-properties path nil nil t)))
            (let ((sw (plist-get plist :state-words)))
              (should (listp sw))
              (should (= 3 (length sw)))
              (should (equal "TEST-AGENT" (car sw)))))

          ;; Body content beyond the property drawer: strip the drawer
          ;; and check there's meaningful text left.
          (let ((body-start (string-match "^:END:$" content)))
            (should body-start)
            (let ((body (substring content
                                   (match-end 0))))
              (should (> (length (string-trim body)) 20)))))
      (delete-directory temp-dir t))))

(ert-deftest scaffold-rejects-bad-name-and-existing-file ()
  "`gptel-agent-scaffold-agent' validates NAME and refuses to overwrite."
  (let ((temp-dir (make-temp-file "gptel-agent-scaffold-" 'dir)))
    (unwind-protect
        (progn
          ;; Malformed names.
          (should-error (gptel-agent-scaffold-agent "Bad Name" temp-dir)
                        :type 'user-error)
          (should-error (gptel-agent-scaffold-agent "1agent" temp-dir)
                        :type 'user-error)
          (should-error (gptel-agent-scaffold-agent "" temp-dir)
                        :type 'user-error)
          ;; Uppercase letters are not allowed either.
          (should-error (gptel-agent-scaffold-agent "FooBar" temp-dir)
                        :type 'user-error)

          ;; Now create a valid one, then try to create it again:
          ;; second call must raise `user-error'.
          (gptel-agent-scaffold-agent "dupe" temp-dir)
          (should (file-exists-p (expand-file-name "dupe.org" temp-dir)))
          (should-error (gptel-agent-scaffold-agent "dupe" temp-dir)
                        :type 'user-error))
      (delete-directory temp-dir t))))


(ert-deftest gptel-org-agent-test-subagent-cleanup-skips-when-from-pending ()
  "Sub-agent cleanup leaves the PENDING IB alive when `:from-pending-id' is set.

Codifies the IB-7 ownership rule: when the sub-agent was dispatched
from a PENDING tool-call (`:from-pending-id' set in the FSM info),
`gptel-agent-subtree--cleanup' must NOT close the IB or transition
the heading.  That ownership belongs to
`gptel-org-agent--update-tool-heading', which runs AFTER the tool
result is delivered to the parent FSM.

If cleanup closed the IB, `--update-tool-heading' would find the
entry but bail on `(buffer-live-p pending-ib)', and the
display-tool-results-advice path would fall through to its default
handler — creating an orphan heading."
  (require 'gptel-agent-tools)
  (let ((org-inhibit-startup t)
        (org-todo-keywords '((sequence "AI-DO" "AI-DOING" "DOING"
                                       "PENDING" "ALLOWED"
                                       "EXECUTE" "EXECUTING"
                                       "FEEDBACK"
                                       "|" "AI-DONE" "DENIED"
                                       "EXECUTED")))
        (gptel-org-todo-keywords '("AI-DO" "AI-DOING")))
    (with-temp-buffer
      (delay-mode-hooks (org-mode))
      (insert "** DOING Dispatch executor\nDescription\n")
      (goto-char (point-min))
      (org-back-to-heading t)
      (let* ((gptel-org-subtree-context t)
             (gptel-org-use-todo-keywords t)
             (gptel-org-tasks-doing-keyword "AI-DOING")
             (gptel-org-agent-tool-confirm-keywords
              '("PENDING" "ALLOWED" "DENIED"))
             (base-buf (current-buffer))
             (marker (gptel-org-agent--create-subtree "main"))
             (indirect-buf nil)
             (pending-ib nil)
             (close-called nil))
        (clrhash gptel-org-agent--pending-tool-calls)
        (unwind-protect
            (cl-letf*
                (((symbol-function 'gptel-agent-state-words)
                  (lambda (agent-name)
                    (if (equal agent-name "executor")
                        '("EXECUTE" "EXECUTING" "EXECUTED")
                      '("PENDING" "RUNNING" "DONE"))))
                 ;; Spy on close to verify it's not called.
                 (orig-close (symbol-function
                              'gptel-org-agent--close-indirect-buffer))
                 ((symbol-function 'gptel-org-agent--close-indirect-buffer)
                  (lambda (buf &optional fold)
                    (setq close-called t)
                    (funcall orig-close buf fold))))
              (setq indirect-buf
                    (gptel-org-agent--open-indirect-buffer base-buf marker))
              (let* ((tool-spec (gptel-make-tool
                                 :name "Agent"
                                 :function #'ignore
                                 :description "Dispatch sub-agent"
                                 :args '((:name "subagent_type"
                                          :type "string"
                                          :description "agent")
                                         (:name "prompt"
                                          :type "string"
                                          :description "prompt"))
                                 :confirm t
                                 :include t))
                     (arg-plist '(:subagent_type "executor"
                                  :prompt "date"))
                     (tool-calls (list (list tool-spec arg-plist
                                             (lambda (_) nil))))
                     (position-marker
                      (with-current-buffer indirect-buf
                        (save-excursion
                          (goto-char (point-max))
                          (copy-marker (point) nil))))
                     (info (list :buffer indirect-buf
                                 :position position-marker
                                 :tracking-marker position-marker
                                 :callback #'gptel--insert-response
                                 :tools (list tool-spec)))
                     (pending-id nil))
                ;; Set up PENDING entry + IB.
                (with-current-buffer indirect-buf
                  (gptel-org-agent--display-tool-calls tool-calls info))
                (maphash (lambda (id e)
                           (setq pending-id id)
                           (setq pending-ib (plist-get e :pending-ib)))
                         gptel-org-agent--pending-tool-calls)
                ;; Hand-craft a sub-agent FSM info plist with
                ;; :from-pending-id set, mimicking what gptel-agent--task
                ;; does when dispatched via the new path.
                (let* ((heading-marker
                        (plist-get
                         (gethash pending-id
                                  gptel-org-agent--pending-tool-calls)
                         :heading-marker))
                       (sub-fsm
                        (gptel-make-fsm
                         :state 'DONE
                         :info (list :buffer base-buf
                                     :agent-main-cb (lambda (_) nil)
                                     :agent-indirect-buffer pending-ib
                                     :agent-heading-marker heading-marker
                                     :agent-type "executor"
                                     :agent-description "Run date"
                                     :from-pending-id pending-id))))
                  (gptel-agent-subtree--cleanup sub-fsm)
                  ;; ASSERTION: cleanup did NOT close the IB.
                  (should-not close-called)
                  (should (buffer-live-p pending-ib))
                  ;; ASSERTION: cleanup did NOT transition the heading.
                  ;; The heading still has its PENDING/ALLOWED REQ word
                  ;; (raw, since we didn't simulate the user transition).
                  (with-current-buffer pending-ib
                    (save-excursion
                      (goto-char (point-min))
                      (should (org-at-heading-p))
                      (should (equal "PENDING"
                                     (org-get-todo-state))))))))
          (clrhash gptel-org-agent--pending-tool-calls)
          (when (and pending-ib (buffer-live-p pending-ib))
            (kill-buffer pending-ib))
          (when (and indirect-buf (buffer-live-p indirect-buf))
            (kill-buffer indirect-buf)))))))

;;; Preview-setup nil-arg defensiveness

(defun gptel-agent-test--call-preview (fn arg-values)
  "Invoke preview-setup FN with ARG-VALUES in a temp buffer.
Returns the buffer string on success.  Re-signals errors."
  (with-temp-buffer
    (funcall fn arg-values nil)
    (buffer-substring-no-properties (point-min) (point-max))))

(ert-deftest gptel-agent-edit-files-preview-setup-handles-nil-args ()
  "Edit preview must not crash when args other than path are nil.

Regression test for the (wrong-type-argument stringp nil) crash that
occurred when the model issued an Edit call with only =path=."
  (let ((s (gptel-agent-test--call-preview
            #'gptel-agent--edit-files-preview-setup
            '("foo.el" nil nil nil))))
    (should (string-match-p "Edit\\?" s))
    (should (string-match-p "foo.el" s))
    (should (string-match-p "malformed Edit call" s)))
  ;; Patch branch with nil diff payload must not crash.
  (let ((s (gptel-agent-test--call-preview
            #'gptel-agent--edit-files-preview-setup
            '("foo.el" nil nil t))))
    (should (string-match-p "Edit\\?" s)))
  ;; Text-replacement branch with old-str but nil new-str-or-diff.
  (let ((s (gptel-agent-test--call-preview
            #'gptel-agent--edit-files-preview-setup
            '("foo.el" "old" nil nil))))
    (should (string-match-p "ReplaceIn" s))
    (should (string-match-p "missing new_str" s))))

(ert-deftest gptel-agent-write-file-preview-setup-handles-nil-args ()
  "Write preview must not crash when content is nil."
  (let ((s (gptel-agent-test--call-preview
            #'gptel-agent--write-file-preview-setup
            '("." "foo.el" nil))))
    (should (string-match-p "Write" s))
    (should (string-match-p "missing content" s))))

(ert-deftest gptel-agent-eval-elisp-preview-setup-handles-nil-args ()
  "Eval preview must not crash when expression is nil."
  (let ((s (gptel-agent-test--call-preview
            #'gptel-agent--eval-elisp-preview-setup
            '(nil))))
    (should (string-match-p "Eval" s))
    (should (string-match-p "missing expression" s))))

(ert-deftest gptel-agent-execute-bash-preview-setup-handles-nil-args ()
  "Bash preview must not crash when command is nil."
  (let ((s (gptel-agent-test--call-preview
            #'gptel-agent--execute-bash-preview-setup
            '(nil nil))))
    (should (string-match-p "Bash" s))
    (should (string-match-p "missing command" s))))

(ert-deftest gptel-agent-insert-in-file-preview-setup-handles-nil-args ()
  "Insert preview must not crash when args are nil."
  (let ((s (gptel-agent-test--call-preview
            #'gptel-agent--insert-in-file-preview-setup
            '(nil nil nil))))
    (should (string-match-p "insert_into_file" s))
    (should (string-match-p "File not readable" s))))

(ert-deftest gptel-agent-task-preview-setup-handles-nil-args ()
  "Agent (task) preview must not crash when args are nil."
  (let ((s (gptel-agent-test--call-preview
            #'gptel-agent--task-preview-setup
            '(nil nil nil))))
    (should (string-match-p "Agent" s))
    (should (string-match-p "no type" s))))

(provide 'gptel-agent-test)
;;; gptel-agent-test.el ends here
