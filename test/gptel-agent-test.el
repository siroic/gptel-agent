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
(require 'gptel-agent)

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

(provide 'gptel-agent-test)
;;; gptel-agent-test.el ends here
