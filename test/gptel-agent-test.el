;;; gptel-agent-test.el --- Tests for gptel-agent loader  -*- lexical-binding: t; -*-

;; Copyright (C) 2025

;; Author: gptel-agent tests

;;; Commentary:

;; ERT tests for `gptel-agent-parse-org-properties' and related
;; loader behaviour.
;;
;; Run from the repo root:
;;
;;   emacs -Q --batch \
;;     -L . -L ../gptel \
;;     -l gptel-agent.el \
;;     -l test/gptel-agent-test.el \
;;     -f ert-run-tests-batch-and-exit

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

(provide 'gptel-agent-test)
;;; gptel-agent-test.el ends here
