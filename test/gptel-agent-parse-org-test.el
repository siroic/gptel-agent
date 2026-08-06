;;; gptel-agent-parse-org-test.el --- Tests for org agent parsing  -*- lexical-binding: t; -*-

;;; Commentary:

;; ERT tests for the org agent file directory scan filter and for
;; `#+transclude:' resolution in `gptel-agent-parse-org-properties'.

;;; Code:

(require 'ert)
(require 'gptel-agent)

(defun gptel-agent-parse-org-test--make-dir ()
  "Return a fresh temporary directory for fixtures."
  (file-name-as-directory (make-temp-file "gptel-agent-parse-org-test" t)))

(defun gptel-agent-parse-org-test--write (dir name content)
  "Write CONTENT to file NAME in DIR and return its full path."
  (let ((path (expand-file-name name dir)))
    (with-temp-file path
      (insert content))
    path))

(defun gptel-agent-parse-org-test--agent-org (body)
  "Return the text of an org agent definition file with BODY."
  (concat ":PROPERTIES:\n"
          ":NAME: transclude-agent\n"
          ":DESCRIPTION: A test agent\n"
          ":END:\n"
          "\n"
          body))

(ert-deftest gptel-agent-parse-org-test-directory-scan-filter ()
  "Only well-formed .org/.md files are picked up as agent definitions."
  (let ((dir (gptel-agent-parse-org-test--make-dir)))
    (unwind-protect
        (progn
          (gptel-agent-parse-org-test--write
           dir "valid.org"
           ":PROPERTIES:\n:NAME: valid-org-agent\n:DESCRIPTION: A test agent\n:END:\n\nBody text here.\n")
          (gptel-agent-parse-org-test--write
           dir "valid.md"
           "---\nname: valid-md-agent\ndescription: A test agent\n---\n\nBody text here.\n")
          (gptel-agent-parse-org-test--write dir "valid.org~" "junk\n")
          (gptel-agent-parse-org-test--write dir "#valid.org#" "junk\n")
          (gptel-agent-parse-org-test--write dir ".#lock.org" "junk\n")
          (gptel-agent-parse-org-test--write dir "notes.txt" "junk\n")
          (let ((gptel-agent-dirs (list dir))
                (gptel-agent--agents nil))
            (let* ((result (gptel-agent--update-agents))
                   (names (sort (mapcar #'car result) #'string<))
                   (paths (mapcar #'cdr result)))
              (should (= 2 (length result)))
              (should (equal names '("valid-md-agent" "valid-org-agent")))
              (dolist (junk '("valid.org~" "#valid.org#" ".#lock.org" "notes.txt"))
                (should-not (member (expand-file-name junk dir) paths)))
              (should (= 2 (length gptel-agent--agents)))
              (should (equal (sort (mapcar #'car gptel-agent--agents) #'string<)
                             '("valid-md-agent" "valid-org-agent")))
              (dolist (bad '("valid.org" "#valid" "lock" "notes" "valid.org~" ".#lock"))
                (should-not (assoc bad gptel-agent--agents))))))
      (delete-directory dir t))))

(ert-deftest gptel-agent-parse-org-test-transclusion-includes-target ()
  "A `#+transclude:' keyword is replaced by the target subtree."
  (let ((dir (gptel-agent-parse-org-test--make-dir)))
    (unwind-protect
        (progn
          (gptel-agent-parse-org-test--write
           dir "common.org" "* Shared\nSHARED-BODY-TEXT\n")
          (let* ((agent-file
                  (gptel-agent-parse-org-test--write
                   dir "agent.org"
                   (gptel-agent-parse-org-test--agent-org
                    "#+transclude: [[file:common.org::*Shared]]\n")))
                 (system (plist-get (gptel-agent-parse-org-properties
                                     agent-file nil nil nil)
                                    :system)))
            (should (stringp system))
            (should (string-match-p "SHARED-BODY-TEXT" system))
            (should-not (let ((case-fold-search t))
                          (string-match-p "#\\+transclude" system)))
            (should (string-match-p "Shared" system))))
      (delete-directory dir t))))

(ert-deftest gptel-agent-parse-org-test-transclusion-only-contents ()
  "The `:only-contents' option drops the transcluded headline."
  (let ((dir (gptel-agent-parse-org-test--make-dir)))
    (unwind-protect
        (progn
          (gptel-agent-parse-org-test--write
           dir "common.org" "* Shared\nSHARED-BODY-TEXT\n")
          (let* ((agent-file
                  (gptel-agent-parse-org-test--write
                   dir "agent.org"
                   (gptel-agent-parse-org-test--agent-org
                    "#+transclude: [[file:common.org::*Shared]] :only-contents\n")))
                 (system (plist-get (gptel-agent-parse-org-properties
                                     agent-file nil nil nil)
                                    :system)))
            (should (stringp system))
            (should (string-match-p "SHARED-BODY-TEXT" system))
            (should-not (string-match-p "^\\*+ Shared" system))))
      (delete-directory dir t))))

(ert-deftest gptel-agent-parse-org-test-transclusion-expands-templates ()
  "Templates are expanded inside read-only transcluded text."
  (let ((dir (gptel-agent-parse-org-test--make-dir)))
    (unwind-protect
        (progn
          (gptel-agent-parse-org-test--write
           dir "common.org" "* Shared\nHello {{WHO}}\n")
          (let* ((agent-file
                  (gptel-agent-parse-org-test--write
                   dir "agent.org"
                   (gptel-agent-parse-org-test--agent-org
                    "#+transclude: [[file:common.org::*Shared]]\n")))
                 (system (plist-get (gptel-agent-parse-org-properties
                                     agent-file nil '(("WHO" . "World")) nil)
                                    :system)))
            (should (stringp system))
            (should (string-match-p "Hello World" system))
            (should-not (string-match-p "{{WHO}}" system))))
      (delete-directory dir t))))

(ert-deftest gptel-agent-parse-org-test-transclusion-missing-target-errors ()
  "An unresolvable `#+transclude:' keyword signals an error."
  (let ((dir (gptel-agent-parse-org-test--make-dir)))
    (unwind-protect
        (let ((agent-file
               (gptel-agent-parse-org-test--write
                dir "agent.org"
                (gptel-agent-parse-org-test--agent-org
                 "#+transclude: [[file:does-not-exist.org::*Nope]]\n"))))
          (should-error (gptel-agent-parse-org-properties agent-file nil nil nil)))
      (delete-directory dir t))))

(ert-deftest gptel-agent-parse-org-test-metadata-only-skips-transclusion ()
  "METADATA-ONLY parsing ignores the body and its transclusions."
  (let ((dir (gptel-agent-parse-org-test--make-dir)))
    (unwind-protect
        (let* ((agent-file
                (gptel-agent-parse-org-test--write
                 dir "agent.org"
                 (gptel-agent-parse-org-test--agent-org
                  "#+transclude: [[file:does-not-exist.org::*Nope]]\n")))
               (plist (gptel-agent-parse-org-properties agent-file nil nil t)))
          (should (equal (plist-get plist :name) "transclude-agent"))
          (should (equal (plist-get plist :description) "A test agent"))
          (should-not (plist-member plist :system)))
      (delete-directory dir t))))

(provide 'gptel-agent-parse-org-test)
;;; gptel-agent-parse-org-test.el ends here
