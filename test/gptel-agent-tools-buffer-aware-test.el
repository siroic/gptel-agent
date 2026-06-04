;;; gptel-agent-tools-buffer-aware-test.el --- Buffer-aware Read/Edit/Insert  -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; ERT tests for buffer-aware behavior of the Read, Edit, and Insert
;; agent file tools.  Each test creates a tempfile, optionally visits
;; it, runs the tool, and verifies that the tool consulted the visiting
;; buffer (when one exists) instead of going straight to disk.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'gptel-agent-tools)

(defconst gptel-agent-test--annotation
  ";; [gptel-agent] Reading from visiting buffer with unsaved edits.\n"
  "Annotation line that Read prepends when the visiting buffer is modified.")

(defmacro gptel-agent-test--with-fixture (vars &rest body)
  "Bind VARS for a tempfile and ensure cleanup of file + visiting buffer.

VARS is a plist-style spec: (PATH-SYM CONTENT).  The file is created
with CONTENT and its path bound to PATH-SYM.  After BODY runs, any
buffer visiting the file is killed (without saving) and the file is
deleted."
  (declare (indent 1))
  (let ((path-sym (nth 0 vars))
        (content (nth 1 vars)))
    `(let ((,path-sym (make-temp-file "gptel-agent-test-" nil ".txt" ,content)))
       (unwind-protect
           (progn ,@body)
         (let ((buf (find-buffer-visiting ,path-sym)))
           (when (buffer-live-p buf)
             (with-current-buffer buf
               (set-buffer-modified-p nil))
             (kill-buffer buf)))
         (when (file-exists-p ,path-sym)
           (delete-file ,path-sym))))))

;;; Read

(ert-deftest gptel-agent-test/read-visiting-unmodified ()
  "Read with a visiting, unmodified buffer returns disk content, no annotation."
  (gptel-agent-test--with-fixture (path "line 1\nline 2\nline 3\n")
    (let ((buf (find-file-noselect path)))
      (should (buffer-live-p buf))
      (should-not (buffer-modified-p buf))
      (let ((result (gptel-agent--read-file-lines path nil nil)))
        (should (equal result "line 1\nline 2\nline 3\n"))
        (should-not (string-prefix-p ";; [gptel-agent]" result))))))

(ert-deftest gptel-agent-test/read-visiting-modified ()
  "Read with a visiting modified buffer returns buffer content with annotation."
  (gptel-agent-test--with-fixture (path "disk line 1\ndisk line 2\n")
    (let ((buf (find-file-noselect path)))
      (with-current-buffer buf
        (goto-char (point-max))
        (insert "buffer-only line\n")
        (should (buffer-modified-p)))
      (let ((result (gptel-agent--read-file-lines path nil nil)))
        (should (string-prefix-p gptel-agent-test--annotation result))
        ;; Strip the annotation and compare to buffer content.
        (let ((without-annot (substring result (length gptel-agent-test--annotation))))
          (should (equal without-annot
                         "disk line 1\ndisk line 2\nbuffer-only line\n")))
        ;; Sanity: disk content unchanged.
        (with-temp-buffer
          (insert-file-contents path)
          (should (equal (buffer-string) "disk line 1\ndisk line 2\n")))))))

(ert-deftest gptel-agent-test/read-no-visiting-buffer ()
  "Read with no visiting buffer returns disk content unchanged."
  (gptel-agent-test--with-fixture (path "alpha\nbeta\ngamma\n")
    (should-not (find-buffer-visiting path))
    (let ((result (gptel-agent--read-file-lines path nil nil)))
      (should (equal result "alpha\nbeta\ngamma\n")))))

(ert-deftest gptel-agent-test/read-line-range-from-modified-buffer ()
  "Read with start/end line slices the visiting (modified) buffer."
  (gptel-agent-test--with-fixture (path "one\ntwo\nthree\n")
    (let ((buf (find-file-noselect path)))
      (with-current-buffer buf
        (goto-char (point-max))
        (insert "four\nfive\nsix\n")
        (should (buffer-modified-p)))
      (let ((result (gptel-agent--read-file-lines path 4 5)))
        (should (string-prefix-p gptel-agent-test--annotation result))
        (let ((without-annot (substring result (length gptel-agent-test--annotation))))
          (should (equal without-annot "four\nfive\n")))))))

;;; Edit (string mode)

(ert-deftest gptel-agent-test/edit-string-visiting-unmodified ()
  "Edit on a visiting unmodified buffer updates buffer + disk, leaves buffer clean."
  (gptel-agent-test--with-fixture (path "hello world\ngoodbye\n")
    (let ((buf (find-file-noselect path)))
      (should-not (buffer-modified-p buf))
      (gptel-agent--edit-files path "hello world" "HELLO WORLD" :json-false)
      ;; Buffer reflects change.
      (with-current-buffer buf
        (should (equal (buffer-string) "HELLO WORLD\ngoodbye\n"))
        ;; Saved during the edit -> not modified.
        (should-not (buffer-modified-p)))
      ;; Disk reflects change.
      (with-temp-buffer
        (insert-file-contents path)
        (should (equal (buffer-string) "HELLO WORLD\ngoodbye\n"))))))

(ert-deftest gptel-agent-test/edit-string-visiting-modified-elsewhere ()
  "Edit proceeds when buffer modified elsewhere; both edits land on disk."
  (gptel-agent-test--with-fixture (path "AAA\nBBB\nCCC\n")
    (let ((buf (find-file-noselect path)))
      ;; User makes an unsaved edit somewhere `old-str' does not match.
      (with-current-buffer buf
        (goto-char (point-max))
        (insert "DDD\n")
        (should (buffer-modified-p)))
      ;; Agent edits a region the user did not touch.
      (gptel-agent--edit-files path "BBB" "BBBBB" :json-false)
      (with-current-buffer buf
        (should (equal (buffer-string) "AAA\nBBBBB\nCCC\nDDD\n"))
        (should-not (buffer-modified-p)))
      ;; Both edits should be on disk.
      (with-temp-buffer
        (insert-file-contents path)
        (should (equal (buffer-string) "AAA\nBBBBB\nCCC\nDDD\n"))))))

(ert-deftest gptel-agent-test/edit-string-no-visiting-buffer ()
  "Edit (string mode) with no visiting buffer still works via disk path."
  (gptel-agent-test--with-fixture (path "foo\nbar\nbaz\n")
    (should-not (find-buffer-visiting path))
    (gptel-agent--edit-files path "bar" "BAR" :json-false)
    (with-temp-buffer
      (insert-file-contents path)
      (should (equal (buffer-string) "foo\nBAR\nbaz\n")))))

;;; Insert

(ert-deftest gptel-agent-test/insert-visiting-unmodified ()
  "Insert with a visiting unmodified buffer updates buffer + disk, leaves clean."
  (gptel-agent-test--with-fixture (path "x\ny\nz\n")
    (let ((buf (find-file-noselect path)))
      (should-not (buffer-modified-p buf))
      (gptel-agent--insert-in-file path 0 "INSERTED\n")
      (with-current-buffer buf
        (should (equal (buffer-string) "INSERTED\nx\ny\nz\n"))
        (should-not (buffer-modified-p)))
      (with-temp-buffer
        (insert-file-contents path)
        (should (equal (buffer-string) "INSERTED\nx\ny\nz\n"))))))

(ert-deftest gptel-agent-test/insert-no-visiting-buffer ()
  "Insert with no visiting buffer still writes to disk."
  (gptel-agent-test--with-fixture (path "a\nb\nc\n")
    (should-not (find-buffer-visiting path))
    (gptel-agent--insert-in-file path -1 "END\n")
    (with-temp-buffer
      (insert-file-contents path)
      (should (equal (buffer-string) "a\nb\nc\nEND\n")))))

(provide 'gptel-agent-tools-buffer-aware-test)
;;; gptel-agent-tools-buffer-aware-test.el ends here
