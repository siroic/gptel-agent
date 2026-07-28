;;; gptel-agent-template-test.el --- Tests for template expansion  -*- lexical-binding: t; -*-

;;; Commentary:

;; ERT tests for `gptel-agent--session-templates' and
;; `gptel-agent--expand-templates'.

;;; Code:

(require 'ert)
(require 'gptel-agent)

(defun gptel-agent-template-test--value (key)
  "Return the session template value for KEY."
  (cdr (assoc key (gptel-agent--session-templates))))

(ert-deftest gptel-agent-template-test-session-templates-keys ()
  "Session templates provide USER, HOME and HOST as non-empty strings."
  (let ((templates (gptel-agent--session-templates)))
    (dolist (key '("USER" "HOME" "HOST"))
      (let ((cell (assoc key templates)))
        (should cell)
        (should (stringp (cdr cell)))
        (should (> (length (cdr cell)) 0))))
    (should (file-directory-p (cdr (assoc "HOME" templates))))))

(ert-deftest gptel-agent-template-test-expand-session-placeholders ()
  "{{USER}}, {{HOME}} and {{HOST}} are replaced with session values."
  (with-temp-buffer
    (insert "user={{USER}} home={{HOME}} host={{HOST}}")
    (gptel-agent--expand-templates (point-min) (gptel-agent--session-templates))
    (should (string= (buffer-string)
                     (format "user=%s home=%s host=%s"
                             (gptel-agent-template-test--value "USER")
                             (gptel-agent-template-test--value "HOME")
                             (gptel-agent-template-test--value "HOST"))))))

(ert-deftest gptel-agent-template-test-expand-multiple-occurrences ()
  "All occurrences of the same placeholder are replaced."
  (with-temp-buffer
    (insert "{{USER}} and {{USER}} and {{USER}}")
    (gptel-agent--expand-templates (point-min) (gptel-agent--session-templates))
    (let ((user (gptel-agent-template-test--value "USER")))
      (should (string= (buffer-string) (format "%s and %s and %s" user user user)))
      (should-not (string-match-p "{{USER}}" (buffer-string))))))

(ert-deftest gptel-agent-template-test-unknown-placeholder-untouched ()
  "An unknown placeholder is left as-is."
  (with-temp-buffer
    (insert "keep {{NOPE}} but expand {{HOST}}")
    (gptel-agent--expand-templates (point-min) (gptel-agent--session-templates))
    (should (string= (buffer-string)
                     (format "keep {{NOPE}} but expand %s"
                             (gptel-agent-template-test--value "HOST"))))))

(ert-deftest gptel-agent-template-test-expansion-starts-at-start ()
  "Placeholders before START are not expanded."
  (with-temp-buffer
    (insert "before {{USER}}\n")
    (let ((start (point)))
      (insert "after {{USER}}")
      (gptel-agent--expand-templates start (gptel-agent--session-templates))
      (should (string= (buffer-string)
                       (format "before {{USER}}\nafter %s"
                               (gptel-agent-template-test--value "USER")))))))

(ert-deftest gptel-agent-template-test-home-has-no-trailing-slash ()
  "HOME is normalised so {{HOME}}/bin never yields a doubled slash.
Also guards against an empty HOME environment variable leaking through."
  (dolist (home '("/tmp/fake-home" "/tmp/fake-home/" ""))
    (let* ((process-environment (cons (concat "HOME=" home) process-environment))
           (value (gptel-agent-template-test--value "HOME")))
      (should (stringp value))
      (should-not (string-empty-p value))
      (should-not (string-suffix-p "/" value))
      (with-temp-buffer
        (insert "{{HOME}}/bin")
        (gptel-agent--expand-templates (point-min) (gptel-agent--session-templates))
        (should-not (string-match-p "//" (buffer-string))))))
  ;; A HOME of "/" is the one legitimate trailing slash.
  (let ((process-environment (cons "HOME=/" process-environment)))
    (should (string= (gptel-agent-template-test--value "HOME") "/"))))

(ert-deftest gptel-agent-template-test-expand-all-variable-kinds ()
  "AGENTS/SKILLS and the session templates expand together.
Mirrors how `gptel-agent-update' builds its templates alist."
  (let ((templates (append (list (cons "AGENTS" "agent-a, agent-b")
                                 (cons "SKILLS" "skill-x, skill-y"))
                           (gptel-agent--session-templates))))
    (with-temp-buffer
      (insert "agents={{AGENTS}}\nskills={{SKILLS}}\n"
              "user={{USER}} home={{HOME}} host={{HOST}}\n")
      (gptel-agent--expand-templates (point-min) templates)
      (should (string= (buffer-string)
                       (format (concat "agents=agent-a, agent-b\n"
                                       "skills=skill-x, skill-y\n"
                                       "user=%s home=%s host=%s\n")
                               (gptel-agent-template-test--value "USER")
                               (gptel-agent-template-test--value "HOME")
                               (gptel-agent-template-test--value "HOST"))))
      ;; No placeholder of any kind survives expansion.
      (should-not (string-match-p "{{[A-Z]+}}" (buffer-string))))))

(ert-deftest gptel-agent-template-test-values-inserted-literally ()
  "Regex-significant characters in a value are inserted verbatim.
Guards the FIXEDCASE and LITERAL arguments of `replace-match'."
  (dolist (value '("back\\slash" "group\\1ref" "amp\\&ersand" "UPPER" "Mixed Case"))
    (with-temp-buffer
      (insert "v={{VAL}}")
      (gptel-agent--expand-templates (point-min) (list (cons "VAL" value)))
      (should (string= (buffer-string) (concat "v=" value))))))

(ert-deftest gptel-agent-template-test-empty-templates-is-noop ()
  "An empty or nil templates alist leaves the buffer unchanged."
  (dolist (templates (list nil '()))
    (with-temp-buffer
      (insert "untouched {{USER}}")
      (gptel-agent--expand-templates (point-min) templates)
      (should (string= (buffer-string) "untouched {{USER}}")))))

(provide 'gptel-agent-template-test)
;;; gptel-agent-template-test.el ends here
