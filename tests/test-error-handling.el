;;; test-error-handling.el --- Error handling and robustness tests -*- lexical-binding: t; -*-

;; Copyright (C) 2024  Free Software Foundation, Inc.

;; Author: Scott Murphy

;;; Commentary:

;; Tests for error handling, edge cases, and system robustness.
;; Ensures Elysium handles unexpected situations gracefully.

;;; Code:

(require 'ert)
(require 'test-helper)
(require 'elysium)

;; Network and backend error simulation
(ert-deftest elysium-test-backend-error-handling ()
  "Test handling of backend errors."
  (let ((original-chat elysium--chat-buffer))
    (unwind-protect
        (progn
          (setq elysium--chat-buffer (elysium-test-create-temp-buffer "*elysium*"))
          (setq elysium-test-mock-responses '("error"))
          ;; Simulate backend error by providing invalid response
          (should-not-error
           (elysium-handle-response (current-buffer) nil '(:error "Backend error"))))
      (setq elysium--chat-buffer original-chat))))

(ert-deftest elysium-test-malformed-response-handling ()
  "Test handling of malformed LLM responses."
  (let ((malformed-responses
         '("No code blocks here"
           "Replace lines: invalid-range\n```code```"
           "Replace lines: 1-2\n```\nNo closing backticks"
           "Replace lines: 10-5\n```python\nprint('invalid range')\n```")))
    (dolist (response malformed-responses)
      (should-not-error (elysium-extract-changes response)))))

(ert-deftest elysium-test-empty-response-handling ()
  "Test handling of empty or nil responses."
  (should-not-error (elysium-extract-changes ""))
  (should-not-error (elysium-extract-changes nil))
  (let ((result (elysium-extract-changes "")))
    (should (= (length (plist-get result :changes)) 0))))

;; Buffer error handling
(ert-deftest elysium-test-killed-buffer-handling ()
  "Test handling when target buffer is killed during operation."
  (let ((buffer (elysium-test-create-temp-buffer nil 'text-mode "test content"))
        (changes '((:start 1 :end 1 :code "replacement"))))
    (kill-buffer buffer)
    (should-error (elysium-apply-code-changes buffer changes))))

(ert-deftest elysium-test-read-only-buffer-handling ()
  "Test handling of read-only buffers."
  (let ((buffer (elysium-test-create-temp-buffer nil 'text-mode "test content"))
        (changes '((:start 1 :end 1 :code "replacement"))))
    (with-current-buffer buffer
      (read-only-mode 1))
    (should-error (elysium-apply-code-changes buffer changes))))

(ert-deftest elysium-test-narrowed-buffer-handling ()
  "Test handling of narrowed buffers."
  (let ((buffer (elysium-test-create-temp-buffer nil 'text-mode
                                                 (elysium-test-create-sample-file-content 10)))
        (changes '((:start 5 :end 6 :code "narrowed test"))))
    (with-current-buffer buffer
      (goto-char (point-min))
      (forward-line 2)
      (narrow-to-region (point) (progn (forward-line 3) (point))))
    (should-not-error (elysium-apply-code-changes buffer changes))))

;; Input validation tests
(ert-deftest elysium-test-invalid-window-size ()
  "Test validation of window size settings."
  (should-error (setq elysium-window-size -0.1))
  (should-error (setq elysium-window-size 0))
  (should-error (setq elysium-window-size 1))
  (should-error (setq elysium-window-size 1.1))
  (should-error (setq elysium-window-size "invalid")))

(ert-deftest elysium-test-invalid-change-format ()
  "Test handling of invalid change formats."
  (let ((buffer (elysium-test-create-temp-buffer nil 'text-mode "test content"))
        (invalid-changes
         '((:start "invalid" :end 2 :code "test")
           (:start 1 :end "invalid" :code "test")
           (:code "missing start/end")
           ((:start 1 :end 2))  ; missing :code
           nil)))
    (dolist (change invalid-changes)
      (should-not-error (elysium-apply-code-changes buffer (list change))))))

;; File system error simulation
(ert-deftest elysium-test-file-permission-errors ()
  "Test handling of file permission errors."
  ;; Create a scenario where file operations might fail
  (let ((buffer (elysium-test-create-temp-buffer "/tmp/readonly-test" 'text-mode "content")))
    (should-not-error
     (elysium-apply-code-changes buffer '((:start 1 :end 1 :code "new content"))))))

;; Memory and resource limits
(ert-deftest elysium-test-memory-exhaustion-simulation ()
  "Test behavior under simulated memory pressure."
  (let ((large-code (make-string 100000 ?x))
        (buffer (elysium-test-create-temp-buffer nil 'text-mode "small content")))
    (should-not-error
     (elysium-apply-code-changes buffer `((:start 1 :end 1 :code ,large-code))))))

(ert-deftest elysium-test-deep-recursion-protection ()
  "Test protection against deep recursion in parsing."
  (let ((deeply-nested-response
         (concat (make-string 1000 ?\n)
                 "Replace lines: 1-2\n```python\nprint('deep')\n```")))
    (should-not-error (elysium-extract-changes deeply-nested-response))))

;; Async operation error handling
(ert-deftest elysium-test-callback-error-handling ()
  "Test error handling in async callbacks."
  (let ((buffer (elysium-test-create-temp-buffer nil 'text-mode "test"))
        (original-chat elysium--chat-buffer))
    (unwind-protect
        (progn
          (setq elysium--chat-buffer (elysium-test-create-temp-buffer "*elysium*"))
          ;; Simulate callback with error
          (should-not-error
           (elysium-handle-response buffer "invalid response format" '(:buffer buffer))))
      (setq elysium--chat-buffer original-chat))))

;; Integration error scenarios
(ert-deftest elysium-test-gptel-unavailable ()
  "Test behavior when gptel functions are unavailable."
  (let ((original-function (symbol-function 'gptel-backend-name)))
    (unwind-protect
        (progn
          (fmakunbound 'gptel-backend-name)
          (should-error
           (elysium-apply-code-changes
            (current-buffer)
            '((:start 1 :end 1 :code "test")))))
      (fset 'gptel-backend-name original-function))))

(ert-deftest elysium-test-smerge-unavailable ()
  "Test behavior when smerge functions are unavailable."
  (let ((buffer (elysium-test-create-temp-buffer nil 'text-mode "test content"))
        (original-next (symbol-function 'smerge-next))
        (original-keep-lower (symbol-function 'smerge-keep-lower)))
    (unwind-protect
        (progn
          (elysium-apply-code-changes buffer '((:start 1 :end 1 :code "new")))
          (fmakunbound 'smerge-next)
          (fmakunbound 'smerge-keep-lower)
          ;; Should handle missing smerge functions gracefully
          (should-not-error (elysium-keep-all-suggested-changes)))
      (fset 'smerge-next original-next)
      (fset 'smerge-keep-lower original-keep-lower))))

;; Configuration error handling
(ert-deftest elysium-test-invalid-backend-configuration ()
  "Test handling of invalid backend configurations."
  (let ((buffer (elysium-test-create-temp-buffer nil 'text-mode "test"))
        (invalid-backend nil))
    (with-current-buffer buffer
      (setq-local gptel-backend invalid-backend))
    ;; Should not crash with invalid backend
    (should-not-error
     (elysium-apply-code-changes buffer '((:start 1 :end 1 :code "test"))))))

;; Race condition simulation
(ert-deftest elysium-test-concurrent-buffer-modification ()
  "Test handling of concurrent buffer modifications."
  (let ((buffer (elysium-test-create-temp-buffer nil 'text-mode
                                                 (elysium-test-create-sample-file-content 10)))
        (changes '((:start 3 :end 5 :code "concurrent change"))))
    ;; Simulate concurrent modification
    (with-current-buffer buffer
      (goto-char (point-min))
      (forward-line 1)
      (insert "Concurrent modification\n"))
    ;; Should handle gracefully even if line numbers are off
    (should-not-error (elysium-apply-code-changes buffer changes))))

;; Recovery and cleanup tests
(ert-deftest elysium-test-partial-failure-recovery ()
  "Test recovery from partial failures."
  (let ((buffer (elysium-test-create-temp-buffer nil 'text-mode
                                                 (elysium-test-create-sample-file-content 5)))
        (mixed-changes '((:start 2 :end 3 :code "good change")
                        (:start 100 :end 200 :code "bad change")
                        (:start 4 :end 4 :code "another good change"))))
    ;; Should apply valid changes despite invalid ones
    (should-not-error (elysium-apply-code-changes buffer mixed-changes))))

(ert-deftest elysium-test-cleanup-after-error ()
  "Test that cleanup happens even after errors."
  (let ((cleanup-called nil)
        (buffer (elysium-test-create-temp-buffer nil 'text-mode "test")))
    (add-hook 'elysium-apply-changes-hook (lambda () (setq cleanup-called t)))
    (unwind-protect
        (progn
          ;; Even with potential errors, hooks should still run
          (elysium-apply-code-changes buffer '((:start 1 :end 1 :code "test")))
          (should cleanup-called))
      (remove-hook 'elysium-apply-changes-hook (lambda () (setq cleanup-called t))))))

(provide 'test-error-handling)

;;; test-error-handling.el ends here
