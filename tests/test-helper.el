;;; test-helper.el --- Test helper utilities for Elysium -*- lexical-binding: t; -*-

;; Copyright (C) 2024  Free Software Foundation, Inc.

;; Author: Scott Murphy

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;;; Commentary:

;; Test helper utilities for Elysium testing framework.
;; Provides common test setup, teardown, and utility functions.

;;; Code:

(require 'ert)
(require 'elysium)

;; Test configuration
(defvar elysium-test-timeout 10
  "Timeout in seconds for async operations in tests.")

(defvar elysium-test-temp-buffers nil
  "List of temporary buffers created during tests.")

;; Test buffer management
(defun elysium-test-create-temp-buffer (&optional name mode content)
  "Create a temporary buffer for testing.
NAME is the buffer name (optional).
MODE is the major mode to set (optional).
CONTENT is initial content to insert (optional)."
  (let ((buffer (generate-new-buffer (or name "*elysium-test*"))))
    (push buffer elysium-test-temp-buffers)
    (with-current-buffer buffer
      (when mode
        (funcall mode))
      (when content
        (insert content))
      (goto-char (point-min)))
    buffer))

(defun elysium-test-cleanup-buffers ()
  "Clean up all temporary buffers created during tests."
  (dolist (buffer elysium-test-temp-buffers)
    (when (buffer-live-p buffer)
      (kill-buffer buffer)))
  (setq elysium-test-temp-buffers nil))

(defun elysium-test-with-temp-buffer (content mode &rest body)
  "Execute BODY with a temporary buffer containing CONTENT in MODE."
  (let ((buffer (elysium-test-create-temp-buffer nil mode content)))
    (unwind-protect
        (with-current-buffer buffer
          (eval `(progn ,@body)))
      (elysium-test-cleanup-buffers))))

;; Mock functions for testing
(defvar elysium-test-mock-responses nil
  "Queue of mock responses for testing.")

(defun elysium-test-mock-gptel-request (&rest args)
  "Mock version of gptel-request for testing.
Uses responses from `elysium-test-mock-responses'."
  (let ((callback (plist-get args :callback))
        (buffer (plist-get args :buffer))
        (response (or (pop elysium-test-mock-responses)
                      "Mock response")))
    (when callback
      (funcall callback response (list :buffer buffer)))))

(defun elysium-test-setup-mocks ()
  "Set up mock functions for testing."
  (advice-add 'gptel-request :override #'elysium-test-mock-gptel-request))

(defun elysium-test-teardown-mocks ()
  "Remove mock functions after testing."
  (advice-remove 'gptel-request #'elysium-test-mock-gptel-request))

;; Test assertions
(defun elysium-test-should-contain-merge-markers (buffer)
  "Assert that BUFFER contains git merge markers."
  (with-current-buffer buffer
    (should (string-match-p "<<<<<<< HEAD" (buffer-string)))
    (should (string-match-p "=======" (buffer-string)))
    (should (string-match-p ">>>>>>> " (buffer-string)))))

(defun elysium-test-should-not-contain-merge-markers (buffer)
  "Assert that BUFFER does not contain git merge markers."
  (with-current-buffer buffer
    (should-not (string-match-p "<<<<<<< HEAD" (buffer-string)))
    (should-not (string-match-p "=======" (buffer-string)))
    (should-not (string-match-p ">>>>>>> " (buffer-string)))))

(defun elysium-test-count-merge-conflicts (buffer)
  "Count the number of merge conflicts in BUFFER."
  (with-current-buffer buffer
    (let ((count 0))
      (save-excursion
        (goto-char (point-min))
        (while (re-search-forward "<<<<<<< HEAD" nil t)
          (setq count (1+ count))))
      count)))

;; Test data generators
(defun elysium-test-generate-code-response (changes &optional explanation)
  "Generate a mock LLM response with CHANGES and optional EXPLANATION."
  (let ((response (or explanation "Here are the requested changes:\n\n")))
    (dolist (change changes)
      (let ((start (plist-get change :start))
            (end (plist-get change :end))
            (code (plist-get change :code))
            (lang (or (plist-get change :language) "text")))
        (setq response
              (concat response
                      (format "Replace lines: %d-%d\n```%s\n%s\n```\n\n"
                              start end lang code)))))
    response))

(defun elysium-test-create-sample-file-content (lines)
  "Create sample file content with LINES number of lines."
  (mapconcat (lambda (n) (format "Line %d" n))
             (number-sequence 1 lines)
             "\n"))

;; Hook for test setup and teardown
(defun elysium-test-setup ()
  "Set up test environment."
  (elysium-test-setup-mocks)
  (setq elysium-test-mock-responses nil))

(defun elysium-test-teardown ()
  "Tear down test environment."
  (elysium-test-teardown-mocks)
  (elysium-test-cleanup-buffers)
  (setq elysium-test-mock-responses nil))

;; Add hooks to run setup/teardown automatically
(add-hook 'ert-test-start-functions #'elysium-test-setup)
(add-hook 'ert-test-end-functions #'elysium-test-teardown)

(provide 'test-helper)

;;; test-helper.el ends here
