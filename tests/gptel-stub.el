;;; gptel-stub.el --- Minimal gptel stub for testing -*- lexical-binding: t; -*-

;; Copyright (C) 2024  Free Software Foundation, Inc.

;; Author: Scott Murphy

;;; Commentary:

;; Minimal stub implementation of gptel functions needed for testing.
;; This allows tests to run without requiring the full gptel package.

;;; Code:

(require 'cl-lib)

;; Define minimal gptel backend structure
(cl-defstruct gptel--backend name)

;; Stub functions that elysium uses
(defun gptel--make-backend (&rest args)
  "Create a mock gptel backend for testing."
  (make-gptel--backend :name (plist-get args :name)))

(defun gptel-backend-name (backend)
  "Get the name of a gptel backend."
  (if (gptel--backend-p backend)
      (gptel--backend-name backend)
    "mock-backend"))

(defun gptel-request (query &rest args)
  "Mock gptel-request function for testing.
This should be overridden by test mocks."
  (let ((callback (plist-get args :callback))
        (buffer (plist-get args :buffer)))
    (when callback
      (funcall callback "Mock gptel response" (list :buffer buffer)))))

(defun gptel (buffer-name)
  "Create a mock gptel buffer."
  (get-buffer-create buffer-name))

(defun gptel--update-status (status &optional face)
  "Mock status update function."
  (message "Status: %s" status))

(defun gptel--insert-response (response info)
  "Mock response insertion."
  (insert response))

(defun gptel--sanitize-model ()
  "Mock model sanitization."
  nil)

(defun gptel-prompt-prefix-string ()
  "Mock prompt prefix."
  "*** ")

;; Mock variables
(defvar gptel-backend nil
  "Mock gptel backend variable.")

;; Provide the feature so (require 'gptel) works
(provide 'gptel)

;;; gptel-stub.el ends here
