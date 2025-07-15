;;; setup-test-environment.el --- Test environment setup for Elysium -*- lexical-binding: t; -*-

;; Copyright (C) 2024  Free Software Foundation, Inc.

;; Author: Scott Murphy

;;; Commentary:

;; Sets up the test environment for Elysium by installing required dependencies
;; using straight.el package manager.

;;; Code:

;; Bootstrap straight.el if not already present
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 6))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

;; Install required packages for testing
(straight-use-package '(gptel :type git :host github :repo "karthink/gptel"))

;; Ensure packages are available
(require 'gptel)

(provide 'setup-test-environment)

;;; setup-test-environment.el ends here
