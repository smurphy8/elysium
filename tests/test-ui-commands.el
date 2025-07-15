;;; test-ui-commands.el --- UI and command interaction tests -*- lexical-binding: t; -*-

;; Copyright (C) 2024  Free Software Foundation, Inc.

;; Author: Scott Murphy

;;; Commentary:

;; Tests for UI commands and interactive functionality as outlined in the test plan.
;; These tests ensure the interactive commands run without errors and set up
;; correct state (e.g., creating the *elysium* buffer).

;;; Code:

(require 'ert)
(require 'test-helper)
(require 'elysium)

;; Helper macro for UI tests
(defmacro elysium-with-test-code-buffer (initial-content &rest body)
  "Execute BODY with a temporary buffer containing INITIAL-CONTENT."
  `(with-temp-buffer
     (insert ,(or initial-content ""))
     (goto-char (point-min))
     ,@body))

;; Window and buffer management tests
(ert-deftest elysium-setup-windows-test ()
  "Test that elysium-setup-windows creates the chat buffer."
  (let ((original-chat elysium--chat-buffer))
    (unwind-protect
        (progn
          (setq elysium--chat-buffer nil)
          (elysium-setup-windows)
          (should (buffer-live-p elysium--chat-buffer))
          (should (string= (buffer-name elysium--chat-buffer) "*elysium*")))
      (setq elysium--chat-buffer original-chat))))

(ert-deftest elysium-toggle-window-test ()
  "Test window toggle functionality."
  (let ((original-chat elysium--chat-buffer))
    (unwind-protect
        (progn
          (setq elysium--chat-buffer nil)
          ;; First toggle should create and show window
          (elysium-toggle-window)
          (should (buffer-live-p elysium--chat-buffer))
          (should (get-buffer-window elysium--chat-buffer))

          ;; Second toggle should hide window
          (elysium-toggle-window)
          (should-not (get-buffer-window elysium--chat-buffer)))
      (setq elysium--chat-buffer original-chat))))

(ert-deftest elysium-clear-buffer-test ()
  "Test that elysium-clear-buffer clears and sets up the chat buffer."
  (let ((original-chat elysium--chat-buffer))
    (unwind-protect
        (progn
          (setq elysium--chat-buffer (elysium-test-create-temp-buffer "*elysium*"))
          (with-current-buffer elysium--chat-buffer
            (insert "Some content to clear"))

          (elysium-clear-buffer)

          (with-current-buffer elysium--chat-buffer
            ;; Should contain the prompt prefix
            (should (string-match-p "^\\*\\*\\* " (buffer-string)))
            ;; Should not contain the old content
            (should-not (string-match-p "Some content to clear" (buffer-string)))))
      (setq elysium--chat-buffer original-chat))))

;; Context addition tests (following test plan structure)
(ert-deftest elysium-add-context-with-selection-test ()
  "Test that elysium-add-context adds region to the chat buffer."
  (let ((original-chat elysium--chat-buffer))
    (unwind-protect
        (elysium-with-test-code-buffer
         "some content\nTHIS IS THE REGION\nmore content"
         (python-mode)
         (setq elysium--chat-buffer nil)

         ;; Set up the region
         (goto-char (point-min))
         (search-forward "THIS IS THE REGION")
         (set-mark (match-beginning 0))
         (goto-char (match-end 0))

         ;; Call the function
         (elysium-add-context)

         ;; Assertions
         (should (get-buffer "*elysium*") "The *elysium* buffer should be created.")
         (with-current-buffer "*elysium*"
           (should (string-match-p "THIS IS THE REGION" (buffer-string))
                   "The selected region text should be in the chat buffer.")
           (should (string-match-p "python" (buffer-string))
                   "The language should be detected and included.")))
      (setq elysium--chat-buffer original-chat))))

(ert-deftest elysium-add-context-whole-buffer-test ()
  "Test adding entire buffer when no region is selected."
  (let ((original-chat elysium--chat-buffer))
    (unwind-protect
        (elysium-with-test-code-buffer
         "def hello():\n    print('world')\n    return True"
         (python-mode)
         (setq elysium--chat-buffer nil)

         ;; No region selected - should use whole buffer
         (elysium-add-context)

         (should (get-buffer "*elysium*"))
         (with-current-buffer "*elysium*"
           (should (string-match-p "def hello" (buffer-string)))
           (should (string-match-p "print('world')" (buffer-string)))
           (should (string-match-p "python" (buffer-string)))))
      (setq elysium--chat-buffer original-chat))))

;; Merge conflict resolution tests
(ert-deftest elysium-keep-all-suggested-changes-test ()
  "Test keeping all LLM suggestions."
  (let ((buffer (elysium-test-create-temp-buffer nil 'text-mode
                                                 "line 1\nline 2\nline 3\nline 4\n"))
        (changes '((:start 2 :end 3 :code "LLM suggestion line 2\nLLM suggestion line 3"))))
    (with-current-buffer buffer
      (setq-local gptel-backend (gptel--make-backend :name "test-backend")))

    (elysium-apply-code-changes buffer changes)
    (elysium-test-should-contain-merge-markers buffer)

    (with-current-buffer buffer
      (elysium-keep-all-suggested-changes)
      (elysium-test-should-not-contain-merge-markers buffer)
      (should (string-match-p "LLM suggestion line 2" (buffer-string)))
      (should (string-match-p "LLM suggestion line 3" (buffer-string))))))

(ert-deftest elysium-discard-all-suggested-changes-test ()
  "Test discarding all LLM suggestions."
  (let ((buffer (elysium-test-create-temp-buffer nil 'text-mode
                                                 "line 1\nline 2\nline 3\nline 4\n"))
        (changes '((:start 2 :end 3 :code "LLM suggestion line 2\nLLM suggestion line 3"))))
    (with-current-buffer buffer
      (setq-local gptel-backend (gptel--make-backend :name "test-backend")))

    (elysium-apply-code-changes buffer changes)
    (elysium-test-should-contain-merge-markers buffer)

    (with-current-buffer buffer
      (elysium-discard-all-suggested-changes)
      (elysium-test-should-not-contain-merge-markers buffer)
      (should-not (string-match-p "LLM suggestion" (buffer-string)))
      (should (string-match-p "line 2" (buffer-string)))
      (should (string-match-p "line 3" (buffer-string))))))

;; Interactive command error handling
(ert-deftest elysium-query-with-no-chat-buffer-test ()
  "Test elysium-query when no chat buffer exists."
  (let ((original-chat elysium--chat-buffer))
    (unwind-protect
        (elysium-with-test-code-buffer
         ("test content")
         (setq elysium--chat-buffer nil)

         ;; Mock gptel-request to avoid actual network calls
         (cl-letf (((symbol-function 'gptel-request)
                    (lambda (query &key system buffer callback)
                      (funcall callback "Mock response" (list :buffer buffer)))))

           (should-not-error (elysium-query "test query"))
           (should (buffer-live-p elysium--chat-buffer))))
      (setq elysium--chat-buffer original-chat))))

(ert-deftest elysium-query-from-chat-buffer-test ()
  "Test elysium-query when called from the chat buffer itself."
  (let ((original-chat elysium--chat-buffer)
        (code-buffer (elysium-test-create-temp-buffer "code" 'python-mode "def test(): pass")))
    (unwind-protect
        (progn
          (setq elysium--chat-buffer (elysium-test-create-temp-buffer "*elysium*"))
          (with-current-buffer elysium--chat-buffer
            (insert "*** Test query\nImprove this function"))

          ;; Mock gptel-request
          (cl-letf (((symbol-function 'gptel-request)
                     (lambda (query &key system buffer callback)
                       (funcall callback "Mock response" (list :buffer buffer)))))

            (with-current-buffer elysium--chat-buffer
              (should-not-error (elysium-query nil)))))
      (setq elysium--chat-buffer original-chat))))

;; Hook tests
(ert-deftest elysium-apply-changes-hook-test ()
  "Test that apply changes hook is called correctly."
  (let ((hook-called nil)
        (hook-buffer nil)
        (buffer (elysium-test-create-temp-buffer nil 'text-mode "test content"))
        (changes '((:start 1 :end 1 :code "new content"))))

    ;; Add a test hook
    (add-hook 'elysium-apply-changes-hook
              (lambda ()
                (setq hook-called t)
                (setq hook-buffer (current-buffer))))

    (unwind-protect
        (progn
          (with-current-buffer buffer
            (setq-local gptel-backend (gptel--make-backend :name "test-backend")))
          (elysium-apply-code-changes buffer changes)
          (should hook-called "Hook should have been called")
          (should (eq hook-buffer buffer) "Hook should run in the target buffer"))

      ;; Clean up hook
      (remove-hook 'elysium-apply-changes-hook
                   (lambda ()
                     (setq hook-called t)
                     (setq hook-buffer (current-buffer)))))))

;; Region handling tests
(ert-deftest elysium-query-with-region-test ()
  "Test elysium-query with active region."
  (let ((original-chat elysium--chat-buffer))
    (unwind-protect
        (elysium-with-test-code-buffer
         ("line 1\nline 2 SELECTED\nline 3 SELECTED\nline 4")
         (setq elysium--chat-buffer (elysium-test-create-temp-buffer "*elysium*"))

         ;; Select region
         (goto-char (point-min))
         (search-forward "SELECTED")
         (beginning-of-line)
         (set-mark (point))
         (forward-line 2)

         ;; Mock gptel-request to capture the query
         (let ((captured-query nil))
           (cl-letf (((symbol-function 'gptel-request)
                      (lambda (query &key system buffer callback)
                        (setq captured-query query)
                        (funcall callback "Mock response" (list :buffer buffer)))))

             (elysium-query "test with region")
             ;; Query should include the selected text
             (should (string-match-p "SELECTED" captured-query))
             (should (string-match-p "Line range: 2-3" captured-query)))))
      (setq elysium--chat-buffer original-chat))))

(provide 'test-ui-commands)

;;; test-ui-commands.el ends here
