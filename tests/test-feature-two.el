;;; test-feature-two.el --- Advanced functionality and integration tests -*- lexical-binding: t; -*-

;; Copyright (C) 2024  Free Software Foundation, Inc.

;; Author: Scott Murphy

;;; Commentary:

;; Tests for advanced Elysium functionality including merge conflict handling,
;; integration scenarios, error cases, and performance.

;;; Code:

(require 'ert)
(require 'test-helper)
(require 'elysium)

;; Code application tests
(ert-deftest elysium-test-apply-single-change ()
  "Test applying a single code change."
  (let ((buffer (elysium-test-create-temp-buffer nil 'text-mode
                                                 (elysium-test-create-sample-file-content 10)))
        (changes '((:start 3 :end 5 :code "New line 3\nNew line 4\nNew line 5"))))
    (elysium-apply-code-changes buffer changes)
    (elysium-test-should-contain-merge-markers buffer)
    (should (= (elysium-test-count-merge-conflicts buffer) 1))))

(ert-deftest elysium-test-apply-multiple-changes ()
  "Test applying multiple code changes with proper offset calculation."
  (let ((buffer (elysium-test-create-temp-buffer nil 'text-mode
                                                 (elysium-test-create-sample-file-content 15)))
        (changes '((:start 2 :end 3 :code "Modified line 2\nModified line 3")
                   (:start 8 :end 10 :code "Modified line 8\nModified line 9\nModified line 10"))))
    (elysium-apply-code-changes buffer changes)
    (elysium-test-should-contain-merge-markers buffer)
    (should (= (elysium-test-count-merge-conflicts buffer) 2))))

(ert-deftest elysium-test-apply-overlapping-changes ()
  "Test handling of overlapping changes."
  (let ((buffer (elysium-test-create-temp-buffer nil 'text-mode
                                                 (elysium-test-create-sample-file-content 10)))
        (changes '((:start 3 :end 5 :code "Change 1")
                   (:start 4 :end 6 :code "Change 2"))))
    (elysium-apply-code-changes buffer changes)
    (elysium-test-should-contain-merge-markers buffer)))

(ert-deftest elysium-test-apply-single-line-change ()
  "Test applying a single line change."
  (let ((buffer (elysium-test-create-temp-buffer nil 'text-mode
                                                 (elysium-test-create-sample-file-content 5)))
        (changes '((:start 3 :end 3 :code "Single modified line"))))
    (elysium-apply-code-changes buffer changes)
    (elysium-test-should-contain-merge-markers buffer)))

;; Merge conflict resolution tests
(ert-deftest elysium-test-keep-all-suggestions ()
  "Test keeping all LLM suggestions."
  (let ((buffer (elysium-test-create-temp-buffer nil 'text-mode
                                                 (elysium-test-create-sample-file-content 10)))
        (changes '((:start 2 :end 3 :code "LLM suggestion"))))
    (elysium-apply-code-changes buffer changes)
    (with-current-buffer buffer
      (elysium-keep-all-suggested-changes)
      (elysium-test-should-not-contain-merge-markers buffer)
      (should (string-match-p "LLM suggestion" (buffer-string))))))

(ert-deftest elysium-test-discard-all-suggestions ()
  "Test discarding all LLM suggestions."
  (let ((buffer (elysium-test-create-temp-buffer nil 'text-mode
                                                 (elysium-test-create-sample-file-content 10)))
        (changes '((:start 2 :end 3 :code "LLM suggestion"))))
    (elysium-apply-code-changes buffer changes)
    (with-current-buffer buffer
      (elysium-discard-all-suggested-changes)
      (elysium-test-should-not-contain-merge-markers buffer)
      (should-not (string-match-p "LLM suggestion" (buffer-string))))))

;; Edge case tests
(ert-deftest elysium-test-empty-code-change ()
  "Test handling empty code changes."
  (let ((buffer (elysium-test-create-temp-buffer nil 'text-mode
                                                 (elysium-test-create-sample-file-content 5)))
        (changes '((:start 2 :end 3 :code ""))))
    (elysium-apply-code-changes buffer changes)
    (elysium-test-should-contain-merge-markers buffer)))

(ert-deftest elysium-test-out-of-bounds-change ()
  "Test handling changes that exceed buffer bounds."
  (let ((buffer (elysium-test-create-temp-buffer nil 'text-mode
                                                 (elysium-test-create-sample-file-content 5)))
        (changes '((:start 10 :end 15 :code "Out of bounds"))))
    ;; Should not error, but may not apply change correctly
    (should-not-error (elysium-apply-code-changes buffer changes))))

(ert-deftest elysium-test-invalid-line-range ()
  "Test handling invalid line ranges (end < start)."
  (let ((buffer (elysium-test-create-temp-buffer nil 'text-mode
                                                 (elysium-test-create-sample-file-content 5)))
        (changes '((:start 5 :end 2 :code "Invalid range"))))
    (should-not-error (elysium-apply-code-changes buffer changes))))

;; Context addition tests
(ert-deftest elysium-test-add-context-with-selection ()
  "Test adding selected context to chat buffer."
  (let ((source-buffer (elysium-test-create-temp-buffer "source" 'python-mode
                                                        "def hello():\n    print('world')\n"))
        (original-chat elysium--chat-buffer))
    (unwind-protect
        (with-current-buffer source-buffer
          (goto-char (point-min))
          (set-mark (point))
          (goto-char (point-max))
          (setq elysium--chat-buffer (elysium-test-create-temp-buffer "*elysium*"))
          (elysium-add-context)
          (with-current-buffer elysium--chat-buffer
            (should (string-match-p "python" (buffer-string)))
            (should (string-match-p "def hello" (buffer-string)))))
      (setq elysium--chat-buffer original-chat))))

;; Performance and stress tests
(ert-deftest elysium-test-large-file-handling ()
  "Test handling large files with many changes."
  (let ((buffer (elysium-test-create-temp-buffer nil 'text-mode
                                                 (elysium-test-create-sample-file-content 1000)))
        (changes (mapcar (lambda (n) (list :start n :end n :code (format "Change %d" n)))
                        (number-sequence 10 100 10))))
    (should-not-error (elysium-apply-code-changes buffer changes))
    (should (= (elysium-test-count-merge-conflicts buffer) 10))))

(ert-deftest elysium-test-many-small-changes ()
  "Test applying many small changes."
  (let ((buffer (elysium-test-create-temp-buffer nil 'text-mode
                                                 (elysium-test-create-sample-file-content 50)))
        (changes (mapcar (lambda (n) (list :start n :end n :code (format "Small change %d" n)))
                        (number-sequence 2 20 2))))
    (should-not-error (elysium-apply-code-changes buffer changes))
    (should (= (elysium-test-count-merge-conflicts buffer) 10))))

;; Integration tests with mock LLM responses
(ert-deftest elysium-test-end-to-end-workflow ()
  "Test complete workflow from query to applied changes."
  (let ((code-buffer (elysium-test-create-temp-buffer "code" 'python-mode
                                                      "def old_function():\n    pass\n"))
        (original-chat elysium--chat-buffer)
        (mock-response (elysium-test-generate-code-response
                       '((:start 1 :end 2 :code "def new_function():\n    return True"
                          :language "python")))))
    (unwind-protect
        (progn
          (setq elysium--chat-buffer (elysium-test-create-temp-buffer "*elysium*"))
          (setq elysium-test-mock-responses (list mock-response))
          (with-current-buffer code-buffer
            (elysium-query "Update this function")
            ;; Simulate async completion
            (run-with-timer 0.1 nil
                           (lambda ()
                             (elysium-test-should-contain-merge-markers code-buffer)
                             (should (string-match-p "new_function" (with-current-buffer code-buffer
                                                                     (buffer-string))))))))
      (setq elysium--chat-buffer original-chat))))

;; Hook tests
(ert-deftest elysium-test-apply-changes-hook ()
  "Test that apply changes hook is called."
  (let ((hook-called nil)
        (buffer (elysium-test-create-temp-buffer nil 'text-mode
                                                 (elysium-test-create-sample-file-content 5)))
        (changes '((:start 2 :end 3 :code "Hook test"))))
    (add-hook 'elysium-apply-changes-hook (lambda () (setq hook-called t)))
    (unwind-protect
        (progn
          (elysium-apply-code-changes buffer changes)
          (should hook-called))
      (remove-hook 'elysium-apply-changes-hook (lambda () (setq hook-called t))))))

;; Buffer clearing tests
(ert-deftest elysium-test-clear-buffer ()
  "Test clearing the elysium chat buffer."
  (let ((original-chat elysium--chat-buffer))
    (unwind-protect
        (progn
          (setq elysium--chat-buffer (elysium-test-create-temp-buffer "*elysium*"))
          (with-current-buffer elysium--chat-buffer
            (insert "Some content to clear"))
          (elysium-clear-buffer)
          (with-current-buffer elysium--chat-buffer
            (should (string-match-p "^\\*\\*\\* " (buffer-string)))))
      (setq elysium--chat-buffer original-chat))))

(provide 'test-feature-two)

;;; test-feature-two.el ends here
