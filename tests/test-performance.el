;;; test-performance.el --- Performance and stress tests -*- lexical-binding: t; -*-

;; Copyright (C) 2024  Free Software Foundation, Inc.

;; Author: Scott Murphy

;;; Commentary:

;; Performance and stress tests for Elysium functionality.
;; Tests memory usage, processing time, and system limits.

;;; Code:

(require 'ert)
(require 'test-helper)
(require 'elysium)
(require 'benchmark)

;; Performance benchmarks
(ert-deftest elysium-test-extract-changes-performance ()
  "Test performance of change extraction with large responses."
  (let* ((large-response (elysium-test-generate-code-response
                         (mapcar (lambda (n)
                                   (list :start n :end (+ n 5)
                                         :code (make-string 100 ?x)
                                         :language "text"))
                                 (number-sequence 1 100 10))))
         (start-time (current-time)))
    (elysium-extract-changes large-response)
    (let ((elapsed (float-time (time-subtract (current-time) start-time))))
      (should (< elapsed 1.0))))) ; Should complete within 1 second

(ert-deftest elysium-test-apply-changes-performance ()
  "Test performance of applying many changes to large buffer."
  (let* ((buffer (elysium-test-create-temp-buffer nil 'text-mode
                                                  (elysium-test-create-sample-file-content 5000)))
         (changes (mapcar (lambda (n)
                           (list :start n :end n
                                 :code (format "Performance test line %d" n)))
                         (number-sequence 100 1000 100)))
         (start-time (current-time)))
    (elysium-apply-code-changes buffer changes)
    (let ((elapsed (float-time (time-subtract (current-time) start-time))))
      (should (< elapsed 5.0))))) ; Should complete within 5 seconds

;; Memory usage tests
(ert-deftest elysium-test-memory-usage-large-buffer ()
  "Test memory usage with very large buffers."
  (let ((initial-gc-count (car (garbage-collect)))
        (buffer (elysium-test-create-temp-buffer nil 'text-mode
                                                 (make-string 1000000 ?x))))
    (elysium-apply-code-changes buffer '((:start 1000 :end 1010 :code "Memory test")))
    (garbage-collect)
    (let ((final-gc-count (car (garbage-collect))))
      ;; Memory usage shouldn't grow excessively
      (should (< (- final-gc-count initial-gc-count) 1000000)))))

;; Stress tests
(ert-deftest elysium-test-many-concurrent-operations ()
  "Test handling many operations concurrently."
  (let ((buffers (mapcar (lambda (n)
                          (elysium-test-create-temp-buffer
                           (format "*test-%d*" n) 'text-mode
                           (elysium-test-create-sample-file-content 100)))
                        (number-sequence 1 10)))
        (changes '((:start 10 :end 15 :code "Concurrent test"))))
    (dolist (buffer buffers)
      (elysium-apply-code-changes buffer changes))
    (dolist (buffer buffers)
      (elysium-test-should-contain-merge-markers buffer))))

(ert-deftest elysium-test-deeply-nested-changes ()
  "Test changes with complex nesting patterns."
  (let ((buffer (elysium-test-create-temp-buffer nil 'text-mode
                                                 (elysium-test-create-sample-file-content 200)))
        (changes (mapcar (lambda (level)
                          (list :start (* level 10)
                                :end (+ (* level 10) 2)
                                :code (format "Nested level %d\n  Inner content %d" level level)))
                        (number-sequence 1 15))))
    (elysium-apply-code-changes buffer changes)
    (should (= (elysium-test-count-merge-conflicts buffer) 15))))

;; Edge case stress tests
(ert-deftest elysium-test-extreme-line-numbers ()
  "Test handling of very large line numbers."
  (let ((buffer (elysium-test-create-temp-buffer nil 'text-mode
                                                 (elysium-test-create-sample-file-content 50)))
        (changes '((:start 999999 :end 1000000 :code "Extreme line test"))))
    (should-not-error (elysium-apply-code-changes buffer changes))))

(ert-deftest elysium-test-unicode-heavy-content ()
  "Test handling of unicode-heavy content."
  (let ((buffer (elysium-test-create-temp-buffer nil 'text-mode
                                                 "Unicode: 你好世界 🌍 ∑∞∆"))
        (changes '((:start 1 :end 1 :code "More unicode: مرحبا بالعالم 🚀 ∇∫∂"))))
    (elysium-apply-code-changes buffer changes)
    (elysium-test-should-contain-merge-markers buffer)))

(ert-deftest elysium-test-very-long-lines ()
  "Test handling of very long lines."
  (let ((buffer (elysium-test-create-temp-buffer nil 'text-mode
                                                 (make-string 10000 ?x)))
        (changes '((:start 1 :end 1 :code "Short replacement"))))
    (elysium-apply-code-changes buffer changes)
    (elysium-test-should-contain-merge-markers buffer)))

;; Regex performance tests
(ert-deftest elysium-test-regex-performance ()
  "Test regex performance with complex patterns."
  (let* ((complex-response (concat
                           "Replace lines: 1-5\n```javascript\n"
                           (make-string 5000 ?x)
                           "\n```\nExplanation with many words "
                           (make-string 1000 ?y)))
         (start-time (current-time)))
    (elysium-extract-changes complex-response)
    (let ((elapsed (float-time (time-subtract (current-time) start-time))))
      (should (< elapsed 0.5))))) ; Should be fast even with complex input

;; Buffer switching performance
(ert-deftest elysium-test-buffer-switching-performance ()
  "Test performance when switching between many buffers."
  (let ((buffers (mapcar (lambda (n)
                          (elysium-test-create-temp-buffer
                           (format "*perf-test-%d*" n) 'text-mode
                           (format "Buffer %d content" n)))
                        (number-sequence 1 50)))
        (start-time (current-time)))
    (dolist (buffer buffers)
      (with-current-buffer buffer
        (goto-char (point-min))
        (insert "Performance test")))
    (let ((elapsed (float-time (time-subtract (current-time) start-time))))
      (should (< elapsed 2.0))))) ; Should handle many buffers efficiently

(provide 'test-performance)

;;; test-performance.el ends here
