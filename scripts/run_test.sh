#!/bin/sh
set -e

# The first argument to the script is an optional test selector (regexp).
TEST_SELECTOR=${1:-.} # Default to "." which matches all tests.

echo "Running tests matching '$TEST_SELECTOR'..."

# Load ERT, then load all test files, then run tests and exit.
# The --batch flag ensures Emacs runs non-interactively.
# The -L . flag adds the current directory to the load-path.
emacs --batch -L . \
  -l ert \
  -l tests/test-helper.el \
  -l tests/test-feature-one.el \
  -l tests/test-feature-two.el \
  -f ert-run-tests-batch-and-exit \
  --eval "(ert-run-tests-batch \"$TEST_SELECTOR\")"

echo "Tests passed."
