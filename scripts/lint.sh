#!/bin/sh
set -e # Exit immediately if a command exits with a non-zero status.

echo "Running checkdoc..."
# Find all .el files, excluding those in dependency directories, and run checkdoc.
find . -name "*.el" -not -path "./.cask/*" -not -path "./straight/*" \
  | xargs emacs -batch -l checkdoc -f checkdoc-file

echo "Running package-lint..."
# Run package-lint on your main package file.
emacs -batch --load my-package.el --eval '(package-lint-current-buffer)'

echo "Linting complete."
