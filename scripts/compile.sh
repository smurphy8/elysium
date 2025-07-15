#!/bin/sh
set -e

echo "Byte-compiling project files..."
# Find and compile all .el files, loading from the current directory.
emacs -batch -L . -f batch-byte-compile $(find . -name "*.el" -not -path "./.cask/*" -not -path "./straight/*")
echo "Compilation complete."
