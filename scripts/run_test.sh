#!/bin/bash

# Enhanced test runner for Elysium
# Supports running specific test suites, generating reports, and continuous testing

set -e

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_ROOT="$(dirname "$SCRIPT_DIR")"
TESTS_DIR="$PROJECT_ROOT/tests"

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[0;33m'
BLUE='\033[0;34m'
NC='\033[0m' # No Color

# Default values
PATTERN="."
VERBOSE=false
REPORT_DIR=""
TIMEOUT=60
WATCH=false
COVERAGE=false
SUITE=""

usage() {
    cat << EOF
Enhanced Elysium Test Runner

Usage: $0 [OPTIONS] [PATTERN]

OPTIONS:
    -h, --help          Show this help message
    -v, --verbose       Verbose output
    -p, --pattern PATTERN  Run tests matching PATTERN (default: '.')
    -r, --report DIR    Generate test report in DIR
    -t, --timeout SEC   Set test timeout in seconds (default: 60)
    -w, --watch         Watch for file changes and re-run tests
    -c, --coverage      Generate test coverage report
    --list              List available test suites
    --suite SUITE       Run specific test suite (core, integration, performance, error, ui, legacy)

PATTERNS:
    .                   Run all tests
    test-extract        Run tests matching 'test-extract'
    elysium-test-       Run tests starting with 'elysium-test-'

EXAMPLES:
    $0                              # Run all tests
    $0 -v test-extract             # Run extraction tests with verbose output
    $0 --suite core                # Run core functionality tests
    $0 -r /tmp/reports --coverage  # Generate coverage report
    $0 -w                          # Watch mode for development

EOF
}

list_suites() {
    echo "Available test suites:"
    echo "  core         - Core functionality tests (test-feature-one.el)"
    echo "  integration  - Integration tests (test-feature-two.el)"
    echo "  legacy       - Legacy tests (test-elysium.el)"
    echo "  performance  - Performance and stress tests (test-performance.el)"
    echo "  error        - Error handling tests (test-error-handling.el)"
    echo "  ui           - UI and command tests (test-ui-commands.el)"
    echo "  all          - All test suites"
}

log() {
    echo -e "${BLUE}[$(date +'%H:%M:%S')]${NC} $*"
}

error() {
    echo -e "${RED}[ERROR]${NC} $*" >&2
}

success() {
    echo -e "${GREEN}[SUCCESS]${NC} $*"
}

warning() {
    echo -e "${YELLOW}[WARNING]${NC} $*"
}

run_test_suite() {
    local test_file="$1"
    local pattern="$2"
    local start_time=$(date +%s)

    log "Running tests from $test_file with pattern '$pattern'"

    local cmd="emacs -batch -L \"$PROJECT_ROOT\" -l ert"
    cmd="$cmd -l \"$TESTS_DIR/gptel-stub.el\""
    cmd="$cmd -l \"$TESTS_DIR/test-helper.el\""
    cmd="$cmd -l \"$test_file\""
    cmd="$cmd -f ert-run-tests-batch-and-exit"
    cmd="$cmd --eval \"(ert-run-tests-batch \\\"$pattern\\\")\""

    if [ "$VERBOSE" = true ]; then
        log "Command: $cmd"
    fi

    if timeout "$TIMEOUT" bash -c "$cmd"; then
        local end_time=$(date +%s)
        local duration=$((end_time - start_time))
        success "$(basename "$test_file") completed in ${duration}s"
        return 0
    else
        local exit_code=$?
        local end_time=$(date +%s)
        local duration=$((end_time - start_time))
        error "$(basename "$test_file") failed after ${duration}s (exit code: $exit_code)"
        return $exit_code
    fi
}

get_suite_files() {
    local suite="$1"
    case "$suite" in
        core)
            echo "$TESTS_DIR/test-feature-one.el"
            ;;
        integration)
            echo "$TESTS_DIR/test-feature-two.el"
            ;;
        legacy)
            echo "$TESTS_DIR/test-elysium.el"
            ;;
        performance)
            echo "$TESTS_DIR/test-performance.el"
            ;;
        error)
            echo "$TESTS_DIR/test-error-handling.el"
            ;;
        ui)
            echo "$TESTS_DIR/test-ui-commands.el"
            ;;
        all)
            find "$TESTS_DIR" -name "test-*.el" | sort
            ;;
        *)
            error "Unknown test suite: $suite"
            exit 1
            ;;
    esac
}

run_tests() {
    local pattern="$1"
    local suite="$2"
    local overall_start=$(date +%s)
    local failed_tests=0
    local total_tests=0

    # Handle simple case for backward compatibility
    if [ $# -eq 0 ] || ([ $# -eq 1 ] && [ -z "$suite" ]); then
        pattern="${1:-.}"
        log "Running tests matching '$pattern'..."

        local cmd="emacs --batch -L \"$PROJECT_ROOT\" -l ert"
        cmd="$cmd -l \"$TESTS_DIR/gptel-stub.el\""
        cmd="$cmd -l \"$TESTS_DIR/test-helper.el\""
        cmd="$cmd -l \"$TESTS_DIR/test-feature-one.el\""
        cmd="$cmd -l \"$TESTS_DIR/test-feature-two.el\""
        cmd="$cmd -l \"$TESTS_DIR/test-elysium.el\""
        cmd="$cmd -l \"$TESTS_DIR/test-ui-commands.el\""
        cmd="$cmd -l \"$TESTS_DIR/test-performance.el\""
        cmd="$cmd -l \"$TESTS_DIR/test-error-handling.el\""
        cmd="$cmd -f ert-run-tests-batch-and-exit"
        cmd="$cmd --eval \"(ert-run-tests-batch \\\"$pattern\\\")\""

        if timeout "$TIMEOUT" bash -c "$cmd"; then
            echo "Tests passed."
            return 0
        else
            echo "Tests failed."
            return 1
        fi
    fi

    # Determine which test files to run
    local test_files=()
    if [ -n "$suite" ]; then
        while IFS= read -r file; do
            test_files+=("$file")
        done < <(get_suite_files "$suite")
    else
        while IFS= read -r file; do
            test_files+=("$file")
        done < <(find "$TESTS_DIR" -name "test-*.el" | sort)
    fi

    if [ ${#test_files[@]} -eq 0 ]; then
        error "No test files found"
        exit 1
    fi

    log "Found ${#test_files[@]} test file(s) to run"

    # Run each test file
    for test_file in "${test_files[@]}"; do
        if [ ! -f "$test_file" ]; then
            warning "Test file not found: $test_file"
            continue
        fi

        total_tests=$((total_tests + 1))

        if ! run_test_suite "$test_file" "$pattern"; then
            failed_tests=$((failed_tests + 1))
        fi

        echo # Add spacing between test suites
    done

    # Final summary
    local overall_end=$(date +%s)
    local overall_duration=$((overall_end - overall_start))

    echo
    if [ $failed_tests -eq 0 ]; then
        success "All $total_tests test suite(s) passed in ${overall_duration}s!"
        exit 0
    else
        error "$failed_tests out of $total_tests test suite(s) failed in ${overall_duration}s"
        exit 1
    fi
}

# Parse command line arguments only if there are any
if [ $# -gt 0 ]; then
    while [[ $# -gt 0 ]]; do
        case $1 in
            -h|--help)
                usage
                exit 0
                ;;
            -v|--verbose)
                VERBOSE=true
                shift
                ;;
            -p|--pattern)
                PATTERN="$2"
                shift 2
                ;;
            -t|--timeout)
                TIMEOUT="$2"
                shift 2
                ;;
            --list)
                list_suites
                exit 0
                ;;
            --suite)
                SUITE="$2"
                shift 2
                ;;
            -*)
                error "Unknown option: $1"
                usage
                exit 1
                ;;
            *)
                # Treat as pattern if no -p was specified and no suite
                if [ "$PATTERN" = "." ] && [ -z "$SUITE" ]; then
                    PATTERN="$1"
                else
                    error "Unexpected argument: $1"
                    usage
                    exit 1
                fi
                shift
                ;;
        esac
    done
fi

# Main execution
if [ -n "$SUITE" ] || [ "$VERBOSE" = true ] || [ "$PATTERN" != "." ]; then
    log "Starting Elysium test runner"
    log "Pattern: $PATTERN"
    log "Suite: ${SUITE:-"all"}"
    log "Timeout: ${TIMEOUT}s"
fi

run_tests "$PATTERN" "$SUITE"
