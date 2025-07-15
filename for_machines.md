[project_summary]
name = "elysium"
purpose = "An Emacs package to automatically apply AI-generated code changes directly into the current buffer based on natural language queries."

# The primary user interaction loop.
core_workflow = '''
1. User invokes `elysium-query` with a request (e.g., "Refactor this function to be more concise").
2. The query, along with the buffer's content (or active region), is sent to an LLM via the `gptel` backend.
3. The AI's suggested changes are streamed back and displayed as a diff using Emacs's built-in `smerge-mode`.
4. User navigates the changes with `smerge-next`/`smerge-previous` and accepts (`smerge-keep-other`) or rejects (`smerge-keep-mine`) each hunk.
'''

# Critical external packages the project relies on.
dependencies = [
  { name = "gptel", purpose = "Provides the backend for all LLM communication. Must be configured separately." },
  { name = "smerge-mode", purpose = "Built-in Emacs mode used to display and merge the AI-generated diffs." }
]

# Key functions available to the user.
key_commands = [
  { name = "elysium-query", description = "Sends a query to the LLM backend." },
  { name = "elysium-add-context", description = "Adds the contents of a region to the Elysium chat buffer." },
  { name = "elysium-keep-all-suggested-changes", description = "Accepts all AI suggestions." },
  { name = "elysium-discard-all-suggested-changes", description = "Rejects all AI suggestions." },
  { name = "smerge-next", description = "Navigates to the next change hunk." },
  { name = "smerge-keep-other", description = "Accepts the current change hunk." },
  { name = "smerge-keep-mine", description = "Rejects the current change hunk." },
]

# Important technical notes and configuration details.
technical_details = '''
- **Backend:** The package is a frontend for `gptel`. It supports any model `gptel` supports, but Claude 3.5 Sonnet is recommended for code generation.
- **Context Scoping:** If a region is active, only that region's content is sent to the LLM. Otherwise, the entire buffer is sent. Using regions is recommended to avoid API rate limits.
- **Configuration:** Requires the user to configure `gptel` with a backend, model, and API key. Elysium itself has customizable variables like `elysium-window-size` and `elysium-window-style`.
'''

# Planned enhancements for the project.
future_work = "Implement prompt caching with the Anthropic API to reduce token usage and avoid rate-limiting."
