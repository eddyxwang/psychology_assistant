# AGENTS.md — Repository AI Assistant Guidelines

When working in this repository (pyschology_assistant), you MUST follow the rules below.

## Process Files Storage Rule (Mandatory)

All **process files** you generate while carrying out a task MUST be placed under the `.copilot_temp/` directory at the repository root. Do not scatter them anywhere else in the repository.

**Process files include (but are not limited to):**
- Temporary scripts (one-off extraction, conversion, or debugging scripts)
- Intermediate data and extracted text (PDF/document extraction, exports, caches)
- Logs, debug output, test artifacts, draft notes
- Any intermediate artifacts used to complete a task that are not final deliverables

**Rules:**
1. Always write to `.copilot_temp/`. You may create subdirectories by purpose, e.g. `.copilot_temp/scripts/`, `.copilot_temp/data/`, `.copilot_temp/logs/`.
2. Do not place process files in the repository root, `temp/`, or any other official directory.
3. After finishing a task, clean up temporary files in `.copilot_temp/` that are no longer needed.
4. Final deliverables (artifacts the user explicitly requested, such as reports, documents, or source code) still belong in the repository's official directories and are NOT process files.
5. `.copilot_temp/` is ignored in `.gitignore`. Never commit anything inside it to git.

## Language Rules (Mandatory)

1. Any code- or system-related content — including source code, code comments, identifiers, file names, commit messages, configuration, and technical documentation — MUST be written in English.
2. Textual materials (prose, notes, reports) may use Chinese or English as appropriate, and should stay consistent with the language of the source material being referenced or produced.
3. When conversing with the user, ALWAYS reply in Chinese.
