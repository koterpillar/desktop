---
name: diffle
description: >
  Showing prepared diffs or complete PRs in the browser, letting the user
  comment on them and receiving the comments back as a prompt for changes.
  Use when the user mentions "diffle".
---

# diffle

`diffle` starts a local web server that shows a git diff in the browser. The
user reads the diff and leaves line comments. When the user closes the last
browser tab, `diffle` stops and prints the open comments to **stdout** as a
prompt for an agent. Status messages go to stderr.

## Choose what to review

| Command | What it shows |
| --- | --- |
| `diffle` | `HEAD..worktree`: uncommitted changes |
| `diffle working` | Same, with staged and untracked files |
| `diffle main` | What this branch added since it left `main`, plus uncommitted changes |
| `diffle HEAD~3` | The last three commits plus uncommitted changes |
| `diffle main..feat` | `main` against `feat` |
| `diffle main...feat` | What `feat` added since it left `main` |
| `diffle show [rev]` | One commit against its first parent (default `HEAD`) |
| `diffle pr [number\|url]` | A GitHub pull request (default: the PR for this branch) |
| `diffle range-diff <a>..<b> <c>..<d>` | How one commit series became another |

`worktree` can be used on either side of `..`. A lone revision compares from
the merge base.

## Useful options

* `-C <path>`: run as if started in `<path>` (any directory in a worktree).
* `-p, --port <port>`: port (default 4966, or the next free one; `0` = random).
* `--no-open`: do not open a browser.
* `--keep-alive`: keep running after all tabs close. Stop with Ctrl+C.
* `--no-watch`: do not watch for file changes.
* `--auto-viewed <glob>`: mark matching files as viewed (repeatable), for
  example `--auto-viewed '*.lock'`.
* `-U, --context <n>`: context lines around changes (default 5).
* `--no-lsp`, `--lsp <language=command>`: control language servers.
* `--no-checkout`: do not check out the new side of the review.

## Other commands

* `diffle config`: print the config file path and contents
  (`~/.config/diffle/config.json`).
* `diffle lsp`: show which language server each language gets, and which are
  missing from `PATH`.
* `diffle completion`: print a shell completion script.

## Tips

* To save the review, redirect stdout: `diffle main > review.md`.
* Ctrl+C stops `diffle` at any time.
* Run `diffle --help` for the full, current option list.
