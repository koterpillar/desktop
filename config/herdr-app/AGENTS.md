# Herdr configuration

Herdr reads its live configuration from `~/.config/herdr-app/`. This directory in the dotfiles repository, `config/herdr-app/`, holds the configuration you can commit.

Edit the repository files when possible. If you change the live configuration, make sure that the corresponding changes are present here before committing. A symbolic link points to a file in this repository. If a live file is a copy or Herdr replaces a symbolic link, bring the intended changes back here.

Mybox copies the files in `my-icons/` because Herdr cannot load those icons through symbolic links. Mybox symlinks the rest of the configuration. The sync rules in [`packages/desktop.yaml`](../../packages/desktop.yaml) exclude this `AGENTS.md`.

Create new configuration files in this repository first. To use a new file immediately, create a symbolic link at the matching path under `~/.config/herdr-app/`. If you add an icon, copy it to the matching path instead.
