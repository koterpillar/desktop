# Configuration

This is my desktop environment configuration repository.

## New machine

### Authentication

* Get `git`
* Generate an SSH keypair
* Upload public key to GitHub
  - and to work code hosting if applicable

### Installation

```shell
./install <roles>
```

FIXME: Might need to re-run because of the ordering between dependencies.

FIXME: Cloning repositories into nested directories (e.g. `asdf` and its
plugins) keeps failing even when re-running. Remove the destination directory,
remove nested entries, re-run and then reinstate the nested entries.

### Configuration

* Change the hostname conditions in `.zshrc` to match the new machine

### Post-install

* Upload public key to own server
* Upload public key to own code hosting
