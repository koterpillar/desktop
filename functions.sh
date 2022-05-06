#shellcheck shell=bash

command_exists() {
    type -f "$1" >/dev/null 2>&1
}

# Prerequisites
case "$OSTYPE" in
  darwin*)
    export OS=macos
    DIR=$(cd "$(dirname "$0")" || exit 1; pwd -P)

    PATH="/usr/local/opt/coreutils/libexec/gnubin:/usr/local/opt/gnu-tar/libexec/gnubin:$PATH"

    if ! command_exists python3
    then
        if ! command_exists brew
        then

            /bin/bash -c "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/HEAD/install.sh)"
        fi
        brew install python3
    fi
  ;;
  linux*)
    export OS=linux
    DIR=$(readlink -f "$(dirname "$0")")

    if ! command_exists python3
    then
        DISTRO=$(grep '^ID=' /etc/os-release | cut -d = -f 2)
        case $DISTRO in
          debian)
            sudo apt install --yes python3{,-{pip,venv}}
            ;;
        esac
    fi
esac

venv() {
    PYTHON_ENV="$DIR/python_env"
    if ! [ -d "$PYTHON_ENV" ]; then python3 -m venv "$PYTHON_ENV"; fi
    PATH="$PYTHON_ENV/bin:$PATH"
    pip --quiet install --upgrade pip
    pip --quiet install -r "$DIR/requirements.txt"
}
