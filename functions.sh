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
    DISTRO=$(grep '^ID=' /etc/os-release | cut -d = -f 2)

    case $DISTRO in
      fedora)
        FEDORA_VERSION=$(rpm -E %fedora)
        for PART in free nonfree
        do
          if ! (rpm -q rpmfusion-$PART-release 2>/dev/null || true) | grep -q "$FEDORA_VERSION" >/dev/null
          then
            sudo dnf -y install "https://download1.rpmfusion.org/$PART/fedora/rpmfusion-$PART-release-${FEDORA_VERSION}.noarch.rpm"
          fi
        done

        if ! rpm -q gpg-pubkey --qf '%{NAME}-%{VERSION}-%{RELEASE}\t%{SUMMARY}\n' | grep -q Microsoft >/dev/null
        then
          sudo rpm --import https://packages.microsoft.com/keys/microsoft.asc
        fi

        if ! [ -f /etc/yum.repos.d/vscode.repo ]
        then
          sudo cp repositories/vscode.repo /etc/yum.repos.d/vscode.repo
        fi

        ;;
      debian)
        if ! command_exists python3
        then
          sudo apt install --yes python3{,-{pip,venv}}
        fi
        ;;
    esac
esac

venv() {
    PYTHON_ENV="$DIR/python_env"
    if ! [ -d "$PYTHON_ENV" ]; then python3 -m venv "$PYTHON_ENV"; fi
    PATH="$PYTHON_ENV/bin:$PATH"
    pip --quiet install --upgrade pip
    pip --quiet install -r "$DIR/requirements.txt"
}
