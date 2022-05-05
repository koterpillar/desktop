command_exists() {
    type -f "$1" >/dev/null 2>&1
}

# Prerequisites
case "$OSTYPE" in
  darwin*)
    OS=macos
    DIR=$(cd $(dirname $0); pwd -P)

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
    OS=linux
    DIR=$(readlink -f $(dirname $0))

    if ! command_exists python3
    then
        DISTRO=$(cat /etc/os-release | grep '^ID=' | cut -d = -f 2)
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
    pip install -r "$DIR/requirements.txt"
}
