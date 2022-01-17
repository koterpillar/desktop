if (echo $OSTYPE | grep -q darwin)
then
    OS=macos
    DIR=$(cd $(dirname $0); pwd -P)
    PATH="/usr/local/opt/coreutils/libexec/gnubin:/usr/local/opt/gnu-tar/libexec/gnubin:$PATH"
else
    OS=linux
    OS_DISTRIBUTION=$(cat /etc/os-release | grep '^ID=' | cut -d = -f 2)
    DIR=$(readlink -f $(dirname $0))
fi

