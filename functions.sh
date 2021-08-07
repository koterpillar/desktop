if (echo $OSTYPE | grep -q darwin)
then
    LN=gln
    READLINK=greadlink
    OS=macos
else
    LN=ln
    READLINK=readlink
    OS=linux
    OS_DISTRIBUTION=$(cat /etc/os-release | grep '^ID=' | cut -d = -f 2)
fi

DIR=$($READLINK -f $(dirname $0))
