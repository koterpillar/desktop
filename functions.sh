if (echo $OSTYPE | grep -q darwin)
then
    LN=gln
    READLINK=greadlink
    OS=macos
else
    LN=ln
    READLINK=readlink
    OS=linux
fi

DIR=$($READLINK -f $(dirname $0))
