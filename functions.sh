function aur_install () {
  PACKAGE=$1
  TMPDIR=$(mktemp -d)
  trap "rm -rf $TMPDIR" EXIT
  curl -s https://aur.archlinux.org/cgit/aur.git/snapshot/$PACKAGE.tar.gz | \
    tar xz -C $TMPDIR
  pushd $TMPDIR/$PACKAGE >/dev/null
  makepkg
  sudo pacman -U --noconfirm $PACKAGE-*.pkg.tar.xz
  popd >/dev/null
  rm -rf $TMPDIR
}
