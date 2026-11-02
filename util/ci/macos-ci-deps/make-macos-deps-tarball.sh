#!/bin/sh
set -e

fn=$1
if [[ "x$fn" = "x" ]]; then
   fn="macos-dependencies.tar.xz"
fi
DIR=$(pwd)

export PREFIX=/Users/runner/gnucash/inst
jhbuild bootstrap-gtk-osx
jhbuild build

cd /Users/runner/gnucash
mv inst arch
cp $(which ninja) arch/bin/
mkdir inst
for i in 'bin' 'include' 'lib' 'share'; do
    j="$DIR/util/ci/macos-ci-deps/macos_$i.manifest"
    mkdir inst/$i
    for k in `cat $j`; do
        mv arch/$i/$k inst/$i
    done
done

"$PREFIX/bin/glib-compile-schemas" --strict "$PREFIX/share/glib-2.0/schemas"
sh "$DIR/util/ci/macos-ci-deps/verify-macos-deps.sh" "$PREFIX"
tar -cJf "$DIR/$fn" -C "$PREFIX" .
