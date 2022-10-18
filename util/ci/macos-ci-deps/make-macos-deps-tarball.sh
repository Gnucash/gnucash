#!/bin/sh

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

tar -cJf $DIR/$fn inst
