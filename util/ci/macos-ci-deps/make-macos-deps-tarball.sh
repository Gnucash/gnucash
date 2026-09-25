#!/bin/sh

set -eu

fn=${1:-gnucash-future-mac-dependencies.tar.xz}
source_dir=$(pwd)
prefix=/Users/runner/gnucash/inst
export PREFIX="$prefix"

jhbuild bootstrap-gtk-osx
jhbuild build

cd /Users/runner/gnucash
mv inst arch
cp "$(command -v ninja)" arch/bin/
mkdir inst
for directory in bin include lib share
do
    manifest="$source_dir/util/ci/macos-ci-deps/macos_$directory.manifest"
    mkdir "inst/$directory"
    while IFS= read -r entry
    do
        [ -n "$entry" ] || continue
        mv "arch/$directory/$entry" "inst/$directory"
    done < "$manifest"
done

"$prefix/bin/pkgconf" --exists \
    'gtk4 >= 4.14' \
    gtk4-macos \
    gwengui-gtk4 \
    aqbanking

tar -cJf "$source_dir/$fn" inst
