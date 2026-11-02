#!/bin/sh
set -eu

prefix=${1:?Usage: verify-macos-deps.sh PREFIX}
missing=0
for resource in \
    share/glib-2.0/schemas/org.gtk.Settings.FileChooser.gschema.xml \
    share/glib-2.0/schemas/gschemas.compiled \
    share/libofx/dtd/opensp.dcl \
    share/libofx/dtd/ofx160.dtd
do
    if [ ! -s "$prefix/$resource" ]; then
        printf 'Missing macOS dependency resource: %s\n' "$prefix/$resource" >&2
        missing=1
    fi
done
if [ "$missing" -ne 0 ]; then
    printf 'Rebuild the dependency archive using util/ci/macos-ci-deps and update the workflow checksum.\n' >&2
    exit 1
fi
"$prefix/bin/glib-compile-schemas" --strict --dry-run "$prefix/share/glib-2.0/schemas"
