#!/usr/bin/env bash
# Capture a stack only after the normal Windows matrix already failed.
set -euo pipefail

if [[ $# -ne 1 ]]; then
    echo "Usage: $0 BUILD_DIR" >&2
    exit 2
fi

build_dir=$(cd "$1" && pwd)
test_dir="$build_dir/gnucash/import-export/test"
ctest_file="$test_dir/CTestTestfile.cmake"
test_binary="$build_dir/bin/test-import-account-matcher.exe"
log_file="$build_dir/matcher-ime-backtrace.log"
build_dir_windows=$(cygpath -m "$build_dir")
schema_dir_windows="$build_dir_windows/share/glib-2.0/schemas"
test_filter='ImportMatcherTest.embedded_matcher_ignores_late_price_dialog_accept'

mkdir -p "$build_dir"
exec > >(tee "$log_file") 2>&1

if [[ ! -f "$build_dir/Testing/Temporary/LastTest.log" ]] ||
   ! grep -Fq 'gtk_im_context_ime_message_filter' "$build_dir/Testing/Temporary/LastTest.log"; then
    echo 'No matcher IME assertion in the completed test log; no diagnostic replay.'
    exit 0
fi

require_ctest_property()
{
    local property=$1

    if ! grep -Fq "$property" "$ctest_file"; then
        echo "CTest property missing for test-import-account-matcher: $property" >&2
        exit 1
    fi
}

if [[ ! -f "$ctest_file" ]]; then
    echo "Missing generated CTest file: $ctest_file" >&2
    exit 1
fi
if [[ ! -x "$test_binary" ]]; then
    echo "Missing built matcher test binary: $test_binary" >&2
    exit 1
fi
if [[ ! -d "$build_dir/share/glib-2.0/schemas" ]]; then
    echo "Missing generated GSettings schemas: $build_dir/share/glib-2.0/schemas" >&2
    exit 1
fi

# Match the generated gnc_add_test contract instead of invoking the binary in
# an ambient shell. The separate PATH modifier is preserved by prepending the
# build DLL directory to the UCRT64 shell PATH used by the matrix.
require_ctest_property 'test-import-account-matcher'
require_ctest_property 'GNC_UNINSTALLED=YES'
require_ctest_property "GNC_BUILDDIR=$build_dir_windows"
require_ctest_property 'GSETTINGS_BACKEND=memory'
require_ctest_property "GSETTINGS_SCHEMA_DIR=$schema_dir_windows"
require_ctest_property 'PATH=path_list_prepend:'

if ! command -v gdb >/dev/null 2>&1; then
    echo 'The UCRT64 toolchain did not provide gdb; cannot capture a matcher stack.' >&2
    exit 1
fi

export GNC_UNINSTALLED=YES
export GNC_BUILDDIR="$build_dir_windows"
export GSETTINGS_BACKEND=memory
export GSETTINGS_SCHEMA_DIR="$schema_dir_windows"
export PATH="$build_dir/bin:$PATH"

record_package_version()
{
    local package=$1

    if ! pacman -Q "$package"; then
        echo "Installed package version unavailable: $package"
    fi
}

record_runtime_provenance()
{
    local package_prefix=${MINGW_PACKAGE_PREFIX:-mingw-w64-ucrt-x86_64}
    local gtk_dll
    local gtk_dll_windows

    echo 'Runtime package provenance:'
    record_package_version "${package_prefix}-gtk4"
    record_package_version "${package_prefix}-glib2"

    if ! gtk_dll=$(type -P libgtk-4-1.dll); then
        echo 'GTK DLL candidate unavailable on PATH: libgtk-4-1.dll'
        return
    fi

    if gtk_dll_windows=$(cygpath -am "$gtk_dll"); then
        echo "GTK DLL candidate from PATH: $gtk_dll_windows"
    else
        echo "GTK DLL candidate from PATH: $gtk_dll"
    fi

    if ! sha256sum "$gtk_dll"; then
        echo "GTK DLL SHA256 unavailable: $gtk_dll"
    fi
}

record_runtime_provenance

# Do not weaken the regular test policy. This diagnostic-only addition makes
# the Gtk critical stop in gdb so its caller stack is retained in the log.
export G_DEBUG="${G_DEBUG:+$G_DEBUG,}fatal-criticals"

cd "$test_dir"
gdb --version
echo "Running $test_filter under gdb"
echo "Working directory: $PWD"
echo "GNC_BUILDDIR=$GNC_BUILDDIR"
echo "GSETTINGS_SCHEMA_DIR=$GSETTINGS_SCHEMA_DIR"

timeout --foreground 120s gdb --quiet --batch --return-child-result \
    -ex 'set pagination off' \
    -ex 'set debuginfod enabled off' \
    -ex run \
    -ex 'thread apply all bt full' \
    -ex 'info sharedlibrary' \
    -ex 'info functions gtk_im_context_ime_message_filter' \
    --args "$test_binary" "--gtest_filter=$test_filter"
