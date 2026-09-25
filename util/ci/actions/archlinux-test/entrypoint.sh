#!/bin/bash -le
set -e

mkdir -p "$HOME"/.local/share
workspace="${GITHUB_WORKSPACE:-/github/workspace}"

mkdir build
cd build
export TZ="America/Los_Angeles"
export PATH="$PATH:/usr/bin/core_perl"
export CTEST_OUTPUT_ON_FAILURE=On
git config --global --add safe.directory "$workspace"
# Arch does not yet package the Gwenhywfar GTK4 frontend. Keep this job
# explicit until gwengui-gtk4 is available in the official repositories.
cmake "$workspace" -DWITH_PYTHON=ON -DWITH_AQBANKING=OFF -DCMAKE_BUILD_TYPE=debug -G Ninja
xvfb-run -a bash "$workspace/util/ci/validate-gtk4-builder-resources.sh" "$workspace/gnucash"
ninja
trap 'cp Testing/Temporary/LastTest.log "$workspace/LastTest.log" 2>/dev/null || true' EXIT
ninja check
