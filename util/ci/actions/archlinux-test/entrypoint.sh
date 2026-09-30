#!/bin/bash -le

mkdir -p "$HOME"/.local/share

mkdir build
cd build
export TZ="America/Los_Angeles"
export PATH="$PATH:/usr/bin/core_perl"
export CTEST_OUTPUT_ON_FAILURE=On
git config --global --add safe.directory /github/workspace
cmake /github/workspace -DWITH_PYTHON=ON -DCMAKE_BUILD_TYPE=debug -G Ninja
ninja
if ! GNC_REQUIRE_DISPLAY=1 dbus-run-session -- xvfb-run -a ninja check; then
    cp Testing/Temporary/LastTest.log /github/workspace
    exit 1
fi

cp Testing/Temporary/LastTest.log /github/workspace
