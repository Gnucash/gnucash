#! /bin/bash -ev
# The -e above says that if any line below fails, the whole script fails

# Run tests in different build configurations depending on the
# value of the BUILDTYPE environment variable
# 1. cmake build using the default Makefile generator
if [[ "$BUILDTYPE" == "cmake-make" ]]; then
  mkdir /tmp/gnucash-build-cmake-make
  cd /tmp/gnucash-build-cmake-make
  cmake $TRAVIS_BUILD_DIR
  make -j 4
  make check

# 2. cmake build using the Ninja generator
elif [[ "$BUILDTYPE" == "cmake-ninja" ]]; then
  mkdir /tmp/gnucash-build-cmake-ninja
  cd /tmp/gnucash-build-cmake-ninja
  cmake -G Ninja $TRAVIS_BUILD_DIR
  ninja
  ninja check

# 3. autotools build
elif [[ "$BUILDTYPE" == "autotools" ]]; then
  cd $TRAVIS_BUILD_DIR
  ./autogen.sh
  ./configure
  make
  make check
fi

