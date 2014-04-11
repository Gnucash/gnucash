#! /bin/bash

function qpushd() { pushd "$@" >/dev/null; }
function qpopd() { popd >/dev/null; }
function unix_path() { echo "$*" | sed 's,^\([A-Za-z]\):,/\1,;s,\\,/,g'; }

qpushd "$(dirname "$0")"
. functions.sh
. defaults.sh

#
# From http://www.mingw.org/MinGWiki/index.php/BuildMingwCross
#

# This is my script for building a complete MinGW cross-compiler toolchain
# that runs under Linux to produce executables that run under Windows.  It
# probably works (or can easily be adapted to work) under any unix system.
#
# It is based in large part on Sam Lantinga's script, which in turn was
# based partly on Ray Kelm's script, which in turn was built on
# Mo Dejong's script for doing the same, but with some added fixes.
#
# My changes:
#       1. Adapted the script to the new packaging of MinGW GCC, which is
#          currently split into core and auxiliary components.
#       2. The script now determines the GCC and BINUTILS directory name
#          directly from the tar file contents.  This gets around common
#          problems due to the directory names not always following the
#          expected patterns.
#       3. Grouped together and simplified the macros that users need to
#          define.
#       4. Made optional components truly optional -- leave the
#          corresponding archive names blank and they will be ignored.
#       5. Included an option to purge the installation directory before
#          installing the current cross-compiler.
#
# NOTE: If you choose a destination directory for the installation (set
# in the macro PREFIX) for which you do not have write access, you will
# need to run this script with root (or equivalent) privileges.
#
#
# Updated by Igor Mikolic-Torreira <igormt@alumni.caltech.edu>



#-----------------------------------------------------
#
# BEGIN USER SETTINGS
#
# You need to review and adjust the macros that follow
#
#-----------------------------------------------------


# What flavor of GCC cross-compiler are we building?

TARGET=mingw32

# What directory will the cross-compiler be built in?
# This is the directory into which source archives will
# be downloaded, expanded, compiled, etc.  You need to
# have write-access to this directory.  If you leave it
# blank, it defaults to the current directory.

BUILDDIR=`unix_path $TMP_DIR`

# Where does the cross-compiler go?
# This should be the directory into which your cross-compiler
# will be installed.  Remember that if you set this to a directory
# that only root has write access to, you will need to run this
# script as root.

PREFIX=`unix_path $MINGW_DIR`

# Purge anything and everything already in the $PREFIX
#(also known as the destination or installation) directory?
# Set to "Y" to purge, any other value omits the purge step.

PURGE_DIR="Y"


# Set the following to the files from the current MinGW release
# (or whichever MinGW release you wish to build and install)
# You need to set both the URL they will be downloaded from
# and the exact name of the individual component files.

MINGW_URL="http://heanet.dl.sourceforge.net/sourceforge/mingw"

# GCC_CORE is required; the other components are optional.
# Set any you don't want to "".  You need binutils,
# mingw runtime and w32api; do not ever set those to "".

GCC_CORE_ARCHIVE="gcc-core-3.4.5-20060117-1-src.tar.gz"
GCC_GPP_ARCHIVE="gcc-g++-3.4.5-20060117-1-src.tar.gz"
GCC_G77_ARCHIVE="" #gcc-g77-3.4.5-20060117-1-src.tar.gz"
GCC_OBJC_ARCHIVE="" #gcc-objc-3.4.5-20060117-1-src.tar.gz"
GCC_JAVA_ARCHIVE="" #gcc-java-3.4.5-20060117-1-src.tar.gz"
GCC_ADA_ARCHIVE=""
GCC_PATCH=""

BINUTILS_ARCHIVE="binutils-2.16.91-20060119-1-src.tar.gz"

MINGW_ARCHIVE="mingw-runtime-3.9.tar.gz"

W32API_ARCHIVE="w32api-3.6.tar.gz"


# These are the files from the SDL website
# These are optional, set them to "" if you don't want them)

SDL_URL="" #http://www.libsdl.org/extras/win32/common"

OPENGL_ARCHIVE="" #opengl-devel.tar.gz"
DIRECTX_ARCHIVE="" #directx-devel.tar.gz"



#-----------------------------------------------------
#
# END USER SETTINGS
#
# The remainder of the script should not neet any edits
#
#-----------------------------------------------------



# Make sure these are initialized as we want them

GCC_CORE=""
BINUTILS=""
GCC_LANGS="c"


# Set our build directory and where our sources will go

if [ "x$BUILDDIR" = "x" ]; then
        # Default to the current directory
        BUILDDIR=$(pwd)
fi
SRCDIR="$BUILDDIR/source"


# Need install directory first on the path so gcc can find binutils

PATH="$PREFIX/bin:$PATH"



#-----------------------------------------------------
#
# Functions that do most of the work
#
#-----------------------------------------------------


function download_files
{
        # Download a file from a given url, only if it is not present
        mkdir -p "$SRCDIR"

        # Make sure wget is installed
        if test "x`which wget`" = "x" ; then
                echo "You need to install wget."
                exit 1
        fi
        download_file "$GCC_CORE_ARCHIVE" "$MINGW_URL"
        if [ "x$GCC_GPP_ARCHIVE" != "x" ]; then
                download_file "$GCC_GPP_ARCHIVE" "$MINGW_URL"
        fi
        if [ "x$GCC_G77_ARCHIVE" != "x" ]; then
                download_file "$GCC_G77_ARCHIVE" "$MINGW_URL"
        fi
        if [ "x$GCC_OBJC_ARCHIVE" != "x" ]; then
                download_file "$GCC_OBJC_ARCHIVE" "$MINGW_URL"
        fi
        if [ "x$GCC_JAVA_ARCHIVE" != "x" ]; then
                download_file "$GCC_JAVA_ARCHIVE" "$MINGW_URL"
        fi
        if [ "x$GCC_ADA_ARCHIVE" != "x" ]; then
                download_file "$GCC_ADA_ARCHIVE" "$MINGW_URL"
        fi

        download_file "$BINUTILS_ARCHIVE" "$MINGW_URL"
        download_file "$MINGW_ARCHIVE" "$MINGW_URL"
        download_file "$W32API_ARCHIVE" "$MINGW_URL"

        if [ "x$OPENGL_ARCHIVE" != "x" ]; then
                download_file "$OPENGL_ARCHIVE" "$SDL_URL"
        fi
        if [ "x$DIRECTX_ARCHIVE" != "x" ]; then
                download_file "$DIRECTX_ARCHIVE" "$SDL_URL"
        fi
}


function download_file
{
        cd "$SRCDIR"
        if test ! -f $1 ; then
                echo "Downloading $1"
                wget "$2/$1"
                if test ! -f $1 ; then
                        echo "Could not download $1"
                        exit 1
                fi
        else
                echo "Found $1 in the srcdir $SRCDIR"
        fi
        cd "$BUILDDIR"
}


function purge_existing_install
{
        echo "Purging the existing files in $PREFIX"
        if cd "$PREFIX"; then
                rm -rf *
        fi
        cd "$BUILDDIR"
}


function install_libs
{
        echo "Installing cross libs and includes"
        mkdir -p "$PREFIX/$TARGET"
        cd "$PREFIX/$TARGET"

        tar -xzf "$SRCDIR/$MINGW_ARCHIVE"
        tar -xzf "$SRCDIR/$W32API_ARCHIVE"

        if [ "x$OPENGL_ARCHIVE" != "x" ]; then
                tar -xzf "$SRCDIR/$OPENGL_ARCHIVE"
        fi

        if [ "x$DIRECTX_ARCHIVE" != "x" ]; then
                tar -xzf "$SRCDIR/$DIRECTX_ARCHIVE"
        fi

        cd "$BUILDDIR"
}


function extract_binutils
{
        cd "$SRCDIR"
        BINUTILS=`tar -tzf "$SRCDIR/$BINUTILS_ARCHIVE" | head -n 1`
        rm -rf "$BINUTILS"
        echo "Extracting binutils"
        tar -xzf "$SRCDIR/$BINUTILS_ARCHIVE"
        cd "$BUILDDIR"
}


function configure_binutils
{
        cd "$BUILDDIR"
        rm -rf "binutils-$TARGET"
        mkdir "binutils-$TARGET"
        cd "binutils-$TARGET"
        echo "Configuring binutils"
        "$SRCDIR/$BINUTILS/configure" --prefix="$PREFIX" --target=$TARGET --disable-nls \
                --with-gcc --with-gnu-as --with-gnu-ld --disable-shared &> configure.log
        cd "$BUILDDIR"
}


function build_binutils
{
        cd "$BUILDDIR/binutils-$TARGET"
        echo "Building binutils"
        make CFLAGS="-O2 -fno-exceptions" LDFLAGS="-s" &> make.log
        if test $? -ne 0; then
                echo "make of binutils failed - log available: binutils-$TARGET/make.log"
                exit 1
        fi
        cd "$BUILDDIR"
}


function install_binutils
{
        cd "$BUILDDIR/binutils-$TARGET"
        echo "Installing binutils"
        make install &> make-install.log
        if test $? -ne 0; then
                echo "install of binutils failed - log available: binutils-$TARGET/make-install.log"
                exit 1
        fi
        cd "$BUILDDIR"
}


function extract_gcc
{
        cd "$SRCDIR"
        GCC=`tar -tzf "$SRCDIR/$GCC_CORE_ARCHIVE" | head -n 1`
        rm -rf "$GCC"
        echo "Extracting gcc"
        tar -xzf "$SRCDIR/$GCC_CORE_ARCHIVE"
        if [ "x$GCC_GPP_ARCHIVE" != "x" ]; then
                GCC_LANGS=${GCC_LANGS}",c++"
                tar -xzf "$SRCDIR/$GCC_GPP_ARCHIVE"
        fi
        if [ "x$GCC_G77_ARCHIVE" != "x" ]; then
                GCC_LANGS=${GCC_LANGS}",f77"
                tar -xzf "$SRCDIR/$GCC_G77_ARCHIVE"
        fi
        if [ "x$GCC_OBJC_ARCHIVE" != "x" ]; then
                GCC_LANGS=${GCC_LANGS}",objc"
                tar -xzf "$SRCDIR/$GCC_OBJC_ARCHIVE"
        fi
        if [ "x$GCC_JAVA_ARCHIVE" != "x" ]; then
                GCC_LANGS=${GCC_LANGS}",java"
                tar -xzf "$SRCDIR/$GCC_JAVA_ARCHIVE"
        fi
        if [ "x$GCC_ADA_ARCHIVE" != "x" ]; then
                GCC_LANGS=${GCC_LANGS}",ada"
                tar -xzf "$SRCDIR/$GCC_ADA_ARCHIVE"
        fi
        cd "$BUILDDIR"
}


function patch_gcc
{
        if [ "$GCC_PATCH" != "" ]; then
                echo "Patching gcc"
                cd "$SRCDIR/$GCC"
                patch -p1 < "$SRCDIR/$GCC_PATCH"
                cd "$BUILDDIR"
        fi
}


function configure_gcc
{
        cd "$BUILDDIR"
        rm -rf "gcc-$TARGET"
        mkdir "gcc-$TARGET"
        cd "gcc-$TARGET"
        echo "Configuring gcc"
        "$SRCDIR/$GCC/configure" -v \
                --prefix="$PREFIX" --target=$TARGET \
                --with-headers="$PREFIX/$TARGET/include" \
                --with-gcc --with-gnu-ld --with-gnu-as \
                --enable-threads --disable-nls --enable-languages=$GCC_LANGS \
                --disable-win32-registry --disable-shared --enable-sjlj-exceptions --enable-libgcj \
                --disable-java-awt --without-x --enable-java-gc=boehm --disable-libgcj-debug \
                --enable-interpreter --enable-hash-synchronization --enable-libstdcxx-debug \
                &> configure.log
        cd "$BUILDDIR"
}


function build_gcc
{
        cd "$BUILDDIR/gcc-$TARGET"
        echo "Building gcc"
        make CFLAGS="-O2" CXXFLAGS="-O2" GCJFLAGS="-O2" LDFLAGS="-s" DEBUG_FLAGS="-g0" &> make.log
        if test $? -ne 0; then
                echo "make of gcc failed - log available: gcc-$TARGET/make.log"
                exit 1
        fi
        if [ "x$GCC_ADA" != "x" ]; then
                cd gcc
                make "CFLAGS=-O2" "LDFLAGS=-s" gnatlib_and_tools &> make-gnatlib.log
                if test $? -ne 0; then
                        echo "make of gnatlib and tools failed - log available: gcc-$TARGET/make-gnatlib.log"
                        exit 1
                fi
        fi
        cd "$BUILDDIR"
}


function install_gcc
{
        cd "$BUILDDIR/gcc-$TARGET"
        echo "Installing gcc"
        make install &> make-install.log
        if test $? -ne 0; then
                echo "install of gcc failed - log available: gcc-$TARGET/make-install.log"
                exit 1
        fi
        cd "$BUILDDIR"
}


function final_tweaks
{
        echo "Finalizing installation"

        # remove gcc build headers
        rm -rf "$PREFIX/$TARGET/sys-include"

        # Add extra binary links
        if [ ! -f "$PREFIX/$TARGET/bin/objdump" ]; then
                ln "$PREFIX/bin/$TARGET-objdump" "$PREFIX/$TARGET/bin/objdump"
        fi

        # make cc and c++ symlinks to gcc and g++
        if [ ! -f "$PREFIX/$TARGET/bin/g++" ]; then
                ln "$PREFIX/bin/$TARGET-g++" "$PREFIX/$TARGET/bin/g++"
        fi
        if [ ! -f "$PREFIX/$TARGET/bin/cc" ]; then
                ln -s "gcc" "$PREFIX/$TARGET/bin/cc"
        fi
        if [ ! -f "$PREFIX/$TARGET/bin/c++" ]; then
                ln -s "g++" "$PREFIX/$TARGET/bin/c++"
        fi

        # strip all the binaries
        ls "$PREFIX"/bin/* "$PREFIX/$TARGET"/bin/* | egrep -v '.dll$' | egrep -v 'gccbug$' |
        while read file; do
                strip "$file"
        done

        echo "Installation complete!"
}



#
# Main part of the script
#

download_files

if [ "x$PURGE_DIR" = "xY" ]; then
        purge_existing_install
fi

install_libs

extract_binutils
configure_binutils
build_binutils
install_binutils

extract_gcc
patch_gcc
configure_gcc
build_gcc
install_gcc

final_tweaks


#
# End
#
qpopd
