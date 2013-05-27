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

# Make sure these are initialized as we want them

GCC_CORE=""
BINUTILS=""
GCC_LANGS="c"


# Set our build directory and where our sources will go

if [ "x$XC_BUILD_DIR" = "x" ]; then
    # Default to the current directory
    XC_BUILD_DIR=$(pwd)
fi
SRCDIR="$XC_BUILD_DIR/source"


# Need install directory first on the path so gcc can find binutils

PATH="$PREFIX/bin:$PATH"

# Prepare directories used by download function

DOWNLOAD_UDIR=`unix_path $DOWNLOAD_DIR`
TMP_UDIR=`unix_path $TMP_DIR`
mkdir -p "$TMP_UDIR"
mkdir -p "$DOWNLOAD_UDIR"
mkdir -p "$SRCDIR"

# Make sure wget is installed
if test "x`which wget`" = "x" ; then
    echo "You need to install wget."
    exit 1
fi

qpushd "$XC_BUILD_DIR"


#-----------------------------------------------------
#
# Functions that do most of the work
#
#-----------------------------------------------------


function purge_existing_install
{
    echo "Purging the existing files in $PREFIX"
    mkdir -p "$PREFIX"
    if cd "$PREFIX"; then
        rm -rf *
    fi
    cd "$XC_BUILD_DIR"
}


function install_libs
{
    echo "Installing cross libs and includes"
    mkdir -p "$PREFIX/$TARGET"

    wget_unpacked "$MINGW_RT_URL" "$DOWNLOAD_DIR" "$PREFIX/$TARGET"
    wget_unpacked "$W32API_URL" "$DOWNLOAD_DIR" "$PREFIX/$TARGET"

    if [ "x$OPENGL_URL" != "x" ]; then
        wget_unpacked "$GCC_OPENGL_SRC_URL" "$DOWNLOAD_DIR" "$SRCDIR"
    fi
    if [ "x$DIRECTX_URL" != "x" ]; then
        wget_unpacked "$GCC_DIRECTX_SRC_URL" "$DOWNLOAD_DIR" "$SRCDIR"
    fi
}


function install_binutils
{
    setup "   binutils (cross-compile)"

    if quiet $PREFIX/bin/$TARGET-ld --version
    then
        echo "binutils already installed.  skipping."
    else
        wget_unpacked "$BINUTILS_SRC_URL" "$DOWNLOAD_DIR" "$SRCDIR"
        BINUTILS_SRC_DIR=$_EXTRACT_UDIR

        BINUTILS_BUILD_DIR="$XC_BUILD_DIR/binutils-$TARGET"
        rm -rf "$BINUTILS_BUILD_DIR"
        mkdir "$BINUTILS_BUILD_DIR"
        qpushd "$BINUTILS_BUILD_DIR"

        echo -n "Configuring ... "
        "$BINUTILS_SRC_DIR/configure" --prefix="$PREFIX" --target=$TARGET --disable-nls \
                --with-gcc --with-gnu-as --with-gnu-ld --disable-shared &> configure.log
        echo done

        echo -n "Building ... "
        make CFLAGS="-O2 -fno-exceptions" LDFLAGS="-s" &> make.log
        if test $? -ne 0; then
            echo "failed - log available: $BINUTILS_BUILD_DIR/make.log"
            exit 1
        fi
        echo done

        echo -n "Installing ... "
        make install &> make-install.log
        if test $? -ne 0; then
            echo "failed - log available: $BINUTILS_BUILD_DIR/make-install.log"
            exit 1
        fi
        echo done

        qpopd
    fi
}


function install_gcc
{
    setup "   gcc (cross-compile)"

    if quiet $PREFIX/bin/$TARGET-g++ --version
    then
        echo "gcc already installed.  skipping."
    else
        # Filename doesn't match expanded directory, let's fix that
        TARGET_FILE=${GCC_CORE_SRC_URL##*/}
        TARGET_FILE=${TARGET_FILE/-core-/-}
        wget_unpacked "$GCC_CORE_SRC_URL" "$DOWNLOAD_DIR" "$SRCDIR" $TARGET_FILE
        GCC_SRC_DIR=$_EXTRACT_UDIR

        if [ "x$GCC_GPP_SRC_URL" != "x" ]; then
            GCC_LANGS=${GCC_LANGS}",c++"
            wget_unpacked "$GCC_GPP_SRC_URL" "$DOWNLOAD_DIR" "$SRCDIR"
        fi
        if [ "x$GCC_G77_SRC_URL" != "x" ]; then
            GCC_LANGS=${GCC_LANGS}",f77"
            wget_unpacked "$GCC_G77_SRC_URL" "$DOWNLOAD_DIR" "$SRCDIR"
        fi
        if [ "x$GCC_OBJC_SRC_URL" != "x" ]; then
            GCC_LANGS=${GCC_LANGS}",objc"
            wget_unpacked "$GCC_OBJC_SRC_URL" "$DOWNLOAD_DIR" "$SRCDIR"
        fi
        if [ "x$GCC_JAVA_SRC_URL" != "x" ]; then
            GCC_LANGS=${GCC_LANGS}",java"
            wget_unpacked "$GCC_JAVA_SRC_URL" "$DOWNLOAD_DIR" "$SRCDIR"
        fi
        if [ "x$GCC_ADA_SRC_URL" != "x" ]; then
            GCC_LANGS=${GCC_LANGS}",ada"
            wget_unpacked "$GCC_ADA_SRC_URL" "$DOWNLOAD_DIR" "$SRCDIR"
        fi

        if [ "$GCC_PATCH" != "" ]; then
            echo -n "Patching ... "
            qpushd "$GCC_SRC_DIR"
            patch -p1 < "$SRCDIR/$GCC_PATCH"
            qpopd
            echo done
        fi

        GCC_BUILD_DIR="$XC_BUILD_DIR/gcc-$TARGET"
        rm -rf "$GCC_BUILD_DIR"
        mkdir "$GCC_BUILD_DIR"
        qpushd "$GCC_BUILD_DIR"

        echo -n "Configuring ... "
        "$GCC_SRC_DIR/configure" -v \
            --prefix="$PREFIX" --target=$TARGET \
            --with-headers="$PREFIX/$TARGET/include" \
            --with-gcc --with-gnu-ld --with-gnu-as \
            --enable-threads --disable-nls --enable-languages=$GCC_LANGS \
            --disable-win32-registry --disable-shared --enable-sjlj-exceptions --enable-libgcj \
            --disable-java-awt --without-x --enable-java-gc=boehm --disable-libgcj-debug \
            --enable-interpreter --enable-hash-synchronization --enable-libstdcxx-debug \
            &> configure.log
        echo done

        echo -n "Building ... "
        make CFLAGS="-O2" CXXFLAGS="-O2" GCJFLAGS="-O2" LDFLAGS="-s" DEBUG_FLAGS="-g0" &> make.log
        if test $? -ne 0; then
            echo "failed - log available: $GCC_BUILD_DIR/make.log"
            exit 1
        fi
        echo done

        # 2010-04-28 I doubt the code below is ever called. GCC_ADA is never defined
        #            Should this be GCC_ADA_SRC_URL ?
        if [ "x$GCC_ADA" != "x" ]; then
            qpushd gcc
            echo -n "Building gnatlib ... "
            make "CFLAGS=-O2" "LDFLAGS=-s" gnatlib_and_tools &> make-gnatlib.log
            if test $? -ne 0; then
                echo "failed - log available: $GCC_BUILD_DIR/gcc/make-gnatlib.log"
                exit 1
            fi
            echo done
            qpopd
        fi

        echo "Installing ... "
        make install &> make-install.log
        if test $? -ne 0; then
            echo "failed - log available: $GCC_BUILD_DIR/make-install.log"
            exit 1
        fi
        echo done

        qpopd
    fi
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
if [ "x$PURGE_DIR" = "xyes" ]; then
    purge_existing_install
fi

install_libs
install_binutils
install_gcc
final_tweaks

#
# End
#
qpopd
