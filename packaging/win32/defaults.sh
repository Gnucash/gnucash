#!/bin/sh # for emacs
#
# Don't edit this file directly. Edit `custom.sh' in the same directory
# instead. That will be read in at the beginning of this script.
#
# You can use the full power of bash 2.04 scripting.  In particular, you can
# set any variable mentioned here to something non-empty and it will not be
# overridden later.  However, you must define all variables you make use of
# yourself.  Expressions registered with late_eval are executed at the end of
# the script.
#
# Note: All directories must be without spaces!
#

[ "$__SOURCED_DEFAULTS" ] && return
__SOURCED_DEFAULTS=1

[ -f "./custom.sh" ] && . ./custom.sh || true

set_default GLOBAL_DIR c:\\soft
set_default TMP_DIR $GLOBAL_DIR\\tmp
set_default DOWNLOAD_DIR $GLOBAL_DIR\\downloads

if [ -z "$BUILD_FROM_TARBALL" ]; then
    if [ -f "../../src/swig-runtime.h" ]; then
        BUILD_FROM_TARBALL=yes
    else
        BUILD_FROM_TARBALL=no
    fi
fi

if [ "$BUILD_FROM_TARBALL" = "yes" ]; then
    UPDATE_SOURCES=no
    set_default GNUCASH_DIR "$(wpwd ..\\..)"
    set_default REPOS_DIR $GNUCASH_DIR
    # keep this pointing from BUILD_DIR to REPOS_DIR
    set_default REL_REPOS_DIR ..
    set_default BUILD_DIR $GNUCASH_DIR\\build
    set_default INSTALL_DIR $GNUCASH_DIR\\inst
else
    # change this to "no" if you are using install.sh from the same repository checkout
    set_default UPDATE_SOURCES yes
    # latest revision that should compile, use HEAD or vwxyz
    set_default SVN_REV "HEAD"
    set_default GNUCASH_DIR $GLOBAL_DIR\\gnucash
    set_default REPOS_URL "http://svn.gnucash.org/repo/gnucash/trunk"
    set_default REPOS_DIR $GNUCASH_DIR\\repos
    # keep this pointing from BUILD_DIR to REPOS_DIR
    set_default REL_REPOS_DIR ..\\repos
    set_default BUILD_DIR $GNUCASH_DIR\\build
    set_default INSTALL_DIR $GNUCASH_DIR\\inst
fi

set_default WITH_CUTECASH no
set_default CUTECASH_BUILD_DIR $GNUCASH_DIR\\build-cutecash


####
# For cross-compiling, change this to "yes"
set_default CROSS_COMPILE "no"

# If "yes", build without optimizations (-O0) and ease debugging
set_default DISABLE_OPTIMIZATIONS no

set_default MSYS_DIR $GLOBAL_DIR\\msys

# tools here means binaries runnable without other DLLs or data files
set_default TOOLS_DIR $GLOBAL_DIR\\tools
set_default WGET_DIR $TOOLS_DIR
#WGET=

set_default SF_MIRROR "http://heanet.dl.sourceforge.net/sourceforge"
set_default GTK_MIRROR "ftp.gtk.org/pub"
set_default GNOME_MIRROR "ftp.gnome.org/pub/gnome"
set_default GNOME_WIN32_URL "$GNOME_MIRROR/binaries/win32"
set_default GNOME_WIN32_DEPS_URL "$GNOME_WIN32_URL/dependencies"

set_default DTK_URL "$SF_MIRROR/mingw/msysDTK-1.0.1.exe"
set_default M4_URL "$SF_MIRROR/mingw/m4-1.4.7-MSYS.tar.bz2"

set_default MINGW_RT_URL "$SF_MIRROR/mingw/mingwrt-3.15.1-mingw32.tar.gz"
set_default W32API_URL "$SF_MIRROR/mingw/w32api-3.13-mingw32-dev.tar.gz"
set_default MINGW_MAKE_URL "$SF_MIRROR/mingw/mingw32-make-3.81-20080326-3.tar.gz"
set_default MINGW_DIR $GLOBAL_DIR\\mingw

if [ "$CROSS_COMPILE" != yes ]; then
    # Use native toolchain
    set_default LD ld
    set_default CC gcc
    set_default DLLTOOL dlltool
    set_default RANLIB ranlib

    # For native build on Windows we can use precompiled binaries
    set_default BINUTILS_URL "$SF_MIRROR/mingw/binutils-2.19-mingw32-bin.tar.gz"
    set_default GCC_CORE_URL "$SF_MIRROR/mingw/gcc-core-3.4.5-20060117-3.tar.gz"
    set_default GCC_GPP_URL "$SF_MIRROR/mingw/gcc-g++-3.4.5-20060117-3.tar.gz"
else
    # What flavor of GCC cross-compiler are we building?
    set_default TARGET "mingw32"

    # Insert your cross-compiler mingw32 bin-directories here
    set_default HOST_XCOMPILE "--host=$TARGET"

    # Where does the cross-compiler go?
    # This should be the directory into which your cross-compiler
    # will be installed.  Remember that if you set this to a directory
    # that only root has write access to, you will need to run this
    # script as root.
    set_default PREFIX `unix_path $MINGW_DIR`

    # Use native toolchain
    set_default LD $TARGET-ld
    set_default CC $TARGET-gcc
    set_default DLLTOOL $TARGET-dlltool
    set_default RANLIB $TARGET-ranlib

    # For cross compilation we need to build our own toolchain
    set_default BINUTILS_SRC_URL "$SF_MIRROR/mingw/binutils-2.20.1-src.tar.gz"
    set_default GCC_CORE_SRC_URL "$SF_MIRROR/mingw/gcc-core-3.4.5-20060117-2-src.tar.gz"
    set_default GCC_GPP_SRC_URL "$SF_MIRROR/mingw/gcc-g++-3.4.5-20060117-2-src.tar.gz"
    # Not required for GnuCash
    set_default GCC_G77_SRC_URL "" #"$SF_MIRROR/mingw/gcc-g77-3.4.5-20060117-2-src.tar.gz"
    set_default GCC_OBJC_SRC_URL "" #"$SF_MIRROR/mingw/gcc-objc-3.4.5-20060117-2-src.tar.gz"
    set_default GCC_JAVA_SRC_URL "" #"$SF_MIRROR/mingw/gcc-java-3.4.5-20060117-2-src.tar.gz"
    set_default GCC_ADA_SRC_URL "" #"$SF_MIRROR/mingw/gcc-ada-3.4.5-20060117-2-src.tar.gz"

    # What directory will the cross-compiler be built in?
    # This is the directory into which source archives will
    # be downloaded, expanded, compiled, etc.  You need to
    # have write-access to this directory.  If you leave it
    # blank, it defaults to the current directory.
    set_default XC_BUILD_DIR `unix_path $TMP_DIR`

    # Purge anything and everything already in the $PREFIX
    #(also known as the destination or installation) directory?
    # Set to "yes" to purge, any other value omits the purge step.
    set_default PURGE_DIR "no"

    # If you wish to apply a patch to GCC, put it in the SRC_DIR
    # and add its filename here.
    set_default GCC_PATCH ""

    # These are the files from the SDL website
    # These are optional, set them to "" if you don't want them
    set_default SDL_URL "" #http://www.libsdl.org/extras/win32/common"
    set_default OPENGL_URL "" #"$SDL_URL/opengl-devel.tar.gz"
    set_default DIRECTX_URL "" #$SDL_URL/directx-devel.tar.gz"
fi

set_default CROSS_GCC_SRC_URL "$SF_MIRROR/mingw/gcc-4.4.0-src.tar.bz2"
set_default CROSS_GCC_SRC2_URL "$SF_MIRROR/mingw/gcc-4.4.0-mingw32-src-2.tar.gz"
#set_default CROSS_GCC_SRC_URL "$SF_MIRROR/mingw/gcc-4.5.0-1-mingw32-src.tar.lzma"
set_default CROSS_BINUTILS_SRC_URL "$SF_MIRROR/mingw/binutils-2.20.1-src.tar.gz"

set_default UNZIP_URL "$SF_MIRROR/gnuwin32/unzip-5.51-1.exe"
set_default UNZIP_DIR $TOOLS_DIR

# do not use regex-gnu or regex-spencer v3.8.g3, see bug #382852
set_default REGEX_URL "$GNOME_WIN32_DEPS_URL/libgnurx-2.5.zip"
set_default REGEX_DEV_URL "$GNOME_WIN32_DEPS_URL/libgnurx-dev-2.5.zip"
set_default REGEX_DIR $GLOBAL_DIR\\regex

set_default READLINE_BIN_URL "$SF_MIRROR/gnuwin32/readline-5.0-1-bin.zip"
set_default READLINE_LIB_URL "$SF_MIRROR/gnuwin32/readline-5.0-1-lib.zip"
set_default READLINE_DIR $GLOBAL_DIR\\readline

set_default ACTIVE_PERL_URL "http://downloads.activestate.com/ActivePerl/releases/5.10.1.1007/ActivePerl-5.10.1.1007-MSWin32-x86-291969.zip"
set_default ACTIVE_PERL_DIR $GLOBAL_DIR\\active-perl

set_default AUTOCONF_URL "http://ftp.gnu.org/gnu/autoconf/autoconf-2.63.tar.bz2"
set_default AUTOMAKE_URL "http://ftp.gnu.org/gnu/automake/automake-1.10.2.tar.bz2"
set_default LIBTOOL_URL "http://ftp.gnu.org/gnu/libtool/libtool-2.2.6a.tar.gz"
set_default AUTOTOOLS_DIR $GLOBAL_DIR\\autotools

set_default GMP_URL "ftp://ftp.gnu.org/gnu/gmp/gmp-4.3.1.tar.bz2"
set_default GMP_ABI 32
set_default GMP_DIR $GLOBAL_DIR\\gmp
set_default GMP5_BIN_URL "$SF_MIRROR/mingw/libgmp-5.0.1-1-mingw32-dll-10.tar.lzma"
set_default GMP5_DEV_URL "$SF_MIRROR/mingw/gmp-5.0.1-1-mingw32-dev.tar.lzma"

set_default GUILE_URL "http://ftp.gnu.org/pub/gnu/guile/guile-1.6.8.tar.gz"
set_default SLIB_URL "http://swiss.csail.mit.edu/ftpdir/scm/OLD/slib3a3.zip"
set_default GUILE_DIR $GLOBAL_DIR\\guile

set_default OPENSSL_URL "http://www.openssl.org/source/openssl-0.9.8j.tar.gz"
set_default OPENSSL_DIR $GLOBAL_DIR\\openssl

set_default GNUTLS_URL "http://josefsson.org/gnutls4win/gnutls-2.8.1.zip"
set_default GNUTLS_DIR $GLOBAL_DIR\\gnutls

set_default MINGW_UTILS_URL "$SF_MIRROR/mingw/mingw-utils-0.3.tar.gz"
set_default MINGW_UTILS_DIR $TOOLS_DIR

set_default EXETYPE_SCRIPT `pwd`/exetype.pl
set_default EXETYPE_DIR $TOOLS_DIR

set_default LIBXSLT_URL "http://xmlsoft.org/sources/win32/libxslt-1.1.26.win32.zip"
set_default LIBXSLT_LIBXML2_URL "http://xmlsoft.org/sources/win32/libxml2-2.7.6.win32.zip"
set_default LIBXSLT_SRC_URL "http://xmlsoft.org/sources/libxslt-1.1.26.tar.gz"
set_default LIBXSLT_DIR $GLOBAL_DIR\\libxslt

set_default LIBXML2_URL "$GNOME_WIN32_DEPS_URL/libxml2_2.7.4-1_win32.zip"
set_default LIBXML2_DEV_URL "$GNOME_WIN32_DEPS_URL/libxml2-dev_2.7.4-1_win32.zip"
set_default GETTEXT_RUNTIME_URL "$GNOME_WIN32_DEPS_URL/gettext-runtime-0.17-1.zip"
set_default GETTEXT_RUNTIME_DEV_URL "$GNOME_WIN32_DEPS_URL/gettext-runtime-dev-0.17-1.zip"
set_default GETTEXT_TOOLS_URL "$GNOME_WIN32_DEPS_URL/gettext-tools-0.17.zip"
set_default LIBICONV_URL "$GNOME_WIN32_DEPS_URL/libiconv-1.9.1.bin.woe32.zip"
set_default GLIB_URL "$GNOME_WIN32_URL/glib/2.22/glib_2.22.4-1_win32.zip"
set_default GLIB_DEV_URL "$GNOME_WIN32_URL/glib/2.22/glib-dev_2.22.4-1_win32.zip"
set_default LIBJPEG_URL "$SF_MIRROR/gnucash/jpeg_7-1-fixed-win32.zip"
set_default LIBJPEG_DEV_URL "$GNOME_WIN32_DEPS_URL/jpeg-dev_7-1_win32.zip"
set_default LIBPNG_URL "$GNOME_WIN32_DEPS_URL/libpng_1.4.0-1_win32.zip"
set_default LIBPNG_DEV_URL "$GNOME_WIN32_DEPS_URL/libpng-dev_1.4.0-1_win32.zip"
set_default LIBTIFF_URL "$GNOME_WIN32_DEPS_URL/libtiff_3.9.1-1_win32.zip"
set_default LIBTIFF_DEV_URL "$GNOME_WIN32_DEPS_URL/libtiff-dev_3.9.1-1_win32.zip"
set_default ZLIB_URL "$GNOME_WIN32_DEPS_URL/zlib-1.2.3.zip"
set_default ZLIB_DEV_URL "$GNOME_WIN32_DEPS_URL/zlib-dev-1.2.3.zip"
set_default PKG_CONFIG_URL "$GNOME_WIN32_DEPS_URL/pkg-config_0.23-3_win32.zip"
set_default PKG_CONFIG_DEV_URL "$GNOME_WIN32_DEPS_URL/pkg-config-dev_0.23-3_win32.zip"
set_default CAIRO_URL "$GNOME_WIN32_DEPS_URL/cairo_1.8.8-4_win32.zip"
set_default CAIRO_DEV_URL "$GNOME_WIN32_DEPS_URL/cairo-dev_1.8.8-4_win32.zip"
set_default PIXMAN_URL "http://cairographics.org/releases/pixman-0.17.6.tar.gz"
set_default EXPAT_URL "$GNOME_WIN32_DEPS_URL/expat_2.0.1-1_win32.zip"
set_default EXPAT_DEV_URL "$GNOME_WIN32_DEPS_URL/expat-dev_2.0.1-1_win32.zip"
set_default FONTCONFIG_URL "$GNOME_WIN32_DEPS_URL/fontconfig_2.8.0-2_win32.zip"
set_default FONTCONFIG_DEV_URL "$GNOME_WIN32_DEPS_URL/fontconfig-dev_2.8.0-2_win32.zip"
set_default FREETYPE_URL "$GNOME_WIN32_DEPS_URL/freetype_2.3.11-2_win32.zip"
set_default FREETYPE_DEV_URL "$GNOME_WIN32_DEPS_URL/freetype-dev_2.3.11-2_win32.zip"
set_default ATK_URL "$GNOME_WIN32_URL/atk/1.28/atk_1.28.0-1_win32.zip"
set_default ATK_DEV_URL "$GNOME_WIN32_URL/atk/1.28/atk-dev_1.28.0-1_win32.zip"
set_default PANGO_URL "$GNOME_WIN32_URL/pango/1.26/pango_1.26.2-1_win32.zip"
set_default PANGO_DEV_URL "$GNOME_WIN32_URL/pango/1.26/pango-dev_1.26.2-1_win32.zip"
set_default LIBART_LGPL_URL "$GNOME_WIN32_URL/libart_lgpl/2.3/libart_lgpl-2.3.20.zip"
set_default LIBART_LGPL_DEV_URL "$GNOME_WIN32_URL/libart_lgpl/2.3/libart_lgpl-dev-2.3.20.zip"
set_default GTK_URL "$GNOME_WIN32_URL/gtk+/2.16/gtk+_2.16.6-2_win32.zip"
set_default GTK_DEV_URL "$GNOME_WIN32_URL/gtk+/2.16/gtk+-dev_2.16.6-2_win32.zip"
set_default INTLTOOL_URL "$GNOME_WIN32_URL/intltool/0.40/intltool_0.40.4-1_win32.zip"
set_default ORBIT2_URL "$GNOME_WIN32_URL/ORBit2/2.14/ORBit2-2.14.13.zip"
set_default ORBIT2_DEV_URL "$GNOME_WIN32_URL/ORBit2/2.14/ORBit2-dev-2.14.13.zip"
set_default GAIL_URL "$GNOME_WIN32_URL/gail/1.22/gail-1.22.0.zip"
set_default GAIL_DEV_URL "$GNOME_WIN32_URL/gail/1.22/gail-dev-1.22.0.zip"
set_default POPT_URL "$GNOME_WIN32_DEPS_URL/popt-1.10.2-tml-20050828.zip"
set_default POPT_DEV_URL "$GNOME_WIN32_DEPS_URL/popt-dev-1.10.2-tml-20050828.zip"
set_default GCONF_URL "$GNOME_WIN32_URL/GConf/2.22/GConf_2.22.0-3_win32.zip"
set_default GCONF_DEV_URL "$GNOME_WIN32_URL/GConf/2.22/GConf-dev_2.22.0-3_win32.zip"
set_default LIBBONOBO_URL "$GNOME_WIN32_URL/libbonobo/2.24/libbonobo_2.24.0-1_win32.zip"
set_default LIBBONOBO_DEV_URL "$GNOME_WIN32_URL/libbonobo/2.24/libbonobo-dev_2.24.0-1_win32.zip"
set_default GNOME_VFS_URL "$GNOME_WIN32_URL/gnome-vfs/2.24/gnome-vfs_2.24.1-1_win32.zip"
set_default GNOME_VFS_DEV_URL "$GNOME_WIN32_URL/gnome-vfs/2.24/gnome-vfs-dev_2.24.1-1_win32.zip"
set_default LIBGNOME_URL "$GNOME_WIN32_URL/libgnome/2.24/libgnome_2.24.1-1_win32.zip"
set_default LIBGNOME_DEV_URL "$GNOME_WIN32_URL/libgnome/2.24/libgnome-dev_2.24.1-1_win32.zip"
set_default LIBGNOMECANVAS_URL "$GNOME_WIN32_URL/libgnomecanvas/2.20/libgnomecanvas-2.20.1.zip"
set_default LIBGNOMECANVAS_DEV_URL "$GNOME_WIN32_URL/libgnomecanvas/2.20/libgnomecanvas-dev-2.20.1.zip"
set_default LIBGNOMEUI_URL "$GNOME_WIN32_URL/libgnomeui/2.22/libgnomeui-2.22.1.zip"
set_default LIBGNOMEUI_DEV_URL "$GNOME_WIN32_URL/libgnomeui/2.22/libgnomeui-dev-2.22.1.zip"
set_default LIBGLADE_URL "$GNOME_WIN32_URL/libglade/2.6/libglade_2.6.4-1_win32.zip"
set_default LIBGLADE_DEV_URL "$GNOME_WIN32_URL/libglade/2.6/libglade-dev_2.6.4-1_win32.zip"
# GnuCash on Windows is built with webkit, no need to install gtkhtml
#set_default GTKHTML_URL "$GNOME_WIN32_URL/gtkhtml/3.24/gtkhtml_3.24.2-1_win32.zip"
#set_default GTKHTML_DEV_URL "$GNOME_WIN32_URL/gtkhtml/3.24/gtkhtml-dev_3.24.2-1_win32.zip"
set_default GTK_DOC_URL "$GNOME_MIRROR/sources/gtk-doc/1.13/gtk-doc-1.13.tar.bz2"
set_default GTK_THEME_URL "$SF_MIRROR/gtk-win/gtk2-themes-2009-09-07-win32_bin.zip"
set_default GTK_PREFS_URL "$SF_MIRROR/gtk-win/gtk2_prefs-0.4.1.bin-gtk2.10-win32.zip"
set_default GNOME_DIR $GLOBAL_DIR\\gnome

set_default LIBBONOBOUI_URL "$GNOME_WIN32_URL/libbonoboui/2.24/libbonoboui_2.24.0-1_win32.zip"
set_default LIBBONOBOUI_DEV_URL "$GNOME_WIN32_URL/libbonoboui/2.24/libbonoboui-dev_2.24.0-1_win32.zip"
set_default LIBBONOBOUI_SRC_URL "$GNOME_MIRROR/sources/libbonoboui/2.24/libbonoboui-2.24.2.tar.bz2"
set_default LIBBONOBOUI_PATCH `pwd`/libbonoboui-2.24.2.patch
set_default LIBBONOBOUI_DIR $GLOBAL_DIR\\libbonoboui

set_default SWIG_URL "$SF_MIRROR/swig/swigwin-1.3.36.zip"
set_default SWIG_DIR $GLOBAL_DIR\\swig

set_default PCRE_BIN_URL "$SF_MIRROR/gnuwin32/pcre-7.0-bin.zip"
set_default PCRE_LIB_URL "$SF_MIRROR/gnuwin32/pcre-7.0-lib.zip"
set_default PCRE_DIR $GLOBAL_DIR\\pcre

set_default LIBGSF_URL "$GNOME_MIRROR/sources/libgsf/1.14/libgsf-1.14.15.tar.bz2"
set_default LIBGSF_DIR $GLOBAL_DIR\\libgsf

set_default GOFFICE_URL "$GNOME_MIRROR/sources/goffice/0.7/goffice-0.7.14.tar.bz2"
set_default GOFFICE_DIR $GLOBAL_DIR\\goffice
#set_default GOFFICE_PATCH `pwd`/goffice-0.7.2-patch.diff

set_default GLADE_URL "$GNOME_MIRROR/sources/glade3/3.0/glade3-3.1.2.tar.bz2"
set_default GLADE_DIR $GLOBAL_DIR\\glade

set_default INNO_URL "http://files.jrsoftware.org/is/5/isetup-5.3.9-unicode.exe"
set_default INNO_DIR $GLOBAL_DIR\\inno

set_default HH_URL "http://download.microsoft.com/download/OfficeXPProf/Install/4.71.1015.0/W98NT42KMe/EN-US/HTMLHELP.EXE"
set_default HH_DIR $GLOBAL_DIR\\hh

set_default WEBKIT_URL "$SF_MIRROR/gnucash/webkit-1.1.90-win32.zip"
set_default WEBKIT_DIR $GLOBAL_DIR\\webkit-1.1.90
set_default WEBKIT_SRC_URL "http://www.webkitgtk.org/webkit-1.1.90.tar.gz"
set_default WEBKIT_PATCH `pwd`/webkit-1.2.0-time.diff
set_default WEBKIT_PATCH2 `pwd`/webkit-1.2.0-vsaprintf.diff
set_default ENCHANT_URL "$GNOME_WIN32_URL/dependencies/enchant_1.5.0-2_win32.zip"
set_default ENCHANT_DEV_URL "$GNOME_WIN32_URL/dependencies/enchant-dev_1.5.0-2_win32.zip"
#set_default LIBSOUP_URL "$GNOME_WIN32_URL/libsoup/2.4/libsoup-2.4.0.zip"
#set_default LIBSOUP_DEV_URL "$GNOME_WIN32_URL/libsoup/2.4/libsoup-dev-2.4.0.zip"
set_default LIBSOUP_SRC_URL "$GNOME_MIRROR/sources/libsoup/2.30/libsoup-2.30.1.tar.bz2"
set_default ICU4C_URL "http://download.icu-project.org/files/icu4c/4.4.1/icu4c-4_4_1-Win32-msvc9.zip"
set_default ICU4C_SRC_URL "http://download.icu-project.org/files/icu4c/4.4.1/icu4c-4_4_1-src.tgz"
set_default ICU4C_DIR $GLOBAL_DIR\\icu-mingw32
set_default ICU4C_PATCH `pwd`/icu-crossmingw.patch

set_default SVN_URL "http://subversion.tigris.org/files/documents/15/47914/svn-win32-1.6.6.zip"
set_default SVN_DIR $GLOBAL_DIR\\svn

# OFX import in gnucash and ofx directconnect support for aqbanking
set_default OPENSP_URL "$SF_MIRROR/openjade/OpenSP-1.5.2.tar.gz"
set_default OPENSP_DIR $GLOBAL_DIR\\opensp
set_default OPENSP_PATCH `pwd`/opensp-1.5.2-patch.diff

set_default LIBOFX_URL "$SF_MIRROR/libofx/libofx-0.8.3.tar.gz"
set_default LIBOFX_DIR $GLOBAL_DIR\\libofx
set_default LIBOFX_PATCH `pwd`/libofx-0.8.3-patch.diff

## online banking: gwenhywfar+aqbanking
set_default AQBANKING5 yes
# If sticking to aqbanking4, make sure to enable the QtCore4.dll et al
# in gnucash.iss.in again because in aqbanking4 those DLLs are still
# needed.

if [ "$AQBANKING5" = "yes" ]; then
    set_default GWENHYWFAR_URL "http://www2.aquamaniac.de/sites/download/download.php?package=01&release=54&file=01&dummy=gwenhywfar-4.0.1.tar.gz"
else
    set_default GWENHYWFAR_URL "http://www2.aquamaniac.de/sites/download/download.php?package=01&release=31&file=01&dummy=gwenhywfar-3.11.3.tar.gz"
    #set_default GWENHYWFAR_PATCH `pwd`/gwenhywfar-3.11.1-patch.diff
fi
set_default GWENHYWFAR_DIR $GLOBAL_DIR\\gwenhywfar

set_default KTOBLZCHECK_URL "$SF_MIRROR/ktoblzcheck/ktoblzcheck-1.24.tar.gz"
# ktoblzcheck is being installed into GWENHYWFAR_DIR

if [ "$AQBANKING5" = "yes" ]; then
    set_default AQBANKING_URL "http://www2.aquamaniac.de/sites/download/download.php?package=03&release=74&file=01&dummy=aqbanking-5.0.1.tar.gz"
    set_default AQBANKING_WITH_QT no
else
    set_default AQBANKING_URL "http://www2.aquamaniac.de/sites/download/download.php?package=03&release=49&file=01&dummy=aqbanking-4.2.3.tar.gz"
    #set_default AQBANKING_PATCH `pwd`/aqbanking-4.1.6-patch.diff
    set_default AQBANKING_WITH_QT yes
fi
set_default AQBANKING_DIR $GLOBAL_DIR\\aqbanking
# If set to yes, download Qt from http://www.trolltech.com/developer/downloads/qt/windows,
# install it and set QTDIR in custom.sh, like "QTDIR=/c/Qt/4.2.3".
set_default QT_WIN_SRC_URL "ftp://ftp.qt.nokia.com/qt/source/qt-all-opensource-src-4.5.3.zip"

set_default SQLITE3_URL "http://sqlite.org/sqlite-amalgamation-3.6.1.tar.gz"
set_default SQLITE3_DIR $GLOBAL_DIR\\sqlite3
set_default MYSQL_LIB_URL "http://mirror.csclub.uwaterloo.ca/mysql/Downloads/Connector-C/mysql-connector-c-noinstall-6.0.1-win32.zip"
set_default MYSQL_LIB_DIR $GLOBAL_DIR\\mysql
set_default LIBMYSQL_DEF `pwd`/libmysql.def
set_default PGSQL_LIB_URL "$SF_MIRROR/gnucash/pgsql-win32-2.tar.gz"
set_default PGSQL_DIR $GLOBAL_DIR\\pgsql
set_default LIBDBI_URL "$SF_MIRROR/libdbi/libdbi-0.8.3.tar.gz"
set_default LIBDBI_DIR $GLOBAL_DIR\\libdbi
set_default LIBDBI_PATCH `pwd`/libdbi-0.8.3.patch
set_default LIBDBI_PATCH2 `pwd`/libdbi-dbd_helper.c.patch
set_default LIBDBI_DRIVERS_URL "$SF_MIRROR/libdbi-drivers/libdbi-drivers-0.8.3-1.tar.gz"
set_default LIBDBI_DRIVERS_DIR $GLOBAL_DIR\\libdbi-drivers
set_default LIBDBI_DRIVERS_PATCH `pwd`/libdbi-drivers-dbd_sqlite3.c.patch
set_default LIBDBI_DRIVERS_PATCH2 `pwd`/libdbi-drivers-Makefile.in.patch
set_default LIBDBI_DRIVERS_PATCH3 `pwd`/libdbi-drivers-dbd_mysql.c.patch
set_default LIBDBI_DRIVERS_PATCH4 `pwd`/libdbi-drivers-dbd_pgsql.c.patch

set_default CMAKE_URL "http://www.cmake.org/files/v2.8/cmake-2.8.0-win32-x86.zip"
set_default CMAKE_DIR $GLOBAL_DIR\\cmake

set_default DOCBOOK_XSL_URL "$SF_MIRROR/docbook/docbook-xsl-1.72.0.zip"
set_default UPDATE_DOCS yes
set_default DOCS_REV "HEAD"
set_default DOCS_URL "http://svn.gnucash.org/repo/gnucash-docs/trunk"
set_default DOCS_DIR $GLOBAL_DIR\\gnucash-docs
set_default XSLTPROCFLAGS ""

set_default ISOCODES_URL "ftp://pkg-isocodes.alioth.debian.org/pub/pkg-isocodes/iso-codes-3.18.tar.bz2"
set_default ISOCODES_DIR $GLOBAL_DIR\\isocodes

### Local Variables: ***
### sh-basic-offset: 4 ***
### indent-tabs-mode: nil ***
### End: ***
