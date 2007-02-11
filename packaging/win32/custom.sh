#!/bin/sh # for emacs
#
# all directories should be without spaces!

GLOBAL_DIR=c:\\soft
TMP_DIR=$GLOBAL_DIR\\tmp
DOWNLOAD_DIR=$GLOBAL_DIR\\downloads

GNUCASH_DIR=$GLOBAL_DIR\\gnucash
REPOS_URL="http://svn.gnucash.org/repo/gnucash/trunk"
REPOS_DIR=$GNUCASH_DIR\\repos
# keep this pointing from BUILD_DIR to REPOS_DIR
REL_REPOS_DIR=..\\repos
BUILD_DIR=$GNUCASH_DIR\\build
INSTALL_DIR=$GNUCASH_DIR\\inst

####
cross_compile="no"
# For cross-compiling, uncomment the following line:
#cross_compile="yes"

if test "x$cross_compile" != xyes ; then
    LIBTOOLIZE=libtoolize
    HOST_XCOMPILE=""
    TARGET_XCOMPILE=""
    LD=ld
    CC=gcc
    DLLTOOL=dlltool
else
    # Insert your cross-compiler mingw32 bin-directories here
    PATH=$GLOBAL_DIR/bin:$GLOBAL_DIR/mingw32/bin:$PATH
    LIBTOOLIZE=$GLOBAL_DIR/autotools/bin/libtoolize
    PKG_CONFIG_PATH="" # to avoid using the host's installed packages
    HOST_XCOMPILE="--host=mingw32"
    TARGET_XCOMPILE="--target=mingw32"
    LD=mingw32-ld
    CC=mingw32-gcc
    DLLTOOL=mingw32-dlltool
fi
####

# If "yes", build without optimizations (-O0) and ease debugging
DISABLE_OPTIMIZATIONS=no

MSYS_DIR=$GLOBAL_DIR\\msys

# tools here means binaries runnable without other DLLs or data files
TOOLS_DIR=$GLOBAL_DIR\\tools
WGET_DIR=$TOOLS_DIR
#WGET=

SF_MIRROR="http://heanet.dl.sourceforge.net/sourceforge"
GTK_MIRROR="ftp.gtk.org/pub"
GNOME_MIRROR="ftp.gnome.org/pub/gnome"
GNOME_WIN32_URL="$GNOME_MIRROR/binaries/win32"
GNOME_WIN32_DEPS_URL="$GNOME_WIN32_URL/dependencies"

DTK_URL="$SF_MIRROR/mingw/msysDTK-1.0.1.exe"

MINGW_URL="$SF_MIRROR/mingw/MinGW-5.1.0.exe"
MINGW_DIR=$GLOBAL_DIR\\mingw

UNZIP_URL="$SF_MIRROR/gnuwin32/unzip-5.51-1.exe"
UNZIP_DIR=$TOOLS_DIR

# do not use regex-gnu or regex-spencer v3.8.g3, see bug #382852
REGEX_URL="$GNOME_WIN32_DEPS_URL/libgnurx-2.5.zip"
REGEX_DEV_URL="$GNOME_WIN32_DEPS_URL/libgnurx-dev-2.5.zip"
REGEX_DIR=$GLOBAL_DIR\\regex

READLINE_BIN_URL="$SF_MIRROR/gnuwin32/readline-5.0-bin.zip"
READLINE_LIB_URL="$SF_MIRROR/gnuwin32/readline-5.0-lib.zip"
READLINE_DIR=$GLOBAL_DIR\\readline

ACTIVE_PERL_URL="http://downloads.activestate.com/ActivePerl/Windows/5.6/ActivePerl-5.6.1.638-MSWin32-x86.zip"
ACTIVE_PERL_DIR=$GLOBAL_DIR\\active-perl

AUTOCONF_URL="http://ftp.gnu.org/gnu/autoconf/autoconf-2.60.tar.bz2"
AUTOMAKE_URL="http://ftp.gnu.org/gnu/automake/automake-1.9.6.tar.bz2"
LIBTOOL_URL="http://ftp.gnu.org/gnu/libtool/libtool-1.5.22.tar.gz"
AUTOTOOLS_DIR=$GLOBAL_DIR\\autotools

GUILE_URL="http://ftp.gnu.org/pub/gnu/guile/guile-1.6.8.tar.gz"
SLIB_URL="http://swiss.csail.mit.edu/ftpdir/scm/OLD/slib3a3.zip"
GUILE_DIR=$GLOBAL_DIR\\guile

OPENSSL_BIN_URL="$SF_MIRROR/gnuwin32/openssl-0.9.7c-bin.zip"
OPENSSL_LIB_URL="$SF_MIRROR/gnuwin32/openssl-0.9.7c-lib.zip"
OPENSSL_DIR=$GLOBAL_DIR\\openssl

PEXPORTS_URL="http://www.emmestech.com/software/cygwin/pexports-0.43/pexports-0.43.zip"
PEXPORTS_DIR=$TOOLS_DIR

EXETYPE_SCRIPT=`pwd`/exetype.pl
EXETYPE_DIR=$TOOLS_DIR

LIBXML2_URL="http://www.zlatkovic.com/pub/libxml/libxml2-2.6.27.win32.zip"
LIBXML2_DIR=$GLOBAL_DIR\\gnome #avoid XML_FLAGS

GETTEXT_URL="$GNOME_WIN32_DEPS_URL/gettext-0.14.5.zip"
GETTEXT_DEV_URL="$GNOME_WIN32_DEPS_URL/gettext-dev-0.14.5.zip"
LIBICONV_URL="$GNOME_WIN32_DEPS_URL/libiconv-1.9.1.bin.woe32.zip"
GLIB_URL="$GNOME_WIN32_URL/glib/2.12/glib-2.12.9.zip"
GLIB_DEV_URL="$GNOME_WIN32_URL/glib/2.12/glib-dev-2.12.9.zip"
LIBJPEG_URL="$GNOME_WIN32_DEPS_URL/libjpeg-6b-4.zip"
LIBPNG_URL="$GTK_MIRROR/gtk/v2.10/win32/dependencies/libpng-1.2.8-bin.zip"
ZLIB_URL="$GTK_MIRROR/gtk/v2.10/win32/dependencies/zlib123-dll.zip"
PKG_CONFIG_URL="$GNOME_WIN32_DEPS_URL/pkg-config-0.20.zip"
CAIRO_URL="$GNOME_WIN32_DEPS_URL/cairo-1.2.6.zip"
CAIRO_DEV_URL="$GNOME_WIN32_DEPS_URL/cairo-dev-1.2.6.zip"
EXPAT_URL="$GNOME_WIN32_DEPS_URL/expat-2.0.0.zip"
FONTCONFIG_URL="$GNOME_WIN32_DEPS_URL/fontconfig-2.3.2-tml-20060825.zip"
FONTCONFIG_DEV_URL="$GNOME_WIN32_DEPS_URL/fontconfig-dev-2.3.2-tml-20060825.zip"
FREETYPE_URL="$GNOME_WIN32_DEPS_URL/freetype-2.2.1.zip"
FREETYPE_DEV_URL="$GNOME_WIN32_DEPS_URL/freetype-dev-2.2.1.zip"
ATK_URL="$GNOME_WIN32_URL/atk/1.12/atk-1.12.3.zip"
ATK_DEV_URL="$GNOME_WIN32_URL/atk/1.12/atk-dev-1.12.3.zip"
PANGO_URL="$GNOME_WIN32_URL/pango/1.14/pango-1.14.9.zip"
PANGO_DEV_URL="$GNOME_WIN32_URL/pango/1.14/pango-dev-1.14.9.zip"
LIBART_LGPL_URL="$GNOME_WIN32_URL/libart_lgpl/2.3/libart_lgpl-2.3.17.zip"
LIBART_LGPL_DEV_URL="$GNOME_WIN32_URL/libart_lgpl/2.3/libart_lgpl-dev-2.3.17.zip"
GTK_URL="$GNOME_WIN32_URL/gtk+/2.10/gtk+-2.10.9.zip"
GTK_DEV_URL="$GNOME_WIN32_URL/gtk+/2.10/gtk+-dev-2.10.9.zip"
INTLTOOL_URL="$GNOME_WIN32_URL/intltool/0.35/intltool-0.35.0.zip"
ORBIT2_URL="$GNOME_WIN32_URL/ORBit2/2.13/ORBit2-2.13.3.zip"
ORBIT2_DEV_URL="$GNOME_WIN32_URL/ORBit2/2.13/ORBit2-dev-2.13.3.zip"
GAIL_URL="$GNOME_WIN32_URL/gail/1.9/gail-1.9.3.zip"
GAIL_DEV_URL="$GNOME_WIN32_URL/gail/1.9/gail-dev-1.9.3.zip"
POPT_URL="$GNOME_WIN32_DEPS_URL/popt-1.10.2-tml-20050828.zip"
POPT_DEV_URL="$GNOME_WIN32_DEPS_URL/popt-dev-1.10.2-tml-20050828.zip"
GCONF_URL="$GNOME_WIN32_URL/GConf/2.14/GConf-2.14.0.zip"
GCONF_DEV_URL="$GNOME_WIN32_URL/GConf/2.14/GConf-dev-2.14.0.zip"
LIBBONOBO_URL="$GNOME_WIN32_URL/libbonobo/2.16/libbonobo-2.16.0.zip"
LIBBONOBO_DEV_URL="$GNOME_WIN32_URL/libbonobo/2.16/libbonobo-dev-2.16.0.zip"
GNOME_VFS_URL="$GNOME_WIN32_URL/gnome-vfs/2.14/gnome-vfs-2.14.2-no-openssl.zip"
GNOME_VFS_DEV_URL="$GNOME_WIN32_URL/gnome-vfs/2.14/gnome-vfs-dev-2.14.2-no-openssl.zip"
LIBGNOME_URL="$GNOME_WIN32_URL/libgnome/2.16/libgnome-2.16.0-1.zip"
LIBGNOME_DEV_URL="$GNOME_WIN32_URL/libgnome/2.16/libgnome-dev-2.16.0.zip"
LIBGNOMECANVAS_URL="$GNOME_WIN32_URL/libgnomecanvas/2.14/libgnomecanvas-2.14.0.zip"
LIBGNOMECANVAS_DEV_URL="$GNOME_WIN32_URL/libgnomecanvas/2.14/libgnomecanvas-dev-2.14.0.zip"
LIBBONOBOUI_URL="$GNOME_WIN32_URL/libbonoboui/2.16/libbonoboui-2.16.0.zip"
LIBBONOBOUI_DEV_URL="$GNOME_WIN32_URL/libbonoboui/2.16/libbonoboui-dev-2.16.0.zip"
LIBGNOMEUI_URL="$GNOME_WIN32_URL/libgnomeui/2.16/libgnomeui-2.16.0.zip"
LIBGNOMEUI_DEV_URL="$GNOME_WIN32_URL/libgnomeui/2.16/libgnomeui-dev-2.16.0.zip"
LIBGLADE_URL="$GNOME_WIN32_URL/libglade/2.6/libglade-2.6.0.zip"
LIBGLADE_DEV_URL="$GNOME_WIN32_URL/libglade/2.6/libglade-dev-2.6.0.zip"
LIBGNOMEPRINT_URL="$GNOME_WIN32_URL/libgnomeprint/2.12/libgnomeprint-2.12.1.zip"
LIBGNOMEPRINT_DEV_URL="$GNOME_WIN32_URL/libgnomeprint/2.12/libgnomeprint-dev-2.12.1.zip"
LIBGNOMEPRINTUI_URL="$GNOME_WIN32_URL/libgnomeprintui/2.12/libgnomeprintui-2.12.1.zip"
LIBGNOMEPRINTUI_DEV_URL="$GNOME_WIN32_URL/libgnomeprintui/2.12/libgnomeprintui-dev-2.12.1.zip"
GTKHTML_URL="$GNOME_WIN32_URL/gtkhtml/3.12/gtkhtml-3.12.2.zip"
GTKHTML_DEV_URL="$GNOME_WIN32_URL/gtkhtml/3.12/gtkhtml-dev-3.12.2.zip"
GNOME_DIR=$GLOBAL_DIR\\gnome

SWIG_URL="$SF_MIRROR/swig/swigwin-1.3.31.zip"
SWIG_DIR=$GLOBAL_DIR\\swig

LIBGSF_URL="$GNOME_MIRROR/sources/libgsf/1.14/libgsf-1.14.3.tar.bz2"
LIBGSF_DIR=$GLOBAL_DIR\\libgsf

# do not update this if you are unwilling to hack gnc-html-graph-gog.c
GOFFICE_URL="$GNOME_MIRROR/sources/goffice/0.3/goffice-0.3.0.tar.bz2"
GOFFICE_DIR=$GLOBAL_DIR\\goffice
GOFFICE_PATCH=`pwd`/goffice-0.3.0-patch.diff

GLADE_URL="$GNOME_MIRROR/sources/glade3/3.0/glade3-3.1.2.tar.bz2"
GLADE_DIR=$GLOBAL_DIR\\glade

INNO_URL="http://files.jrsoftware.org/is/5/isetup-5.1.9.exe"
INNO_DIR=$GLOBAL_DIR\\inno

SVN_URL="http://subversion.tigris.org/files/documents/15/35379/svn-1.4.2-setup.exe"
SVN_DIR=$GLOBAL_DIR\\svn

# OFX import in gnucash and ofx directconnect support for aqbanking
OPENSP_URL="$SF_MIRROR/openjade/OpenSP-1.5.2.tar.gz"
OPENSP_DIR=$GLOBAL_DIR\\opensp
OPENSP_PATCH=`pwd`/opensp-1.5.2-patch.diff

LIBOFX_URL="$SF_MIRROR/libofx/libofx-0.8.3.tar.gz"
LIBOFX_DIR=$GLOBAL_DIR\\libofx
LIBOFX_PATCH=`pwd`/libofx-0.8.3-patch.diff

## online banking: gwenhywfar+aqbanking
GWENHYWFAR_URL="$SF_MIRROR/gwenhywfar/gwenhywfar-2.5.3.tar.gz"
GWENHYWFAR_DIR=$GLOBAL_DIR\\gwenhywfar

KTOBLZCHECK="$SF_MIRROR/ktoblzcheck/ktoblzcheck-1.12.tar.gz"
# ktoblzcheck is being installed into GWENHYWFAR_DIR

AQBANKING_URL="$SF_MIRROR/aqbanking/aqbanking-2.2.7.tar.gz"
AQBANKING_DIR=$GLOBAL_DIR\\aqbanking
##

# There is no reason to ever need to comment these out!
# * commented out glade, as it is not needed to run gnucash
if test x$cross_compile != xyes ; then
 add_step inst_wget
 add_step inst_dtk
 add_step inst_mingw
 add_step inst_unzip
fi
add_step inst_regex
add_step inst_readline
if test x$cross_compile != xyes ; then
 add_step inst_active_perl
fi
add_step inst_autotools
add_step inst_guile
if test x$cross_compile != xyes ; then
 add_step inst_openssl
fi
add_step inst_pexports
add_step inst_exetype
add_step inst_libxml2
add_step inst_gnome
add_step inst_swig
add_step inst_libgsf
add_step inst_goffice
#add_step inst_glade
add_step inst_opensp
add_step inst_libofx
## Online banking:
add_step inst_gwenhywfar
#add_step inst_ktoblzcheck
add_step inst_aqbanking
##
if test x$cross_compile != xyes ; then
 add_step inst_inno
fi
add_step inst_svn
add_step svn_up
add_step inst_gnucash
