#!/bin/sh # for emacs
#
# Instead of just editing this file, it is recommended to create a file `custom.sh'
# in the same directory that will be read in at the beginning of this script.
#
# You can use the full power of bash 2.04 scripting.  In particular, you can
# set any variable mentioned here to something non-empty and it will not be
# overridden later.  However, you must define all variables you make use of
# yourself.  Expressions registered with late_eval are executed at the end of
# the script.
#
# Steps will be executed in the order they were added.  They can only be added
# at most once if they have not been blocked already (adding implies blocking).
# To add a custom step <s>, just implement "function <s>()".  Keep in mind that
# blocking or reordering may render install.sh & friends non-functional.
#
# Note: All directories must be without spaces!
#
# Here is an example custom.sh file:
#
# REPOS_URL="svn+ssh://<dev>@svn.gnucash.org/repo/gnucash/trunk"
# SF_MIRROR="http://switch.dl.sourceforge.net/sourceforge"
# DISABLE_OPTIMIZATIONS=yes
# AQBANKING_WITH_QT=no
# GLOBAL_DIR=Z:\\mydir  # all directories will use this
# WGET_RATE=50k         #limit download bandwith to 50KB/s
# NO_SAVE_PROFILE=yes   # don't save env settings to /etc/profile.d
# late_eval 'INSTALL_DIR=$GNUCASH_DIR\\myinst'  # no need to define GNUCASH_DIR
# block_step inst_docs
# late_eval 'add_step greetings'
# function greetings() { setup Greetings; }


[ "$__SOURCED_DEFAULTS" ] && return
__SOURCED_DEFAULTS=1

[ -f "custom.sh" ] && . custom.sh || true

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


####
set_default LD ld
set_default CC gcc
set_default DLLTOOL dlltool
set_default RANLIB ranlib

# For cross-compiling, change this to "yes"
set_default CROSS_COMPILE "no"

if [ "$CROSS_COMPILE" = yes ]; then
    # Insert your cross-compiler mingw32 bin-directories here
    set_default HOST_XCOMPILE "--host=mingw32"
fi
####

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

set_default BINUTILS_URL "$SF_MIRROR/mingw/binutils-2.19-mingw32-bin.tar.gz"
set_default GCC_CORE_URL "$SF_MIRROR/mingw/gcc-core-3.4.5-20060117-3.tar.gz"
set_default GCC_GPP_URL "$SF_MIRROR/mingw/gcc-g++-3.4.5-20060117-3.tar.gz"
set_default MINGW_RT_URL "$SF_MIRROR/mingw/mingwrt-3.15.1-mingw32.tar.gz"
set_default W32API_URL "$SF_MIRROR/mingw/w32api-3.13-mingw32-dev.tar.gz"
set_default MINGW_MAKE_URL "$SF_MIRROR/mingw/mingw32-make-3.81-20080326-3.tar.gz"
set_default MINGW_DIR $GLOBAL_DIR\\mingw

set_default UNZIP_URL "$SF_MIRROR/gnuwin32/unzip-5.51-1.exe"
set_default UNZIP_DIR $TOOLS_DIR

# do not use regex-gnu or regex-spencer v3.8.g3, see bug #382852
set_default REGEX_URL "$GNOME_WIN32_DEPS_URL/libgnurx-2.5.zip"
set_default REGEX_DEV_URL "$GNOME_WIN32_DEPS_URL/libgnurx-dev-2.5.zip"
set_default REGEX_DIR $GLOBAL_DIR\\regex

set_default READLINE_BIN_URL "$SF_MIRROR/gnuwin32/readline-5.0-1-bin.zip"
set_default READLINE_LIB_URL "$SF_MIRROR/gnuwin32/readline-5.0-1-lib.zip"
set_default READLINE_DIR $GLOBAL_DIR\\readline

set_default ACTIVE_PERL_URL "http://downloads.activestate.com/ActivePerl/Windows/5.10/ActivePerl-5.10.0.1004-MSWin32-x86-287188.zip"
set_default ACTIVE_PERL_DIR $GLOBAL_DIR\\active-perl

set_default AUTOCONF_URL "http://ftp.gnu.org/gnu/autoconf/autoconf-2.63.tar.bz2"
set_default AUTOMAKE_URL "http://ftp.gnu.org/gnu/automake/automake-1.10.2.tar.bz2"
set_default LIBTOOL_URL "http://ftp.gnu.org/gnu/libtool/libtool-2.2.6a.tar.gz"
set_default AUTOTOOLS_DIR $GLOBAL_DIR\\autotools

set_default GMP_URL "ftp://ftp.gnu.org/gnu/gmp/gmp-4.2.4.tar.bz2"
set_default GMP_ABI 32
set_default GMP_DIR $GLOBAL_DIR\\gmp

set_default GUILE_URL "http://ftp.gnu.org/pub/gnu/guile/guile-1.6.8.tar.gz"
set_default SLIB_URL "http://swiss.csail.mit.edu/ftpdir/scm/OLD/slib3a3.zip"
set_default GUILE_DIR $GLOBAL_DIR\\guile

set_default OPENSSL_URL "http://www.openssl.org/source/openssl-0.9.8j.tar.gz"
set_default OPENSSL_DIR $GLOBAL_DIR\\openssl

set_default GNUTLS_URL "http://josefsson.org/gnutls4win/gnutls-2.7.3.zip"
set_default GNUTLS_DIR $GLOBAL_DIR\\gnutls

set_default MINGW_UTILS_URL "$SF_MIRROR/mingw/mingw-utils-0.3.tar.gz"
set_default MINGW_UTILS_DIR $TOOLS_DIR

set_default EXETYPE_SCRIPT `pwd`/exetype.pl
set_default EXETYPE_DIR $TOOLS_DIR

set_default LIBXSLT_URL "http://xmlsoft.org/sources/win32/oldreleases/libxslt-1.1.23+.win32.zip"
set_default LIBXSLT_DIR $GLOBAL_DIR\\libxslt

set_default LIBXML2_URL "$GNOME_WIN32_DEPS_URL/libxml2-2.6.27.zip"
set_default LIBXML2_DEV_URL "$GNOME_WIN32_DEPS_URL/libxml2-dev-2.6.27.zip"
set_default GETTEXT_RUNTIME_URL "$GNOME_WIN32_DEPS_URL/gettext-runtime-0.17-1.zip"
set_default GETTEXT_RUNTIME_DEV_URL "$GNOME_WIN32_DEPS_URL/gettext-runtime-dev-0.17-1.zip"
set_default GETTEXT_TOOLS_URL "$GNOME_WIN32_DEPS_URL/gettext-tools-0.17.zip"
set_default LIBICONV_URL "$GNOME_WIN32_DEPS_URL/libiconv-1.9.1.bin.woe32.zip"
set_default GLIB_URL "$GNOME_WIN32_URL/glib/2.18/glib_2.18.3-1_win32.zip"
set_default GLIB_DEV_URL "$GNOME_WIN32_URL/glib/2.18/glib-dev_2.18.3-1_win32.zip"
set_default LIBJPEG_URL "$GNOME_WIN32_DEPS_URL/libjpeg-6b-4.zip"
set_default LIBPNG_URL "$GNOME_WIN32_DEPS_URL/libpng_1.2.32-1_win32.zip"
set_default LIBPNG_DEV_URL "$GNOME_WIN32_DEPS_URL/libpng-dev_1.2.32-1_win32.zip"
set_default LIBTIFF_URL "$GNOME_WIN32_DEPS_URL/libtiff-3.8.2.zip"
set_default LIBTIFF_DEV_URL "$GNOME_WIN32_DEPS_URL/libtiff-dev-3.8.2.zip"
set_default ZLIB_URL "$GNOME_WIN32_DEPS_URL/zlib-1.2.3.zip"
set_default ZLIB_DEV_URL "$GNOME_WIN32_DEPS_URL/zlib-dev-1.2.3.zip"
set_default PKG_CONFIG_URL "$GNOME_WIN32_DEPS_URL/pkg-config-0.23.zip"
set_default CAIRO_URL "$GNOME_WIN32_DEPS_URL/cairo_1.8.6-1_win32.zip"
set_default CAIRO_DEV_URL "$GNOME_WIN32_DEPS_URL/cairo-dev_1.8.6-1_win32.zip"
set_default PIXMAN_URL "http://cairographics.org/releases/pixman-0.14.0.tar.gz"
set_default EXPAT_URL "$GNOME_WIN32_DEPS_URL/expat-2.0.0.zip"
set_default FONTCONFIG_URL "$GNOME_WIN32_DEPS_URL/fontconfig-2.4.2-tml-20071015.zip"
set_default FONTCONFIG_DEV_URL "$GNOME_WIN32_DEPS_URL/fontconfig-dev-2.4.2-tml-20071015.zip"
set_default FREETYPE_URL "$GNOME_WIN32_DEPS_URL/freetype-2.3.6.zip"
set_default FREETYPE_DEV_URL "$GNOME_WIN32_DEPS_URL/freetype-dev-2.3.6.zip"
set_default ATK_URL "$GNOME_WIN32_URL/atk/1.24/atk_1.24.0-1_win32.zip"
set_default ATK_DEV_URL "$GNOME_WIN32_URL/atk/1.24/atk-dev_1.24.0-1_win32.zip"
set_default PANGO_URL "$GNOME_WIN32_URL/pango/1.22/pango_1.22.2-1_win32.zip"
set_default PANGO_DEV_URL "$GNOME_WIN32_URL/pango/1.22/pango-dev_1.22.2-1_win32.zip"
set_default LIBART_LGPL_URL "$GNOME_WIN32_URL/libart_lgpl/2.3/libart_lgpl-2.3.20.zip"
set_default LIBART_LGPL_DEV_URL "$GNOME_WIN32_URL/libart_lgpl/2.3/libart_lgpl-dev-2.3.20.zip"
set_default GTK_URL "$GNOME_WIN32_URL/gtk+/2.14/gtk+_2.14.7-1_win32.zip"
set_default GTK_DEV_URL "$GNOME_WIN32_URL/gtk+/2.14/gtk+-dev_2.14.7-1_win32.zip"
set_default INTLTOOL_URL "$GNOME_WIN32_URL/intltool/0.40/intltool_0.40.4-1_win32.zip"
set_default ORBIT2_URL "$GNOME_WIN32_URL/ORBit2/2.14/ORBit2-2.14.13.zip"
set_default ORBIT2_DEV_URL "$GNOME_WIN32_URL/ORBit2/2.14/ORBit2-dev-2.14.13.zip"
set_default GAIL_URL "$GNOME_WIN32_URL/gail/1.22/gail-1.22.0.zip"
set_default GAIL_DEV_URL "$GNOME_WIN32_URL/gail/1.22/gail-dev-1.22.0.zip"
set_default POPT_URL "$GNOME_WIN32_DEPS_URL/popt-1.10.2-tml-20050828.zip"
set_default POPT_DEV_URL "$GNOME_WIN32_DEPS_URL/popt-dev-1.10.2-tml-20050828.zip"
set_default GCONF_URL "$GNOME_WIN32_URL/GConf/2.22/GConf_2.22.0-2_win32.zip"
set_default GCONF_DEV_URL "$GNOME_WIN32_URL/GConf/2.22/GConf-dev_2.22.0-2_win32.zip"
set_default LIBBONOBO_URL "$GNOME_WIN32_URL/libbonobo/2.24/libbonobo_2.24.0-1_win32.zip"
set_default LIBBONOBO_DEV_URL "$GNOME_WIN32_URL/libbonobo/2.24/libbonobo-dev_2.24.0-1_win32.zip"
# work-around #504261, GetVolumePathNamesForVolumeNameW missing on Win2k
set_default GNOME_VFS_URL "$GNOME_WIN32_URL/gnome-vfs/2.14/gnome-vfs-2.14.2-no-openssl.zip" 
set_default GNOME_VFS_DEV_URL "$GNOME_WIN32_URL/gnome-vfs/2.14/gnome-vfs-dev-2.14.2-no-openssl.zip" 
#set_default GNOME_VFS_URL "$GNOME_WIN32_URL/gnome-vfs/2.24/gnome-vfs_2.24.0-1_win32.zip"
#set_default GNOME_VFS_DEV_URL "$GNOME_WIN32_URL/gnome-vfs/2.24/gnome-vfs-dev_2.24.0-1_win32.zip"
set_default LIBGNOME_URL "$GNOME_WIN32_URL/libgnome/2.24/libgnome_2.24.1-1_win32.zip"
set_default LIBGNOME_DEV_URL "$GNOME_WIN32_URL/libgnome/2.24/libgnome-dev_2.24.1-1_win32.zip"
set_default LIBGNOMECANVAS_URL "$GNOME_WIN32_URL/libgnomecanvas/2.20/libgnomecanvas-2.20.1.zip"
set_default LIBGNOMECANVAS_DEV_URL "$GNOME_WIN32_URL/libgnomecanvas/2.20/libgnomecanvas-dev-2.20.1.zip"
set_default LIBBONOBOUI_URL "$GNOME_WIN32_URL/libbonoboui/2.24/libbonoboui_2.24.0-1_win32.zip"
set_default LIBBONOBOUI_DEV_URL "$GNOME_WIN32_URL/libbonoboui/2.24/libbonoboui-dev_2.24.0-1_win32.zip"
set_default LIBGNOMEUI_URL "$GNOME_WIN32_URL/libgnomeui/2.22/libgnomeui-2.22.1.zip"
set_default LIBGNOMEUI_DEV_URL "$GNOME_WIN32_URL/libgnomeui/2.22/libgnomeui-dev-2.22.1.zip"
set_default LIBGLADE_URL "$GNOME_WIN32_URL/libglade/2.6/libglade_2.6.3-1_win32.zip"
set_default LIBGLADE_DEV_URL "$GNOME_WIN32_URL/libglade/2.6/libglade-dev_2.6.3-1_win32.zip"
set_default GTKHTML_URL "$GNOME_WIN32_URL/gtkhtml/3.24/gtkhtml_3.24.2-1_win32.zip"
set_default GTKHTML_DEV_URL "$GNOME_WIN32_URL/gtkhtml/3.24/gtkhtml-dev_3.24.2-1_win32.zip"
set_default GTK_DOC_URL "$GNOME_MIRROR/sources/gtk-doc/1.11/gtk-doc-1.11.tar.bz2"
set_default GNOME_DIR $GLOBAL_DIR\\gnome

set_default SWIG_URL "$SF_MIRROR/swig/swigwin-1.3.36.zip"
set_default SWIG_DIR $GLOBAL_DIR\\swig

set_default PCRE_BIN_URL "$SF_MIRROR/gnuwin32/pcre-7.0-bin.zip"
set_default PCRE_LIB_URL "$SF_MIRROR/gnuwin32/pcre-7.0-lib.zip"
set_default PCRE_DIR $GLOBAL_DIR\\pcre

set_default LIBGSF_URL "$GNOME_MIRROR/sources/libgsf/1.14/libgsf-1.14.11.tar.bz2"
set_default LIBGSF_DIR $GLOBAL_DIR\\libgsf

set_default GOFFICE_URL "$GNOME_MIRROR/sources/goffice/0.7/goffice-0.7.2.tar.bz2"
set_default GOFFICE_DIR $GLOBAL_DIR\\goffice
set_default GOFFICE_PATCH `pwd`/goffice-0.7.2-patch.diff

set_default GLADE_URL "$GNOME_MIRROR/sources/glade3/3.0/glade3-3.1.2.tar.bz2"
set_default GLADE_DIR $GLOBAL_DIR\\glade

set_default INNO_URL "http://files.jrsoftware.org/is/5/isetup-5.1.12.exe"
set_default INNO_DIR $GLOBAL_DIR\\inno

set_default HH_URL "http://download.microsoft.com/download/OfficeXPProf/Install/4.71.1015.0/W98NT42KMe/EN-US/HTMLHELP.EXE"
set_default HH_DIR $GLOBAL_DIR\\hh

set_default WEBKIT_URL "http://www.gnucash.org/pub/gnucash/sources/unstable/2.3.x/webkit-1.1.5-win32.zip"
set_default WEBKIT_DIR $GLOBAL_DIR\\webkit-1.1.5

set_default SVN_URL "http://subversion.tigris.org/files/documents/15/35379/svn-1.4.2-setup.exe"
set_default SVN_DIR $GLOBAL_DIR\\svn

# OFX import in gnucash and ofx directconnect support for aqbanking
set_default OPENSP_URL "$SF_MIRROR/openjade/OpenSP-1.5.2.tar.gz"
set_default OPENSP_DIR $GLOBAL_DIR\\opensp
set_default OPENSP_PATCH `pwd`/opensp-1.5.2-patch.diff

set_default LIBOFX_URL "$SF_MIRROR/libofx/libofx-0.8.3.tar.gz"
set_default LIBOFX_DIR $GLOBAL_DIR\\libofx
set_default LIBOFX_PATCH `pwd`/libofx-0.8.3-patch.diff

## online banking: gwenhywfar+aqbanking
set_default AQBANKING3 yes

if [ "$AQBANKING3" != "yes" ]; then
    set_default GWENHYWFAR_URL "$SF_MIRROR/gwenhywfar/gwenhywfar-2.6.2.tar.gz"
else
    set_default GWENHYWFAR_URL "http://www2.aquamaniac.de/sites/download/download.php?package=01&release=17&file=01&dummy=gwenhywfar-3.6.0.tar.gz"
    set_default GWENHYWFAR_PATCH `pwd`/gwenhywfar-3.6.0-patch.diff
fi
set_default GWENHYWFAR_DIR $GLOBAL_DIR\\gwenhywfar

set_default KTOBLZCHECK_URL "$SF_MIRROR/ktoblzcheck/ktoblzcheck-1.20.tar.gz"
# ktoblzcheck is being installed into GWENHYWFAR_DIR

if [ "$AQBANKING3" != "yes" ]; then
    set_default AQBANKING_URL "$SF_MIRROR/aqbanking/aqbanking-2.3.3.tar.gz"
else
    set_default AQBANKING_URL "http://www2.aquamaniac.de/sites/download/download.php?package=03&release=35&file=01&dummy=aqbanking-3.8.3.tar.gz"
    set_default AQBANKING_PATCH `pwd`/aqbanking-3.8.3-patch.diff
fi
set_default AQBANKING_DIR $GLOBAL_DIR\\aqbanking
set_default AQBANKING_WITH_QT yes
# If set to yes, download Qt from http://www.trolltech.com/developer/downloads/qt/windows,
# install it and set QTDIR in custom.sh, like "QTDIR=/c/Qt/4.2.3".

set_default SQLITE3_URL "http://sqlite.org/sqlite-amalgamation-3.6.1.tar.gz"
set_default SQLITE3_DIR $GLOBAL_DIR\\sqlite3
set_default MYSQL_LIB_URL "http://mirror.csclub.uwaterloo.ca/mysql/Downloads/Connector-C/mysql-connector-c-noinstall-6.0.1-win32.zip"
set_default MYSQL_LIB_DIR $GLOBAL_DIR\\mysql
set_default LIBDBI_URL "http://downloads.sourceforge.net/libdbi/libdbi-0.8.3.tar.gz"
set_default LIBDBI_DIR $GLOBAL_DIR\\libdbi
set_default LIBDBI_PATCH `pwd`/libdbi-0.8.3.patch
set_default LIBDBI_DRIVERS_URL "http://downloads.sourceforge.net/libdbi-drivers/libdbi-drivers-0.8.3-1.tar.gz"
set_default LIBDBI_DRIVERS_DIR $GLOBAL_DIR\\libdbi-drivers
set_default LIBDBI_DRIVERS_PATCH `pwd`/libdbi-drivers-dbd_sqlite3.c.patch
set_default LIBDBI_DRIVERS_PATCH2 `pwd`/libdbi-drivers-Makefile.in.patch
set_default LIBDBI_DRIVERS_PATCH3 `pwd`/libdbi-drivers-dbd_mysql.c.patch

set_default DOCBOOK_XSL_URL "$SF_MIRROR/docbook/docbook-xsl-1.72.0.zip"
set_default UPDATE_DOCS yes
set_default DOCS_REV "HEAD"
set_default DOCS_URL "http://svn.gnucash.org/repo/gnucash-docs/trunk"
set_default DOCS_DIR $GLOBAL_DIR\\gnucash-docs
set_default XSLTPROCFLAGS ""
##

# There is no reason to ever need to comment these out!
# * commented out glade, as it is not needed to run gnucash
add_step prepare
if [ "$CROSS_COMPILE" != "yes" ]; then
 add_step inst_wget
 add_step inst_dtk
 add_step inst_unzip
fi
add_step inst_mingw
add_step inst_regex
add_step inst_readline
if [ "$CROSS_COMPILE" != "yes" ]; then
 add_step inst_active_perl
fi
add_step inst_autotools
if [ "$AQBANKING3" = "yes" ]; then
 add_step inst_gmp
fi
add_step inst_guile
if [ "$CROSS_COMPILE" != "yes" ]; then
 add_step inst_svn
 add_step inst_mingwutils
 if [ "$AQBANKING3" != "yes" ]; then
  add_step inst_openssl
 fi
fi
add_step inst_exetype
add_step inst_libxslt
add_step inst_gnome
if [ "$AQBANKING3" = "yes" ]; then
 add_step inst_gnutls
fi
add_step inst_swig
add_step inst_pcre
add_step inst_libgsf
if [ "$CROSS_COMPILE" != "yes" ]; then
 add_step inst_hh
fi
add_step inst_goffice
#add_step inst_glade
add_step inst_opensp
add_step inst_libofx
## Online banking:
add_step inst_gwenhywfar
add_step inst_ktoblzcheck
add_step inst_aqbanking
add_step inst_libdbi
add_step inst_webkit
##
if [ "$UPDATE_SOURCES" = "yes" ]; then
 add_step svn_up
fi
add_step inst_gnucash
add_step inst_docs
if [ "$CROSS_COMPILE" != "yes" ]; then
 add_step inst_inno
fi
add_step finish

# run commands registered with late_eval
eval_now
