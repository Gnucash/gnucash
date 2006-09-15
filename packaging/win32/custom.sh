#!/bin/sh # for emacs
#
# all directories should be without spaces!

GLOBAL_DIR=c:\\soft
TMP_DIR=$GLOBAL_DIR\\tmp
DOWNLOAD_DIR=$GLOBAL_DIR\\downloads

REPOS_URL="http://svn.gnucash.org/repo/gnucash/trunk"
REPOS_DIR=$GLOBAL_DIR\\repos
GNUCASH_DIR=$GLOBAL_DIR\\gnucash

####
HOST_XCOMPILE=""
# For cross-compiling, uncomment the following lines:
#HOST_XCOMPILE="--host=mingw32"
#PKG_CONFIG_PATH="" # to avoid using the host's installed packages
####

MSYS_DIR=$GLOBAL_DIR\\msys

WGET_DIR=$GLOBAL_DIR\\wget
#WGET=

SF_MIRROR="http://heanet.dl.sourceforge.net/sourceforge"
GTK_MIRROR="ftp://ftp.gtk.org/pub"
GNOME_MIRROR="ftp.gnome.org/pub/gnome"

DTK_URL="$SF_MIRROR/mingw/msysDTK-1.0.1.exe"

MINGW_URL="$SF_MIRROR/mingw/MinGW-5.0.2.exe"
MINGW_DIR=$GLOBAL_DIR\\mingw

UNZIP_URL="$SF_MIRROR/gnuwin32/unzip-5.51-1.exe"
UNZIP_DIR=$GLOBAL_DIR\\unzip

REGEX_BIN_URL="$SF_MIRROR/gnuwin32/regex-0.12-bin.zip"
REGEX_LIB_URL="$SF_MIRROR/gnuwin32/regex-0.12-lib.zip"
REGEX_DIR=$GLOBAL_DIR\\regex

READLINE_BIN_URL="$SF_MIRROR/gnuwin32/readline-5.0-bin.zip"
READLINE_LIB_URL="$SF_MIRROR/gnuwin32/readline-5.0-lib.zip"
READLINE_DIR=$GLOBAL_DIR\\readline

INDENT_BIN_URL="$SF_MIRROR/gnuwin32/indent-2.2.9-bin.zip"
INDENT_DIR=$GLOBAL_DIR\\indent

ACTIVE_PERL_URL="http://downloads.activestate.com/ActivePerl/Windows/5.6/ActivePerl-5.6.1.638-MSWin32-x86.zip"
ACTIVE_PERL_DIR=$GLOBAL_DIR\\active-perl

GUILE_URL="http://ftp.gnu.org/pub/gnu/guile/guile-1.6.8.tar.gz"
SLIB_URL="http://swiss.csail.mit.edu/ftpdir/scm/slib3a3.zip"
GUILE_DIR=$GLOBAL_DIR\\guile

OPENSSL_URL="http://www.slproweb.com/download/Win32OpenSSL-v0.9.8b.exe"
OPENSSL_DIR=$GLOBAL_DIR\\openssl

PEXPORTS_URL="http://www.emmestech.com/software/cygwin/pexports-0.43/pexports-0.43.zip"
PEXPORTS_DIR=$GLOBAL_DIR\\pexports

LIBXML2_URL="http://www.zlatkovic.com/pub/libxml/libxml2-2.6.26.win32.zip"
LIBXML2_DIR=$GLOBAL_DIR\\gnome #avoid XML_FLAGS

EXPAT_URL="ftp://ftp.jclark.com/pub/xml/expat.zip"
EXPAT_DIR=$GLOBAL_DIR\\gnome #avoid EXPAT_FLAGS

GNOME_WIN32_URL="$GNOME_MIRROR/binaries/win32"
GETTEXT_URL="$GTK_MIRROR/glib/2.12/win32/dependencies/gettext-0.14.5.zip"
GETTEXT_DEV_URL="$GTK_MIRROR/glib/2.12/win32/dependencies/gettext-dev-0.14.5.zip"
LIBICONV_URL="$GTK_MIRROR/glib/2.12/win32/dependencies/libiconv-1.9.1.bin.woe32.zip"
GLIB_URL="$GNOME_WIN32_URL/glib/2.12/glib-2.12.3.zip"
GLIB_DEV_URL="$GNOME_WIN32_URL/glib/2.12/glib-dev-2.12.3.zip"
LIBJPEG_URL="$GNOME_MIRROR/platform/2.13/2.13.92/win32/dependencies/libjpeg-6b-4.zip"
LIBPNG_URL="$GTK_MIRROR/gtk/v2.10/win32/dependencies/libpng-1.2.8-bin.zip"
ZLIB_URL="$GTK_MIRROR/gtk/v2.10/win32/dependencies/zlib123-dll.zip"
PKG_CONFIG_URL="$GTK_MIRROR/gtk/v2.10/win32/dependencies/pkg-config-0.20.zip"
CAIRO_URL="$GTK_MIRROR/gtk/v2.8/win32/cairo-1.2.4.zip"
CAIRO_DEV_URL="$GTK_MIRROR/gtk/v2.8/win32/cairo-dev-1.2.4.zip"
FONTCONFIG_URL="$GTK_MIRROR/gtk/v2.8/win32/dependencies/fontconfig-2.2.2-20040412.zip"
FONTCONFIG_DEV_URL="$GTK_MIRROR/gtk/v2.8/win32/dependencies/fontconfig-dev-2.2.2-20040412.zip"
FREETYPE_URL="$GTK_MIRROR/gtk/v2.8/win32/dependencies/freetype-2.1.10.zip"
FREETYPE_DEV_URL="$SF_MIRROR/gnuwin32/freetype-2.1.10-lib.zip"
ATK_URL="$GNOME_WIN32_URL/atk/1.12/atk-1.12.2.zip"
ATK_DEV_URL="$GNOME_WIN32_URL/atk/1.12/atk-dev-1.12.2.zip"
PANGO_URL="$GNOME_WIN32_URL/pango/1.14/pango-1.14.3.zip"
PANGO_DEV_URL="$GNOME_WIN32_URL/pango/1.14/pango-dev-1.14.3.zip"
LIBART_LGPL_URL="$GNOME_WIN32_URL/libart_lgpl/2.3/libart_lgpl-2.3.17.zip"
LIBART_LGPL_DEV_URL="$GNOME_WIN32_URL/libart_lgpl/2.3/libart_lgpl-dev-2.3.17.zip"
GTK_URL="$GNOME_WIN32_URL/gtk+/2.10/gtk+-2.10.3.zip"
GTK_DEV_URL="$GNOME_WIN32_URL/gtk+/2.10/gtk+-dev-2.10.3.zip"
INTLTOOL_URL="$GNOME_WIN32_URL/intltool/0.35/intltool-0.35.0.zip"
ORBIT2_URL="$GNOME_WIN32_URL/ORBit2/2.14/ORBit2-2.14.2.zip"
ORBIT2_DEV_URL="$GNOME_WIN32_URL/ORBit2/2.14/ORBit2-dev-2.14.2.zip"
GAIL_URL="$GNOME_WIN32_URL/gail/1.9/gail-1.9.2.zip"
GAIL_DEV_URL="$GNOME_WIN32_URL/gail/1.9/gail-dev-1.9.2.zip"
POPT_URL="$GNOME_MIRROR/platform/2.13/2.13.92/win32/dependencies/popt-1.10.2-tml-20050828.zip"
POPT_DEV_URL="$GNOME_MIRROR/platform/2.13/2.13.92/win32/dependencies/popt-dev-1.10.2-tml-20050828.zip"
GCONF_URL="$GNOME_WIN32_URL/GConf/2.14/GConf-2.14.0.zip"
GCONF_DEV_URL="$GNOME_WIN32_URL/GConf/2.14/GConf-dev-2.14.0.zip"
LIBBONOBO_URL="$GNOME_WIN32_URL/libbonobo/2.16/libbonobo-2.16.0.zip"
LIBBONOBO_DEV_URL="$GNOME_WIN32_URL/libbonobo/2.16/libbonobo-dev-2.16.0.zip"
GNOME_VFS_URL="$GNOME_WIN32_URL/gnome-vfs/2.14/gnome-vfs-2.14.2.zip"
GNOME_VFS_DEV_URL="$GNOME_WIN32_URL/gnome-vfs/2.14/gnome-vfs-dev-2.14.2.zip"
LIBGNOME_URL="$GNOME_WIN32_URL/libgnome/2.16/libgnome-2.16.0.zip"
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
GTKHTML_URL="$GNOME_WIN32_URL/gtkhtml/3.12/gtkhtml-3.12.0.zip"
GTKHTML_DEV_URL="$GNOME_WIN32_URL/gtkhtml/3.12/gtkhtml-dev-3.12.0.zip"
GNOME_DIR=$GLOBAL_DIR\\gnome

GWRAP_URL="http://download.savannah.gnu.org/releases/g-wrap/g-wrap-1.9.6.tar.gz"
GWRAP_DIR=$GLOBAL_DIR\\g-wrap

AUTOCONF_URL="http://ftp.gnu.org/gnu/autoconf/autoconf-2.60.tar.bz2"
AUTOMAKE_URL="http://ftp.gnu.org/gnu/automake/automake-1.9.6.tar.bz2"
LIBTOOL_URL="http://ftp.gnu.org/gnu/libtool/libtool-1.5.22.tar.gz"
AUTOTOOLS_DIR=$GLOBAL_DIR\\autotools

LIBGSF_URL="$GNOME_MIRROR/sources/libgsf/1.14/libgsf-1.14.1.tar.bz2"
LIBGSF_DIR=$GLOBAL_DIR\\libgsf

GOFFICE_URL="$GNOME_MIRROR/sources/goffice/0.3/goffice-0.3.0.tar.bz2"
GOFFICE_DIR=$GLOBAL_DIR\\goffice
GOFFICE_PATCH=`pwd`/goffice-0.3.0-patch.diff

GLADE_URL="$GNOME_MIRROR/sources/glade3/3.0/glade3-3.0.1.tar.bz2"
GLADE_DIR=$GLOBAL_DIR\\glade

SVN_URL="http://subversion.tigris.org/files/documents/15/32856/svn-1.3.2-setup.exe"
SVN_DIR=$GLOBAL_DIR\\svn

# There is no reason to ever need to comment these out!
# * commented out glade, as it is not needed to run gnucash
add_step inst_wget
add_step inst_dtk
add_step inst_mingw
add_step inst_unzip
add_step inst_regex
add_step inst_readline
add_step inst_indent
add_step inst_active_perl
add_step inst_guile
add_step inst_openssl
add_step inst_pexports
add_step inst_libxml2
add_step inst_expat
add_step inst_gnome
add_step inst_gwrap
add_step inst_autotools
add_step inst_libgsf
add_step inst_goffice
#add_step inst_glade
add_step inst_svn
add_step svn_up
add_step inst_gnucash
