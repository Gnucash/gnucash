#!/bin/sh # for emacs
#
# all directories should be without spaces!

GLOBAL_DIR=c:\\soft
TMP_DIR=$GLOBAL_DIR\\tmp
DOWNLOAD_DIR=$GLOBAL_DIR\\downloads

REPOS_URL="http://svn.gnucash.org/repo/gnucash/trunk"
REPOS_DIR=$GLOBAL_DIR\\repos
GNUCASH_DIR=$GLOBAL_DIR\\gnucash

MSYS_DIR=$GLOBAL_DIR\\msys

WGET_DIR=$GLOBAL_DIR\\wget
#WGET=

SF_MIRROR="http://heanet.dl.sourceforge.net/sourceforge"
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

GUILE_URL="http://ftp.gnu.org/pub/gnu/guile/guile-1.6.8.tar.gz"
SLIB_URL="http://swiss.csail.mit.edu/ftpdir/scm/slib3a3.zip"
GUILE_DIR=$GLOBAL_DIR\\guile

GWRAP_URL="http://download.savannah.gnu.org/releases/g-wrap/g-wrap-1.9.6.tar.gz"
GWRAP_DIR=$GLOBAL_DIR\\g-wrap

GLADE_URL="$SF_MIRROR/gladewin32/gtk-win32-devel-2.8.18-rc1.exe"
GLADE_DIR=$GLOBAL_DIR\\glade

GNOME_PLATFORM_URL="$GNOME_MIRROR/platform/2.14/2.14.3/win32"
GNOME_DESKTOP_URL="$GNOME_MIRROR/desktop/2.14/2.14.2/win32"
INTLTOOL_URL="$GNOME_PLATFORM_URL/intltool-0.35.0.zip"
GCONF_URL="$GNOME_PLATFORM_URL/GConf-2.14.0.zip"
GCONF_DEV_URL="$GNOME_PLATFORM_URL/GConf-dev-2.14.0.zip"
ORBIT2_URL="$GNOME_PLATFORM_URL/ORBit2-2.14.2.zip"
ORBIT2_DEV_URL="$GNOME_PLATFORM_URL/ORBit2-dev-2.14.2.zip"
LIBBONOBO_URL="$GNOME_PLATFORM_URL/libbonobo-2.14.0-20060619.zip"
LIBBONOBO_DEV_URL="$GNOME_PLATFORM_URL/libbonobo-dev-2.14.0-20060619.zip"
GNOME_VFS_URL="$GNOME_PLATFORM_URL/gnome-vfs-2.14.2.zip"
GNOME_VFS_DEV_URL="$GNOME_PLATFORM_URL/gnome-vfs-dev-2.14.2.zip"
LIBGNOME_URL="$GNOME_PLATFORM_URL/libgnome-2.14.1-20060613.zip"
LIBGNOME_DEV_URL="$GNOME_PLATFORM_URL/libgnome-dev-2.14.1-20060613.zip"
LIBGNOMECANVAS_URL="$GNOME_PLATFORM_URL/libgnomecanvas-2.14.0.zip"
LIBGNOMECANVAS_DEV_URL="$GNOME_PLATFORM_URL/libgnomecanvas-dev-2.14.0.zip"
LIBBONOBOUI_URL="$GNOME_PLATFORM_URL/libbonoboui-2.14.0.zip"
LIBBONOBOUI_DEV_URL="$GNOME_PLATFORM_URL/libbonoboui-dev-2.14.0.zip"
LIBGNOMEUI_URL="$GNOME_PLATFORM_URL/libgnomeui-2.14.1.zip"
LIBGNOMEUI_DEV_URL="$GNOME_PLATFORM_URL/libgnomeui-dev-2.14.1.zip"
LIBGNOMEPRINT_URL="$GNOME_DESKTOP_URL/libgnomeprint-2.12.1.zip"
LIBGNOMEPRINT_DEV_URL="$GNOME_DESKTOP_URL/libgnomeprint-dev-2.12.1.zip"
LIBGNOMEPRINTUI_URL="$GNOME_DESKTOP_URL/libgnomeprintui-2.12.1.zip"
LIBGNOMEPRINTUI_DEV_URL="$GNOME_DESKTOP_URL/libgnomeprintui-dev-2.12.1.zip"
GTKHTML_URL="$GNOME_DESKTOP_URL/gtkhtml-3.10.2.zip"
GTKHTML_DEV_URL="$GNOME_DESKTOP_URL/gtkhtml-dev-3.10.2.zip"
GNOME_DIR=$GLOBAL_DIR\\gnome

AUTOCONF_URL="http://ftp.gnu.org/gnu/autoconf/autoconf-2.60.tar.bz2"
AUTOMAKE_URL="http://ftp.gnu.org/gnu/automake/automake-1.9.6.tar.bz2"
LIBTOOL_URL="http://ftp.gnu.org/gnu/libtool/libtool-1.5.22.tar.gz"
AUTOTOOLS_DIR=$GLOBAL_DIR\\autotools

SVN_URL="http://subversion.tigris.org/files/documents/15/32856/svn-1.3.2-setup.exe"
SVN_DIR=$GLOBAL_DIR\\svn

# There is no reason to ever need to comment these out!
add_step inst_wget
add_step inst_dtk
add_step inst_mingw
add_step inst_unzip
add_step inst_regex
add_step inst_readline
add_step inst_indent
add_step inst_guile
add_step inst_glade
add_step inst_gwrap
add_step inst_gnome
add_step inst_autotools
add_step inst_svn
add_step svn_up
add_step inst_gnucash
