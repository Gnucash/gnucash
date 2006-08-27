#!/bin/sh

set -e

function add_step() { steps=("${steps[@]}" "$@"); }
function qpushd() { pushd "$@" >/dev/null; }
function qpopd() { popd >/dev/null; }

# c:/dir/sub
function win_fs_path() {
    echo "$*" | sed 's,\\,/,g'
}

# /c/dir/sub
function unix_path() {
    echo "$*" | sed 's,^\([A-Za-z]\):,/\1,;s,\\,/,g'
}

qpushd "$(dirname $(unix_path "$0"))"
. custom.sh

SEPS_ACLOCAL_FLAGS=" "
SEPS_AUTOTOOLS_CPPFLAGS=" "
SEPS_AUTOTOOLS_LDFLAGS=" "
SEPS_GUILE_LOAD_PATH=";"
SEPS_PATH=":"
SEPS_PKG_CONFIG_PATH=":"
SEPS_READLINE_CPPFLAGS=" "
SEPS_READLINE_LDFLAGS=" "
SEPS_REGEX_CPPFLAGS=" "
SEPS_REGEX_LDFLAGS=" "
SEPS_SCHEME_LIBRARY_PATH=";"
ENV_VARS="\
ACLOCAL_FLAGS \
AUTOTOOLS_CPPFLAGS \
AUTOTOOLS_LDFLAGS \
GUILE_LOAD_PATH \
PATH \
PKG_CONFIG_PATH \
READLINE_CPPFLAGS \
READLINE_LDFLAGS \
REGEX_CPPFLAGS \
REGEX_LDFLAGS \
SCHEME_LIBRARY_PATH \
"

function setup() {
    echo
    echo "############################################################"
    echo "###  $*"
    echo "############################################################"
}

function die() {
    echo
    echo "!!! $* !!!"
    echo "!!! ABORTING !!!"
    exit -1
}

function quiet() { "$@" &>/dev/null; }
function add_to_env() {
    _SEP=`eval echo '"$'"SEPS_$2"'"'`
    _ENV=`eval echo '"$'"$2"'"'`
    _SED=`eval echo '"s#.*'"${_SEP}$1${_SEP}"'.*##"'`
    _TEST=`echo "${_SEP}${_ENV}${_SEP}" | sed "${_SED}"`
    if [ "$_TEST" ]; then
	if [ "$_ENV" ]; then
	    eval "$2_ADDS"'="'"$1${_SEP}"'$'"$2_ADDS"'"'
	else
	    eval "$2_ADDS"'="'"$1"'"'
	fi
	eval "$2"'="$'"$2_ADDS"'$'"$2_BASE"'"'
    fi
}

function prepare() {
    mkdir -p $TMP_DIR
    for _ENV in $ENV_VARS; do
	eval "${_ENV}_BASE"'=$'"${_ENV}"
	eval "${_ENV}_ADDS="
	eval export "${_ENV}"
    done
    DOWNLOAD_UDIR=`unix_path $DOWNLOAD_DIR`
    TMP_UDIR=`unix_path $TMP_DIR`
}

# usage:  smart_wget URL DESTDIR
function smart_wget() {
    _FILE=`basename $1`
    _DLD=`unix_path $2`

    # If the file already exists in the download directory ($2)
    # then don't do anything.  But if it does NOT exist then
    # download the file to the tmpdir and then when that completes
    # move it to the dest dir.
    if [ ! -f $_DLD/$_FILE ] ; then
	wget -c $1 -P $TMP_DIR
	mv $TMP_UDIR/$_FILE $_DLD
    fi
}

function inst_wget() {
    setup Wget
    _WGET_UDIR=`unix_path $WGET_DIR`
    set +e
    quiet $_WGET_UDIR/wget --version
    if [ $? = 0 ] ; then
        set -e
        echo "already installed.  skipping."
    else
        set -e
        mkdir -p $WGET_DIR
        tar -xjpf $DOWNLOAD_UDIR/wget*.tar.bz2 -C $WGET_DIR
        cp $_WGET_UDIR/*/*/wget.exe $WGET_DIR
    fi
    add_to_env $_WGET_UDIR PATH
    quiet wget --version || die "wget unavailable"
}

function inst_dtk() {
    setup MSYS DTK
    set +e
    quiet perl --help
    if [ $? = 0 ] ; then
        set -e
        echo "msys dtk already installed.  skipping."
    else
        set -e
        smart_wget $DTK_URL $DOWNLOAD_DIR
        echo "!!! When asked for an installation path, specify $MSYS_DIR !!!"
        $DOWNLOAD_UDIR/msysDTK-*.exe
        for file in \
	    /bin/{aclocal*,auto*,ifnames,libtool*,guile*} \
	    /share/{aclocal,aclocal-1.7,autoconf,autogen,automake-1.7,libtool}
        do
	    mv $file $file.bak
        done
    fi
    quiet perl --help || die "msys dtk not installed correctly"
}

function inst_mingw() {
    setup MinGW
    set +e
    quiet gcc --version
    if [ $? = 0 ] ; then
        set -e
	echo "mingw already installed.  skipping."
    else
        set -e
        _MINGW_WFSDIR=`win_fs_path $MINGW_DIR`
        smart_wget $MINGW_URL $DOWNLOAD_DIR
        echo "!!! Install g++ !!!"
        echo "!!! When asked for an installation path, specify $MINGW_DIR !!!"
        $DOWNLOAD_UDIR/MinGW-*.exe
        (echo "y"; echo "y"; echo "$_MINGW_WFSDIR") | sh pi.sh
    fi
    quiet gcc --version && quiet ld --help || die "mingw not installed correctly"
}

function inst_unzip() {
    setup Unzip
    _UNZIP_UDIR=`unix_path $UNZIP_DIR`
    set +e
    quiet $_UNZIP_UDIR/bin/unzip --help
    if [ $? = 0 ] ; then
        set -e
        echo "unzip already installed.  skipping."
    else
        set -e
        smart_wget $UNZIP_URL $DOWNLOAD_DIR
        echo "!!! When asked for an installation path, specify $UNZIP_DIR !!!"
        $DOWNLOAD_UDIR/unzip-*.exe
    fi
    add_to_env $_UNZIP_UDIR/bin PATH
    quiet unzip --help || die "unzip unavailable"
}

function inst_regex() {
    setup RegEx
    _REGEX_UDIR=`unix_path $REGEX_DIR`
    add_to_env -I$_REGEX_UDIR/include REGEX_CPPFLAGS
    add_to_env -L$_REGEX_UDIR/lib REGEX_LDFLAGS
    add_to_env $_REGEX_UDIR/bin PATH
    set +e
    quiet ld $REGEX_LDFLAGS -lregex -o tmp
    if [ $? = 0 ] ; then
        set -e
        echo "regex already installed.  skipping."
    else
        set -e
        smart_wget $REGEX_BIN_URL $DOWNLOAD_DIR
        smart_wget $REGEX_LIB_URL $DOWNLOAD_DIR
        mkdir -p $REGEX_DIR
        unzip $DOWNLOAD_UDIR/regex-*-bin.zip -d $REGEX_DIR
        unzip $DOWNLOAD_UDIR/regex-*-lib.zip -d $REGEX_DIR
    fi
    rm -f tmp
    quiet ld $REGEX_LDFLAGS -lregex -o tmp && rm tmp || die "regex not installed correctly"
}

function inst_readline() {
    setup Readline
    _READLINE_UDIR=`unix_path $READLINE_DIR`
    add_to_env -I$_READLINE_UDIR/include READLINE_CPPFLAGS
    add_to_env -L$_READLINE_UDIR/lib READLINE_LDFLAGS
    add_to_env $_READLINE_UDIR/bin PATH
    set +e
    quiet ld $READLINE_LDFLAGS -lreadline -o tmp
    if [ $? = 0 ] ; then
        set -e
        echo "readline already installed.  skipping."
    else
        set -e
        smart_wget $READLINE_BIN_URL $DOWNLOAD_DIR
        smart_wget $READLINE_LIB_URL $DOWNLOAD_DIR
        mkdir -p $READLINE_DIR
        unzip $DOWNLOAD_DIR/readline-*-bin.zip -d $READLINE_DIR
        unzip $DOWNLOAD_DIR/readline-*-lib.zip -d $READLINE_DIR
    fi
    rm -f tmp
    quiet ld $READLINE_LDFLAGS -lreadline -o tmp && rm tmp || die "readline not installed correctly"
}

function inst_indent() {
    setup Indent
    _INDENT_UDIR=`unix_path $INDENT_DIR`
    add_to_env $_INDENT_UDIR/bin PATH
    set +e
    quiet which indent
    if [ $? = 0 ] ; then
        set -e
        echo "indent already installed.  skipping."
    else
        set -e
        smart_wget $INDENT_BIN_URL $DOWNLOAD_DIR
        mkdir -p $INDENT_DIR
        unzip $DOWNLOAD_UDIR/indent-*-bin.zip -d $INDENT_DIR
    fi
    quiet which indent || die "indent unavailable"
}

function inst_guile() {
    setup Guile
    _GUILE_WFSDIR=`win_fs_path $GUILE_DIR`
    _GUILE_UDIR=`unix_path $GUILE_DIR`
    add_to_env $_GUILE_UDIR/bin PATH
    add_to_env $_GUILE_WFSDIR/share/guile/site/slib/ SCHEME_LIBRARY_PATH
    set +e
    quiet guile -c '(use-modules (srfi srfi-39))' &&
    quiet guile -c "(use-modules (ice-9 slib)) (require 'printf)"
    if [ $? = 0 ] ; then
        set -e
        echo "guile and slib already installed.  skipping."
    else
        set -e
        smart_wget $GUILE_URL $DOWNLOAD_DIR
        smart_wget $SLIB_URL $DOWNLOAD_DIR
        tar -xzpf $DOWNLOAD_UDIR/guile-*.tar.gz -C $TMP_UDIR
        qpushd $TMP_UDIR/guile-*
            qpushd ice-9
                cp boot-9.scm boot-9.scm.bak
                cat boot-9.scm.bak | sed '/SIGBUS/d' > boot-9.scm
            qpopd
            qpushd libguile
                cp fports.c fports.c.bak
                cat fports.c.bak | sed 's,#elif defined (FIONREAD),#elif 0,' > fports.c
                cp load.c load.c.bak
                cat load.c.bak | sed '/scan !=/s,:,;,' > load.c
            qpopd
            qpushd libguile-ltdl
                cp raw-ltdl.c raw-ltdl.c.bak
                cat raw-ltdl.c.bak | sed 's,\(SCMLTSTATIC\) LT_GLOBAL_DATA,\1,' > raw-ltdl.c
                touch upstream/ltdl.c.diff
            qpopd
            ./configure \
	        --disable-elisp \
	        --disable-networking \
	        --disable-dependency-tracking \
	        --disable-libtool-lock \
	        --disable-linuxthreads \
	        -C --prefix=$_GUILE_WFSDIR \
	        ac_cv_func_regcomp_rx=yes \
	        CPPFLAGS="${READLINE_CPPFLAGS} ${REGEX_CPPFLAGS}" \
	        LDFLAGS="-lwsock32 ${READLINE_LDFLAGS} ${REGEX_LDFLAGS} -lregex"
	    cp config.status config.status.bak
	    cat config.status.bak | sed 's# fileblocks[$.A-Za-z]*,#,#' > config.status
	    ./config.status
	    for file in {srfi,libguile-ltdl,libguile}/Makefile; do
	        cp $file $file.bak
	        cat $file.bak | sed '/^lib.*LDFLAGS =/s,\(\\*\)$, -no-undefined \1,' > $file
	    done
	    qpushd guile-config
	      cp Makefile Makefile.bak
	      cat Makefile.bak | sed '/-bindir-/s,:,^,g' > Makefile
	    qpopd
	    make
	    make install
	qpopd
	qpushd $GUILE_DIR/bin
	    mv libguilereadline-v-12-12.dll libguilereadline-v-12.dll
	    mv libguile-srfi-srfi-4-v-1-1.dll libguile-srfi-srfi-4-v-1.dll
	    mv libguile-srfi-srfi-13-14-v-1-1.dll libguile-srfi-srfi-13-14-v-1.dll
	qpopd
	_SLIB_DIR=$GUILE_DIR\\share\\guile\\site
	mkdir -p $_SLIB_DIR
	unzip $DOWNLOAD_DIR/slib*.zip -d $_SLIB_DIR
	qpushd $_SLIB_DIR/slib
	    cp guile.init guile.init.bak
	    echo "(define software-type (lambda () 'MS-DOS))" >> guile.init
	qpopd
    fi
    add_to_env "-I $_GUILE_UDIR/share/aclocal" ACLOCAL_FLAGS
    guile -c '(use-modules (srfi srfi-39))' &&
    guile -c "(use-modules (ice-9 slib)) (require 'printf)" || die "guile not installed correctly"
}

function inst_glade() {
    setup Glade
    set +e
    quiet pkg-config --exists glib-2.0 gtk+-2.0
    if [ $? = 0 ] ; then
        set -e
        echo "glade already installed.  skipping."
    else
        set -e
        smart_wget $GLADE_URL $DOWNLOAD_DIR
	echo "!!! When asked for an installation path, specify $GLADE_DIR !!!"
	$DOWNLOAD_UDIR/gtk-win32-devel-*.exe
	qpushd $GLADE_DIR\\lib\\pkgconfig
	    cp cairo.pc cairo.pc.bak
	    cat cairo.pc.bak | sed 's,libpng12,libpng13,' > cairo.pc
	qpopd
	qpushd $GLADE_DIR\\bin
	    cp intl.dll libintl-2.dll
	qpopd
    fi
    pkg-config --exists glib-2.0 gtk+-2.0 || die "glade not installed correctly"
}

function inst_gwrap() {
    setup G-Wrap
    _GWRAP_WFSDIR=`win_fs_path $GWRAP_DIR`
    _GWRAP_UDIR=`unix_path $GWRAP_DIR`
    add_to_env $_GWRAP_UDIR/bin PATH
    add_to_env $_GWRAP_WFSDIR/share/guile/site GUILE_LOAD_PATH
    set +e
    quiet g-wrap-config --version
    if [ $? = 0 ] ; then
        set -e
        echo "g-wrap already installed.  skipping."
    else
        set -e
        smart_wget $GWRAP_URL $DOWNLOAD_DIR
        tar -xzpf $DOWNLOAD_UDIR/g-wrap-*.tar.gz -C $TMP_UDIR
        qpushd $TMP_UDIR/g-wrap-*
            qpushd g-wrap
                cp core-runtime.c core-runtime.c.bak
                cat core-runtime.c.bak | sed '/vasprintf/d' > core-runtime.c
            qpopd
            cp configure configure.bak
            cat configure.bak | sed 's,"glib","glib-2.0",g' > configure
            ./configure \
	        --prefix=$_GWRAP_WFSDIR \
	        LDFLAGS="-no-undefined"
            qpushd guile/g-wrap/gw
                cp Makefile Makefile.bak
                cat Makefile.bak | sed '/^libgw_guile_standard_la_LIBADD/s,$, ../../../libffi/libffi.la ../../../g-wrap/libgwrap-core-runtime.la,;/libgw_guile_gw_glib_la_LIBADD/s,$, ../../../g-wrap/libgwrap-core-runtime.la,' > Makefile
                make standard.c gw-glib.c
                cp standard.scm standard.scm.bak
                cat standard.scm.bak | sed -c 's,\(libgw-guile-standard\),../lib/\1-0,' > standard.scm
                cp gw-glib.scm gw-glib.scm.bak
                cat gw-glib.scm.bak | sed -c 's,\(libgw-guile-gw-glib\),../lib/\1-0,' > gw-glib.scm
            qpopd
            qpushd guile/test
                cp Makefile Makefile.bak
                cat Makefile.bak | sed '/^std_libs/s,\\$, ../../libffi/libffi.la \\,' > Makefile
            qpopd
            make
            make install
        qpopd
    fi    
    add_to_env $_GWRAP_UDIR/lib/pkgconfig PKG_CONFIG_PATH
    guile -c '(use-modules (srfi srfi-39))' &&
    quiet g-wrap-config --version || die "g-wrap not installed correctly"
}

function inst_gnome() {
    setup Gnome platform
    _GNOME_UDIR=`unix_path $GNOME_DIR`
    add_to_env $_GNOME_UDIR/bin PATH
    add_to_env $_GNOME_UDIR/lib/pkgconfig PKG_CONFIG_PATH
    set +e
    quiet gconftool-2 --version &&
    pkg-config --exists gconf-2.0 libgnome-2.0 libgnomeui-2.0 libgnomeprint-2.2 libgnomeprintui-2.2 libgtkhtml-3.8 &&
    quiet intltoolize --version
    if [ $? = 0 ] ; then
        set -e
        echo "gnome packages installed.  skipping."
    else
        set -e
        mkdir -p $GNOME_DIR
	smart_wget $INTLTOOL_URL $DOWNLOAD_DIR
	smart_wget $ORBIT2_URL $DOWNLOAD_DIR
	smart_wget $ORBIT2_DEV_URL $DOWNLOAD_DIR
	smart_wget $GCONF_URL $DOWNLOAD_DIR
	smart_wget $GCONF_DEV_URL $DOWNLOAD_DIR
	smart_wget $LIBBONOBO_URL $DOWNLOAD_DIR
	smart_wget $LIBBONOBO_DEV_URL $DOWNLOAD_DIR
	smart_wget $GNOME_VFS_URL $DOWNLOAD_DIR
	smart_wget $GNOME_VFS_DEV_URL $DOWNLOAD_DIR
	smart_wget $LIBGNOME_URL $DOWNLOAD_DIR
	smart_wget $LIBGNOME_DEV_URL $DOWNLOAD_DIR
	smart_wget $LIBGNOMECANVAS_URL $DOWNLOAD_DIR
	smart_wget $LIBGNOMECANVAS_DEV_URL $DOWNLOAD_DIR
	smart_wget $LIBBONOBOUI_URL $DOWNLOAD_DIR
	smart_wget $LIBBONOBOUI_DEV_URL $DOWNLOAD_DIR
	smart_wget $LIBGNOMEUI_URL $DOWNLOAD_DIR
	smart_wget $LIBGNOMEUI_DEV_URL $DOWNLOAD_DIR
	smart_wget $LIBGNOMEPRINT_URL $DOWNLOAD_DIR
	smart_wget $LIBGNOMEPRINT_DEV_URL $DOWNLOAD_DIR
	smart_wget $LIBGNOMEPRINTUI_URL $DOWNLOAD_DIR
	smart_wget $LIBGNOMEPRINTUI_DEV_URL $DOWNLOAD_DIR
	smart_wget $GTKHTML_URL $DOWNLOAD_DIR
	smart_wget $GTKHTML_DEV_URL $DOWNLOAD_DIR
	unzip $DOWNLOAD_UDIR/intltool-*.zip -d $GNOME_DIR
	unzip $DOWNLOAD_UDIR/ORBit2-[^d]*.zip -d $GNOME_DIR
	unzip $DOWNLOAD_UDIR/ORBit2-dev-*.zip -d $GNOME_DIR
	unzip $DOWNLOAD_UDIR/GConf-[^d]*.zip -d $GNOME_DIR
	unzip $DOWNLOAD_UDIR/GConf-dev-*.zip -d $GNOME_DIR
	unzip $DOWNLOAD_UDIR/libbonobo-[^d]*.zip -d $GNOME_DIR
	unzip $DOWNLOAD_UDIR/libbonobo-dev-*.zip -d $GNOME_DIR
	unzip $DOWNLOAD_UDIR/gnome-vfs-[^d]*.zip -d $GNOME_DIR
	unzip $DOWNLOAD_UDIR/gnome-vfs-dev-*.zip -d $GNOME_DIR
	unzip $DOWNLOAD_UDIR/libgnome-[^d]*.zip -d $GNOME_DIR
	unzip $DOWNLOAD_UDIR/libgnome-dev-*.zip -d $GNOME_DIR
	unzip $DOWNLOAD_UDIR/libgnomecanvas-[^d]*.zip -d $GNOME_DIR
	unzip $DOWNLOAD_UDIR/libgnomecanvas-dev-*.zip -d $GNOME_DIR
	unzip $DOWNLOAD_UDIR/libbonoboui-[^d]*.zip -d $GNOME_DIR
	unzip $DOWNLOAD_UDIR/libbonoboui-dev-*.zip -d $GNOME_DIR
	unzip $DOWNLOAD_UDIR/libgnomeui-[^d]*.zip -d $GNOME_DIR
	unzip $DOWNLOAD_UDIR/libgnomeui-dev-*.zip -d $GNOME_DIR
	unzip $DOWNLOAD_UDIR/libgnomeprint-[^d]*.zip -d $GNOME_DIR
	unzip $DOWNLOAD_UDIR/libgnomeprint-dev-*.zip -d $GNOME_DIR
	unzip $DOWNLOAD_UDIR/libgnomeprintui-[^d]*.zip -d $GNOME_DIR
	unzip $DOWNLOAD_UDIR/libgnomeprintui-dev-*.zip -d $GNOME_DIR
	unzip $DOWNLOAD_UDIR/gtkhtml-[^d]*.zip -d $GNOME_DIR
	unzip $DOWNLOAD_UDIR/gtkhtml-dev-*.zip -d $GNOME_DIR
    fi
    add_to_env "-I $_GNOME_UDIR/share/aclocal" ACLOCAL_FLAGS
    quiet gconftool-2 --version &&
    pkg-config --exists gconf-2.0 libgnome-2.0 libgnomeui-2.0 libgnomeprint-2.2 libgnomeprintui-2.2 libgtkhtml-3.8 &&
    quiet intltoolize --version || die "gnome not installed correctly"
}

function inst_autotools() {
    setup Autotools
    _AUTOTOOLS_UDIR=`unix_path $AUTOTOOLS_DIR`
    add_to_env $_AUTOTOOLS_UDIR/bin PATH
    set +e
    quiet autoconf --help && quiet automake --help && quiet libtool --help 
    if [ $? = 0 ] ; then
        set -e
        echo "auto tools already installed.  skipping."
    else
        set -e
        smart_wget $AUTOCONF_URL $DOWNLOAD_DIR
        smart_wget $AUTOMAKE_URL $DOWNLOAD_DIR
        smart_wget $LIBTOOL_URL $DOWNLOAD_DIR
        tar -xjpf $DOWNLOAD_UDIR/autoconf-*.tar.bz2 -C $TMP_UDIR
        qpushd $TMP_UDIR/autoconf-*
	    echo "building autoconf..."
           ./configure --prefix=$_AUTOTOOLS_UDIR
            make
            make install
        qpopd
        tar -xjpf $DOWNLOAD_UDIR/automake-*.tar.bz2 -C $TMP_UDIR
        qpushd $TMP_UDIR/automake-*
	    echo "building automake..."
            ./configure --prefix=$_AUTOTOOLS_UDIR
            make
            make install
        qpopd
        tar -xzpf $DOWNLOAD_UDIR/libtool-*.tar.gz -C $TMP_UDIR
        qpushd $TMP_UDIR/libtool-*
	    echo "building libtool..."
            ./configure --prefix=$_AUTOTOOLS_UDIR
            make
            make install
        qpopd
    fi
    add_to_env -I$_AUTOTOOLS_UDIR/include AUTOTOOLS_CPPFLAGS
    add_to_env -L$_AUTOTOOLS_UDIR/lib AUTOTOOLS_LDFLAGS
    add_to_env "-I $_AUTOTOOLS_UDIR/share/aclocal" ACLOCAL_FLAGS
    quiet autoconf --help &&
    quiet automake --help &&
    quiet libtool --help || die "autotools not installed correctly"
}

function inst_libgsf() {
    setup libGSF
    _LIBGSF_UDIR=`unix_path $LIBGSF_DIR`
    add_to_env $_LIBGSF_UDIR/bin PATH
    add_to_env $_LIBGSF_UDIR/lib/pkgconfig PKG_CONFIG_PATH
    set +e
    quiet pkg-config --exists libgsf-1 libgsf-gnome-1
    if [ $? = 0 ] ; then
	set -e
	echo "libgsf already installed.  skipping."
    else
	set -e
	smart_wget $LIBGSF_URL $DOWNLOAD_DIR
	tar -xjpf $DOWNLOAD_UDIR/libgsf-*.tar.bz2 -C $TMP_UDIR
	qpushd $TMP_UDIR/libgsf-*
	    cp configure.in configure.in.bak
	    cat configure.in.bak | sed '/AC_PROG_INTLTOOL/s#$#([],[no-xml])#' > configure.in
	    autoconf
	    ./configure --prefix=$_LIBGSF_UDIR
	    make
	    make install
	qpopd
    fi
    pkg-config --exists libgsf-1 libgsf-gnome-1 || die "libgsf not installed correctly"
}

function inst_goffice() {
    setup GOffice
    _GOFFICE_UDIR=`unix_path $GOFFICE_DIR`
    add_to_env $_GOFFICE_UDIR/lib/pkgconfig PKG_CONFIG_PATH
    set +e
    quiet pkg-config --exists libgoffice-0.3
    if [ $? = 0 ] ; then
	set -e
	echo "goffice already installed.  skipping."
    else
	set -e
	smart_wget $GOFFICE_URL $DOWNLOAD_DIR
	tar -xjpf $DOWNLOAD_UDIR/goffice-*.tar.bz2 -C $TMP_UDIR
	mydir=`pwd`
	qpushd $TMP_UDIR/goffice-*
	    cp configure.in configure.in.bak
	    cat configure.in.bak | sed '/AC_PROG_INTLTOOL/s#)$#,[no-xml])#' > configure.in
	    [ -n "$GOFFICE_PATCH" -a -f "$GOFFICE_PATCH" ] && \
		patch -p1 < $GOFFICE_PATCH
	    cp goffice/Makefile.am goffice/Makefile.am.bak
	    cat goffice/Makefile.am.bak \
		| sed '/LIBADD/s#-lurlmon##;s#-lhtmlhelp##' \
		> goffice/Makefile.am
	    automake
	    autoconf
	    ./configure --prefix=$_GOFFICE_UDIR
	    [ -f dumpdef.pl ] || cp -p ../libgsf-*/dumpdef.pl .
	    [ -f $mydir/intltool-merge ] && \
		( mv intltool-merge intltool-merge.bak ; \
		  cp -p $mydir/intltool-merge . )
	    make
	    make install
	qpopd
    fi
    pkg-config --exists libgoffice-0.3 || die "goffice not installed correctly"
}

function inst_svn() {
    setup Subversion
    _SVN_UDIR=`unix_path $SVN_DIR`
    export PATH="$_SVN_UDIR/bin:$PATH"
    set +e
    quiet svn --version
    if [ $? = 0 ] ; then
        set -e
        echo "subversion already installed.  skipping."
    else
        set -e
        smart_wget $SVN_URL $DOWNLOAD_DIR
        echo "!!! When asked for an installation path, specify $SVN_DIR !!!"
        $DOWNLOAD_UDIR/svn-*.exe
    fi
}

function svn_up() {
    mkdir -p $REPOS_DIR
    qpushd $REPOS_DIR
    if [ -x .svn ]; then
	setup svn up
	svn up
    else
	setup svn co
	svn co $REPOS_URL .
    fi
    qpopd
}

function inst_gnucash() {
    setup GnuCash
    _GNUCASH_WFSDIR=`win_fs_path $GNUCASH_DIR`
    qpushd $REPOS_DIR
    cp configure.in configure.in.bak
    cat configure.in.bak | sed '/AC_PROG_INTLTOOL/s#TOOL$#TOOL([],[no-xml])#;/GUILE_LOAD_PATH/s,:,;,g' > configure.in
    ./autogen.sh
    ./configure \
	--prefix=$_GNUCASH_WFSDIR \
	--enable-debug \
	CPPFLAGS="${AUTOTOOLS_CPPFLAGS} ${REGEX_CPPFLAGS}" \
	CFLAGS="${AUTOTOOLS_CPPFLAGS} ${REGEX_CPPFLAGS}" \
	LDFLAGS="${AUTOTOOLS_LDFLAGS} ${REGEX_LDFLAGS}" \
	PKG_CONFIG_PATH="${PKG_CONFIG_PATH}" \
	PATH="${PATH}"
#    make
    qpopd
}

function finish() {
    _NEW=x
    for _ENV in $ENV_VARS; do
	_ADDS=`eval echo '"\$'"${_ENV}"'_ADDS"'`
	if [ "$_ADDS" ]; then
	    if [ "$_NEW" ]; then
		echo
		echo "Environment variables changed, please do the following"
		echo
		_NEW=
	    fi
	    _VAL=`eval echo '"$'"${_ENV}_BASE"'"'`
	    if [ "$_VAL" ]; then
		_CHANGE="export ${_ENV}=\"${_ADDS}"'$'"${_ENV}\""
	    else
		_CHANGE="export ${_ENV}=\"${_ADDS}\""
	    fi
	    echo $_CHANGE
	    echo echo "'${_CHANGE}' >> /etc/profile.d/installer.sh"
	fi
    done
}

prepare
for step in "${steps[@]}" ; do
    eval $step
done
finish
qpopd


### Local Variables: ***
### sh-basic-offset: 4 ***
### tab-width: 8 ***
### End: ***
