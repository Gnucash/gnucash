#!/bin/bash

[ ! "$BASH" -a -x /bin/bash ] && exec /bin/bash "$0" "$@"

set -e

echo -n "Build Starting at "
date

function qpushd() { pushd "$@" >/dev/null; }
function qpopd() { popd >/dev/null; }
function unix_path() { echo "$*" | sed 's,^\([A-Za-z]\):,/\1,;s,\\,/,g'; }

qpushd "$(dirname $(unix_path "$0"))"
. functions.sh
. defaults.sh

register_env_var ACLOCAL_FLAGS " "
register_env_var AUTOTOOLS_CPPFLAGS " "
register_env_var AUTOTOOLS_LDFLAGS " "
register_env_var GMP_CPPFLAGS " "
register_env_var GMP_LDFLAGS " "
register_env_var GNOME_CPPFLAGS " "
register_env_var GNOME_LDFLAGS " "
register_env_var GNUTLS_CPPFLAGS " "
register_env_var GNUTLS_LDFLAGS " "
register_env_var GUILE_LOAD_PATH ";"
register_env_var GUILE_CPPFLAGS " "
register_env_var GUILE_LDFLAGS " "
register_env_var HH_CPPFLAGS " "
register_env_var HH_LDFLAGS " "
register_env_var INTLTOOL_PERL " "
register_env_var LIBDBI_CPPFLAGS " "
register_env_var LIBDBI_LDFLAGS " "
register_env_var KTOBLZCHECK_CPPFLAGS " "
register_env_var KTOBLZCHECK_LDFLAGS " "
register_env_var PATH ":"
register_env_var PCRE_CPPFLAGS " "
register_env_var PCRE_LDFLAGS " "
register_env_var PKG_CONFIG ":" ""
register_env_var PKG_CONFIG_PATH ":"
register_env_var READLINE_CPPFLAGS " "
register_env_var READLINE_LDFLAGS " "
register_env_var REGEX_CPPFLAGS " "
register_env_var REGEX_LDFLAGS " "
register_env_var WEBKIT_CFLAGS " "
register_env_var WEBKIT_LIBS " "

function prepare() {
# check gnome

   setup "prepare"
   TMP_UDIR=`unix_path $TMP_DIR`
   DOWNLOAD_UDIR=`unix_path $DOWNLOAD_DIR`
   _GLOBAL_UDIR=`unix_path $GLOBAL_DIR`
   _MINGW_UDIR=`unix_path $MINGW_DIR`
   add_to_env $_MINGW_UDIR/bin PATH
   mkdir -p $TMP_UDIR
   if [ "$CROSS_COMPILE" != "yes" ]; then
      die "Cross Compile needed"
   fi
   _UNAME_O=`uname -o`

   if [ "$_UNAME_O" = "Msys" ]; then
      die "You can't build webkit on msys. Use Cygwin on Windows instead."
   fi

   if [ "$_UNAME_O" = "Cygwin" ] &&
       [ "$_GLOBAL_UDIR/mingw" = "$_MINGW_UDIR" ] ||
       [ "$_GLOBAL_UDIR/msys" = "$_MINGW_UDIR" ];
   then
      die "Please set \$MINGW_DIR other than \$GLOBAL_DIR\\\\mingw or \$GLOBAL_DIR\\\\msys on Cygwin."
   fi

   if [ -n "$CCACHE_LINK_DIR" ]; then
      export PATH=$CCACHE_LINK_DIR:$PATH
   fi
}

function inst_crossmingw() {
    setup "Cross mingw"
    _MINGW_UDIR=`unix_path $MINGW_DIR`
    mkdir -p `unix_path $MINGW_DIR\\\\mingw32`
    if [ -d $_MINGW_UDIR ] && 
        quiet $CC -v 
    then
        echo "Cross mingw installed."
       if [ ` $CC -dumpversion | cut -d. -f1` -le 3 ] ;then
           die "GCC 4.4 or higher needed"
       fi
    else
        echo "Installing Cross mingw32 gcc 4.x"
        mkdir -p $_MINGW_UDIR
        wget_unpacked $MINGW_RT_URL $DOWNLOAD_DIR $MINGW_DIR\\mingw32
        [ -n "$MINGW_RT_DEV_URL" ] && wget_unpacked $MINGW_RT_DEV_URL $DOWNLOAD_DIR $MINGW_DIR\\mingw32
        wget_unpacked $W32API_URL $DOWNLOAD_DIR $MINGW_DIR\\mingw32
        [ -n "$PTHREADS_W32_URL" ] && wget_unpacked $PTHREADS_W32_URL $DOWNLOAD_DIR $MINGW_DIR\\mingw32
        [ -n "$PTHREADS_W32_DEV_URL" ] && wget_unpacked $PTHREADS_W32_DEV_URL $DOWNLOAD_DIR $MINGW_DIR\\mingw32
 
        inst_crossbinutils
        inst_crossgcc
    fi
}

function inst_crossbinutils() {
    _MINGW_UDIR=`unix_path $MINGW_DIR`
    if quiet $LD -v
    then 
        echo "Cross binutils already installed."
    else
         wget_unpacked $CROSS_BINUTILS_SRC_URL $DOWNLOAD_DIR $TMP_DIR
         assert_one_dir $TMP_UDIR/binutils-*
         mkdir -p $TMP_UDIR/build-binutils
         qpushd $TMP_UDIR/build-binutils
             $TMP_UDIR/binutils-*/configure  \
                 --prefix=$_MINGW_UDIR --target=mingw32 \
                 --with-gcc --with-gnu-as --with-gnu-ld --enable-shared 
             make
             make install
         qpopd
        if quiet $_MINGW_UDIR/bin/mingw32-ld  -v
        then 
            rm -rf $TMP_UDIR/binutils-* $TMP_UDIR/build-binutils
            echo "Cross binutils installed successfully."
        else
            die "Cross binutils install failure"
        fi
    fi
}

function inst_crossgcc(){
    _MINGW_UDIR=`unix_path $MINGW_DIR`
    if quiet $CC
    then 
        echo "Cross gcc alreadyinstalled."
    else
        mkdir -p $_MINGW_UDIR/mingw32/include
        mkdir -p $TMP_UDIR/build-gcc
        mkdir -p $TMP_UDIR/mydir
        wget_unpacked $CROSS_GCC_SRC_URL $DOWNLOAD_DIR $TMP_DIR
        wget_unpacked $CROSS_GCC_SRC2_URL $DOWNLOAD_DIR $TMP_DIR\\mydir
        assert_one_dir $TMP_UDIR/gcc-*
        qpushd $TMP_UDIR/gcc-*
            dos2unix $TMP_UDIR/mydir/patches/gcc*.patch
# FIXME: gcc-4.4.0 patch workaround
            for i in $TMP_UDIR/mydir/patches/gcc*.patch; do
#                 patch -p0 < $i
                 patch -p0 < $i || echo -n ""
            done
        cat >> libstdc++-v3/include/bits/c++config << EOF
#ifndef _GLIBCXX_IMPORT
#ifdef _GLIBCXX_DLL
#define _GLIBCXX_IMPORT __attribute__((dllimport))
#else
#define _GLIBCXX_IMPORT
#endif
#endif
EOF
        qpopd
        qpushd $TMP_UDIR/build-gcc
            $TMP_UDIR/gcc-*/configure  \
                --prefix=$_MINGW_UDIR --target=$TARGET \
                --with-headers=$_MINGW_UDIR/mingw32/include \
                --enable-languages=c,c++ \
                --with-gcc --with-gnu-ld --with-gnu-as \
                --disable-sjlj-exceptions --enable-shared \
                --with-dwarf2 --disable-win32-registry \
                --enable-libstdcxx-debug --enable-version-specific-runtime-libs \
                --enable-threads
            make
            make install
        qpopd
        if quiet $CC -v
        then 
            rm -rf $TMP_UDIR/build-gcc $TMP_UDIR/gcc-* $TMP_UDIR/mydir
            echo "Cross gcc installed successfully."
        else
            die "Cross install failure"
        fi
    fi 
}

function inst_libxslt_gnome() {
    setup libxslt-gnome
    _GNOME_UDIR=`unix_path $GNOME_DIR`
    add_to_env -I$_GNOME_UDIR/include GNOME_CPPFLAGS
    add_to_env -L$_GNOME_UDIR/lib GNOME_LDFLAGS
    add_to_env $_GNOME_UDIR/lib/pkgconfig PKG_CONFIG_PATH
    if [ "$CROSS_COMPILE" != "yes" ]; then
        add_to_env $_GNOME_UDIR//pkg-config-msys.sh PKG_CONFIG
    else
        add_to_env pkg-config PKG_CONFIG
    fi
    if quiet gconftool-2 --version &&
        quiet ${PKG_CONFIG} --exists libxslt &&
        [ -f $_GNOME_UDIR/lib/pkgconfig/libxslt.pc ] &&
        quiet intltoolize --version
    then
        echo "libxslt-gnome installed.  skipping."
    else
         # create dummy xml2-config
         cat > $_GNOME_UDIR/bin/xml2-config << EOF
#!/bin/bash
if [ "\$1" = "--version" ]; then
    pkg-config  --modversion libxml-2.0
else
    pkg-config \$1 libxml-2.0
fi
EOF
         chmod a+x $_GNOME_UDIR/bin/xml2-config

         wget_unpacked $LIBXSLT_SRC_URL $DOWNLOAD_DIR $TMP_DIR
         assert_one_dir $TMP_UDIR/libxslt-*
         qpushd $TMP_UDIR/libxslt-*
             ./configure ${HOST_XCOMPILE} \
                 --with-libxml-prefix=$_GNOME_UDIR \
                 --with-libxml-include-prefix=$_GNOME_UDIR/include/libxml2 \
                 --with-libxml-libs-prefix=$_GNOME_UDIR/lib \
                 PKG_CONFIG_PATH="$_GNOME_UDIR/lib/pkgconfig" \
                 --prefix=${_GNOME_UDIR} \
                 --enable-static=no \
                 --with-plugins=no \
                 --with-python=no \
                 CPPFLAGS="$GNOME_CPPFLAGS $GNUTLS_CPPFLAGS" \
                 LDFLAGS="$GNOME_LDFLAGS $GNUTLS_LDFLAGS"
            make .
            cd libxslt
            make EXTRA_LIBS="-L$_GNOME_UDIR/bin -lxml2-2"
            cd ..
            make
            make install
         qpopd
         rm -r $TMP_UDIR/libxslt-*
    fi
}

function check_gnome() {
    setup "Checking GNOME "
    _GNOME_UDIR=`unix_path $GNOME_DIR`
    add_to_env -I$_GNOME_UDIR/include GNOME_CPPFLAGS
    add_to_env -L$_GNOME_UDIR/lib GNOME_LDFLAGS
    add_to_env $_GNOME_UDIR/lib/pkgconfig PKG_CONFIG_PATH
    if [ "$CROSS_COMPILE" != "yes" ]; then
        add_to_env $_GNOME_UDIR/bin/pkg-config-msys.sh PKG_CONFIG
    else
        add_to_env pkg-config PKG_CONFIG
    fi
    if quiet gconftool-2 --version &&
        quiet ${PKG_CONFIG} --exists gconf-2.0 libgnome-2.0 libgnomeui-2.0 libgtkhtml-3.14 pixman-1 &&
        [ -f $_GNOME_UDIR/lib/pkgconfig/pixman-1.pc ] &&
        quiet intltoolize --version
    then
        echo "gnome packages installed."
    else
        die "gnome packages are not installed. Please install by install.sh" 
    fi
}

function check_gnutls() {
    setup "Checking GNUTLS"
    _GNUTLS_UDIR=`unix_path ${GNUTLS_DIR}`
    add_to_env ${_GNUTLS_UDIR}/lib/pkgconfig PKG_CONFIG_PATH
    add_to_env "-I${_GNUTLS_UDIR}/include" GNUTLS_CPPFLAGS
    add_to_env "-L${_GNUTLS_UDIR}/lib" GNUTLS_LDFLAGS
    if quiet ${PKG_CONFIG} --exists gnutls &&
       [ -f $_GNUTLS_UDIR/lib/pkgconfig/gnutls.pc ] &&
       quiet intltoolize --version
    then
        echo "GNUTLS already installed."
    else
        die "GNUTLS packages are not installed. Please install by install.sh" 
    fi
}

function inst_enchant () {
    setup enchant
    _GNOME_UDIR=`unix_path $GNOME_DIR`
    add_to_env -I$_GNOME_UDIR/include GNOME_CPPFLAGS
    add_to_env -L$_GNOME_UDIR/lib GNOME_LDFLAGS
    add_to_env $_GNOME_UDIR/lib/pkgconfig PKG_CONFIG_PATH
    if [ "$CROSS_COMPILE" != "yes" ]; then
        add_to_env $_GNOME_UDIR/bin/pkg-config-msys.sh PKG_CONFIG
    else
        add_to_env pkg-config PKG_CONFIG
    fi
    if quiet gconftool-2 --version &&
        quiet ${PKG_CONFIG} --exists enchant &&
        [ -f $_GNOME_UDIR/lib/pkgconfig/enchant.pc ] &&
        quiet intltoolize --version
    then
        echo "enchant installed.  skipping."
    else
        wget_unpacked $ENCHANT_URL $DOWNLOAD_DIR $GNOME_DIR
        wget_unpacked $ENCHANT_DEV_URL $DOWNLOAD_DIR $GNOME_DIR
        fix_pkgconfigprefix $_GNOME_UDIR $_GNOME_UDIR/lib/pkgconfig/enchant.pc
    fi
}


function inst_libsoup () {
    setup libsoup
    _GNOME_UDIR=`unix_path $GNOME_DIR`
    add_to_env -I$_GNOME_UDIR/include GNOME_CPPFLAGS
    add_to_env -L$_GNOME_UDIR/lib GNOME_LDFLAGS
    add_to_env $_GNOME_UDIR/lib/pkgconfig PKG_CONFIG_PATH
    if [ "$CROSS_COMPILE" != "yes" ]; then
        add_to_env $_GNOME_UDIR/bin/pkg-config-msys.sh PKG_CONFIG
    else
        add_to_env pkg-config PKG_CONFIG
    fi
    if quiet gconftool-2 --version &&
        quiet ${PKG_CONFIG} --exists libsoup-2.4 &&
        [ -f $_GNOME_UDIR/lib/pkgconfig/libsoup-2.4.pc ] &&
        quiet intltoolize --version
    then
        echo "libsoup installed.  skipping."
    else
#        wget_unpacked $LIBSOUP_URL $DOWNLOAD_DIR $GNOME_DIR
#        wget_unpacked $LIBSOUP_DEV_URL $DOWNLOAD_DIR $GNOME_DIR
#        libsoup binaries are too old so it is necessary to build new ones.
         wget_unpacked $LIBSOUP_SRC_URL $DOWNLOAD_DIR $TMP_DIR
         assert_one_dir $TMP_UDIR/libsoup-*
         qpushd $TMP_UDIR/libsoup-*
             ./configure ${HOST_XCOMPILE} \
                 --prefix=${_GNOME_UDIR} \
                 --with-libxml-prefix=${_GNOME_UDIR} \
                 CPPFLAGS="$GNOME_CPPFLAGS $GNUTLS_CPPFLAGS" \
                 LDFLAGS="$GNOME_LDFLAGS $GNUTLS_LDFLAGS"
            make
            make install 
         qpopd
         rm -r $TMP_UDIR/libsoup-*
    fi
}

function inst_webkit() {
    setup WebKitGTK+
    _MINGW_UDIR=`unix_path $MINGW_DIR`
    _GNOME_UDIR=`unix_path $GNOME_DIR`
    _WEBKIT_DIR=`unix_path $WEBKIT_DIR`
    add_to_env -I$_GNOME_UDIR/include GNOME_CPPFLAGS
    add_to_env -L$_GNOME_UDIR/lib GNOME_LDFLAGS
    add_to_env $_GNOME_UDIR/lib/pkgconfig PKG_CONFIG_PATH
    _GNUTLS_UDIR=`unix_path $GNUTLS_DIR`
    add_to_env -I$_GNUTLS_UDIR/include GNOME_CPPFLAGS
    add_to_env -L$_GNUTLS_UDIR/lib GNOME_LDFLAGS
    add_to_env $_GNUTLS_UDIR/lib/pkgconfig PKG_CONFIG_PATH
    _SQLITE3_UDIR=`unix_path $SQLITE3_DIR`
    add_to_env -I$_SQLITE3_UDIR/include GNOME_CPPFLAGS
    add_to_env -L$_SQLITE3_UDIR/lib GNOME_LDFLAGS
    add_to_env $_SQLITE3_UDIR/lib/pkgconfig PKG_CONFIG_PATH
    _ICU4C_UDIR=`unix_path $ICU4C_DIR`
    add_to_env -I$_ICU4C_UDIR/include GNOME_CPPFLAGS
    add_to_env -L$_ICU4C_UDIR/lib GNOME_LDFLAGS

    if [ "$CROSS_COMPILE" != "yes" ]; then
        add_to_env $_GNOME_UDIR/bin/pkg-config-msys.sh PKG_CONFIG
    else
        add_to_env pkg-config PKG_CONFIG
    fi
    if quiet gconftool-2 --version &&
        quiet ${PKG_CONFIG} --exists webkit-1.0 &&
        [ -f $_GNOME_UDIR/lib/pkgconfig/webkit-1.0.pc ] &&
        quiet intltoolize --version
    then
        echo "WebKitGTK+ installed.  skipping."
    else
        wget_unpacked $WEBKIT_SRC_URL $DOWNLOAD_DIR $TMP_DIR
        assert_one_dir $TMP_UDIR/webkit-*
        qpushd $TMP_UDIR/webkit-*
            if [ -n "$WEBKIT_PATCH" -a -f "$WEBKIT_PATCH" ] ; then
                patch -p1 < $WEBKIT_PATCH
            fi
            if [ -n "$WEBKIT_PATCH2" -a -f "$WEBKIT_PATCH2" ] ; then
                patch -p1 < $WEBKIT_PATCH2
            fi
            perl -pi.bak -e"s!/usr/bin/gcc!$CC!" \
                WebCore/dom/make_names.pl \
                WebCore/css/make-css-file-arrays.pl \
                WebCore/bindings/scripts/IDLParser.pm
            ./configure ${HOST_XCOMPILE} \
               CPPFLAGS="$GNOME_CPPFLAGS" \
                --prefix=${_WEBKIT_DIR} \
                --enable-silent-rules \
                --disable-datalist \
                --disable-dom-storage \
                --disable-eventsource \
                --disable-filters \
                --disable-gtk-doc-html \
                --disable-offline-web-applications \
                --disable-ruby \
                --disable-shared-workers \
                --disable-silent-rules \
                --disable-svg-animation \
                --disable-svg-as-image \
                --disable-svg-fonts \
                --disable-svg-foreign-object \
                --disable-svg-use \
                --disable-video \
                --disable-workers \
                --disable-xpath \
                --with-target=win32 \
                --with-unicode-backend=icu \
               CFLAGS="-g -O2 -std=gnu99" \
               CXXFLAGS="-g -O2 -std=gnu++98" \
               LDFLAGS="$GNOME_LDFLAGS"
            make
            make install
        qpopd
        rm -rf $TMP_UDIR/webkit-*
    fi
}

function inst_sqlite3() {
    setup sqlite3
    _SQLITE3_UDIR=`unix_path ${SQLITE3_DIR}`
    if [ -f ${_SQLITE3_UDIR}/bin/libsqlite3-0.dll ] 
    then
        echo "SQLite3 already installed.  Skipping."
    else
        wget_unpacked $SQLITE3_URL $DOWNLOAD_DIR $TMP_DIR
        assert_one_dir $TMP_UDIR/sqlite-*
        qpushd $TMP_UDIR/sqlite-*
            ./configure ${HOST_XCOMPILE} \
                --prefix=${_SQLITE3_UDIR}
            make
            make install
        qpopd
        test -f ${_SQLITE3_UDIR}/bin/libsqlite3-0.dll || die "SQLite3 not installed correctly"
        rm -rf ${TMP_UDIR}/sqlite-*
    fi
}

function inst_icu4c() {
    setup icu4c
    _ICU4C_UDIR=`unix_path $ICU4C_DIR`
    if [ -f "$_ICU4C_UDIR/bin/uconv.exe" ]
    then
        echo "icu4c already installed.  Skipping."
    else
        wget_unpacked $ICU4C_SRC_URL $DOWNLOAD_DIR $TMP_DIR
        qpushd $TMP_UDIR/icu
            patch -p1 < $ICU4C_PATCH
        qpopd
        build_icu4c_native
        inst_icu4c_mingw32
        # cleanup
        rm -rf $TMP_UDIR/icu*
    fi
}

function build_icu4c_native() {
    setup icu4c-native
    mkdir -p $TMP_UDIR/icu-native
    if [ -e "$TMP_UDIR/icu-native/bin/uconv.exe" ] 
    then
        echo "native icu4c already compiled. Skipping."
    else
        qpushd $TMP_UDIR/icu-native
            if uname -o |grep Linux > /dev/null 2>&1 
            then
                $TMP_UDIR/icu/source/runConfigureICU Linux
            elif uname -o |grep Cygwin > /dev/null 2>&1
            then
                $TMP_UDIR/icu/source/runConfigureICU Cygwin
            else
                die "Failed to execute runConfigICU. Please edit this script"
            fi
            make
            make check
            for i in $TMP_UDIR/icu-native/bin/* ; do
                [ ! -f $i.exe ] && ln -s $i $i.exe
            done
        qpopd
        echo "icu4c native compiled successfully"
    fi
}

function inst_icu4c_mingw32() {
    setup icu4c-mingw32
    _ICU4C_UDIR=`unix_path $ICU4C_DIR`
    rm -rf $TMP_UDIR/icu-cross
    mkdir -p $TMP_UDIR/icu-cross

    # Note: If you have TARGET=i586-mingw32mingw, for this library you
    # need to set a different --host argument here due to some known
    # bug in the ICU source package
    #HOST_XCOMPILE="--host=i586-mingw32"

    qpushd $TMP_UDIR/icu-cross
        $TMP_UDIR/icu/source/configure ${HOST_XCOMPILE} \
            --prefix=$_ICU4C_UDIR \
            --with-cross-build=$TMP_UDIR/icu-native \
            --with-data-packaging=library \
            CC=$CC CPP="$CC -E" RANLIB=$RANLIB CXX=$TARGET-g++
        make  \
            CFLAGS="-g -O2 -std=gnu99" \
            CXXFLAGS="-g -O2 -std=gnu++98"
        make install
    qpopd
    if [ -f "$_ICU4C_UDIR/bin/uconv.exe" ]
    then
        echo "icu4c installed successfully."
    else
        die "icu4c installation falure"
    fi
}

reset_steps
add_step prepare
add_step inst_crossmingw
add_step prepare
add_step check_gnome
add_step check_gnutls
add_step inst_libxslt_gnome
add_step inst_enchant
add_step inst_libsoup
add_step inst_sqlite3
add_step inst_icu4c
add_step inst_webkit

eval_now

for step in "${steps[@]}" ; do
    eval $step
done
qpopd

echo -n "Build Finished at "
date

