#!/bin/sh
#
# GnuCash shellscript functions for dist.sh
# 

function dist_prepare() {
    # this directory is hardcoded in gnucash.iss.in
    DIST_DIR=${INSTALL_DIR}\\..\\dist
    DIST_UDIR=`unix_path $DIST_DIR`
    DIST_WFSDIR=`win_fs_path $DIST_DIR`
    TMP_UDIR=`unix_path $TMP_DIR`
    if [ -x $DIST_DIR ]; then
        die "Please remove ${DIST_DIR} first"
    fi
    _UNZIP_UDIR=`unix_path $UNZIP_DIR`
    _AUTOTOOLS_UDIR=`unix_path $AUTOTOOLS_DIR`
    _GUILE_UDIR=`unix_path $GUILE_DIR`
    _WIN_UDIR=`unix_path $WINDIR`
    _EXETYPE_UDIR=`unix_path $EXETYPE_DIR`
    _GNOME_UDIR=`unix_path $GNOME_DIR`
    _PCRE_UDIR=`unix_path $PCRE_DIR`
    _LIBSOUP_UDIR=`unix_path $LIBSOUP_DIR`
    _ENCHANT_UDIR=`unix_path $ENCHANT_DIR`
    _LIBGSF_UDIR=`unix_path $LIBGSF_DIR`
    _GOFFICE_UDIR=`unix_path $GOFFICE_DIR`
    _OPENSP_UDIR=`unix_path $OPENSP_DIR`
    _LIBOFX_UDIR=`unix_path $LIBOFX_DIR`
    _LIBXSLT_UDIR=`unix_path $LIBXSLT_DIR`
    _GMP_UDIR=`unix_path $GMP_DIR`
    _GNUTLS_UDIR=`unix_path $GNUTLS_DIR`
    _GWENHYWFAR_UDIR=`unix_path $GWENHYWFAR_DIR`
    _AQBANKING_UDIR=`unix_path $AQBANKING_DIR`
    _SQLITE3_UDIR=`unix_path ${SQLITE3_DIR}`
    _MYSQL_LIB_UDIR=`unix_path ${MYSQL_LIB_DIR}`
    _PGSQL_UDIR=`unix_path ${PGSQL_DIR}`
    _LIBDBI_UDIR=`unix_path ${LIBDBI_DIR}`
    _LIBDBI_DRIVERS_UDIR=`unix_path ${LIBDBI_DRIVERS_DIR}`
    _LIBGDA_UDIR=`unix_path $LIBGDA_DIR`
    _GNUCASH_UDIR=`unix_path $GNUCASH_DIR`
    _REPOS_UDIR=`unix_path $REPOS_DIR`
    _BUILD_UDIR=`unix_path $BUILD_DIR`
    _MINGW_UDIR=`unix_path $MINGW_DIR`
    _INSTALL_UDIR=`unix_path $INSTALL_DIR`
    _INNO_UDIR=`unix_path $INNO_DIR`
    _WEBKIT_UDIR=`unix_path $WEBKIT_DIR`
    _ISOCODES_UDIR=`unix_path $ISOCODES_DIR`
    _MINGW_WFSDIR=`win_fs_path $MINGW_DIR`
    add_to_env $_UNZIP_UDIR/bin PATH # unzip
    add_to_env $_EXETYPE_UDIR/bin PATH # exetype

    _PID=$$
    configure_msys "$_PID" "$_MINGW_WFSDIR"
}

function dist_aqbanking() {
    setup aqbanking
    cp -a ${_AQBANKING_UDIR}/bin/*.exe ${DIST_UDIR}/bin
    cp -a ${_AQBANKING_UDIR}/bin/*.dll ${DIST_UDIR}/bin
    cp -a ${_AQBANKING_UDIR}/lib/aqbanking ${DIST_UDIR}/lib
    cp -a ${_AQBANKING_UDIR}/share/aqbanking ${DIST_UDIR}/share
    cp -a ${_AQBANKING_UDIR}/share/locale ${DIST_UDIR}/share
}

function dist_autotools() {
    setup Autotools
    mkdir -p $DIST_UDIR/bin
    cp -a $_AUTOTOOLS_UDIR/bin/*.dll $DIST_UDIR/bin
}

function dist_gmp() {
    setup gmp
    cp -a ${_GMP_UDIR}/bin/*.dll ${DIST_UDIR}/bin
}

function dist_gnome() {
    setup Gnome platform
    wget_unpacked $ATK_URL $DOWNLOAD_DIR $DIST_DIR
    wget_unpacked $CAIRO_URL $DOWNLOAD_DIR $DIST_DIR
    wget_unpacked $EXPAT_URL $DOWNLOAD_DIR $DIST_DIR
    wget_unpacked $FONTCONFIG_URL $DOWNLOAD_DIR $DIST_DIR
    wget_unpacked $FREETYPE_URL $DOWNLOAD_DIR $DIST_DIR
    wget_unpacked $GAIL_URL $DOWNLOAD_DIR $DIST_DIR
    wget_unpacked $GDK_PIXBUF_URL $DOWNLOAD_DIR $DIST_DIR
    wget_unpacked $GETTEXT_RUNTIME_URL $DOWNLOAD_DIR $DIST_DIR
    wget_unpacked $GLIB_URL $DOWNLOAD_DIR $DIST_DIR
    wget_unpacked $GTK_URL $DOWNLOAD_DIR $DIST_DIR
    wget_unpacked $LIBART_LGPL_URL $DOWNLOAD_DIR $DIST_DIR
    wget_unpacked $LIBGNOMECANVAS_URL $DOWNLOAD_DIR $DIST_DIR
    smart_wget $LIBICONV_URL $DOWNLOAD_DIR
    unzip -q $LAST_FILE bin/iconv.dll -d $DIST_DIR
    wget_unpacked $LIBJPEG_URL $DOWNLOAD_DIR $DIST_DIR
    wget_unpacked $LIBPNG_URL $DOWNLOAD_DIR $DIST_DIR
    wget_unpacked $LIBTIFF_URL $DOWNLOAD_DIR $DIST_DIR
    #wget_unpacked $LIBXML2_URL $DOWNLOAD_DIR $DIST_DIR
    wget_unpacked $PANGO_URL $DOWNLOAD_DIR $DIST_DIR
    wget_unpacked $ZLIB_URL $DOWNLOAD_DIR $DIST_DIR
    echo 'gtk-theme-name = "Nimbus"' > $DIST_DIR/etc/gtk-2.0/gtkrc

    wget_unpacked $GTK_THEME_URL $DOWNLOAD_DIR $TMP_DIR
    assert_one_dir $TMP_UDIR/gtk2-themes-*
    cp -a $TMP_UDIR/gtk2-themes-*/lib $DIST_DIR/
    cp -a $TMP_UDIR/gtk2-themes-*/share $DIST_DIR/
    rm -rf $TMP_UDIR/gtk2-themes-*

    wget_unpacked $GTK_PREFS_URL $DOWNLOAD_DIR $TMP_DIR
    assert_one_dir $TMP_UDIR/gtk2_prefs-*
    mv $TMP_UDIR/gtk2_prefs-*/gtk2_prefs.exe $DIST_DIR/bin
    rm -rf $TMP_UDIR/gtk2_prefs-*

    cp -a $_GNOME_UDIR/bin/libxml*.dll $DIST_DIR/bin

    if [ -d $DIST_UDIR/lib/locale ] ; then
        # Huh, is this removed in newer gtk?
        cp -a $DIST_UDIR/lib/locale $DIST_UDIR/share
        rm -rf $DIST_UDIR/lib/locale
    fi
}

function dist_gnutls() {
    setup gnutls
    cp -a ${_GNUTLS_UDIR}/bin/*.dll ${DIST_UDIR}/bin
    cp -a ${_GNUTLS_UDIR}/bin/*.exe ${DIST_UDIR}/bin
}

function dist_goffice() {
    setup GOffice
    mkdir -p $DIST_UDIR/bin
    cp -a $_GOFFICE_UDIR/bin/libgoffice*.dll $DIST_UDIR/bin
    mkdir -p $DIST_UDIR/lib
    cp -a $_GOFFICE_UDIR/lib/goffice $DIST_UDIR/lib
    mkdir -p $DIST_UDIR/share
    cp -a $_GOFFICE_UDIR/share/{goffice,locale,pixmaps} $DIST_UDIR/share
}

function dist_guile() {
    setup Guile
    mkdir -p $DIST_UDIR/bin
    cp -a $_GUILE_UDIR/bin/libguile{.,-srfi}*dll $DIST_UDIR/bin
    cp -a $_GUILE_UDIR/bin/guile.exe $DIST_UDIR/bin
    mkdir -p $DIST_UDIR/share
    cp -a $_GUILE_UDIR/share/guile $DIST_UDIR/share
}

function dist_gwenhywfar() {
    setup gwenhywfar
    cp -a ${_GWENHYWFAR_UDIR}/bin/*.dll ${DIST_UDIR}/bin
    mkdir -p ${DIST_UDIR}/etc
    cp -a ${_GWENHYWFAR_UDIR}/lib/gwenhywfar ${DIST_UDIR}/lib
    mkdir -p ${DIST_UDIR}/share
    cp -a ${_GWENHYWFAR_UDIR}/share/gwenhywfar ${DIST_UDIR}/share
}

function dist_isocodes() {
    setup isocodes
    mkdir -p $DIST_UDIR/share
    cp -a $_ISOCODES_UDIR/share/{locale,xml} $DIST_UDIR/share
}

function dist_ktoblzcheck() {
    setup ktoblzcheck
    # dll is already copied in dist_gwenhywfar
    cp -a ${_GWENHYWFAR_UDIR}/share/ktoblzcheck ${DIST_UDIR}/share
}

function dist_libdbi() {
    setup LibDBI
    cp -a ${_SQLITE3_UDIR}/bin/* ${DIST_UDIR}/bin
    cp -a ${_MYSQL_LIB_UDIR}/bin/*.{dll,manifest} ${DIST_UDIR}/bin
    cp -a ${_MYSQL_LIB_UDIR}/lib/*.dll ${DIST_UDIR}/bin
    cp -a ${_PGSQL_UDIR}/bin/* ${DIST_UDIR}/bin
    cp -a ${_PGSQL_UDIR}/lib/*.dll ${DIST_UDIR}/bin
    cp -a ${_LIBDBI_UDIR}/bin/* ${DIST_UDIR}/bin
    mkdir ${DIST_UDIR}/lib/dbd
    cp -a ${_LIBDBI_DRIVERS_UDIR}/lib/dbd/*.dll ${DIST_UDIR}/lib/dbd
}

function dist_libgsf() {
    setup libGSF
    mkdir -p $DIST_UDIR/bin
    cp -a $_LIBGSF_UDIR/bin/libgsf*.dll $DIST_UDIR/bin
    mkdir -p $DIST_UDIR/share
    cp -a $_LIBGSF_UDIR/share/locale $DIST_UDIR/share
}

function dist_libofx() {
    setup OpenSP and LibOFX
    cp -a ${_OPENSP_UDIR}/bin/*.dll ${DIST_UDIR}/bin
    cp -a ${_OPENSP_UDIR}/share/OpenSP ${DIST_UDIR}/share
    cp -a ${_LIBOFX_UDIR}/bin/*.dll ${DIST_UDIR}/bin
    cp -a ${_LIBOFX_UDIR}/bin/*.exe ${DIST_UDIR}/bin
    cp -a ${_LIBOFX_UDIR}/share/libofx ${DIST_UDIR}/share
}

function dist_openssl() {
    setup OpenSSL
    _OPENSSL_UDIR=`unix_path $OPENSSL_DIR`
    mkdir -p $DIST_UDIR/bin
    cp -a $_OPENSSL_UDIR/bin/*.dll $DIST_UDIR/bin
}

function dist_pcre() {
    setup pcre
    mkdir -p $DIST_UDIR/bin
    cp -a $_PCRE_UDIR/bin/pcre3.dll $DIST_UDIR/bin
}

function dist_regex() {
    setup RegEx
    smart_wget $REGEX_URL $DOWNLOAD_DIR
    unzip -q $LAST_FILE bin/libgnurx-0.dll -d $DIST_DIR
}

function dist_webkit() {
    setup WebKit
    cp -a ${_LIBSOUP_UDIR}/bin/* ${DIST_UDIR}/bin
    cp -a ${_LIBXSLT_UDIR}/bin/* ${DIST_UDIR}/bin
    cp -a ${_ENCHANT_UDIR}/bin/* ${DIST_UDIR}/bin
    cp -a ${_WEBKIT_UDIR}/bin/* ${DIST_UDIR}/bin
}

function dist_gnucash() {
    setup GnuCash
    mkdir -p $DIST_UDIR/bin
    cp $_MINGW_UDIR/bin/pthreadGC2.dll $DIST_UDIR/bin
    cp -a $_INSTALL_UDIR/bin/* $DIST_UDIR/bin
    mkdir -p $DIST_UDIR/etc/gnucash
    cp -a $_INSTALL_UDIR/etc/gnucash/* $DIST_UDIR/etc/gnucash
    cp -a $_INSTALL_UDIR/lib/lib*.la $DIST_UDIR/bin
    mkdir -p $DIST_UDIR/share
    cp -a $_INSTALL_UDIR/share/{doc,gnucash,locale,glib-2.0} $DIST_UDIR/share
    cp -a $_REPOS_UDIR/packaging/win32/{getperl.vbs,gnc-path-check,install-fq-mods.cmd} $DIST_UDIR/bin

    _QTDIR_WIN=$(unix_path $QTDIR | sed 's,^/\([A-Za-z]\)/,\1:/,g' )
    # aqbanking >= 5.0.0
    AQBANKING_VERSION_H=${_AQBANKING_UDIR}/include/aqbanking5/aqbanking/version.h
    GWENHYWFAR_VERSION_H=${_GWENHYWFAR_UDIR}/include/gwenhywfar4/gwenhywfar/version.h

    _AQBANKING_SO_EFFECTIVE=$(awk '/AQBANKING_SO_EFFECTIVE / { print $3 }' ${AQBANKING_VERSION_H} )
    _GWENHYWFAR_SO_EFFECTIVE=$(awk '/GWENHYWFAR_SO_EFFECTIVE / { print $3 }' ${GWENHYWFAR_VERSION_H} )
    sed < $_BUILD_UDIR/packaging/win32/gnucash.iss \
        > $_GNUCASH_UDIR/gnucash.iss \
        -e "s#@-qtbindir-@#${_QTDIR_WIN}/bin#g" \
	-e "s#@-gwenhywfar_so_effective-@#${_GWENHYWFAR_SO_EFFECTIVE}#g" \
	-e "s#@-aqbanking_so_effective-@#${_AQBANKING_SO_EFFECTIVE}#g"
}

function dist_finish() {
    # Strip redirections in distributed libtool .la files
    for file in $DIST_UDIR/bin/*.la; do
        cat $file | sed 's,^libdir=,#libdir=,' > $file.new
        mv $file.new $file
    done

    echo "Now running the Inno Setup Compiler for creating the setup.exe"
    ${_INNO_UDIR}/iscc //Q ${_GNUCASH_UDIR}/gnucash.iss

    if [ "$BUILD_FROM_TARBALL" = "no" ]; then
        # And changing output filename
        PKG_VERSION=`grep PACKAGE_VERSION ${_BUILD_UDIR}/config.h | cut -d" " -f3 | cut -d\" -f2 `
        REVISION=`grep GNUCASH_SCM_REV ${_BUILD_UDIR}/src/core-utils/gnc-scm-info.h | cut -d" " -f3 | cut -d\" -f2 `
        if [ "$REPOS_TYPE" = "svn" ]; then
          SETUP_FILENAME="gnucash-${PKG_VERSION}-${REPOS_TYPE}-r${REVISION}-setup.exe"
        else
          SETUP_FILENAME="gnucash-${PKG_VERSION}-$(date +'%Y-%m-%d')-${REPOS_TYPE}-${REVISION}-setup.exe"
        fi
        qpushd ${_GNUCASH_UDIR}
            mv gnucash-${PKG_VERSION}-setup.exe ${SETUP_FILENAME}
        qpopd
        echo "Final resulting Setup program is:"
        echo ${_GNUCASH_UDIR}/${SETUP_FILENAME}
    fi
}

### Local Variables: ***
### sh-basic-offset: 4 ***
### indent-tabs-mode: nil ***
### End: ***
