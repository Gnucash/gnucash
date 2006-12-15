#!/bin/sh

set -e

function qpushd() { pushd "$@" >/dev/null; }
function qpopd() { popd >/dev/null; }
function unix_path() { echo "$*" | sed 's,^\([A-Za-z]\):,/\1,;s,\\,/,g'; }

qpushd "$(dirname $(unix_path "$0"))"
. functions
. custom.sh

function prepare() {
    DIST_DIR=${GNUCASH_DIR}\\dist
    DIST_UDIR=`unix_path $DIST_DIR`
    DIST_WFSDIR=`win_fs_path $DIST_DIR`
    TMP_UDIR=`unix_path $TMP_DIR`
    if [ -x $DIST_DIR ]; then
        die "Please remove ${DIST_DIR} first"
    fi
}

function dist_regex() {
    setup RegEx
    smart_wget $REGEX_BIN_URL $DOWNLOAD_DIR
    unzip -q $LAST_FILE bin/regex.dll -d $DIST_DIR
}

function dist_autotools() {
    setup Autotools
    _AUTOTOOLS_UDIR=`unix_path $AUTOTOOLS_DIR`
    mkdir -p $DIST_UDIR/bin
    cp $_AUTOTOOLS_UDIR/bin/*.dll $DIST_UDIR/bin
}

function dist_guile() {
    setup Guile
    _GUILE_UDIR=`unix_path $GUILE_DIR`
    mkdir -p $DIST_UDIR/bin
    cp -a $_GUILE_UDIR/bin/libguile{,-ltdl,-srfi}*dll $DIST_UDIR/bin
    mkdir -p $DIST_UDIR/share
    cp -r $_GUILE_UDIR/share/guile $DIST_UDIR/share
}

function dist_openssl() {
    setup OpenSSL
    _WIN_UDIR=`unix_path $WINDIR`
    mkdir -p $DIST_UDIR/bin
    cp $_WIN_UDIR/system32/lib{eay,ssl}*.dll $DIST_UDIR/bin
}

function dist_libxml2() {
    setup LibXML2
    _LIBXML2_UDIR=`unix_path $LIBXML2_DIR`
    mkdir -p $DIST_UDIR/bin
    cp $_LIBXML2_UDIR/bin/libxml2.dll $DIST_UDIR/bin
}

function dist_gnome() {
    setup Gnome platform
    _GNOME_UDIR=`unix_path $GNOME_DIR`
    wget_unpacked $GETTEXT_URL $DOWNLOAD_DIR $DIST_DIR
    smart_wget $LIBICONV_URL $DOWNLOAD_DIR
    unzip -q $LAST_FILE bin/iconv.dll -d $DIST_DIR
    wget_unpacked $GLIB_URL $DOWNLOAD_DIR $DIST_DIR
    wget_unpacked $LIBJPEG_URL $DOWNLOAD_DIR $DIST_DIR
    smart_wget $LIBPNG_URL $DOWNLOAD_DIR
    unzip -q $LAST_FILE bin/libpng13.dll -d $DIST_DIR
    smart_wget $ZLIB_URL $DOWNLOAD_DIR
    unzip -q $LAST_FILE zlib1.dll -d $DIST_DIR\\bin
    wget_unpacked $CAIRO_URL $DOWNLOAD_DIR $DIST_DIR
    wget_unpacked $EXPAT_URL $DOWNLOAD_DIR $DIST_DIR
    wget_unpacked $FONTCONFIG_URL $DOWNLOAD_DIR $DIST_DIR
    wget_unpacked $FREETYPE_URL $DOWNLOAD_DIR $DIST_DIR
    wget_unpacked $ATK_URL $DOWNLOAD_DIR $DIST_DIR
    wget_unpacked $PANGO_URL $DOWNLOAD_DIR $DIST_DIR
    wget_unpacked $LIBART_LGPL_URL $DOWNLOAD_DIR $DIST_DIR
    wget_unpacked $GTK_URL $DOWNLOAD_DIR $DIST_DIR
    wget_unpacked $ORBIT2_URL $DOWNLOAD_DIR $DIST_DIR
    wget_unpacked $GAIL_URL $DOWNLOAD_DIR $DIST_DIR
    wget_unpacked $POPT_URL $DOWNLOAD_DIR $DIST_DIR
    wget_unpacked $GCONF_URL $DOWNLOAD_DIR $DIST_DIR
    wget_unpacked $LIBBONOBO_URL $DOWNLOAD_DIR $DIST_DIR
    wget_unpacked $GNOME_VFS_URL $DOWNLOAD_DIR $DIST_DIR
    wget_unpacked $LIBGNOME_URL $DOWNLOAD_DIR $DIST_DIR
    wget_unpacked $LIBGNOMECANVAS_URL $DOWNLOAD_DIR $DIST_DIR
    wget_unpacked $LIBBONOBOUI_URL $DOWNLOAD_DIR $DIST_DIR
    wget_unpacked $LIBGNOMEUI_URL $DOWNLOAD_DIR $DIST_DIR
    wget_unpacked $LIBGLADE_URL $DOWNLOAD_DIR $DIST_DIR
    wget_unpacked $LIBGNOMEPRINT_URL $DOWNLOAD_DIR $DIST_DIR
    wget_unpacked $LIBGNOMEPRINTUI_URL $DOWNLOAD_DIR $DIST_DIR
    wget_unpacked $GTKHTML_URL $DOWNLOAD_DIR $DIST_DIR
    rm -rf $DIST_UDIR/etc/gconf/gconf.xml.defaults/{desktop,schemas}
}

function dist_libgsf() {
    setup libGSF
    _LIBGSF_UDIR=`unix_path $LIBGSF_DIR`
    mkdir -p $DIST_UDIR/bin
    cp $_LIBGSF_UDIR/bin/libgsf*.dll $DIST_UDIR/bin
    mkdir -p $DIST_UDIR/etc/gconf/schemas
    cp $_LIBGSF_UDIR/etc/gconf/schemas/* $DIST_UDIR/etc/gconf/schemas
    mkdir -p $DIST_UDIR/lib
    cp -r $_LIBGSF_UDIR/lib/locale $DIST_UDIR/lib
}

function dist_goffice() {
    setup GOffice
    _GOFFICE_UDIR=`unix_path $GOFFICE_DIR`
    mkdir -p $DIST_UDIR/bin
    cp $_GOFFICE_UDIR/bin/libgoffice*.dll $DIST_UDIR/bin
    mkdir -p $DIST_UDIR/lib
    cp -r $_GOFFICE_UDIR/lib/{goffice,locale} $DIST_UDIR/lib
    mkdir -p $DIST_UDIR/share
    cp -r $_GOFFICE_UDIR/share/{goffice,pixmaps} $DIST_UDIR/share
}

function dist_gnucash() {
    setup GnuCash
    _GNUCASH_UDIR=`unix_path $GNUCASH_DIR`
    mkdir -p $DIST_UDIR/bin
    cp $_GNUCASH_UDIR/bin/* $DIST_UDIR/bin
    mkdir -p $DIST_UDIR/etc/gconf/schemas
    cp $_GNUCASH_UDIR/etc/gconf/schemas/* $DIST_UDIR/etc/gconf/schemas
    mkdir -p $DIST_UDIR/lib
    cp -r $_GNUCASH_UDIR/lib/{bin,locale} $DIST_UDIR/lib
    cp $_GNUCASH_UDIR/lib/lib*.la $DIST_UDIR/lib
    mkdir -p $DIST_UDIR/lib/gnucash
    cp $_GNUCASH_UDIR/lib/gnucash/lib*.{dll,la} $DIST_UDIR/lib/gnucash
    cp -r $_GNUCASH_UDIR/libexec $DIST_UDIR
    mkdir -p $DIST_UDIR/share
    cp -r $_GNUCASH_UDIR/share/{gnucash,pixmaps,xml} $DIST_UDIR/share
}

function finish() {
    for file in $DIST_UDIR/etc/gconf/schemas/*.schemas; do
        echo -n "Installing $file ... "
        gconftool-2 \
            --config-source=xml:merged:${DIST_WFSDIR}/etc/gconf/gconf.xml.defaults \
            --install-schema-file $file >/dev/null
        echo "done"
    done
}

prepare
dist_regex
dist_autotools
dist_guile
dist_openssl
dist_libxml2
dist_gnome
dist_libgsf
dist_goffice
dist_gnucash
finish
qpopd


### Local Variables: ***
### sh-basic-offset: 4 ***
### tab-width: 8 ***
### End: ***
