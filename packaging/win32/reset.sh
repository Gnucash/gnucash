#!/bin/sh

set -e

####  Load defaults.sh

function qpushd() { pushd "$@" >/dev/null; }
function qpopd() { popd >/dev/null; }
function unix_path() { echo "$*" | sed 's,^\([A-Za-z]\):,/\1,;s,\\,/,g'; }

qpushd "$(dirname $(unix_path "$0"))"
. functions.sh
. defaults.sh

## too bad, bash 2.04 has no real support for arrays

# 0 = get confirmation
# 1 = delete
_phase=0

# usage: add_precious_path <step> <path>
function add_precious_path() {
    _precious_paths="${_precious_paths} $1|$2"
}

add_precious_path msys $MSYS_DIR
add_precious_path wget $WGET_DIR
add_precious_path svn $SVN_DIR
add_precious_path repos $REPOS_DIR
add_precious_path hh $HH_DIR

# usage: eval_path <path> <force>
function eval_path() {
    if [ "$2" != "force" ]; then
        for _precious in $_precious_paths; do
            _prec_step="${_precious%%|*}"
            _prec_path="${_precious##*|}"
            _preclen="${#_prec_path}"
            _dirss="${1:0:${_preclen}}"
            _dirlen="${#1}"
            _precss="${_prec_path:0:${_dirlen}}"
            if [ "$_dirss" = "$_prec_path" -o "$_precss" = "$1" ]; then
                if [ "$_phase" != "1" ]; then
                    echo " - will not remove $1 to save $_prec_path ($_prec_step)"
                fi
                return
            fi
        done
    fi
    if [ "$_phase" = "1" ]; then
        echo rm -rf $1
        rm -rf $1
    else
        echo " * rm -rf $1"
    fi
}

function eval_all() {
    eval_path $UNZIP_DIR
    eval_path $REGEX_DIR
    eval_path $READLINE_DIR
    eval_path $ACTIVE_PERL_DIR
    eval_path $AUTOTOOLS_DIR
    eval_path $GUILE_DIR
    eval_path $OPENSSL_DIR
    eval_path $MINGW_UTILS_DIR
    eval_path $EXETYPE_DIR
    eval_path $LIBXML2_DIR
    eval_path $GNOME_DIR
    eval_path $SWIG_DIR
    eval_path $PCRE_DIR
    eval_path $LIBGSF_DIR
    eval_path $GOFFICE_DIR
    eval_path $GLADE_DIR
    eval_path $INNO_DIR
    eval_path $HH_DIR
    eval_path $OPENSP_DIR
    eval_path $LIBOFX_DIR
    eval_path $GWENHYWFAR_DIR
    eval_path $AQBANKING_DIR
    eval_path $LIBGDA_DIR
    eval_path $SQLITE3_DIR
    eval_path $LIBDBI_DIR
    eval_path $LIBDBI_DRIVERS_DIR
    eval_path $BUILD_DIR
    eval_path $INSTALL_DIR
    eval_path $GNUCASH_DIR\\dist
    eval_path $TMP_DIR
    eval_path $MSYS_DIR\\etc\\profile.d\\installer.sh force
    eval_path $DOCS_DIR
    eval_path $LIBXSLT_DIR
}

echo
echo "This will reset your installation of gnucash."
echo "The following tasks will be executed:"

while true; do
    echo
    eval_all
    echo
    echo -n "Are you sure you want to do this:  Yes, first add safe paths or no?  [y/s/N] "
    read resp
    case "$resp" in
        y*|Y*)
            _phase=1
            break
            ;;
        s*|S*)
            echo
            echo -n "Add safe path (e.g. c:\\\\soft\\\\tmp): "
            read path
            [ "$path" ] && add_precious_path user_defined $path
            ;;
        *)
            exit 0
            ;;
    esac
done

####  Now clear out the install

echo
qpushd $GLOBAL_DIR
    eval_all
qpopd

echo "Done"
exit 0

### Local Variables: ***
### sh-basic-offset: 4 ***
### indent-tabs-mode: nil ***
### End: ***
