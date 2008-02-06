[ "$__SOURCED_FUNCTIONS" ] && return
__SOURCED_FUNCTIONS=1

function set_default() {
    local _varname=$1; shift
    if [ -z "`eval echo '"$'"$_varname"'"'`" ]; then
        eval "$_varname"'="'"$*"'"'
    fi
}

function block_step() { blocked_steps=("${blocked_steps[@]}" "$@"); }
function reset_steps() { steps=(); block_steps=(); }
function add_step() {
    while [ "$1" ]; do
        _is_blocked=
        for blocked in "${blocked_steps[@]}"; do
            if [ "$blocked" = "$1" ]; then
                _is_blocked=yes
                break
            fi
        done
        if [ "$_is_blocked" != "yes" ]; then
            steps=("${steps[@]}" "$1")
            block_step "$1"
        fi
        shift
    done
}

function late_eval() { late_cmds=("${late_cmds[@]}" "$@"); }
function eval_now() {
    for cmd in "${late_cmds[@]}"; do
        eval $cmd
    done
}

function quiet() { "$@" &>/dev/null; }

# c:/dir/sub
function win_fs_path() { echo "$*" | sed 's,\\,/,g'; }

# usage:  wpwd [rel]
# rel can be any relative path
function wpwd() {
    qpushd `unix_path "${1:-.}"`
        pwd -W
    qpopd
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
    # If WGET_RATE is set (in bytes/sec), limit download bandwith
    if [ ! -z "$WGET_RATE" ] ; then
            wget --passive-ftp -c $1 -P $TMP_UDIR --limit-rate=$WGET_RATE
        else
            wget --passive-ftp -c $1 -P $TMP_UDIR
        fi
    mv $TMP_UDIR/$_FILE $_DLD
    fi
    LAST_FILE=$_DLD/$_FILE
}

# usage:  wget_unpacked URL DOWNLOAD_DIR UNPACK_DIR
function wget_unpacked() {
    smart_wget $1 $2
    _UPD=`unix_path $3`
    echo -n "Extracting ${LAST_FILE##*/} ... "
    case $LAST_FILE in
        *.zip)     unzip -q -o $LAST_FILE -d $_UPD;;
        *.tar.gz)  tar -xzpf $LAST_FILE -C $_UPD;;
        *.tar.bz2) tar -xjpf $LAST_FILE -C $_UPD;;
        *)         die "Cannot unpack file $LAST_FILE!";;
    esac
    echo "done"
}

function setup() {
    echo
    echo "############################################################"
    echo "###  $*"
    echo "############################################################"
}

function die() {
    echo
    [ "$*" ] && echo "!!! $* !!!"
    echo "!!! ABORTING !!!"
    exit -1
}

# usage: register_env_var NAME SEPARATOR [DEFAULT]
function register_env_var() {
    [ $# -ge 2 -a $# -le 3 ] || die hard
    eval "SEPS_$1"'="'"$2"'"'
    if [ $# -eq 3 ]; then
        eval "$1_BASE=$3"
    else
        eval "$1_BASE"'=$'"$1"
    fi
    eval "$1_ADDS="
    eval export "$1"
    ENV_VARS="$ENV_VARS $1"
}
ENV_VARS=

# usage: add_to_env VALUE NAME
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

# usage: set_env_or_die VALUE NAME
# like add_to_env, but die if $NAME has been set to a different value
function set_env_or_die() {
    _OLDADDS=`eval echo '"$'"$2_ADDS"'"'`
    add_to_env "$1" "$2"
    _NEWADDS=`eval echo '"$'"$2_ADDS"'"'`
    if [ "$_OLDADDS" != "$_NEWADDS" ]; then
        _BASE=`eval echo '"$'"$2_BASE"'"'`
        if [ "$_BASE" ]; then
            _ENV=`eval echo '"$'"$2"'"'`
            echo "Must not overwrite environment variable '$2' (${_OLDADDS}${_BASE}) by '$1'."
            echo "Try to remove the offending installed software or unset the variable."
            die
        fi
    fi
}

# usage set_env VALUE NAME
# like $NAME=$VALUE, but also reset env tracking variables
function set_env() {
    eval "$2=$1"
    eval "$2_BASE="
    eval "$2_ADDS=$1"
}

function assert_one_dir() {
    quiet [ -d "$@" ] || die "Detected multiple directories where only one was expected; please delete all but the latest one: $@"
}

### Local Variables: ***
### mode: shell-script ***
### sh-basic-offset: 4 ***
### indent-tabs-mode: nil ***
### End: ***
