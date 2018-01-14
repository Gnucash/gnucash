#! /bin/bash

# Gnucash has borrowed a couple of source files from goffice.
# Those files contain a number of translatable strings. The
# goffice translation teams have already put effort in translating
# those in many languages. To reduce our translation effort, this
# script can be used to import these translations into our own po files.

# Some notes:
# 1. **Important** this script assumes it lives in the contrib directory
#    of the gnucash source tree. It should be called from there. Don't
#    move it to another location before calling it.
#
# 2. The script takes two parameters:
#    - the full path to the goffice sources
#    - the full path to the gnucash.pot to merge our po files against

usage() { echo "Usage: $0 goffice-src-dir gnucash-pot-file" 1>&2; exit 1; }

# Check for at least two command line arguments
if [[ "x$2" = "x" ]]
then
    usage
    exit 1
fi

goffice_base_dir="$1"
gnucash_pot_file="$2"

if [[ ! -d "$goffice_base_dir/po" ]]
then
    echo "'$goffice_base_dir'"
    echo "doesn't seem to contain a po directory."
    echo "Please double check the goffice-src-dir parameter you passed"
    exit 1
fi

if [[ ! -f "$gnucash_pot_file" ]]
then
    echo "'$gnucash_pot_file'"
    echo "doesn't seem to point to a gnucash.pot file."
    echo "Please double check the gnucash-pot-file parameter you passed"
    exit 1
fi

goffice_po_dir="$goffice_base_dir/po"

# Deduce path to our po files from the location of this script
gc_po_dir="$(dirname $0)/../po/"
for pofile in $(cd "$gc_po_dir"; ls *.po)
do
    if [[ -e "$goffice_po_dir/$pofile" ]]
    then
        gopofile="$pofile"
    else
        shortpofile=${pofile%%_*}.po
        if [[ "$shortpofile" != "$pofile" ]] && [[ -e "$goffice_po_dir/$shortpofile" ]]
        then
            echo "$pofile - doesn't exist in goffice but $shortpofile does, continuing with that one."
            gopofile="$shortpofile"
        else
            echo "$pofile - skipping because no equivalent found in goffice"
            continue
        fi
    fi

    echo "$pofile - importing translations from goffice's $gopofile..."
    msgcat --use-first -o update.po "$gc_po_dir/$pofile" "$goffice_po_dir/$gopofile"
    msgmerge update.po "$gnucash_pot_file" | msgattrib --no-obsolete > "$gc_po_dir/$pofile"
    rm update.po
done
