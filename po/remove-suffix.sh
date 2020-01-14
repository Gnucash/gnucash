#!/bin/bash

# This script modifies the *.po files, removing some chosen suffix
# from the msgstr of the given msgid. Use this if you removed e.g. a
# suffixing colon ":" from the original string in the source code
# (i.e. the msgid), and now for simplicity you want to remove them
# from all translations as well.

# Example: Say you changed the string from "Start Date:" to "Start
# Date" in the source. Now: 1. Create the most up-to-date template pot
# file using "make pot". 2. Update all po files using
# msgmerge. 3. Call this script as follows:
#   ./remove-suffix.sh Start Date
# This will modify all *.po files in-place, removing the suffixing
# colon in the translations.


# Use all command line arguments as the single msgid string
MSGID="$@"
# Optionally change the suffix you want to remove here
REMOVED_SUFFIX=":"

for PO in *.po ; do
#    echo perl -p0 -i -e"s^#, fuzzy\\n(msgid \"${MSGID}\"\\nmsgstr \".*)${REMOVED_SUFFIX}\"^\$1\"^" $PO
    perl -p0 -i -e"s^#, fuzzy\\n(msgid \"${MSGID}\"\\nmsgstr \".*)${REMOVED_SUFFIX}\"^\$1\"^" $PO
done
