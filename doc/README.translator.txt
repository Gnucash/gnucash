This document explains how to create a GnuCash translation file for
use with GNU gettext.

   by Yannick  LE  NY <y-le-ny@ifrance.com>, the French translator
                           V1.0 - 20000707


1) Take the file gnucash.pot, edit it with any text editor and save it
   as 'CODE-COUNTRY.po' (it must be a plain text file only not formatted text)
   where the CODE-COUNTRY is two or more letters which denote the language
   and country for the translation.

   For example: 'it' for Italy, 'fr' for France, 'de' for Germany,
                and fr_BE.po for people in Belgium who speak French.

                For france, the file is 'fr.po'.


2) Now at the top of the file, you have this:

# SOME DESCRIPTIVE TITLE.
# Copyright (C) YEAR Free Software Foundation, Inc.
# FIRST AUTHOR <EMAIL@ADDRESS>, YEAR.

You need to update this with the current information.

For example in the de.po file, we have this:

# Messages in Deutsch für GnuCash
# Copyright (C) 1999 Free Software Foundation, Inc.
# Jan-Uwe Finck <Jan-Uwe.Finck@bigfoot.de>, 1999.


3) Next, you need to translate each English string into the target
   language, for example:

Before:

#: messages-i18n.c:11
msgid ""
"The GnuCash personal finance manager.\n"
"The GNU way to manage your money!"
msgstr ""

After, the tranlation in the de.po file:

#: messages-i18n.c:11
msgid ""
"The GnuCash personal finance manager.\n"
"The GNU way to manage your money!"
msgstr ""
"GnuCash: Ihr persönlicher Finanzmanager.\n"
"Der GNU-Weg, ihr Geld zu verwalten !"


4) When you are ready to test out the strings you have translated,
   save the file.


5) Now, if you have the gettext package installed, you can compile your
   po file with this command (run it in the gnucash/po directory):

msgfmt fr.po --output=./gnucash.mo

   This will create the file 'gnucash.mo' which you can copy to the
   appropriate locale directory for your installation. On a RedHat 6.1
   installation, the directory is /usr/share/locale/fr/LC_MESSAGES.

When running GnuCash, you must set the appropriate locale environment
variables:

In French, with bash:
    export LANG=fr_FR
    
In French, with tcsh:
    setenv LANG fr_FR

6) Once you have a finished, working po file, need to compress your file
   with gzip. On command line, type 'gzip fr.po' (for the French file) and
   now you have a new compressed file named 'fr.po.gz'.
   Email this file to gnucash-patches@gnucash.org.


------------------------------------------------------------------------------
Remarks

You can get more information about gettext and the po file format in
the 'info' pages for GNU gettext. Type 'info gettext' at the command
line.

Dave Peticolas <dave@krondo.com>, the CVS maintainer for GnuCash,
regularly updates the po files, and you may need to add some
translations to the file or correct some strings.

In the updated po files, you should not have the word 'fuzzy',
otherwise GnuCash will not use the translated string.

Two examples from the file de.po:

1) You have this:

#: messages-i18n.c:35
#, fuzzy, c-format
msgid ""
"There was an error writing the file\n"
"     %s\n"
"\n"
"%s"
msgstr ""
"Es gab einen Fehler beim Öffnen der Datei. \n"
"     %s."

You need to correct the translated string and remove the 'fuzzy' keyword.
For example:

#: messages-i18n.c:35
#, c-format
msgid ""
"There was an error writing the file\n"
"     %s\n"
"\n"
"%s"
msgstr ""
"Es gab einen Fehler beim Öffnen der Datei. \n"
"     %s."

2) You have this:

#: messages-i18n.c:251
#, fuzzy
msgid "Show Income/Expense"
msgstr "Einnahmen/Ausgaben anzeigen"

You need to correct the translated string and remove the 'fuzzy' keyword.
For example:

#: messages-i18n.c:251
msgid "Show Income/Expense"
msgstr "Einnahmen/Ausgaben anzeigen"
