This document describes the different things needed to translate
GnuCash for a particular locale.

----------------------------------------------------------------------
This section explains how to create a GnuCash translation file for
use with GNU gettext.

   by Yannick  LE  NY <y-le-ny@ifrance.com>, the French translator
                           V1.1 - 20000813


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
   save the file in the 'po' directory of the gnucash source tree.
   You can test if your file is good with this command line:
   msgfmt -c -v -o /dev/null FILE.po where FILE is the CODE-COUNTRY.
   If you have fuzzy errors, take a look at the end of this document.


5) Now, if you have the gettext package installed, you can compile your
   po file with this command (run it in the gnucash/po directory):

msgfmt fr.po --output=./gnucash.mo

   This will create the file 'gnucash.mo' which you can copy to the
   appropriate locale directory for your installation. On a RedHat 6.1
   
   installation, the directory is /usr/share/locale/fr/LC_MESSAGES.
   Alternatively, instead of creating and installing the gnucash.mo
   file by hand, you can edit the file 'configure.in' and the file 
   'configure' and add your language string ('it', 'fr', etc.) to 
   the definition of the ALL_LINGUAS variable.
   If you add the be.po file, the old line is in the 2 files:
   ALL_LINGUAS = "de en_GB fr it ja ru sv"
   and the new line is in the 2 files:
   ALL_LINGUAS = "de en_GB fr it ja ru sv be"
   Now rerun 'make' and 'make install' to build and install the
   gnucash.mo file.

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

----------------------------------------------------------------------
This section describes the actions needed to translate the manual.

1) Create a new directory doc/sgml/<locale> (where <locale> is
   something like es, en_GB, or pt_PT).
2) Copy the files from doc/sgml/C into this directory.
3) Recreate the image files in doc/sgml/C/gnucash so that they are
   appropriate to the locale.
4) Edit all the sgml files and translate for the locale.

----------------------------------------------------------------------
This section describes the actions needed to translate the files
containing the new account hierarchies.

1) Create a new directory accounts/<locale>.
2) Copy the files from accounts/C to accounts/<locale>
3) Do not change any xml tags.
For each file:
4) Change the gnc-act:title, gnc-act:short-description, and
   gnc-act:long-description to contain appropriately translated text.
   Do not add any newlines in the long description except at the end
   and begining of the string.
5) For each gnc:account in the file translate the act:name, and
   act:description fields.  Please do not translate any other fields.

----------------------------------------------------------------------

Thanks so very much to all the translators for their hard effort and
excellent work.
