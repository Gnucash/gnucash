#!/bin/sh
# This is a very, _very_, simple script to convert the .csv file into a .pot/.po.
# Its not clever but it took me 2 minutes to write :)
# Michael Twomey <michael.twomey@ireland.sun.com>
# 23 March 2001
# with slight GnuCash modifications by Christian Stimming <stimming@tuhh.de> 
# 19 Aug 2001
# and 04 Sep 2020 by Frank H. Ellenberger <frank.h.ellenberger@gmail.com>
#check args
if [ $# -eq 0 ]
then
	cat <<!
Usage: `basename $0` gnc-glossary.txt > gnucash-glossary.pot
!
	exit 1;
fi

GLOSSARY_CSV="$1";

if [ ! -f "$GLOSSARY_CSV" ]
then
	echo "Can't find $GLOSSARY_CSV.";
	exit 1;
fi

# Note: Line 2, 3, 9, 18: PROJECT, CHARSET and probably a few other variables are hardcoded
cat <<!
# SOME DESCRIPTIVE TITLE. (Glossary)
# This file is distributed under the same license as the GnuCash package.
# Copyright (C) YEAR by the GnuCash developers and the translators:
# FIRST AUTHOR <EMAIL@ADDRESS>, YEAR.
#
#, fuzzy
msgid ""
msgstr ""
"Project-Id-Version: GnuCash VERSION\n"
"Report-Msgid-Bugs-To: https://bugs.gnucash.org/enter_bug.cgi?"
"product=GnuCash&component=Translations\n"
"POT-Creation-Date: `date +'%Y-%m-%d %H:%M%z'`\n"
"PO-Revision-Date: YEAR-MO-DA HO:MI+ZONE\n"
"Last-Translator: FULL NAME <EMAIL@ADDRESS>\n"
"Language-Team: LANGUAGE <LL@li.org>\n"
"Language: \n"
"MIME-Version: 1.0\n"
"Content-Type: text/plain; charset=UTF-8\n"
"Content-Transfer-Encoding: 8bit\n"
"Plural-Forms: nplurals=INTEGER; plural=EXPRESSION;\n"

!

#Yes this is the most simple awk script you've ever seen :)
awk -F'\t' '{if ($2 != "") print "#. "$2; print "msgid "$1; print "msgstr \"\"\n"}' \
$GLOSSARY_CSV
