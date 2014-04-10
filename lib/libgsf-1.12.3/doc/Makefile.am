## Process this file with automake to produce Makefile.in

# The name of the module.
DOC_MODULE=gsf

# The top-level SGML file.
DOC_MAIN_SGML_FILE=gsf-docs.sgml

# The directory containing the source code. Relative to $(srcdir)
DOC_SOURCE_DIR=../gsf

# Extra options to pass to gtkdoc-scangobj
SCANGOBJ_OPTIONS= --nogtkinit --type-init-func="g_type_init ()" 

# Extra options to supply to gtkdoc-scan
SCAN_OPTIONS=

# Extra options to supply to gtkdoc-mkdb
MKDB_OPTIONS=--sgml-mode --output-format=xml

# Extra options to supply to gtkdoc-fixref
FIXXREF_OPTIONS=

# Used for dependencies
HFILE_GLOB=$(top_srcdir)/gsf/*.h
CFILE_GLOB=$(top_srcdir)/gsf/*.c

# Header files to ignore when scanning
IGNORE_HFILES=			 \
	biff-types.h		 \
	gsf-command-context.h	 \
	gsf-infile-msvba.h	 \
	gsf-io-context.h	 \
	gsf-output-transaction.h \
	gsf-infile-ar.h 	\
	gsf-infile-tar.h 	\
	gsf-infile-tar.c 	\
	gsf-outfile-tar.h	\
	gsf-outfile-tar.c

EXTRA_HFILES = ../gsf-gnome/*.h

# Images to copy into HTML directory
HTML_IMAGES =

# Extra SGML files that are included by $(DOC_MAIN_SGML_FILE)
content_files =

# Other files to distribute
extra_files =

# CFLAGS and LDFLAGS for compiling scan program. Only needed
# if $(DOC_MODULE).types is non-empty.
GTKDOC_CFLAGS = -I$(top_srcdir) $(LIBGSF_CFLAGS)
GTKDOC_LIBS = $(top_builddir)/gsf/libgsf-1.la $(LIBGSF_LIBS)

manpagedir   = $(datadir)/man/man1
manpage_DATA = gsf-office-thumbnailer.1

if GTK_DOC_INSTALLED
include $(top_srcdir)/lib/libgsf-1.12.3/gtk-doc.make
CLEANFILES += gsf-scan.*
.PHONY : dist-hook-local
EXTRA_DIST += $(manpage_DATA)
else
EXTRA_DIST = $(manpage_DATA)
endif
