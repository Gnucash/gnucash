SUBDIRS = . schemas binary-import qif qif-import ${OFX_DIR} ${HBCI_DIR} ${MT940_DIR} log-replay test
DIST_SUBDIRS = schemas binary-import qif qif-import qif-io-core ofx hbci mt940 log-replay test

pkglib_LTLIBRARIES=libgncmod-generic-import.la

libgncmod_generic_import_la_SOURCES = \
	import-account-matcher.c \
	import-commodity-matcher.c \
	import-backend.c \
	import-format-dialog.c \
	import-match-picker.c \
	import-parse.c \
	import-utilities.c \
	import-settings.c \
	import-match-map.c \
	import-main-matcher.c \
	gnc-import-desc-format.c \
	gnc-import-format-cb.c \
	gnc-import-format-gnome.c \
	gncmod-generic-import.c

gncincludedir = ${GNC_INCLUDE_DIR}
gncinclude_HEADERS = \
  import-match-map.h \
  gnc-import-desc-format.h \
  gnc-import-format-cb.h \
  import-parse.h

noinst_HEADERS = \
  import-account-matcher.h \
  import-backend.h \
  import-commodity-matcher.h \
  import-main-matcher.h \
  import-match-map.h \
  import-match-picker.h \
  import-settings.h \
  import-utilities.h \
  gnc-import-format-gnome.h

libgncmod_generic_import_la_LIBADD = \
  ${top_builddir}/src/gnome-utils/libgncmod-gnome-utils.la \
  ${top_builddir}/src/gnc-module/libgncmodule.la \
  ${top_builddir}/src/engine/libgncmod-engine.la \
  ${top_builddir}/src/register/ledger-core/libgncmod-ledger-core.la \
  ${top_builddir}/src/core-utils/libcore-utils.la \
  ${GLADE_LIBS} \
  ${QOF_LIBS} \
  ${GLIB_LIBS}

gladedir = ${GNC_GLADE_DIR}
glade_DATA = \
  generic-import.glade \
  import-provider-format.glade

AM_CFLAGS = \
  -I${top_srcdir}/src \
  -I${top_srcdir}/src/core-utils \
  -I${top_srcdir}/src/engine \
  -I${top_srcdir}/src/gnc-module \
  -I${top_srcdir}/src/app-utils \
  -I${top_srcdir}/src/gnome \
  -I${top_srcdir}/src/gnome-utils \
  ${G_WRAP_COMPILE_ARGS} \
  ${GNOME_CFLAGS} \
  ${GTKHTML_CFLAGS} \
  ${GDK_PIXBUF_CFLAGS} \
  ${GLADE_CFLAGS} \
  ${GUILE_INCS} \
  ${QOF_CFLAGS} \
  ${GLIB_CFLAGS} 

EXTRA_DIST = \
  $(glade_DATA) \
  generic-import-design.txt

if GNUCASH_SEPARATE_BUILDDIR
#For compiling
SCM_FILE_LINKS = 
#For executing test cases
SCM_FILE_LINKS +=
endif

noinst_DATA = .scm-links

.scm-links:
	rm -f g-wrapped gnucash generic-import import-export
	$(LN_S) -f . import-export
	$(LN_S) -f . gnucash
	$(LN_S) -f . generic-import
	$(LN_S) -f . g-wrapped
if GNUCASH_SEPARATE_BUILDDIR
	for X in ${SCM_FILE_LINKS} ; do \
	  $(LN_S) -f ${srcdir}/$$X . ; \
	done
endif
	touch .scm-links

CLEANFILES = gnucash generic-import g-wrapped .scm-links import-export
DISTCLEANFILES = ${SCM_FILE_LINKS}
