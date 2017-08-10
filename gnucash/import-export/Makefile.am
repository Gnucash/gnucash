if WITH_OFX
    OFX_DIR=ofx
endif
if WITH_AQBANKING
    AQBANKING_DIR=aqb
endif
SUBDIRS = . gschemas qif qif-imp \
	${OFX_DIR} ${AQBANKING_DIR} log-replay test csv-imp csv-exp

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
	import-main-matcher.c \
	import-pending-matches.c \
	gncmod-generic-import.c

gncincludedir = ${GNC_INCLUDE_DIR}
gncinclude_HEADERS = \
  import-parse.h

noinst_HEADERS = \
  import-account-matcher.h \
  import-backend.h \
  import-commodity-matcher.h \
  import-main-matcher.h \
  import-match-picker.h \
  import-settings.h \
  import-pending-matches.h \
  import-utilities.h

libgncmod_generic_import_la_LDFLAGS = -avoid-version

libgncmod_generic_import_la_LIBADD = \
  ${top_builddir}/gnucash/gnome-utils/libgncmod-gnome-utils.la \
  ${top_builddir}/libgnucash/app-utils/libgncmod-app-utils.la \
  ${top_builddir}/libgnucash/engine/libgncmod-engine.la \
  ${top_builddir}/libgnucash/core-utils/libgnc-core-utils.la \
  ${top_builddir}/libgnucash/gnc-module/libgnc-module.la \
  ${REGEX_LIBS} \
  ${GLIB_LIBS}

gtkbuilderdir = ${GNC_GTKBUILDER_DIR}
gtkbuilder_DATA = dialog-import.glade

AM_CPPFLAGS = \
  -I${top_srcdir}/common \
  -I${top_srcdir}/libgnucash/core-utils \
  -I${top_srcdir}/libgnucash/engine \
  -I${top_srcdir}/libgnucash/gnc-module \
  -I${top_srcdir}/libgnucash/app-utils \
  -I${top_srcdir}/gnucash/gnome \
  -I${top_srcdir}/gnucash/gnome-utils \
  ${GTK_CFLAGS} \
  ${GLIB_CFLAGS}

EXTRA_DIST = \
  $(gtkbuilder_DATA) \
  generic-import-design.txt \
  CMakeLists.txt

AM_CPPFLAGS += -DG_LOG_DOMAIN=\"gnc.i-e\"
