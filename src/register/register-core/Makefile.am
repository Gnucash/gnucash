SUBDIRS = . test
PWD := $(shell pwd)

pkglib_LTLIBRARIES = libgncmod-register-core.la libgw-register-core.la

libgncmod_register_core_la_LIBADD = \
  ${top_builddir}/src/gnc-module/libgncmodule.la \
  ${top_builddir}/src/engine/libgncmod-engine.la \
  ${top_builddir}/src/app-utils/libgncmod-app-utils.la \
  ${top_builddir}/src/gnome-utils/libgncmod-gnome-utils.la \
  ${QOF_LIBS} ${GLIB_LIBS}

libgncmod_register_core_la_SOURCES = \
  gncmod-register-core.c \
  basiccell.c \
  cell-factory.c \
  cellblock.c \
  formulacell.c \
  gtable.c \
  numcell.c \
  pricecell.c \
  quickfillcell.c \
  recncell.c \
  checkboxcell.c \
  register-common.c \
  table-allgui.c \
  table-control.c \
  table-layout.c \
  table-model.c


gncincludedir = ${GNC_INCLUDE_DIR}
gncinclude_HEADERS = \
  basiccell.h \
  cell-factory.h \
  cellblock.h \
  combocell.h \
  datecell.h \
  formulacell.h \
  gtable.h \
  numcell.h \
  pricecell.h \
  quickfillcell.h \
  recncell.h \
  checkboxcell.h \
  register-common.h \
  table-allgui.h \
  table-control.h \
  table-layout.h \
  table-model.h

libgw_register_core_la_SOURCES = gw-register-core.c

gwmoddir = ${GNC_GWRAP_LIBDIR}
gwmod_DATA = gw-register-core.scm gw-register-core-spec.scm

noinst_HEADERS = \
  gw-register-core.h

EXTRA_DIST = \
  ${gwmod_DATA}

AM_CFLAGS = \
  -I${top_srcdir}/src \
  -I${top_srcdir}/src/engine \
  -I${top_srcdir}/src/app-utils \
  -I${top_srcdir}/src/gnc-module \
  -I${top_srcdir}/src/gnome-utils \
  ${GUILE_INCS} \
  ${GLADE_CFLAGS} \
  ${G_WRAP_COMPILE_ARGS} \
  ${GNOME_CFLAGS} \
  ${QOF_CFLAGS} \
  ${GLIB_CFLAGS}


if GNUCASH_SEPARATE_BUILDDIR
#Only needed when srcdir and builddir are different
SCM_FILE_LINKS = gw-register-core-spec.scm
endif

.scm-links: 
	rm -f gnucash g-wrapped
	ln -sf . gnucash 
	ln -sf . g-wrapped 
	ln -sf . register
if GNUCASH_SEPARATE_BUILDDIR
	for X in ${SCM_FILE_LINKS} ; do \
	  ln -sf ${srcdir}/$$X . ; \
	done
endif
	touch .scm-links

gw-register-core.scm gw-register-core.c gw-register-core.h: \
  .scm-links gw-register-core-spec.scm ${top_builddir}/config.status
	FLAVOR=gnome $(GUILE) -c \
	 "(set! %load-path (cons \"${G_WRAP_MODULE_DIR}\" %load-path)) \
	  (set! %load-path (cons \"${PWD}\" %load-path)) \
	  (set! %load-path (cons \"${top_srcdir}/src/engine\" %load-path)) \
	  (primitive-load \"./gw-register-core-spec.scm\") \
	  (gw:generate-wrapset \"gw-register-core\")"

BUILT_SOURCES = gw-register-core.scm gw-register-core.c gw-register-core.h
CLEANFILES = gnucash g-wrapped register .scm-links
DISTCLEANFILES = ${SCM_FILE_LINKS} gw-register-core.html
