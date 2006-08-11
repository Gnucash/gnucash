PWD := $(shell pwd)

pkglib_LTLIBRARIES = libgncmod-dialog-tax-table.la

AM_CFLAGS = \
  -I${top_srcdir}/src \
  -I${top_srcdir}/src/core-utils \
  -I${top_srcdir}/src/engine \
  -I${top_srcdir}/src/gnc-module \
  -I${top_srcdir}/src/gnome-utils \
  -I${top_srcdir}/src/app-utils \
  -I${top_srcdir}/src/business/business-core \
  ${GNOME_PRINT_CFLAGS} \
  ${GNOME_CFLAGS} \
  ${GLADE_CFLAGS} \
  ${GTKHTML_CFLAGS} \
  ${GDK_PIXBUF_CFLAGS} \
  ${GLIB_CFLAGS} \
  ${QOF_CFLAGS} \
  ${G_WRAP_COMPILE_ARGS} \
  ${GUILE_INCS}

libgncmod_dialog_tax_table_la_SOURCES = \
  swig-dialog-tax-table.c \
  gncmod-dialog-tax-table.c \
  dialog-tax-table.c

noinst_HEADERS = \
  dialog-tax-table.h

libgncmod_dialog_tax_table_la_LIBADD = \
  ${top_builddir}/src/business/business-core/libgncmod-business-core.la \
  ${top_builddir}/src/gnome-utils/libgncmod-gnome-utils.la \
  ${top_builddir}/src/app-utils/libgncmod-app-utils.la \
  ${top_builddir}/src/engine/libgncmod-engine.la \
  ${top_builddir}/src/gnc-module/libgncmodule.la \
  ${GLADE_LIBS} \
  ${GUILE_LIBS} \
  ${GNOME_LIBS} \
  ${G_WRAP_LINK_ARGS} \
  ${GLIB_LIBS} \
  ${QOF_LIBS} \
  ${EFENCE_LIBS}

swig-dialog-tax-table.c: dialog-tax-table.i
	swig -guile $(SWIG_ARGS) -Linkage module -o $@ $<

gncmoddir = ${GNC_SHAREDIR}/guile-modules/gnucash
gncmod_DATA = dialog-tax-table.scm

gladedir = $(GNC_GLADE_DIR)
glade_DATA = tax-tables.glade

EXTRA_DIST = \
  ${glade_DATA} \
  ${gwmod_DATA} \
  ${gncmod_DATA}

if GNUCASH_SEPARATE_BUILDDIR
#Only needed when srcdir and builddir are different
# for compline
SCM_FILE_LINKS = gw-dialog-tax-table-spec.scm
# for running
SCM_FILE_LINKS += ${gncmod_DATA}
endif

.scm-links: 
	rm -f gnucash g-wrapped
	$(LN_S) -f . gnucash 
	$(LN_S) -f . g-wrapped 
if GNUCASH_SEPARATE_BUILDDIR
	for X in ${SCM_FILE_LINKS} ; do \
	  $(LN_S) -f ${srcdir}/$$X . ; \
	done
endif
	touch .scm-links

#BUILT_SOURCES = gw-dialog-tax-table.scm gw-dialog-tax-table.c \
#	gw-dialog-tax-table.h
CLEANFILES = $(BUILT_SOURCES) g-wrapped gnucash .scm-links \
	${SCM_FILE_LINKS} gw-dialog-tax-table.html
