pkglib_LTLIBRARIES = libgncmod-dialog-tax-table.la

AM_CPPFLAGS = \
  -I${top_srcdir}/src \
  -I${top_srcdir}/src/core-utils \
  -I${top_srcdir}/src/engine \
  -I${top_srcdir}/src/gnc-module \
  -I${top_srcdir}/src/gnome-utils \
  -I${top_srcdir}/src/app-utils \
  -I${top_srcdir}/src/business/business-core \
  ${QOF_CFLAGS} \
  ${GNOME_CFLAGS} \
  ${GLADE_CFLAGS} \
  ${GTKHTML_CFLAGS} \
  ${GDK_PIXBUF_CFLAGS} \
  ${GLIB_CFLAGS} \
  ${GUILE_INCS}

libgncmod_dialog_tax_table_la_SOURCES = \
  swig-dialog-tax-table.c \
  gncmod-dialog-tax-table.c \
  dialog-tax-table.c

noinst_HEADERS = \
  dialog-tax-table.h

libgncmod_dialog_tax_table_la_LDFLAGS = -avoid-version

libgncmod_dialog_tax_table_la_LIBADD = \
  ${top_builddir}/src/business/business-core/libgncmod-business-core.la \
  ${top_builddir}/src/gnome-utils/libgncmod-gnome-utils.la \
  ${top_builddir}/src/app-utils/libgncmod-app-utils.la \
  ${top_builddir}/src/engine/libgncmod-engine.la \
  ${top_builddir}/src/gnc-module/libgnc-module.la \
  ${GLADE_LIBS} \
  ${GUILE_LIBS} \
  ${GNOME_LIBS} \
  ${GLIB_LIBS} \
  ${QOF_LIBS} \
  ${EFENCE_LIBS}

if BUILDING_FROM_SVN
swig-dialog-tax-table.c: dialog-tax-table.i ${top_srcdir}/src/base-typemaps.i
	$(SWIG) -guile $(SWIG_ARGS) -Linkage module \
	-I${top_srcdir}/src -o $@ $<
endif

gncmoddir = ${GNC_SHAREDIR}/guile-modules/gnucash
gncmod_DATA = dialog-tax-table.scm

gladedir = $(GNC_GLADE_DIR)
glade_DATA = tax-tables.glade

EXTRA_DIST = \
  dialog-tax-table.i \
  ${glade_DATA} \
  ${gncmod_DATA}

if GNUCASH_SEPARATE_BUILDDIR
#Only needed when srcdir and builddir are different
# for running
SCM_FILE_LINKS = ${gncmod_DATA}
endif

.scm-links: 
	$(RM) -rf gnucash
	mkdir -p  gnucash
if GNUCASH_SEPARATE_BUILDDIR
	for X in ${SCM_FILE_LINKS} ; do \
	  $(LN_S) -f ${srcdir}/$$X . ; \
	done
endif
	( cd gnucash; for A in $(gncmod_DATA) ; do $(LN_S) -f ../$$A . ; done )
	touch .scm-links

noinst_DATA = .scm-links
CLEANFILES = gnucash .scm-links ${SCM_FILE_LINKS}
MAINTAINERCLEANFILES = swig-dialog-tax-table.c

INCLUDES = -DG_LOG_DOMAIN=\"gnc.business.tax\"
