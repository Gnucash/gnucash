lib_LTLIBRARIES = libgnc-core-utils.la

libgnc_core_utils_la_SOURCES = \
  gnc-main.c \
  gnc-gconf-utils.c \
  gnc-gdate-utils.c \
  gnc-gkeyfile-utils.c \
  gnc-glib-utils.c \
  gnc-gobject-utils.c \
  swig-core-utils.c \
  gnc-gtk-utils.c

libgnc_core_utils_la_LIBADD = \
  ${GUILE_LIBS} \
  ${GLIB_LIBS} \
  ${GCONF_LIBS} \
  ${GTK_LIBS}

noinst_HEADERS = \
  gnc-main.h \
  gnc-gconf-utils.h \
  gnc-gdate-utils.h \
  gnc-gkeyfile-utils.h \
  gnc-glib-utils.h \
  gnc-gobject-utils.h \
  gnc-gtk-utils.h

if BUILDING_FROM_SVN
swig-core-utils.c: core-utils.i ${top_srcdir}/src/base-typemaps.i
	$(SWIG) -guile $(SWIG_ARGS) -Linkage module \
	-I${top_srcdir}/src -o $@ $<
endif

AM_CPPFLAGS = \
  ${GUILE_INCS} \
  ${GLIB_CFLAGS} \
  ${GCONF_CFLAGS} \
  ${GTK_CFLAGS}

gncmoddir = ${GNC_SHAREDIR}/guile-modules/gnucash
gncmod_DATA = core-utils.scm

if GNUCASH_SEPARATE_BUILDDIR
#Only needed when srcdir and builddir are different
#for running
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
EXTRA_DIST = $(gncmod_DATA) core-utils.i
CLEANFILES = gnucash .scm-links
MAINTAINERCLEANFILES = swig-core-utils.c

INCLUDES = -DG_LOG_DOMAIN=\"gnc.core-utils\"
