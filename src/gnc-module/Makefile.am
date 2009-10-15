SUBDIRS = . test

AM_CPPFLAGS = \
   -I${top_srcdir}/lib/libc \
   -I${top_srcdir}/src \
   -I${top_srcdir}/src/core-utils \
   ${GLIB_CFLAGS}

lib_LTLIBRARIES = libgnc-module.la

libgnc_module_la_SOURCES = gnc-module.c swig-gnc-module.c

libgnc_module_la_LIBADD = \
  ../core-utils/libgnc-core-utils.la \
  ${top_builddir}/lib/libc/libc-missing.la \
  ${GLIB_LIBS}

gncmoddir = ${GNC_SHAREDIR}/guile-modules/gnucash
gncmod_DATA = gnc-module.scm

gncincludedir = ${GNC_INCLUDE_DIR}
gncinclude_HEADERS = \
  gnc-module-api.h \
  gnc-module.h

noinst_DATA = .scm-links

if BUILDING_FROM_SVN
swig-gnc-module.c: gnc-module.i ${top_srcdir}/src/base-typemaps.i
	$(SWIG) -guile $(SWIG_ARGS) -Linkage module \
	-I${top_srcdir}/src -o $@ $<
endif

EXTRA_DIST = \
  ${gncmod_DATA} \
  gnc-module.i

if GNUCASH_SEPARATE_BUILDDIR
#For executing test cases
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
if ! OS_WIN32
# Windows knows no "ln -s" but uses "cp": must copy every time (see bug #566567).
	touch .scm-links
endif

CLEANFILES = .scm-links gnucash ${SCM_FILE_LINKS}
MAINTAINERCLEANFILES = swig-gnc-module.c

INCLUDES = -DG_LOG_DOMAIN=\"gnc.module\"
