SUBDIRS = . test

AM_CFLAGS = \
   -I${top_srcdir}/lib/libc \
   -I${top_srcdir}/src \
   -I${top_srcdir}/src/core-utils \
   ${GLIB_CFLAGS} ${GUILE_INCS}

lib_LTLIBRARIES = libgncmodule.la 

libgncmodule_la_SOURCES = gnc-module.c swig-gnc-module.c

libgncmodule_la_LIBADD = \
  ../core-utils/libcore-utils.la \
 ${top_builddir}/lib/libc/libc-missing.la \
  ${GLIB_LIBS} \
  -lltdl

gncmoddir = ${GNC_SHAREDIR}/guile-modules/gnucash
gncmod_DATA = gnc-module.scm

gncincludedir = ${GNC_INCLUDE_DIR}
gncinclude_HEADERS = \
  gnc-module-api.h \
  gnc-module.h

noinst_DATA = .scm-links

swig-gnc-module.c: gnc-module.i
	swig -guile $(SWIG_ARGS) -Linkage module -o $@ $<

EXTRA_DIST = \
  ${gncmod_DATA}

if GNUCASH_SEPARATE_BUILDDIR
#For executing test cases
SCM_FILE_LINKS = ${gncmod_DATA}
endif

.scm-links: 
	rm -f gnucash
	$(LN_S) -f . gnucash
if GNUCASH_SEPARATE_BUILDDIR
	for X in ${SCM_FILE_LINKS} ; do \
	  $(LN_S) -f ${srcdir}/$$X . ; \
	done
endif
	touch .scm-links

BUILT_SOURCES = swig-gnc-module.c
CLEANFILES = $(BUILT_SOURCES) .scm-links gnucash \
	${SCM_FILE_LINKS}
