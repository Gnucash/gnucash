SUBDIRS = . test

AM_CFLAGS = -I${top_srcdir}/src/core-utils ${GLIB_CFLAGS} ${G_WRAP_COMPILE_ARGS} ${GUILE_INCS}

lib_LTLIBRARIES = libgncmodule.la 
pkglib_LTLIBRARIES = libgw-gnc-module.la

libgncmodule_la_SOURCES = gnc-module.c
libgncmodule_la_LDFLAGS = -module
libgncmodule_la_LIBADD = \
  -L../core-utils -L../core-utils/.libs -lcore-utils \
  ${G_WRAP_LINK_ARGS} \
  ${GLIB_LIBS} \
  -lltdl

libgw_gnc_module_la_SOURCES = gw-gnc-module.c
libgw_gnc_module_la_LDFLAGS = -module 
libgw_gnc_module_la_LIBADD = libgncmodule.la \
  ${G_WRAP_LINK_ARGS} \
  ${GUILE_LIBS} \
  ${GLIB_LIBS}

gncmoddir = ${GNC_SHAREDIR}/guile-modules/gnucash
gncmod_DATA = gnc-module.scm

gwmoddir = ${GNC_GWRAP_LIBDIR}
gwmod_DATA = gw-gnc-module.scm gw-gnc-module-spec.scm 

gncincludedir = ${GNC_INCLUDE_DIR}
gncinclude_HEADERS = \
  gnc-module-api.h \
  gnc-module.h

noinst_DATA = .scm-links

EXTRA_DIST = \
  ${gncmod_DATA} \
  ${gwmod_DATA}

if GNUCASH_SEPARATE_BUILDDIR
#For compiling
SCM_FILE_LINKS = gw-gnc-module-spec.scm
#For executing test cases
SCM_FILE_LINKS += gnc-module.scm
endif

.scm-links: 
	rm -f gnucash g-wrapped
	ln -sf . gnucash
	ln -sf . g-wrapped
if GNUCASH_SEPARATE_BUILDDIR
	for X in ${SCM_FILE_LINKS} ; do \
	  ln -sf ${srcdir}/$$X . ; \
	done
endif
	touch .scm-links

gnc-module.c: gw-gnc-module.h

gw-gnc-module.scm gw-gnc-module.c gw-gnc-module.h: .scm-links gw-gnc-module-spec.scm
	FLAVOR=gnome guile -c \
	  "(set! %load-path (cons \"${G_WRAP_MODULE_DIR}\" %load-path)) \
	  (primitive-load \"./gw-gnc-module-spec.scm\") \
	  (gw:generate-wrapset \"gw-gnc-module\")"

BUILT_SOURCES = gw-gnc-module.scm gw-gnc-module.h gw-gnc-module.c
CLEANFILES = ${BUILT_SOURCES} gw-gnc-module.html \
             ${SCM_FILE_LINKS} gnucash g-wrapped .scm-links

