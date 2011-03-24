SUBDIRS = . pycons 
#test

pkglib_LTLIBRARIES = libgncmod-python.la

libgncmod_python_la_SOURCES = \
  gncmod-python.c

libgncmod_python_la_LDFLAGS = -avoid-version \
  ${PYTHON_LDFLAGS}

libgncmod_python_la_LIBADD = \
  ${top_builddir}/src/gnc-module/libgnc-module.la \
  ${top_builddir}/src/core-utils/libgnc-core-utils.la \
  ${top_builddir}/src/app-utils/libgncmod-app-utils-python.la \
  ${PYTHON_LIBS} \
  ${PYTHON_EXTRA_LIBS} \
  ${GLIB_LIBS}

AM_CPPFLAGS = \
  -I${top_srcdir}/src \
  -I${top_srcdir}/src/core-utils \
  -I${top_srcdir}/src/gnc-module \
  ${PYTHON_CPPFLAGS} \
  ${GLIB_CFLAGS}


gncpymoddir = ${GNC_SHAREDIR}/python/
gncpymod_DATA = \
   init.py

if GNUCASH_SEPARATE_BUILDDIR
#For executing test cases
PY_FILE_LINKS = ${gncpymod_DATA}
endif

.py-links:
	$(RM) -rf gnucash
	mkdir -p  gnucash
	mkdir -p  gnucash/python
if GNUCASH_SEPARATE_BUILDDIR
	for X in ${PY_FILE_LINKS} ; do \
	  $(LN_S) -f ${srcdir}/$$X . ; \
	done
endif
	( cd gnucash/python; for A in $(gncpymod_DATA) ; do $(LN_S) -f ../../$$A . ; done )
if ! OS_WIN32
# Windows knows no "ln -s" but uses "cp": must copy every time (see bug #566567).
	touch .py-links
endif

clean-local:
	$(RM) -rf gnucash

noinst_DATA = .py-links

EXTRA_DIST = ${gncpymod_DATA}

CLEANFILES = .py-links
DISTCLEANFILES = ${PY_FILE_LINKS}

INCLUDES = -DG_LOG_DOMAIN=\"gnc.python\"
