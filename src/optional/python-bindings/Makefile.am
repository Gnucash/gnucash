SUBDIRS = . tests

SWIG_SOURCES = gnucash_core.i timespec.i

pkgpython_PYTHON = \
  __init__.py \
  function_class.py \
  gnucash_core.py \
  gnucash_core_c.py \
  gnucash_business.py

pkgpyexec_LTLIBRARIES = _gnucash_core_c.la

_gnucash_core_c_la_SOURCES = \
  gnucash_core.c

_gnucash_core_c_la_CPPFLAGS = \
  $(PYTHON_CPPFLAGS) \
  $(GLIB_CFLAGS) \
  $(GUILE_INCS) \
  -I${top_srcdir}/src/libqof/qof \
  -I$(top_srcdir)/src  \
  -I$(top_srcdir)/src/engine \
  -I$(top_srcdir)/src/business/business-core

# Suppress all warnings for now, but we really only need to -Wno-implicit
AM_CFLAGS = -w

_gnucash_core_c_la_LDFLAGS = -avoid-version -module

_gnucash_core_c_la_LIBADD = \
  ${GLIB_LIBS} \
  ${GUILE_LIBS} \
  ${top_builddir}/src/libqof/qof/libgnc-qof.la \
  ${top_builddir}/src/gnc-module/libgnc-module.la \
  ${top_builddir}/src/engine/libgncmod-engine.la \
  ${top_builddir}/src/business/business-core/libgncmod-business-core.la

if BUILDING_FROM_SVN
gnucash_core.c: $(SWIG_SOURCES)
	swig $(SWIG_PYTHON_OPT) -Wall -Werror \
        -I$(top_srcdir)/src -I$(top_srcdir)/src/engine \
		-I$(top_srcdir)/src/business/business-core \
        -I${top_srcdir}/src/libqof/qof \
		-o $@ $<

gnucash_core_c.py: gnucash_core.c $(SWIG_SOURCES)
endif

EXTRA_DIST = \
  $(SWIG_SOURCES) \
  example_scripts/simple_book.py \
  example_scripts/simple_session.py \
  example_scripts/simple_test.py \
  glib.i

MAINTAINERCLEANFILES = gnucash-core.c
