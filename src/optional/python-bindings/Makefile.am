SUBDIRS = . tests

SWIG_FILES = gnucash_core.i timespec.i

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
  -I${top_srcdir}/src/libqof/qof \
  -I$(top_srcdir)/src  \
  -I$(top_srcdir)/src/engine \
  -I$(top_srcdir)/src/business/business-core

# Suppress all warnings for now, but we really only need to -Wno-implicit
AM_CFLAGS = -w

_gnucash_core_c_la_LDFLAGS = -avoid-version -module

_gnucash_core_c_la_LIBADD = \
  ${GLIB_LIBS} \
  ${top_builddir}/src/libqof/qof/libgnc-qof.la \
  ${top_builddir}/src/gnc-module/libgnc-module.la \
  ${top_builddir}/src/engine/libgncmod-engine.la \
  ${top_builddir}/src/business/business-core/libgncmod-business-core.la \
  ${top_builddir}/src/backend/xml/libgncmod-backend-xml.la \
  ${top_builddir}/src/business/business-core/xml/libgncmod-business-backend-xml.la \
  ${top_builddir}/src/backend/dbi/libgncmod-backend-dbi.la \
  ${top_builddir}/src/business/business-core/sql/libgncmod-business-backend-sql.la
if BUILDING_FROM_SVN
gnucash_core.c: $(SWIG_FILES) ${top_srcdir}/src/base-typemaps.i $(top_builddir)/config.h
	swig -python -Wall -Werror \
        -I$(top_srcdir)/src -I$(top_srcdir)/src/engine \
		-I$(top_srcdir)/src/business/business-core \
        -I${top_srcdir}/src/libqof/qof \
		-o $@ $<

gnucash_core_c.py: gnucash_core.c $(SWIG_FILES)
endif

EXTRA_DIST = \
  $(SWIG_FILES) \
  example_scripts/simple_book.py \
  example_scripts/simple_session.py \
  example_scripts/simple_test.py \
  glib.i

MAINTAINERCLEANFILES = gnucash-core.c
