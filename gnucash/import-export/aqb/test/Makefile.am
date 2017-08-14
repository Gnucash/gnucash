# A template Makefile.am for GLib g_test-based test directories.
# Copyright 2011 John Ralls <jralls@ceridwen.us>

include $(top_srcdir)/test-templates/Makefile.decl


#You will only need one of these: It points to the module directory
#after $(top_srcdir) or ${top_builddir}:
MODULEPATH = gnucash/import-export/aqb

#The test program. You'll need to add to this if you have more than one module above.

check_PROGRAMS = test-aqb

TESTS = ${check_PROGRAMS}

test_aqbdir = ${top_srcdir}/${MODULEPATH}/test

#Program files for tests go here. It's probably best to have one for
#each file in the parent directory. Include
#test_foo_support.c if you have one and aren't building the
#support library.
test_aqb_SOURCES = \
  test-aqb.c \
  test-kvp.c

test_aqb_HEADERSS =

#The tests might require more libraries, but try to keep them
#as independent as possible.
test_aqb_LDADD = \
  ${top_builddir}/${MODULEPATH}/libgncmod-aqbanking.la \
  ${top_builddir}/gnucash/import-export/libgncmod-generic-import.la \
  ${top_builddir}/gnucash/gnome/libgnc-gnome.la \
  ${top_builddir}/gnucash/gnome-utils/libgncmod-gnome-utils.la \
  ${top_builddir}/gnucash/register/ledger-core/libgncmod-ledger-core.la \
  ${top_builddir}/gnucash/report/report-gnome/libgncmod-report-gnome.la \
  ${top_builddir}/libgnucash/app-utils/libgncmod-app-utils.la \
  ${top_builddir}/libgnucash/backend/xml/libgnc-backend-xml-utils.la \
  ${top_builddir}/libgnucash/engine/libgncmod-engine.la \
  ${top_builddir}/libgnucash/core-utils/libgnc-core-utils.la \
  ${top_builddir}/libgnucash/gnc-module/libgnc-module.la \
  ${GLIB_LIBS}

test_aqb_CFLAGS = \
	-DTESTPROG=test_aqb \
	${DEFAULT_INCLUDES} \
	-I$(top_srcdir)/${MODULEPATH}/ \
  -I${top_srcdir}/common \
  -I${top_srcdir}/gnucash/import-export \
  -I${top_srcdir}/gnucash/gnome \
  -I${top_srcdir}/gnucash/register/ledger-core \
  -I${top_srcdir}/gnucash/register/register-gnome \
  -I${top_srcdir}/gnucash/register/register-core \
  -I${top_srcdir}/gnucash/gnome-utils \
  -I${top_srcdir}/libgnucash/app-utils \
  -I${top_srcdir}/libgnucash/engine \
  -I${top_srcdir}/libgnucash/backend/xml \
  -I${top_srcdir}/libgnucash/core-utils \
  -I${top_srcdir}/libgnucash/gnc-module \
  ${GTK_CFLAGS} \
  ${GLIB_CFLAGS} \
  ${AQBANKING_CFLAGS}

  TESTS_ENVIRONMENT = \
  GNC_BUILDDIR=${abs_top_builddir} \
  SRCDIR=${srcdir} \
  G_DEBUG=

EXTRA_DIST += \
  file-book-hbcislot.gnucash \
  file-book.gnucash \
  CMakeLists.txt
