# A template Makefile.am for GLib g_test-based test directories.
# Copyright 2011 John Ralls <jralls@ceridwen.us>

include $(top_srcdir)/test-templates/Makefile.decl


#You will only need one of these: It points to the module directory
#after $(top_srcdir) or $(top_builddir):
MODULEPATH = src/optional/gtkmm

#The test program. You'll need to add to this if you have more than one module above.

check_PROGRAMS = test-gtkmm

TESTS = ${check_PROGRAMS}

test_gtkmmdir = ${top_srcdir}/${MODULEPATH}/test

#Program files for tests go here. It's probably best to have one for
#each file in the parent directory. Include
#test_foo_support.c if you have one and aren't building the
#support library.
test_gtkmm_SOURCES = \
	test-gtkmm.cpp \
	test-book.cpp

test_gtkmm_HEADERS = \
	$(top_srcdir)/${MODULEPATH}/gncmm/Book.hpp

#The tests might require more libraries, but try to keep them
#as independent as possible.
test_gtkmm_LDADD = ${top_builddir}/${MODULEPATH}/libgncmod-gtkmm.la \
  ${top_builddir}/src/libqof/qof/libgnc-qof.la \
  ${GTKMM_LIBS} \
  ${GLIB_LIBS}

AM_CPPFLAGS = \
	${DEFAULT_INCLUDES} \
	-DTESTPROG=test_gtkmm \
  -I${top_srcdir}/src \
  -I${top_srcdir}/src/test-core \
  -I${top_srcdir}/src/libqof/qof \
  -I${top_srcdir}/src/gnc-module \
  -I${top_srcdir}/src/core-utils \
  -I${top_srcdir}/src/engine \
  -I${top_srcdir}/src/optional/gtkmm \
  ${GTKMM_CFLAGS} \
  ${GLIB_CFLAGS}
