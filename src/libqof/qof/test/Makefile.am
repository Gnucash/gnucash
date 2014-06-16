# A template Makefile.am for GLib g_test-based test directories.
# Copyright 2011 John Ralls <jralls@ceridwen.us>

include $(top_srcdir)/test-templates/Makefile.decl


#You will only need one of these: It points to the module directory
#after $(top_srcdir) or $(top_builddir):
MODULEPATH = src/libqof/qof

test_qof_SOURCES = \
	test-gnc-date.c \
	test-qof.c \
	test-qofbook.c \
	test-qofinstance.c \
	test-kvp_frame.c \
	test-qofobject.c \
	test-qofsession.c \
	test-qof-string-cache.c \
	test-gnc-guid.cpp \
	${top_srcdir}/src/test-core/unittest-support.c

test_qof_HEADERS = \
	$(top_srcdir)/${MODULEPATH}/qofbook.h \
	$(top_srcdir)/${MODULEPATH}/qofinstance.h \
	$(top_srcdir)/${MODULEPATH}/kvp_frame.h \
	$(top_srcdir)/${MODULEPATH}/qofobject.h \
	$(top_srcdir)/${MODULEPATH}/qofsession.h \
	$(top_srcdir)/src/test-core/unittest-support.h

TEST_PROGS += test-qof

noinst_PROGRAMS = ${TEST_PROGS}
test_qofdir = ${GNC_LIBEXECDIR}/${MODULEPATH}/test

#The tests might require more libraries, but try to keep them
#as independent as possible.
test_qof_LDADD = \
	${top_builddir}/${MODULEPATH}/libgnc-qof.la \
	$(GLIB_LIBS)

test_qof_CPPFLAGS = \
	${DEFAULT_INCLUDES} \
	-I$(top_srcdir)/${MODULEPATH} \
	-I$(top_srcdir)/src/test-core \
	-DTESTPROG=test_qof \
	-I$(top_srcdir)/lib/libc \
	${GLIB_CFLAGS}
