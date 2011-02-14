# A template Makefile.am for GLib g_test-based test directories.
# Copyright 2011 John Ralls <jralls@ceridwen.us>

include $(top_srcdir)/test-templates/Makefile.decl


#You will only need one of these: It points to the module directory
#after $(top_srcdir) or $(top_builddir):
MODULEPATH = src/libqof/qof

test_qof_SOURCES = \
	test-qof.c \
	test-qofbook.c \
	test-qofinstance.c \
	test-qofsession.c

test_qof_HEADERSS = \
	$(top_srcdir)/${MODULEPATH}/qofbook.h \
	$(top_srcdir)/${MODULEPATH}/qofinstance.h \
	$(top_srcdir)/${MODULEPATH/}qofsession.h

TEST_PROGS += test-qof

noinst_PROGRAMS = ${TEST_PROGS}

#The tests might require more libraries, but try to keep them
#as independent as possible.
test_qof_LDADD = ${top_builddir}/${MODULEPATH}/libgnc-qof.la $(GLIB_LIBS)

test_qof_CFLAGS = \
	${DEFAULT_INCLUDES} \
	-I$(top_srcdir)/${MODULEPATH}/ \
	-DTESTPROG=test_qof \
	${GLIB_CFLAGS}
