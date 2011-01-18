# A template Makefile.am for GLib g_test-based test directories.
# Copyright 2011 John Ralls <jralls@ceridwen.us>

include $(top_srcdir)/test-templates/Makefile.decl


#You will only need one of these: It points to the module directory
#after $(top_srcdir) or ${top_builddir}:
MODULEPATH = path/to/foo

#The libtool convenience library to assemble the common test code
#(fixture code, setup and teardown routines, mocks, etc.). Use it only
#if there are a lot of test programs and support programs. For most
#cases it will make more sense to just include test_module_support.h
#and test_module_support.c in the test program sources & headers
#check_LTLIBRARIES = libgncmod_test_foo.la
#
#libgnc_test_foo_la_SOURCES = \
#	test_foo_support.c

#The test program. You'll need to add to this if you have more than one module above.
TEST_PROGS += test-foo

noinst_PROGRAMS = ${TEST_PROGS}

#Program files for tests go here. It's probably best to have one for
#each file in the parent directory. Include
#test_foo_support.c if you have one and aren't building the
#support library.
test_foo_SOURCES = \
	test-foo.c \
	test-suite-module1.c \
	test-suite-module2.c \
	#etc.

test_foo_HEADERSS = \
	$(top_srcdir)/${MODULEPATH}/module1.h \
	$(top_srcdir)/${MODULEPATH}/module2.h \
	#etc.


#The tests might require more libraries, but try to keep them
#as independent as possible.
test_foo_LDADD = ${top_buildir}/${MODULEPATH}/libgnc_foo.la

test_foo_CFLAGS = \
	${DEFAULT_INCLUDES} \
	-I$(top_srcdir)/${MODULEPATH}/ \
	${GLIB_CFLAGS}
