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

check_PROGRAMS = \
  test-qof

TESTS = ${check_PROGRAMS}

if WITH_GOOGLE_TEST
test_gnc_int128_SOURCES = \
        $(top_srcdir)/${MODULEPATH}/gnc-int128.cpp \
        ${GTEST_ROOT}/src/gtest_main.cc \
        gtest-gnc-int128.cpp

test_gnc_int128_CPPFLAGS = \
        -I${GTEST_HEADERS} \
        ${GLIB_CFLAGS}

test_gnc_int128_LDADD = \
        ${GLIB_LIBS} \
        $(top_builddir)/src/test-core/libgtest.a

check_PROGRAMS += test-gnc-int128
endif

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

if WITH_GOOGLE_TEST
test_kvp_value_SOURCES = \
    $(top_srcdir)/$(MODULEPATH)/kvp-value.cpp \
    $(GTEST_ROOT)/src/gtest_main.cc \
	test-kvp-value.cpp
test_kvp_value_LDADD = \
	$(top_builddir)/$(MODULEPATH)/libgnc-qof.la \
    $(top_builddir)/src/test-core/libgtest.a \
    $(GLIB_LIBS) \
	$(BOOST_LDFLAGS)
test_kvp_value_CPPFLAGS = \
    -I$(GTEST_HEADERS) \
    -I$(top_srcdir)/$(MODULEPATH) \
    $(BOOST_CPPFLAGS) \
    $(GLIB_CFLAGS) 

check_PROGRAMS += test_kvp_value

endif
