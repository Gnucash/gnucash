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
test_kvp_value_SOURCES = \
    $(top_srcdir)/$(MODULEPATH)/kvp-value.cpp \
    test-kvp-value.cpp \
    test-kvp-frame.cpp
test_kvp_value_LDADD = \
	$(top_builddir)/$(MODULEPATH)/libgnc-qof.la \
        $(GLIB_LIBS) \
	$(GTEST_LIBS) \
	$(BOOST_LDFLAGS)

if !GOOGLE_TEST_LIBS
nodist_test_kvp_value_SOURCES = \
        ${GTEST_SRC}/src/gtest_main.cc
test_kvp_value_LDADD += $(top_builddir)/src/test-core/libgtest.a
endif

test_kvp_value_CPPFLAGS = \
    -I$(GTEST_HEADERS) \
    -I$(top_srcdir)/$(MODULEPATH) \
    $(GLIB_CFLAGS) \
    $(BOOST_CPPFLAGS)

check_PROGRAMS += test-kvp-value

test_gnc_int128_SOURCES = \
        $(top_srcdir)/${MODULEPATH}/gnc-int128.cpp \
        gtest-gnc-int128.cpp
test_gnc_int128_CPPFLAGS = -I${GTEST_HEADERS}

test_gnc_int128_LDADD = ${GTEST_LIBS}
if !GOOGLE_TEST_LIBS
nodist_test_gnc_int128_SOURCES = \
        ${GTEST_SRC}/src/gtest_main.cc
test_gnc_int128_LDADD += $(top_builddir)/src/test-core/libgtest.a
endif
check_PROGRAMS += test-gnc-int128

test_gnc_timezone_SOURCES = \
        $(top_srcdir)/${MODULEPATH}/gnc-timezone.cpp \
        gtest-gnc-timezone.cpp

test_gnc_timezone_CPPFLAGS = \
        -I${GTEST_HEADERS} \
	-I${top_srcdir}/src \
	$(BOOST_CPPFLAGS)
test_gnc_timezone_LDADD = $(GTEST_LIBS)
if !GOOGLE_TEST_LIBS
nodist_test_gnc_timezone_SOURCES = \
        ${GTEST_SRC}/src/gtest_main.cc
test_gnc_timezone_LDADD += $(top_builddir)/src/test-core/libgtest.a
endif
check_PROGRAMS += test-gnc-timezone

test_gnc_datetime_SOURCES = \
	$(top_srcdir)/$(MODULEPATH)/gnc-datetime.cpp \
	$(top_srcdir)/$(MODULEPATH)/gnc-timezone.cpp \
	gtest-gnc-datetime.cpp
test_gnc_datetime_CPPFLAGS =\
	-I$(GTEST_HEADERS) \
	-I$(top_srcdir)/src \
	$(BOOST_CPPFLAGS)
test_gnc_datetime_LDADD = \
	-lboost_date_time \
	$(GTEST_LIBS)
if !GOOGLE_TEST_LIBS
nodist_test_gnc_datetime_SOURCES = \
	$(GTEST_SRC)/src/gtest_main.cc
test_gnc_datetime_LDADD += $(top_builddir)/src/test-core/libgtest.a
endif
check_PROGRAMS += test-gnc-datetime

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
