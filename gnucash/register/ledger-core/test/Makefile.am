TESTS =  test-link-module

check_PROGRAMS = test-link-module

test_link_module_SOURCES=test-link-module.c
test_link_module_LDADD=\
	$(top_builddir)/libgnucash/engine/libgncmod-engine.la \
        $(top_builddir)/libgnucash/app-utils/libgncmod-app-utils.la \
	${top_builddir}/gnucash/gnome-utils/libgncmod-gnome-utils.la \
	${top_builddir}/gnucash/gnome/libgnc-gnome.la \
    ../libgncmod-ledger-core.la

AM_CPPFLAGS = -I${top_srcdir}/common/test-core -I.. ${GLIB_CFLAGS}

EXTRA_DIST = CMakeLists.txt
