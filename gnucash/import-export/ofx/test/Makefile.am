TESTS=test-link

AM_CPPFLAGS=${LIBOFX_CFLAGS}
check_PROGRAMS=test-link

test_link_SOURCES=test-link.c
test_link_LDADD=\
	$(top_builddir)/libgnucash/engine/libgncmod-engine.la \
	$(top_builddir)/libgnucash/app-utils/libgncmod-app-utils.la \
	${top_builddir}/gnucash/gnome-utils/libgncmod-gnome-utils.la \
	../libgncmod-ofx.la

EXTRA_DIST = CMakeLists.txt
