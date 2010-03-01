TESTS=test-link

AM_CPPFLAGS=${LIBOFX_CFLAGS}
check_PROGRAMS=test-link

test_link_SOURCES=test-link.c
test_link_LDADD=\
	${top_builddir}/src/gnome-utils/libgncmod-gnome-utils.la \
    ../libgncmod-ofx.la 
