TESTS=test-link

check_PROGRAMS=test-link

test_link_SOURCES = \
  test-link.c

test_link_LDADD=../libgncmod-qif-import.la \
  ${top_builddir}/src/core-utils/libgnc-core-utils.la \
  ${top_builddir}/src/gnome/libgnc-gnome.la \
  ${top_builddir}/src/engine/libgncmod-engine.la \
  ${top_builddir}/src/gnome-utils/libgncmod-gnome-utils.la \
  ${top_builddir}/src/app-utils/libgncmod-app-utils.la \
  ${top_builddir}/src/libqof/qof/libgnc-qof.la \
  ${LIBXML2_LIBS}

