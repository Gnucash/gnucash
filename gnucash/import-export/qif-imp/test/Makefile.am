TESTS=test-link

check_PROGRAMS=test-link

test_link_SOURCES = \
  test-link.c

test_link_LDADD=../libgncmod-qif-import.la \
  ${top_builddir}/gnucash/register/ledger-core/libgncmod-ledger-core.la \
  ${top_builddir}/libgnucash/core-utils/libgnc-core-utils.la \
  ${top_builddir}/gnucash/gnome/libgnc-gnome.la \
  ${top_builddir}/libgnucash/engine/libgncmod-engine.la \
  ${top_builddir}/gnucash/gnome-utils/libgncmod-gnome-utils.la \
  ${top_builddir}/libgnucash/app-utils/libgncmod-app-utils.la \
  ${LIBXML2_LIBS}

EXTRA_DIST = CMakeLists.txt
