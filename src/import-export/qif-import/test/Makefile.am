TESTS=test-link

noinst_PROGRAMS=test-link

test_link_SOURCES = \
  test-link.c

test_link_LDADD=../libgncmod-qif-import.la \
  ${top_builddir}/src/gnome/libgncgnome.la \
  ${top_builddir}/src/engine/libgncmod-engine.la \
  ${top_builddir}/src/gnome-utils/libgncmod-gnome-utils.la \
  ${top_builddir}/src/app-utils/libgncmod-app-utils.la \
  ${top_builddir}/src/core-utils/libcore-utils.la \
  ${LIBXML2_LIBS} \
  ${QOF_LIBS}

