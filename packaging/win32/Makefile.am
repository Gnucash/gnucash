EXTRA_DIST = \
  README \
  defaults.sh \
  dist.sh \
  exetype.pl \
  functions.sh \
  goffice-0.7.2-patch.diff \
  gwenhywfar-3.6.0-patch.diff \
  install.sh \
  install-fq-mods.cmd \
  libmysql.def \
  libofx-0.8.3-patch.diff \
  opensp-1.5.2-patch.diff \
  pi.sh \
  ChineseSimp-12-5.1.11.isl \
  ChineseTrad-2-5.1.11.isl \
  Greek-4-5.1.11.isl \
  Japanese-5-5.1.11.isl \
  Latvian-1-5.1.11.isl \
  reset.sh

if OS_WIN32
  bin_PROGRAMS = redirect
  redirect_SOURCES = redirect.c
  redirect_LDADD = $(GLIB_LIBS)
  AM_CPPFLAGS = $(GLIB_CFLAGS)
  pkgdata_DATA = ChineseSimp-12-5.1.11.isl
  pkgdata_DATA += ChineseTrad-2-5.1.11.isl
  pkgdata_DATA += Greek-4-5.1.11.isl
  pkgdata_DATA += Japanese-5-5.1.11.isl
  pkgdata_DATA += Latvian-1-5.1.11.isl
else
  EXTRA_DIST += redirect.c
endif
