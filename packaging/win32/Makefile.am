EXTRA_DIST = \
  README \
  defaults.sh \
  dist.sh \
  exetype.pl \
  functions.sh \
  goffice-0.4.3-patch.diff \
  install.sh \
  install-fq-mods.bat \
  libofx-0.8.3-patch.diff \
  opensp-1.5.2-patch.diff \
  libgda-3.1.2-patch.diff \
  libgda-3.1.2-patch2.diff \
  pi.sh \
  Greek-4-5.1.11.isl \
  reset.sh

if OS_WIN32
  bin_PROGRAMS = redirect
  redirect_SOURCES = redirect.c
  redirect_LDADD = $(GLIB_LIBS)
  AM_CFLAGS = $(GLIB_CFLAGS)
  pkgdata_DATA = Greek-4-5.1.11.isl
else
  EXTRA_DIST += redirect.c
endif
