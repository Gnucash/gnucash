SUBDIRS = . #test

pkglib_LTLIBRARIES = libgncmod-gnome-utils.la

CFLAGS = @CFLAGS@ ${GLIB_CFLAGS}

#FIXME remove the following deps
#  -I${top_srcdir}/src \
#  -I${top_srcdir}/src/gnome \
#  -I${top_srcdir}/src/register/ledger-core \
#  -I${top_srcdir}/src/register/register-core \

INCLUDES = \
  -I${top_srcdir}/src/gnc-module \
  -I${top_srcdir}/src/engine \
  -I${top_srcdir}/src/network-utils \
  -I${top_srcdir}/src/app-utils \
  -I${top_srcdir}/src \
  -I${top_srcdir}/src/gnome \
  -I${top_srcdir}/src/register/ledger-core \
  -I${top_srcdir}/src/register/register-core \
  ${GNOME_INCLUDEDIR} \
  ${GUILE_INCS} \
  ${GUPPI_CFLAGS}

libgncmod_gnome_utils_la_SOURCES = \
  cursors.c \
  dialog-commodity.c \
  dialog-utils.c \
  druid-utils.c \
  gnc-account-tree.c \
  gnc-amount-edit.c \
  gnc-commodity-edit.c \
  gnc-currency-edit.c \
  gnc-date-delta.c \
  gnc-date-edit.c \
  gnc-frequency.c \
  gnc-gui-query.c \
  gnc-html-history.c \
  gnc-html-guppi.c \
  gnc-html.c \
  gnc-menu-extensions.c \
  gncmod-gnome-utils.c \
  gtkselect.c \
  print-session.c

noinst_HEADERS = \
  dialog-commodity.h \
  dialog-utils.h \
  druid-utils.h \
  gnc-account-tree.h \
  gnc-amount-edit.h \
  gnc-commodity-edit.h \
  gnc-currency-edit.c \
  gnc-date-delta.h \
  gnc-date-edit.h \
  gnc-dir.h \
  gnc-frequency.h \
  gnc-gui-query.h \
  gnc-html-history.h \
  gnc-html-guppi.h \
  gnc-html.h \
  gnc-menu-extensions.h \
  gtkselect.h \
  print-session.h

libgncmod_gnome_utils_la_LDFLAGS = -module

libgncmod_gnome_utils_la_LIBADD = \
  -L../gnc-module -L../gnc-module/.libs -lgncmodule \
  -L../engine -L../engine/.libs -lgncmod-engine \
  -L../calculation -L../calculation/.libs -lgncmod-calculation \
  -L../network-utils -L../network-utils/.libs -lgncmod-network-utils \
  -L../app-utils -L../app-utils/.libs -lgncmod-app-utils \
  ${GNOMEUI_LIBS} \
  ${GNOME_LIBDIR} \
  ${GNOME_PRINT_LIBS} \
  ${GTKHTML_LIBS} \
  ${GLADE_LIBS} \
  ${GUILE_LIBS} \
  ${GUPPI_LIBS} \
  ${GLIB_LIBS}

EXTRA_DIST = \
  .cvsignore \
  gnc-dir.h.in

## We borrow guile's convention and use @-...-@ as the substitution
## brackets here, instead of the usual @...@.  This prevents autoconf
## from substituting the values directly into the left-hand sides of
## the sed substitutions.
gnc-dir.h: gnc-dir.h.in
	rm -f $@.tmp
	sed < $@.in > $@.tmp \
	    -e 's:@-GNC_ACCOUNTS_DIR-@:${GNC_ACCOUNTS_DIR}:g' \
	    -e 's:@-GNC_GLADE_DIR-@:${GNC_GLADE_DIR}:g' \
	    -e 's:@-GNC_PIXMAP_DIR-@:${GNC_PIXMAP_DIR}:g'
	mv $@.tmp $@
BUILT_SOURCES += gnc-dir.h

# We have to do this because otherwise automake insists on putting
# these files into the dist tarfile.  If there's a a better way,
# by all means, let us know...
dist-hook:
	rm ${distdir}/gnc-dir.h
