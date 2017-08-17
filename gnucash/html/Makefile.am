#SUBDIRS = . test
SUBDIRS = .

pkglib_LTLIBRARIES = libgncmod-html.la

AM_CPPFLAGS = \
  -I${top_srcdir}/libgnucash/core-utils \
  -I${top_srcdir}/libgnucash/gnc-module \
  -I${top_srcdir}/libgnucash/engine \
  -I${top_srcdir}/gnucash/gnome-utils \
  -I${top_srcdir}/libgnucash/app-utils \
  -I${top_srcdir}/common \
  -I${top_builddir}/common \
  -I${top_srcdir}/borrowed/libc \
  ${GLIB_CFLAGS} \
  ${GUILE_CFLAGS} \
  ${GTK_CFLAGS} \
  ${GNOME_CFLAGS} \
  ${WEBKIT_CFLAGS}

libgncmod_html_la_SOURCES = \
  gncmod-html.c \
  gnc-html.c \
  gnc-html-history.c \
  gnc-html-factory.c \
  gnc-html-webkit2.c \
  swig-gnc-html.c

gncincludedir = ${GNC_INCLUDE_DIR}
gncinclude_HEADERS = \
  gnc-html-history.h \
  gnc-html.h \
  gnc-html-p.h \
  gnc-html-factory.h \
  gnc-html-webkit2.h \
  gnc-html-extras.h \
  gnc-html-webkit-p.h

libgncmod_html_la_LDFLAGS = -avoid-version

libgncmod_html_la_LIBADD = \
  ${top_builddir}/libgnucash/core-utils/libgnc-core-utils.la \
  ${top_builddir}/libgnucash/gnc-module/libgnc-module.la \
  ${top_builddir}/libgnucash/engine/libgncmod-engine.la \
  ${top_builddir}/gnucash/gnome-utils/libgncmod-gnome-utils.la \
  ${top_builddir}/libgnucash/app-utils/libgncmod-app-utils.la \
  $(top_builddir)/borrowed/libc/libc-missing.la \
  ${WEBKIT_LIBS} \
  ${GTK_LIBS} \
  ${GLIB_LIBS} \
  ${DB_LIBS} \
  ${GUILE_LIBS} \
  ${REGEX_LIBS}

if BUILDING_FROM_VCS
swig-gnc-html.c: gnc-html.i gnc-html.h \
                    ${top_srcdir}/common/base-typemaps.i
	$(SWIG) -guile $(SWIG_ARGS) -Linkage module \
	-I${top_srcdir}/common -o $@ $<
if ! OS_WIN32
if ! SWIG_DIST_FAIL
	if ! `grep "define scm_from_utf8_string" $@ > /dev/null 2>&1`; then \
	  patch $@ $(top_srcdir)/common/swig-utf8.patch; \
	fi
endif
endif
endif

EXTRA_DIST = \
  gnc-html.i \
  CMakeLists.txt \
  gnc-html-webkit.h \
  gnc-html-webkit1.h \
  gnc-html-webkit1.c

CLEANFILES = $(BUILT_SOURCES) gnucash
MAINTAINERCLEANFILES = swig-gnc-html.c

# We do this only for the gnome help tool on Linux and MacPorts.
if !PLATFORM_WIN32
if !PLATFORM_OSX_QUARTZ
#
# I hate inconsistent standards. Autotools puts help files into
# ${datadir}/gnome/help/${program} while the gnome2 libraries expect
# them in ${pkgdatadir}/gnome/help/${program}.
#
install-data-hook:
	$(LN_S) -f ../gnome ${DESTDIR}${pkgdatadir}

uninstall-hook:
	rm -f ${DESTDIR}${pkgdatadir}/gnome
endif
endif

AM_CPPFLAGS += -DG_LOG_DOMAIN=\"gnc.html\"
