#SUBDIRS = . test
SUBDIRS = .

pkglib_LTLIBRARIES = libgncmod-html.la

AM_CPPFLAGS = \
  -I${top_srcdir}/src/core-utils \
  -I${top_srcdir}/src/gnc-module \
  -I${top_srcdir}/src/engine \
  -I${top_srcdir}/src/gnome-utils \
  -I${top_srcdir}/src/app-utils \
  -I${top_srcdir}/src \
  -I${top_builddir}/src \
  -I${top_srcdir}/lib/libc \
  ${QOF_CFLAGS} \
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
  gnc-html-webkit.c \
  swig-gnc-html.c

gncincludedir = ${GNC_INCLUDE_DIR}
gncinclude_HEADERS = \
  gnc-html-history.h \
  gnc-html.h \
  gnc-html-p.h \
  gnc-html-factory.h \
  gnc-html-webkit.h \
  gnc-html-extras.h \
  gnc-html-webkit-p.h

libgncmod_html_la_LDFLAGS = -avoid-version

libgncmod_html_la_LIBADD = \
  ${top_builddir}/src/core-utils/libgnc-core-utils.la \
  ${top_builddir}/src/gnc-module/libgnc-module.la \
  ${top_builddir}/src/engine/libgncmod-engine.la \
  ${top_builddir}/src/gnome-utils/libgncmod-gnome-utils.la \
  ${top_builddir}/src/app-utils/libgncmod-app-utils.la \
  $(top_builddir)/lib/libc/libc-missing.la \
  ${WEBKIT_LIBS} \
  ${GTK_LIBS} \
  ${GLIB_LIBS} \
  ${DB_LIBS} \
  ${QOF_LIBS} \
  ${GUILE_LIBS} \
  ${REGEX_LIBS}

if BUILDING_FROM_VCS
swig-gnc-html.c: gnc-html.i gnc-html.h \
                    ${top_srcdir}/src/base-typemaps.i
	$(SWIG) -guile $(SWIG_ARGS) -Linkage module \
	-I${top_srcdir}/src -o $@ $<
if ! OS_WIN32
if ! SWIG_DIST_FAIL
	if ! `grep "define scm_from_utf8_string" $@ > /dev/null 2>&1`; then \
	  patch $@ $(top_srcdir)/src/swig-utf8.patch; \
	fi
endif
endif
endif

EXTRA_DIST = \
  gnc-html.i

CLEANFILES = $(BUILT_SOURCES) gnucash
MAINTAINERCLEANFILES = swig-gnc-html.c

# FIXME: Symlinking directories only works on non-win32.
if !PLATFORM_WIN32
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

AM_CPPFLAGS += -DG_LOG_DOMAIN=\"gnc.html\"
