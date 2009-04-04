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
  ${GTK_CFLAGS} \
  ${GNOME_CFLAGS} \
  ${GOFFICE_CFLAGS}

libgncmod_html_la_SOURCES = \
  gncmod-html.c \
  gnc-html.c \
  gnc-html-history.c \
  gnc-html-graph-gog.c \
  gnc-html-factory.c \
  swig-gnc-html.c

gncincludedir = ${GNC_INCLUDE_DIR}
gncinclude_HEADERS = \
  gnc-html-graph-gog.h \
  gnc-html-history.h \
  gnc-html.h \
  gnc-html-factory.h

libgncmod_html_la_LDFLAGS = -avoid-version

libgncmod_html_la_LIBADD = \
  ${top_builddir}/src/core-utils/libgnc-core-utils.la \
  ${top_builddir}/src/gnc-module/libgnc-module.la \
  ${top_builddir}/src/engine/libgncmod-engine.la \
  ${top_builddir}/src/calculation/libgncmod-calculation.la \
  ${top_builddir}/src/gnome-utils/libgncmod-gnome-utils.la \
  ${top_builddir}/src/app-utils/libgncmod-app-utils.la \
  $(top_builddir)/lib/libc/libc-missing.la \
  ${GNOME_LIBS} \
  ${GDK_PIXBUF_LIBS} \
  ${GLIB_LIBS} \
  ${DB_LIBS} \
  ${QOF_LIBS} \
  ${GOFFICE_LIBS} \
  ${GUILE_LIBS} \
  ${REGEX_LIBS}

if HTML_USING_WEBKIT
AM_CPPFLAGS += ${WEBKIT_CFLAGS}
libgncmod_html_la_SOURCES += \
  gnc-html-webkit.c \
  gnc-html-graph-gog-webkit.c
libgncmod_html_la_LIBADD += \
  ${WEBKIT_LIBS}
else
AM_CPPFLAGS += ${GTKHTML_CFLAGS}
libgncmod_html_la_SOURCES += \
  gnc-html-gtkhtml.c \
  gnc-html-graph-gog-gtkhtml.c
libgncmod_html_la_LIBADD += \
  ${GTKHTML_LIBS}

if !GTKHTML_USES_GTKPRINT
  AM_CPPFLAGS += ${GNOME_PRINT_CFLAGS}
  libgncmod_html_la_LIBADD += ${GNOME_PRINT_LIBS}
endif

endif

if BUILDING_FROM_SVN
swig-gnc-html.c: gnc-html.i gnc-html.h \
                    ${top_srcdir}/src/base-typemaps.i
	$(SWIG) -guile $(SWIG_ARGS) -Linkage module \
	-I${top_srcdir}/src -o $@ $<
endif

EXTRA_DIST = \
  gnc-svninfo.h \
  gnc-html.i

if !GTKHTML_USES_GTKPRINT
  AM_CPPFLAGS += ${GNOME_PRINT_CFLAGS}
  libgncmod_html_la_LIBADD += ${GNOME_PRINT_LIBS}
endif

CLEANFILES = $(BUILT_SOURCES) gnucash
MAINTAINERCLEANFILES = swig-html.c

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

INCLUDES = -DG_LOG_DOMAIN=\"gnc.html\"
