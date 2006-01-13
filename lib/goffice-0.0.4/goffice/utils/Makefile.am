if WITH_WIN32
    lib_LTLIBRARIES = libgoffice-win32-stub.la

    libgoffice_win32_stub_la_SOURCES = win32-stub.c win32-stub.h
    libgoffice_win32_stub_la_LIBADD = -luuid -lhtmlhelp -lurlmon
    libgoffice_win32_stub_la_LDFLAGS = -no-undefined -export-symbols $(srcdir)/goffice-win32-stub.def
endif
EXTRA_DIST = goffice-win32-stub.def

patternsdir = $(goffice_datadir)/patterns
dist_patterns_DATA = svg-patterns.xml

noinst_LTLIBRARIES = libgoffice-utils.la
libgoffice_utils_la_SOURCES =	\
	go-marshalers.list	\
	\
	go-color.c		\
	go-file.c		\
	go-font.c		\
	\
	go-gradient.c		\
	go-line.c		\
	go-marker.c		\
	go-pattern.c		\
	go-geometry.c		\
	go-math.c		\
	datetime.c		\
	format.c		\
	formats.c		\
	go-format.c		\
	go-format-match.c	\
	regutf8.c 		\
	\
	go-glib-extras.c	\
	go-libxml-extras.c	\
	go-rangefunc.c	\
	go-regression.c

libgoffice_utils_ladir = $(goffice_include_dir)/utils
noinst_HEADERS = 	\
	goffice-utils.h		\
	go-color.h		\
	go-file.h		\
	go-font.h		\
	go-gradient.h		\
	go-line.h		\
	go-marker.h		\
	go-pattern.h		\
	go-units.h		\
	go-geometry.h		\
	go-math.h		\
	datetime.h		\
	format-impl.h		\
	go-format.h		\
	go-format-match.h	\
	regutf8.h 		\
	go-glib-extras.h	\
	go-libxml-extras.h	\
	go-rangefunc.h	\
	go-regression.h

CLEANFILES =			\
	go-marshalers.h		\
	go-marshalers.c

# A hint is needed to build the header first:
BUILT_SOURCES = go-marshalers.h

# Another hint, see bugs #172211 and #172212:
non-intermediate: go-marshalers.c

GENMARSHAL_COMMAND = $(GLIB_GENMARSHAL) --prefix=go_

.list.h:
	$(GENMARSHAL_COMMAND) --header $< >$@

.list.c:
	(echo '/* This file has been automatically generated.  Do not edit. */' && \
	echo '#include <goffice/goffice-config.h>' && \
	echo '#include "$*.h"' && \
	$(GENMARSHAL_COMMAND) --body $< ) >$@

include $(top_srcdir)/lib/goffice-0.0.4/goffice.mk
