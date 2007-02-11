SUBDIRS = utils data app graph gtk drawing ms-compat cut-n-paste

pkglib_LTLIBRARIES = libgoffice-1.la

libgoffice_1_la_LIBADD = 			\
	cut-n-paste/pcre/libpcre.la		\
	utils/libgoffice-utils.la		\
	app/libgoffice-app.la			\
	data/libgoffice-data.la			\
	gtk/libgoffice-gtk.la			\
	cut-n-paste/foocanvas/libfoocanvas.la		\
	graph/libgoffice-graph.la		\
	drawing/libgoffice-drawing.la		\
	ms-compat/libgoffice-ms-compat.la	\
	$(GOFFICE_DEPS_LIBS)

libgoffice_1_la_LDFLAGS = -version-info $(GOFFICE_LIB_VERSION)
if WITH_WIN32
libgoffice_1_la_LDFLAGS += -no-undefined -Wl,-export-all-symbols
libgoffice_1_la_LIBADD += utils/libgoffice-win32-stub.la
endif

BUILT_SOURCES =	goffice-paths.h
CLEANFILES = $(BUILT_SOURCES)

libgoffice_1_la_SOURCES = 	\
	goffice.c		\
	goffice-priv.h

libgoffice_1_ladir = $(goffice_include_dir)
noinst_HEADERS = 	\
	goffice.h

# Depends on this Makefile, because it uses make variables.
goffice-paths.h: Makefile
	@echo 'creating $@'
	@( \
	echo '/* This file has been automatically generated.  Do not edit. */'; \
	echo ''; \
	echo '#ifndef GOFFICE_PATHS_H'; \
	echo '#define GOFFICE_PATHS_H'; \
	echo ''; \
	echo '#define GOFFICE_DATADIR "$(goffice_datadir)"'; \
	echo '#define GOFFICE_LIBDIR "$(goffice_libdir)"'; \
	echo '#define GOFFICE_ICONDIR "$(goffice_icondir)"'; \
	echo '#define GOFFICE_LOCALEDIR "$(goffice_localedir)"'; \
	echo ''; \
	echo '#endif /* GOFFICE_PATHS_H */'; \
	) >$@

include $(top_srcdir)/lib/goffice-0.0.4/goffice.mk
