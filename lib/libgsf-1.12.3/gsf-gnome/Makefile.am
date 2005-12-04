AM_CPPFLAGS = -I$(top_srcdir)/lib/libgsf-1.12.3 $(LIBGSF_GNOME_CFLAGS)

if WITH_LIBGSF_GNOME
lib_LTLIBRARIES = libgsf-gnome-1.la

libgsf_gnome_1_la_LIBADD  = $(top_builddir)/lib/libgsf-1.12.3/gsf/libgsf-1.la $(LIBGSF_GNOME_LIBS)
libgsf_gnome_1_la_LDFLAGS = -version-info $(VERSION_INFO)
if PLATFORM_WIN32
  libgsf_gnome_1_la_LDFLAGS += -no-undefined
endif
libgsf_gnome_1_la_SOURCES =	\
	gsf-input-gnomevfs.c 	\
	gsf-output-gnomevfs.c	\
	gsf-input-bonobo.c	\
	gsf-output-bonobo.c	\
	gsf-shared-bonobo-stream.c

endif

## But headers are installed always:
libgsf_gnome_1_includedir = $(includedir)/libgsf-1/gsf-gnome
libgsf_gnome_1_include_HEADERS =  \
	gsf-input-gnomevfs.h	\
	gsf-output-gnomevfs.h	\
	gsf-input-bonobo.h 	\
	gsf-output-bonobo.h	\
	gsf-shared-bonobo-stream.h
