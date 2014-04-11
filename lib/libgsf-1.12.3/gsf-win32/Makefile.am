AM_CPPFLAGS = -I$(top_srcdir) -I$(srcdir)/.. $(LIBGSF_CFLAGS)

if PLATFORM_WIN32

lib_LTLIBRARIES = libgsf-win32-1.la

libgsf_win32_1_la_LIBADD  = $(LIBGSF_LIBS) ../gsf/libgsf-1.la
libgsf_win32_1_la_LDFLAGS = -version-info $(VERSION_INFO) -no-undefined
libgsf_win32_1_la_SOURCES =	\
	gsf-input-win32.c	\
	gsf-output-win32.c

endif

## But headers are installed on all systems:
libgsf_win32_1_includedir = $(includedir)/libgsf-1/gsf-win32
noinst_HEADERS =  \
	gsf-input-win32.h	\
	gsf-output-win32.h
