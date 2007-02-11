if OS_WIN32

install-libtool-import-lib:
	$(INSTALL) .libs/libgsf-1.dll.a $(DESTDIR)$(libdir)

uninstall-libtool-import-lib:
	-rm $(DESTDIR)$(libdir)/libgsf-1.dll.a
else
install-libtool-import-lib:
uninstall-libtool-import-lib:
endif

AM_CPPFLAGS = -I$(top_srcdir)/lib/libgsf-1.12.3 $(LIBGSF_CFLAGS) $(Z_CPPFLAGS) \
	-DGNOMELOCALEDIR=\"$(datadir)/locale\" $(GLIB_CFLAGS)

pkglib_LTLIBRARIES = libgsf-1.la

libgsf_1_la_LIBADD  = $(LIBGSF_LIBS) $(Z_LIBS) $(BZ2_LIBS)
libgsf_1_la_LDFLAGS = -version-info $(VERSION_INFO)
if PLATFORM_WIN32
  libgsf_1_la_LDFLAGS += -no-undefined
endif

libgsf_1_la_SOURCES =		\
	gsf-utils.c		\
	gsf-libxml.c		\
	gsf-doc-meta-data.c	\
	gsf-docprop-vector.c	\
	gsf-msole-impl.h	\
	gsf-msole-utils.c	\
	gsf-timestamp.c		\
	gsf-zip-impl.h		\
	gsf-zip-utils.c		\
	\
	gsf-input.c		\
	gsf-input-bzip.c	\
	gsf-input-gzip.c 	\
	gsf-input-iochannel.c	\
	gsf-input-memory.c 	\
	gsf-input-proxy.c 	\
	gsf-input-stdio.c 	\
	gsf-input-textline.c 	\
	\
	gsf-infile.c 		\
	gsf-infile-msole.c 	\
	gsf-infile-msvba.c 	\
	gsf-infile-stdio.c	\
	gsf-infile-zip.c	\
	\
	gsf-output.c 		\
	gsf-output-bzip.c	\
	gsf-output-csv.c	\
	gsf-output-gzip.c 	\
	gsf-output-iconv.c	\
	gsf-output-iochannel.c	\
	gsf-output-memory.c	\
	gsf-output-stdio.c	\
	\
	gsf-outfile.c 		\
	gsf-outfile-msole.c 	\
	gsf-outfile-stdio.c	\
	gsf-outfile-zip.c	\
	\
	gsf-shared-memory.c	\
	gsf-shared-memory.h	\
	gsf-structured-blob.c	\
	gsf-blob.c		\
	gsf-clip-data.c
#	gsf-output-transaction.c	\
#	gsf-command-context.c	\
#	gsf-io-context.c

libgsf_1_includedir = $(includedir)/libgsf-1/gsf
noinst_HEADERS = 	\
	gsf.h			\
	gsf-utils.h		\
	gsf-libxml.h		\
	gsf-impl-utils.h	\
	gsf-doc-meta-data.h	\
	gsf-docprop-vector.h	\
	gsf-msole-utils.h	\
	gsf-timestamp.h		\
	\
	gsf-input.h		\
	gsf-input-impl.h	\
	gsf-input-bzip.h	\
	gsf-input-gzip.h 	\
	gsf-input-iochannel.h	\
	gsf-input-memory.h 	\
	gsf-input-proxy.h 	\
	gsf-input-stdio.h 	\
	gsf-input-textline.h 	\
	\
	gsf-infile.h 		\
	gsf-infile-impl.h 	\
	gsf-infile-msole.h 	\
	gsf-infile-msvba.h 	\
	gsf-infile-stdio.h	\
	gsf-infile-zip.h	\
	\
	gsf-output.h		\
	gsf-output-impl.h	\
	gsf-output-bzip.h	\
	gsf-output-csv.h	\
	gsf-output-gzip.h 	\
	gsf-output-iconv.h	\
	gsf-output-iochannel.h	\
	gsf-output-memory.h	\
	gsf-output-stdio.h	\
	\
	gsf-outfile.h 		\
	gsf-outfile-impl.h 	\
	gsf-outfile-msole.h	\
	gsf-outfile-stdio.h	\
	gsf-outfile-zip.h	\
	\
	gsf-structured-blob.h   \
	gsf-meta-names.h	\
	gsf-blob.h		\
	gsf-clip-data.h
#	gsf-command-context.h	\
#	gsf-io-context.h

install-data-local: install-libtool-import-lib

uninstall-local: uninstall-libtool-import-lib
