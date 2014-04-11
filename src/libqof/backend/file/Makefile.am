SUBDIRS = . 

pkglib_LTLIBRARIES = libgncqof-backend-qsf.la

AM_CPPFLAGS = \
  -I.. -I../.. \
  -I$(top_srcdir)/lib/libc \
  -DLOCALE_DIR=\""$(datadir)/locale"\" \
  -I${top_srcdir}/src/libqof/qof \
  ${LIBXML2_CFLAGS} \
  ${GLIB_CFLAGS}

libgncqof_backend_qsf_la_SOURCES = \
  qsf-backend.c \
  qsf-xml-map.c \
  qsf-xml.c

libgncqof_backend_qsf_la_LDFLAGS = -module -avoid-version
libgncqof_backend_qsf_la_LIBADD = \
  ${top_builddir}/src/libqof/qof/libgnc-qof.la \
  ${GLIB_LIBS} \
  ${REGEX_LIBS} \
  ${LIBXML2_LIBS} \
  $(top_builddir)/lib/libc/libc-missing.la

qsfschemadir = $(QOF_XML_DIR)
qsfschema_DATA = \
 qsf-object.xsd.xml \
 qsf-map.xsd.xml \
 pilot-qsf-GnuCashInvoice.xml \
 pilot-qsf-gncCustomer.xml

EXTRA_DIST = \
  $(qsfschema_DATA) \
  qsf-dir.h.in \
  qof-backend-qsf.h \
  qsf-xml.h

qsf-dir.h: qsf-dir.h.in Makefile
	rm -f $@.tmp
	sed < $< > $@.tmp \
	        -e 's#@-QSF_SCHEMA_DIR-@#${QOF_XML_DIR}#g'
	mv $@.tmp $@

BUILT_SOURCES = qsf-dir.h
CLEANFILES = qsf-dir.h
