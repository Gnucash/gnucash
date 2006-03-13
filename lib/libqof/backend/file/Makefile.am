SUBDIRS = . 

lib_LTLIBRARIES = libqof-backend-qsf.la

AM_CFLAGS = \
  -I.. -I../.. \
  -DLOCALE_DIR=\""$(datadir)/locale"\" \
  ${QOF_CFLAGS} \
  ${LIBXML2_CFLAGS} \
  ${GLIB_CFLAGS}

libqof_backend_qsf_la_SOURCES = \
  qsf-backend.c \
  qsf-xml-map.c \
  qsf-xml.c

libqof_backend_qsf_la_LIBADD = \
 ${QOF_LIBS} \
 ${GLIB_LIBS} \
 ${LIBXML2_LIBS}

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
	        -e 's:@-QSF_SCHEMA_DIR-@:${QOF_XML_DIR}:g'
	mv $@.tmp $@

BUILT_SOURCES = qsf-dir.h

CONFIG_CLEAN_FILES = qsf-dir.h

