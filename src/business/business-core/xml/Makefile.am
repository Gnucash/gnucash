SUBDIRS = .

pkglib_LTLIBRARIES = libgncmod-business-backend-xml.la

AM_CPPFLAGS = \
  -I${top_srcdir}/src \
  -I${top_srcdir}/src/backend \
  -I${top_srcdir}/src/backend/xml \
  -I${top_srcdir}/src/engine \
  -I${top_srcdir}/src/gnc-module \
  -I${top_srcdir}/src/business/business-core \
  -I${top_srcdir}/src/libqof/qof \
  ${LIBXML2_CFLAGS} \
  ${GLIB_CFLAGS}

libgncmod_business_backend_xml_la_SOURCES =

noinst_HEADERS =

libgncmod_business_backend_xml_la_LDFLAGS = -module -avoid-version

libgncmod_business_backend_xml_la_LIBADD = \
  ${top_builddir}/src/business/business-core/libgncmod-business-core.la \
  ${top_builddir}/src/backend/xml/libgnc-backend-xml-utils.la \
  ${top_builddir}/src/engine/libgncmod-engine.la \
  ${top_builddir}/src/gnc-module/libgnc-module.la \
  ${top_builddir}/src/libqof/qof/libgnc-qof.la \
  ${LIBXML2_LIBS} \
  ${GLIB_LIBS}
