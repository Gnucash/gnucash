pkglib_LTLIBRARIES = libgncmod-business-gnome.la libgw-business-gnome.la

AM_CFLAGS = \
  -I${top_srcdir}/src \
  -I${top_srcdir}/src/engine \
  -I${top_srcdir}/src/gnc-module \
  -I${top_srcdir}/src/gnome-utils \
  -I${top_srcdir}/src/gnome \
  -I${top_srcdir}/src/app-utils \
  -I${top_srcdir}/src/gnome-search \
  -I${top_srcdir}/src/report/report-gnome \
  -I${top_srcdir}/src/business/business-core \
  -I${top_srcdir}/src/business/business-ledger \
  ${GNOME_PRINT_CFLAGS} \
  ${GNOME_INCLUDEDIR} \
  ${GLADE_CFLAGS} \
  ${GTKHTML_CFLAGS} \
  ${GDK_PIXBUF_CFLAGS} \
  ${GLIB_CFLAGS} \
  ${G_WRAP_COMPILE_ARGS} \
  ${GUILE_INCS} \
  -I${top_srcdir}/src/register/register-core \
  -I${top_srcdir}/src/register/register-gnome \
  -I${top_srcdir}/src/register/ledger-core

libgncmod_business_gnome_la_SOURCES = \
  businessmod-gnome.c \
  business-utils.c \
  dialog-customer.c \
  dialog-date-close.c \
  dialog-employee.c \
  dialog-invoice.c \
  dialog-job.c \
  dialog-order.c \
  dialog-payment.c \
  dialog-vendor.c \
  search-owner.c \
  gnc-business-utils.c

noinst_HEADERS = \
  business-utils.h \
  dialog-customer.h \
  dialog-date-close.h \
  dialog-employee.h \
  dialog-invoice.h \
  dialog-job.h \
  dialog-order.h \
  dialog-payment.h \
  dialog-vendor.h \
  search-owner.h \
  gnc-business-utils.h

libgncmod_business_gnome_la_LDFLAGS = -module

libgncmod_business_gnome_la_LIBADD = \
  ${top_builddir}/src/business/business-ledger/libgnc-business-ledger.la \
  ${top_builddir}/src/gnc-module/libgncmodule.la \
  ${GUILE_LIBS} \
  ${GNOMEUI_LIBS} \
  ${G_WRAP_LINK_ARGS} \
  ${GLIB_LIBS} \
  ${EFENCE_LIBS}


libgw_business_gnome_la_SOURCES = gw-business-gnome.c
libgw_business_gnome_la_LDFLAGS = -module ${G_WRAP_LINK_ARGS}

gncmoddir = ${GNC_SHAREDIR}/guile-modules/gnucash
gncmod_DATA = business-gnome.scm

gladedir = $(GNC_GLADE_DIR)
glade_DATA = \
  customer.glade \
  date-close.glade \
  employee.glade \
  invoice.glade \
  job.glade \
  order.glade \
  payment.glade \
  vendor.glade

gwmoddir = ${GNC_GWRAP_LIBDIR}
gwmod_DATA = \
  gw-business-gnome.scm \
  gw-business-gnome-spec.scm

EXTRA_DIST = \
  .cvsignore \
  ${glade_DATA} \
  ${gncmod_DATA} \
  ${gwmod_DATA}

if GNUCASH_SEPARATE_BUILDDIR
#Only needed when srcdir and builddir are different
SCM_FILE_LINKS = gw-business-gnome-spec.scm
endif

.scm-links: 
	rm -f gnucash g-wrapped
	ln -sf . gnucash 
	ln -sf . g-wrapped 
if GNUCASH_SEPARATE_BUILDDIR
	for X in ${SCM_FILE_LINKS} ; do \
	  ln -sf ${srcdir}/$$X . ; \
	done
endif
	touch .scm-links

gncmod-business-gnome.c: gw-business-gnome.h

gw-business-gnome.scm gw-business-gnome.c gw-business-gnome.h: .scm-links gw-business-gnome-spec.scm
	FLAVOR=gnome guile -c \
	 "(set! %load-path (cons \"${G_WRAP_MODULE_DIR}\" %load-path)) \
	 (set! %load-path (cons \"${PWD}\" %load-path)) \
	 (set! %load-path (cons \"../business-core\" %load-path)) \
	 (set! %load-path (cons \"../../engine\" %load-path)) \
	 (set! %load-path (cons \"../../app-utils\" %load-path)) \
	 (set! %load-path (cons \"../../gnome-utils\" %load-path)) \
	 (primitive-load \"./gw-business-gnome-spec.scm\") \
	 (gw:generate-wrapset \"gw-business-gnome\")"

BUILT_SOURCES = gw-business-gnome.scm gw-business-gnome.c gw-business-gnome.h
CLEANFILES = ${BUILT_SOURCES} gw-business-gnome.html \
             ${SCM_FILE_LINKS} g-wrapped gnucash .scm-links


