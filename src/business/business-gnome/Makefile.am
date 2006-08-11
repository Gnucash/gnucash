SUBDIRS = . glade schemas ui
PWD := $(shell pwd)

pkglib_LTLIBRARIES = libgncmod-business-gnome.la

AM_CFLAGS = \
  -I${top_srcdir}/src \
  -I${top_srcdir}/src/core-utils \
  -I${top_srcdir}/src/engine \
  -I${top_srcdir}/src/gnc-module \
  -I${top_srcdir}/src/gnome-utils \
  -I${top_srcdir}/src/gnome \
  -I${top_srcdir}/src/app-utils \
  -I${top_srcdir}/src/gnome-search \
  -I${top_srcdir}/src/report/report-gnome \
  -I${top_srcdir}/src/business/business-core \
  -I${top_srcdir}/src/business/business-ledger \
  -I${top_srcdir}/src/business/dialog-tax-table \
  ${GNOME_PRINT_CFLAGS} \
  ${GNOME_CFLAGS} \
  ${GLADE_CFLAGS} \
  ${GTKHTML_CFLAGS} \
  ${GDK_PIXBUF_CFLAGS} \
  ${GLIB_CFLAGS} \
  ${G_WRAP_COMPILE_ARGS} \
  ${GUILE_INCS} \
  ${QOF_CFLAGS} \
  -I${top_srcdir}/src/register/register-core \
  -I${top_srcdir}/src/register/register-gnome \
  -I${top_srcdir}/src/register/ledger-core

libgncmod_business_gnome_la_SOURCES = \
  swig-business-gnome.c \
  businessmod-gnome.c \
  business-options-gnome.c \
  business-urls.c \
  business-gnome-utils.c \
  dialog-billterms.c \
  dialog-choose-owner.c \
  dialog-customer.c \
  dialog-date-close.c \
  dialog-employee.c \
  dialog-invoice.c \
  dialog-job.c \
  dialog-order.c \
  dialog-payment.c \
  dialog-vendor.c \
  gnc-plugin-business.c \
  gnc-plugin-page-invoice.c \
  search-owner.c

noinst_HEADERS = \
  business-options-gnome.h \
  business-urls.h \
  business-gnome-utils.h \
  dialog-billterms.h \
  dialog-choose-owner.h \
  dialog-customer.h \
  dialog-date-close.h \
  dialog-employee.h \
  dialog-invoice.h \
  dialog-job.h \
  dialog-order.h \
  dialog-payment.h \
  dialog-vendor.h \
  gnc-plugin-business.h \
  gnc-plugin-page-invoice.h \
  search-owner.h

libgncmod_business_gnome_la_LIBADD = \
  ${top_builddir}/src/business/business-core/libgncmod-business-core.la \
  ${top_builddir}/src/business/business-ledger/libgnc-business-ledger.la \
  ${top_builddir}/src/business/dialog-tax-table/libgncmod-dialog-tax-table.la \
  ${top_builddir}/src/register/register-core/libgncmod-register-core.la \
  ${top_builddir}/src/register/register-gnome/libgncmod-register-gnome.la \
  ${top_builddir}/src/report/report-gnome/libgncmod-report-gnome.la \
  ${top_builddir}/src/gnome-search/libgncmod-gnome-search.la \
  ${top_builddir}/src/gnome-utils/libgncmod-gnome-utils.la \
  ${top_builddir}/src/app-utils/libgncmod-app-utils.la \
  ${top_builddir}/src/engine/libgncmod-engine.la \
  ${top_builddir}/src/core-utils/libcore-utils.la \
  ${top_builddir}/src/gnc-module/libgncmodule.la \
  ${GLADE_LIBS} \
  ${GNOME_LIBS} \
  ${G_WRAP_LINK_ARGS} \
  ${GUILE_LIBS} \
  ${GLIB_LIBS} \
  ${QOF_LIBS} \
  ${EFENCE_LIBS}


swig-business-gnome.c: business-gnome.i
	swig -guile $(SWIG_ARGS) -Linkage module -o $@ $<

# For Business Reports
gncscmmoddir = ${GNC_SHAREDIR}/guile-modules/gnucash/report/
gncscmmod_DATA =

gncmoddir = ${GNC_SHAREDIR}/guile-modules/gnucash
gncmod_DATA = business-gnome.scm

EXTRA_DIST = \
  ${gncmod_DATA} \
  ${gncscmmod_DATA}

if GNUCASH_SEPARATE_BUILDDIR
#Only needed when srcdir and builddir are different
#for compiling
SCM_FILE_LINKS = gw-business-gnome-spec.scm
#for running
SCM_FILE_LINKS += ${gncmod_DATA}
endif

.scm-links: 
	rm -f gnucash g-wrapped
	$(LN_S) -f . gnucash 
	$(LN_S) -f . g-wrapped 
if GNUCASH_SEPARATE_BUILDDIR
	for X in ${SCM_FILE_LINKS} ; do \
	  $(LN_S) -f ${srcdir}/$$X . ; \
	done
endif
	touch .scm-links

#BUILT_SOURCES = gw-business-gnome.scm gw-business-gnome.c gw-business-gnome.h
CLEANFILES = $(BUILT_SOURCES) g-wrapped gnucash .scm-links \
	${SCM_FILE_LINKS} gw-business-gnome.html
