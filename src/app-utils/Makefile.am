# This program is free software; you can redistribute it and/or    
# modify it under the terms of the GNU General Public License as   
# published by the Free Software Foundation; either version 2 of   
# the License, or (at your option) any later version.              
#                                                                  
# This program is distributed in the hope that it will be useful,  
# but WITHOUT ANY WARRANTY; without even the implied warranty of   
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the    
# GNU General Public License for more details.                     
#                                                                  
# You should have received a copy of the GNU General Public License
# along with this program; if not, contact:
#
# Free Software Foundation           Voice:  +1-617-542-5942
# 51 Franklin Street, Fifth Floor    Fax:    +1-617-542-2652
# Boston, MA  02110-1301,  USA       gnu@gnu.org

SUBDIRS = . test

pkglib_LTLIBRARIES = libgncmod-app-utils.la

AM_CPPFLAGS = \
  -Wno-error=deprecated-declarations \
  -I$(top_srcdir)/lib/libc \
  -I${top_srcdir}/src \
  -I${top_builddir}/src \
  -I${top_srcdir}/src/gnc-module \
  -I${top_srcdir}/src/app-utils/calculation \
  -I${top_srcdir}/src/core-utils \
  -I${top_srcdir}/src/engine \
  -I${top_srcdir}/src/libqof/qof \
  -I${top_srcdir}/src/backend/xml \
  ${GUILE_CFLAGS} \
  ${PYTHON_CPPFLAGS} \
  ${GLIB_CFLAGS} \
  ${GTK_CFLAGS} \
  ${LIBXML2_CFLAGS} \
  ${LIBXSLT_CFLAGS}

libgncmod_app_utils_la_SOURCES = \
  calculation/expression_parser.c \
  calculation/fin.c \
  swig-app-utils-guile.c \
  business-helpers.c \
  business-options.c \
  QuickFill.c \
  file-utils.c \
  gfec.c \
  gnc-account-merge.c \
  gnc-accounting-period.c \
  gnc-addr-quickfill.c \
  gnc-component-manager.c \
  gnc-entry-quickfill.c \
  gnc-euro.c \
  gnc-exp-parser.c \
  gnc-gettext-util.c \
  gnc-gsettings.c \
  gnc-helpers.c \
  gnc-prefs-utils.c \
  gnc-sx-instance-model.c \
  gnc-state.c \
  gncmod-app-utils.c \
  gnc-ui-balances.c \
  gnc-ui-util.c \
  guile-util.c \
  option-util.c

gncincludedir = ${GNC_INCLUDE_DIR}
gncinclude_HEADERS = \
  QuickFill.h \
  business-helpers.h \
  business-options.h \
  file-utils.h \
  gfec.h \
  gnc-basic-gobject.h \
  gnc-account-merge.h \
  gnc-accounting-period.h \
  gnc-addr-quickfill.h \
  gnc-component-manager.h \
  gnc-entry-quickfill.h \
  gnc-euro.h \
  gnc-exp-parser.h \
  gnc-gettext-util.h \
  gnc-gsettings.h \
  gnc-help-utils.h \
  gnc-helpers.h \
  gnc-prefs-utils.h \
  gnc-sx-instance-model.h \
  gnc-state.h \
  gnc-ui-balances.h \
  gnc-ui-util.h \
  guile-util.h \
  option-util.h

libgncmod_app_utils_la_LDFLAGS = -avoid-version

libgncmod_app_utils_la_LIBADD = \
  ${top_builddir}/src/gnc-module/libgnc-module.la \
  ${top_builddir}/src/engine/libgncmod-engine.la \
  ${top_builddir}/src/core-utils/libgnc-core-utils.la \
  ${top_builddir}/src/libqof/qof/libgnc-qof.la \
  ${GTK_LIBS} \
  ${GUILE_LIBS} \
  ${GLIB_LIBS} \
  ${LIBXML2_LIBS} \
  ${LIBXSLT_LIBS}

if BUILDING_FROM_SCM
swig-app-utils-guile.c: app-utils.i ${top_srcdir}/src/base-typemaps.i
	$(SWIG) -guile $(SWIG_ARGS) -Linkage module \
	-I${top_srcdir}/src -o $@ $<
if ! OS_WIN32
if ! SWIG_DIST_FAIL
	if ! `grep "define scm_from_utf8_string" $@ > /dev/null 2>&1`; then \
	  patch $@ $(top_srcdir)/src/swig-utf8.patch; \
	fi
endif
endif
swig-app-utils-python.c: app-utils.i ${top_srcdir}/src/base-typemaps.i
	$(SWIG) -python -Wall -Werror $(SWIG_ARGS) \
	-I${top_srcdir}/src -o $@ $<
endif

if WITH_PYTHON
pkglib_LTLIBRARIES += libgncmod-app-utils-python.la

libgncmod_app_utils_python_la_SOURCES = \
  swig-app-utils-python.c

libgncmod_app_utils_python_la_LDFLAGS = \
  ${PYTHON_LDFLAGS}

libgncmod_app_utils_python_la_LIBADD = \
  ${PYTHON_LIBS} \
  ${PYTHON_EXTRA_LIBS} \
  libgncmod-app-utils.la
endif

gncscmmoddir = ${GNC_SCM_INSTALL_DIR}/gnucash
gncscmmod_DATA = \
  gettext.scm \
  app-utils.scm

gncscmdir = ${GNC_SCM_INSTALL_DIR}
gncscm_DATA = \
  business-options.scm \
  business-prefs.scm \
  c-interface.scm \
  config-var.scm \
  date-utilities.scm \
  hooks.scm \
  migrate-prefs.scm \
  options.scm \
  prefs.scm \
  simple-obj.scm
  
prefsmigrationdir = $(GNC_SHAREDIR)
prefsmigration_DATA = \
  make-prefs-migration-script.xsl \
  migratable-prefs.xml

noinst_DATA = .scm-links

noinst_HEADERS = \
  calculation/finvar.h \
  calculation/finproto.h \
  calculation/fin_spl_protos.h \
  calculation/fin_static_proto.h

EXTRA_DIST = \
  swig-app-utils-python.c \
  app-utils.i \
  ${gncscmmod_DATA} \
  ${gncscm_DATA} \
  ${prefsmigration_DATA}

if OS_WIN32
libgncmod_app_utils_la_SOURCES += gnc-help-utils.c
libgncmod_app_utils_la_LIBADD += $(HTMLHELP_LIBS)
else
EXTRA_DIST += gnc-help-utils.c
endif

if GNUCASH_SEPARATE_BUILDDIR
#For executing test cases
SCM_FILE_LINKS = \
  ${gncscmmod_DATA} \
  ${gncscm_DATA}
endif

.scm-links: 
	$(RM) -rf gnucash
	mkdir -p  gnucash
if GNUCASH_SEPARATE_BUILDDIR
	for X in ${SCM_FILE_LINKS} ; do \
	  $(LN_S) -f ${srcdir}/$$X . ; \
	done
endif
	( cd gnucash; for A in $(gncscmmod_DATA) ; do $(LN_S) -f ../$$A . ; done )
if ! OS_WIN32
# Windows knows no "ln -s" but uses "cp": must copy every time (see bug #566567).
	touch .scm-links
endif

if GNC_HAVE_GUILE_2
GUILE_COMPILE_ENV = \
  --gnc-module-dir ${top_builddir}/src/engine \
  --guile-load-dir ${top_builddir}/src/app-utils \
  --guile-load-dir ${top_builddir}/src/core-utils \
  --guile-load-dir ${top_builddir}/src/engine \
  --guile-load-dir ${top_builddir}/src/gnc-module \
  --guile-load-dir ${top_builddir}/src/scm \
  --library-dir    ${top_builddir}/src/app-utils \
  --library-dir    ${top_builddir}/src/core-utils \
  --library-dir    ${top_builddir}/src/gnc-module

%.go : %.scm .scm-links $(pkglib_LTLIBRARIES)
	GNC_UNINSTALLED=yes \
	GNC_BUILDDIR=${top_builddir} \
	$(shell ${top_builddir}/src/gnc-test-env --no-exports ${GUILE_COMPILE_ENV}) \
	$(GUILD) compile -o $@ $<

gncscmmodcachedir = ${GNC_SCM_INSTALL_DIR}/ccache/@GUILE_EFFECTIVE_VERSION@/gnucash
gncscmmodcache_DATA = $(gncscmmod_DATA:.scm=.go)

gncscmcachedir = ${GNC_SCM_INSTALL_DIR}/ccache/@GUILE_EFFECTIVE_VERSION@
gncscmcache_DATA = $(gncscm_DATA:.scm=.go)
endif

clean-local:
	rm -rf gnucash

CLEANFILES = ${SCM_FILE_LINKS}  .scm-links
MAINTAINERCLEANFILES = swig-app-utils-guile.c swig-app-utils-python.c

AM_CPPFLAGS += -DG_LOG_DOMAIN=\"gnc.app-utils\"
