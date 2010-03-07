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
  -I$(top_srcdir)/lib/libc \
  -I${top_srcdir}/src \
  -I${top_builddir}/src \
  -I${top_srcdir}/src/gnc-module \
  -I${top_srcdir}/src/calculation \
  -I${top_srcdir}/src/core-utils \
  -I${top_srcdir}/src/engine \
  -I${top_srcdir}/src/libqof/qof \
  ${GUILE_INCS} \
  ${GLIB_CFLAGS} \
  ${GCONF_CFLAGS} \
  ${GTK_CFLAGS}

libgncmod_app_utils_la_SOURCES = \
  swig-app-utils.c \
  file-utils.c \
  gfec.c \
  gnc-account-merge.c \
  gnc-accounting-period.c \
  gnc-component-manager.c \
  gnc-druid.c \
  gnc-druid-cb.c \
  gnc-druid-provider.c \
  gnc-druid-provider-desc.c \
  gnc-druid-provider-desc-edge.c \
  gnc-druid-provider-desc-file.c \
  gnc-druid-provider-desc-multifile.c \
  gnc-druid-provider-file-cb.c \
  gnc-exp-parser.c \
  gnc-gettext-util.c \
  gnc-helpers.c \
  gnc-sx-instance-model.c \
  gncmod-app-utils.c \
  guile-util.c \
  option-util.c

gncincludedir = ${GNC_INCLUDE_DIR}
gncinclude_HEADERS = \
  file-utils.h \
  gfec.h \
  gnc-basic-gobject.h \
  gnc-account-merge.h \
  gnc-accounting-period.h \
  gnc-component-manager.h \
  gnc-druid.h \
  gnc-druid-cb.h \
  gnc-druid-provider.h \
  gnc-druid-provider-desc.h \
  gnc-druid-provider-desc-edge.h \
  gnc-druid-provider-desc-file.h \
  gnc-druid-provider-desc-multifile.h \
  gnc-druid-provider-file-cb.h \
  gnc-exp-parser.h \
  gnc-gettext-util.h \
  gnc-help-utils.h \
  gnc-helpers.h \
  gnc-sx-instance-model.h \
  gnc-ui-common.h \
  guile-util.h \
  option-util.h

libgncmod_app_utils_la_LDFLAGS = -avoid-version

libgncmod_app_utils_la_LIBADD = \
  ${top_builddir}/src/gnc-module/libgnc-module.la \
  ${top_builddir}/src/engine/libgncmod-engine.la \
  ${top_builddir}/src/calculation/libgncmod-calculation.la \
  ${top_builddir}/src/core-utils/libgnc-core-utils.la \
  ${top_builddir}/src/libqof/qof/libgnc-qof.la \
  ${GCONF_LIBS} \
  ${GTK_LIBS} \
  ${GUILE_LIBS} \
  ${GLIB_LIBS}

if BUILDING_FROM_SVN
swig-app-utils.c: app-utils.i ${top_srcdir}/src/base-typemaps.i
	$(SWIG) -guile $(SWIG_ARGS) -Linkage module \
	-I${top_srcdir}/src -o $@ $<
endif

gncmoddir = ${GNC_SHAREDIR}/guile-modules/gnucash
gncmod_DATA = app-utils.scm 

gncscmdir = ${GNC_SHAREDIR}/scm
gncscm_DATA = \
  c-interface.scm \
  config-var.scm \
  date-utilities.scm \
  hooks.scm \
  options.scm \
  prefs.scm \
  simple-obj.scm

noinst_DATA = .scm-links

EXTRA_DIST = \
  app-utils.i \
  ${gncmod_DATA} \
  ${gncscm_DATA}

if OS_WIN32
libgncmod_app_utils_la_SOURCES += gnc-help-utils.c
libgncmod_app_utils_la_LIBADD += $(HTMLHELP_LIBS)
else
EXTRA_DIST += gnc-help-utils.c
endif

if GNUCASH_SEPARATE_BUILDDIR
#For executing test cases
SCM_FILE_LINKS = \
  ${gncmod_DATA} \
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
	( cd gnucash; for A in $(gncmod_DATA) ; do $(LN_S) -f ../$$A . ; done )
if ! OS_WIN32
# Windows knows no "ln -s" but uses "cp": must copy every time (see bug #566567).
	touch .scm-links
endif

CLEANFILES = ${SCM_FILE_LINKS} gnucash .scm-links
MAINTAINERCLEANFILES = swig-app-utils.c

INCLUDES = -DG_LOG_DOMAIN=\"gnc.app-utils\"
