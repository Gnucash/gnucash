SUBDIRS = . test

pkglib_LTLIBRARIES = libgncmod-app-utils.la

CFLAGS = @CFLAGS@ ${GLIB_CFLAGS} ${G_WRAP_COMPILE_ARGS}

INCLUDES = -I${top_srcdir}/src/gnc-module \
           -I${top_srcdir}/src/calculation \
           -I${top_srcdir}/src/engine \
           ${GNOME_INCLUDEDIR} \
           ${GUILE_INCS}

libgncmod_app_utils_la_SOURCES = \
  gfec.c \
  global-options.c \
  gnc-component-manager.c \
  gnc-euro.c \
  gnc-exp-parser.c \
  gnc-gettext-util.c \
  gnc-helpers.c \
  gncmod-app-utils.c \
  gnc-ui-util.c \
  guile-util.c \
  option-util.c 

gncincludedir = ${GNC_INCLUDE_DIR}
gncinclude_HEADERS = \
  gfec.h \
  global-options.h \
  gnc-component-manager.h \
  gnc-euro.h \
  gnc-exp-parser.h \
  gnc-gettext-util.h \
  gnc-helpers.h \
  gnc-ui-common.h \
  gnc-ui-util.h \
  guile-util.h \
  option-util.h

libgncmod_app_utils_la_LDFLAGS = -module

libgncmod_app_utils_la_LIBADD = \
  -L../gnc-module -L../gnc-module/.libs -lgncmodule \
  -L../engine -L../engine/.libs -lgncmod-engine \
  -L../calculation -L../calculation/.libs -lgncmod-calculation \
  ${GUILE_LIBS} \
  ${GLIB_LIBS}

libgw_app_utils_la_SOURCES = gw-app-utils.c
libgw_app_utils_la_LDFLAGS = -module

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
  utilities.scm

gwmoddir = ${GNC_GWRAP_LIBDIR}
gwmod_LTLIBRARIES = libgw-app-utils.la
gwmod_DATA = gw-app-utils-spec.scm

noinst_DATA = .scm-links

EXTRA_DIST = \
  .cvsignore \
  ${gncmod_DATA} \
  ${gncscm_DATA} \
  ${gwmod_DATA}

.scm-links: 
	rm -f gnucash g-wrapped
	ln -sf . gnucash 
	ln -sf . g-wrapped 
	touch .scm-links

gw-app-utils.c gw-app-utils.h: .scm-links gw-app-utils-spec.scm
	FLAVOR=gnome guile -c \
	 "(set! %load-path (cons \"${G_WRAP_MODULE_DIR}\" %load-path)) \
	  (set! %load-path (cons \"${PWD}\" %load-path)) \
	  (set! %load-path (cons \"${top_srcdir}/src/engine\" %load-path)) \
	  (primitive-load \"./gw-app-utils-spec.scm\") \
	  (gw:generate-module \"gw-app-utils\")"

BUILT_SOURCES += gw-app-utils.c gw-app-utils.h
CLEANFILES += gw-app-utils.c gw-app-utils.h gw-app-utils.html \
              gnucash g-wrapped .scm-links
