lib_LTLIBRARIES = libcore-utils.la libgw-core-utils.la

libcore_utils_la_SOURCES = \
  gnc-main.c \
  gnc-gconf-utils.c \
  gnc-gdate-utils.c \
  gnc-gkeyfile-utils.c \
  gnc-glib-utils.c \
  gnc-gobject-utils.c \
  gnc-gtk-utils.c

libcore_utils_la_LIBADD = \
  ${GLIB_LIBS} \
  ${GCONF_LIBS} \
  ${GTK_LIBS}

nodist_libgw_core_utils_la_SOURCES = \
  gw-core-utils.c

libgw_core_utils_la_LDFLAGS = ${G_WRAP_LINK_ARGS}
libgw_core_utils_la_LIBADD = \
  libcore-utils.la \
  ${GLIB_LIBS} \
  ${GUILE_LIBS}

noinst_HEADERS = \
  gnc-main.h \
  gnc-gconf-utils.h \
  gnc-gdate-utils.h \
  gnc-gkeyfile-utils.h \
  gnc-glib-utils.h \
  gnc-gobject-utils.h \
  gnc-gtk-utils.h

EXTRA_DIST = ${gwmod_DATA}

AM_CFLAGS = \
  ${G_WRAP_COMPILE_ARGS} \
  ${GUILE_INCS} \
  ${GLIB_CFLAGS} \
  ${GCONF_CFLAGS} \
  ${GTK_CFLAGS}

gwmoddir = ${GNC_GWRAP_LIBDIR}
gwmod_DATA = gw-core-utils-spec.scm
nodist_gwmod_DATA = gw-core-utils.scm

if GNUCASH_SEPARATE_BUILDDIR
#Only needed when srcdir and builddir are different
SCM_FILE_LINKS = gw-core-utils-spec.scm
endif

.scm-links: 
	$(RM) -rf gnucash g-wrapped
	mkdir -p  gnucash g-wrapped
if GNUCASH_SEPARATE_BUILDDIR
	for X in ${SCM_FILE_LINKS} ; do \
	  $(LN_S) -f ${srcdir}/$$X . ; \
	done
endif
	touch .scm-links

clean-local:
	$(RM) -rf gnucash g-wrapped

.INTERMEDIATE: gwrap-files

gw-core-utils.scm gw-core-utils.h gw-core-utils.c gw-core-utils.html: \
  gwrap-files

gwrap-files: \
  gw-core-utils-spec.scm .scm-links ${top_builddir}/config.status
	FLAVOR=gnome $(GUILE) -c \
          "(set! %load-path (cons \"${G_WRAP_MODULE_DIR}\" %load-path)) \
           (primitive-load \"./gw-core-utils-spec.scm\") \
           (gw:generate-wrapset \"gw-core-utils\")"
	touch $@
	( cd g-wrapped; $(LN_S) ../gw-*.scm . )

BUILT_SOURCES = gw-core-utils.scm gw-core-utils.h gw-core-utils.c
CLEANFILES = $(BUILT_SOURCES) .scm-links \
	${SCM_FILE_LINKS} gw-core-utils.html
