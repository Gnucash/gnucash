SUBDIRS=. test

CFLAGS=@CFLAGS@ ${GLIB_CFLAGS} ${G_WRAP_COMPILE_ARGS}

lib_LTLIBRARIES=libgncmodule.la 

libgncmodule_la_SOURCES=gnc-module.c 
libgncmodule_la_LIBADD=${G_WRAP_LINK_ARGS} ${GLIB_LIBS}
libgncmodule_la_LDFLAGS=-module

libgw_gnc_module_la_SOURCES=gw-gnc-module.c
libgw_gnc_module_la_DEPENDENCIES = libgncmodule.la
libgw_gnc_module_la_LIBADD=${G_WRAP_LINK_ARGS} ${GLIB_LIBS}
libgw_gnc_module_la_LDFLAGS=-module -lgncmodule

gncmoddir=${GNC_SHAREDIR}/guile-modules/gnucash
gncmod_DATA=gnc-module.scm

gwmoddir=${GNC_SHAREDIR}/guile-modules/g-wrapped
gwmod_LTLIBRARIES=libgw-gnc-module.la
gwmod_DATA=gw-gnc-module-spec.scm 

noinst_HEADERS = \
  gnc-module-api.h \
  gnc-module.h

noinst_DATA=.scm-links

.scm-links: 
	rm -f gnucash g-wrapped
	ln -sf . gnucash
	ln -sf . g-wrapped
	touch .scm-links

gw-gnc-module.c gw-gnc-module.h: gw-gnc-module-spec.scm
	FLAVOR=gnome guile -c \
	  "(set! %load-path (cons \"${G_WRAP_MODULE_DIR}\" %load-path)) \
	  (primitive-load \"./gw-gnc-module-spec.scm\") \
	  (gw:generate-module \"gw-gnc-module\")"

BUILT_SOURCES += gw-gnc-module.h gw-gnc-module.c
CLEANFILES += gw-gnc-module.h gw-gnc-module.c gw-gnc-module.html g-wrapped \
gnucash .scm-links
