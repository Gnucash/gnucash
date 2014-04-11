SUBDIRS = . tests

BUILT_SOURCES = gnucash_core.c
SWIG_SOURCES = gnucash_core.i

pkgpython_PYTHON = __init__.py function_class.py \
gnucash_core.py gnucash_core_c.py gnucash_business.py

pkgpyexec_LTLIBRARIES = _gnucash_core_c.la
_gnucash_core_c_la_SOURCES = $(BUILT_SOURCES) $(SWIG_SOURCES)
_gnucash_core_c_la_CPPFLAGS = $(PYTHON_CPPFLAGS) \
                              -I$(top_srcdir)/src $(QOF_CFLAGS) \
                              $(GLIB_CFLAGS) $(GUILE_INCS) \
							  -I$(top_srcdir)/src/engine \
							  -I$(top_srcdir)/src/business/business-core 

# Suppress all warnings for now, but we really only need to -Wno-implicit
AM_CFLAGS = -w

_gnucash_core_c_la_LDFLAGS = -avoid-version -module
_gnucash_core_c_la_LIBADD = ${QOF_LIBS} ${GUILE_LIBS} ${GLIB_LIBS} \
    ${top_builddir}/src/gnc-module/libgnc-module.la \
	${top_builddir}/src/engine/libgncmod-engine.la \
	${top_builddir}/src/business/business-core/libgncmod-business-core.la

gnucash_core.c : $(SWIG_SOURCES)
	swig $(SWIG_PYTHON_OPT) -Wall -Werror \
        -I$(top_srcdir)/src -I$(top_srcdir)/src/engine \
		-I$(top_srcdir)/src/business/business-core \
		$(QOF_CFLAGS) -o $@ $<
