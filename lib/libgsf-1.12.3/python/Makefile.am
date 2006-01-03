AM_CPPFLAGS = $(LIBGSF_CFLAGS) $(PYTHON_INCLUDES) $(PYGTK_CFLAGS)

#defsdir = $(datadir)/pygtk/2.0/defs

#defs_DATA = gconf.defs

# libgsf binding

#pyexec_LTLIBRARIES = gsfmodule.la
lib_LTLIBRARIES = gsfmodule.la
gsfmodule_la_CFLAGS = $(LIBGSF_CFLAGS)
gsfmodule_la_LDFLAGS = -module -avoid-version -export-symbols-regex initgsf
gsfmodule_la_LIBADD = $(LIBGSF_LIBS) -lgsf-1
gsfmodule_la_SOURCES = gsfmodule.c
nodist_gsfmodule_la_SOURCES = gsf.c
CLEANFILES = gsf.c
EXTRA_DIST = gsf.defs gsf.override  

# Additional dependency:
gsf.c: gsf.override

.defs.c:
	(cd $(srcdir) \
	 && $(PYGTK_CODEGEN) \
	    --override $*.override \
	    --prefix py$* $*.defs) > gen-$*.c \
	&& cp gen-$*.c $*.c \
	&& rm -f gen-$*.c
