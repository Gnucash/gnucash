AM_CPPFLAGS = \
    -I$(top_srcdir)/lib/libgsf-1.12.3	    \
    -I$(top_srcdir)/lib/goffice-0.0.4 		\
    -I$(top_builddir)/lib/goffice-0.0.4		\
    $(GOFFICE_DEPS_CFLAGS)

GOFFICE_PLUGIN_FLAGS = $(GOFFICE_PLUGIN_LDFLAGS)
