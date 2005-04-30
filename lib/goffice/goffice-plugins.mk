INCLUDES = \
    -I$(top_srcdir)/lib					\
    -I$(top_srcdir)/lib/goffice				\
    -I$(top_srcdir)/lib/goffice/split			\
    $(GNUCASH_CFLAGS)

GNUCASH_PLUGIN_LDFLAGS =
GOFFICE_PLUGIN_FLAGS = $(GNUCASH_PLUGIN_LDFLAGS)

AM_CFLAGS = ${GLIB_CFLAGS} ${XML_CFLAGS} ${GSF_CFLAGS} ${ART_CFLAGS} ${GNOME_CFLAGS} ${GDK_PIXBUF_CLFAGS} ${GLADE_CFLAGS}

# This will produce automake complaints, but it allows everything
# outside of goffice to be copiled with -Werror.  Its worth the one
# complaint.
CFLAGS += -Wno-error
