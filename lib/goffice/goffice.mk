# prune this when the code moves
INCLUDES = -I$(top_srcdir)/lib/goffice \
           -I$(top_srcdir)/lib/goffice/split \
           -I$(top_srcdir)/lib \
           $(GNUCASH_CFLAGS)

GOFFICE_PLUGIN_FLAGS = 
# GOFFICE_PLUGIN_FLAGS = $(GNUCASH_PLUGIN_LDFLAGS)
