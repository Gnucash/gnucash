check_PROGRAMS = pie-demo 

include $(top_srcdir)/lib/goffice-0.0.4/goffice.mk

AM_CFLAGS = $(GOFFICE_DEPS_CFLAGS)
AM_LDFLAGS += $(GOFFICE_DEPS_LIBS) ../goffice/libgoffice-1.la
pie_demo_SOURCES = pie-demo.c
