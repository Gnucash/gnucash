# Makefile.am for goffice

SUBDIRS = goffice tests pixmaps plugins
# po

EXTRA_DIST = README NEWS BUGS MAINTAINERS AUTHORS \
	goffice.mk \
	goffice-plugins.mk \
	intltool-extract.in	\
	intltool-update.in	\
	intltool-merge.in
DISTCLEANFILES = intltool-extract intltool-merge intltool-update

pkgconfigdir = $(libdir)/pkgconfig
pkgconfig_DATA = libgoffice-1.pc

include $(top_srcdir)/lib/goffice-0.0.4/goffice.mk
