SUBDIRS = widgets

noinst_LTLIBRARIES = libgoffice-split.la

AM_CFLAGS = $(GLIB_CFLAGS) $(GSF_CFLAGS) $(PRINT_CFLAGS) $(GNOME_CFLAGS) $(GLADE_CFLAGS)

libgoffice_split_la_SOURCES = \
        application.h \
	command-context.c \
	command-context.h \
	command-context-priv.h \
	command-context-stderr.h \
	command-context-stderr.c \
	dependent.h \
	error-info.c \
	error-info.h \
	file.h \
	func.h \
	value.c \
	value.h \
	str.h \
	str.c \
	mathfunc.h \
	mathfunc.c \
	numbers.h \
	number-match.h \
	number-match.c \
	dates.h \
	dates.c \
	format.h \
	format.c \
	gutils.h \
	gutils.c \
	formats.c \
	datetime.h \
	datetime.c \
        position.h \
	ranges.h \
	regutf8.h \
	regutf8.c \
	style-color.h \
	style-color.c \
	style-border.h \
	style-border.c \
	mstyle.h \
	mstyle.c \
	style.h \
	style.c \
	gnumeric.h \
	gnumeric-gconf.h \
	gnumeric-gconf-priv.h \
	gnumeric-gconf.c \
	global-gnome-font.h \
	global-gnome-font.c \
	plugin.h \
	plugin.c \
	plugin-util.h \
	plugin-util.c \
	plugin-service.h \
	plugin-service.c \
	plugin-service-impl.h \
	plugin-loader.h \
	plugin-loader.c \
	plugin-loader-module.h \
	plugin-loader-module.c \
	module-plugin-defs.h \
	io-context.h \
	io-context-priv.h \
	io-context.c \
	gui-file.h \
	gui-util.h \
	gui-util.c \
	gui-gnumeric.h \
	xml-io.h \
	xml-io-version.h \
	workbook-control-gui.h

include $(srcdir)/../goffice.mk
