noinst_LTLIBRARIES = libgoffice-gui-utils.la

AM_CFLAGS = $(GNOME_CFLAGS) $(GSF_CFLAGS) $(GLADE_CFLAGS)

BUILT_SOURCES =				\
	go-marshalers.h			\
	go-marshalers.c

libgoffice_gui_utils_la_SOURCES =	\
	$(BUILT_SOURCES)		\
	go-combo-box.c			\
	go-combo-box.h			\
	go-color-group.c		\
	go-color-group.h		\
	go-color-palette.c		\
	go-color-palette.h		\
	go-combo-color.c		\
	go-combo-color.h		\
	go-combo-pixmaps.h		\
	go-combo-pixmaps.c		\
	go-combo-text.c			\
	go-combo-text.h			\
	\
	go-action-combo-color.c 	\
	go-action-combo-color.h 	\
	go-action-combo-pixmaps.c 	\
	go-action-combo-pixmaps.h 	\
	go-action-combo-stack.c 	\
	go-action-combo-stack.h 	\
	go-action-combo-text.c 		\
	go-action-combo-text.h

go-marshalers.h : go-marshalers.list $(GLIB_GENMARSHAL)
	$(GLIB_GENMARSHAL) $< --header --prefix=go_ > $@
go-marshalers.c : go-marshalers.list $(GLIB_GENMARSHAL)
	$(GLIB_GENMARSHAL) $< --body --prefix=go_ > $@.tmp
	echo '/* This file has been automatically generated.  Do not edit.  */' >$@
	echo '#include "'$*.h'"' >>$@
	cat $@.tmp >>$@
	rm -f $@.tmp

EXTRA_DIST = go-marshalers.list

include $(srcdir)/../goffice.mk
