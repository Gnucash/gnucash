INCLUDES = \
  $(GNOME_CFLAGS) \
  -DGTK_DISABLE_DEPRECATED \
  -DGDK_DISABLE_DEPRECATED \
  -DG_DISABLE_DEPRECATED

noinst_LTLIBRARIES = libegg.la

EGGSOURCES = \
  egg-action.c \
  egg-action-group.c \
  egg-toggle-action.c \
  eggtreemodelfilter.c \
  eggtreemultidnd.c \
  egg-radio-action.c \
  egg-menu-merge.c \
  egg-accel-dialog.c \
  eggradiotoolbutton.c \
  eggtoggletoolbutton.c \
  eggtoolitem.c	\
  eggseparatortoolitem.c \
  eggtoolbar.c \
  eggtoolbutton.c \
  egg-editable-toolbar.c \
  egg-toolbars-model.c \
  egg-toolbar-editor.c

libegg_la_SOURCES = \
  $(EGGSOURCES) \
  eggmarshalers.c

EGGHEADERS = \
  eggtreemodelfilter.h \
  eggtreemultidnd.h \
  egg-menu.h \
  egg-action.h \
  egg-action-group.h \
  egg-toggle-action.h \
  egg-radio-action.h \
  egg-menu-merge.h \
  egg-accel-dialog.h \
  eggradiotoolbutton.h \
  eggtoggletoolbutton.h \
  eggtoolitem.h	\
  eggseparatortoolitem.h \
  eggtoolbar.h \
  eggtoolbutton.h \
  egg-editable-toolbar.h \
  egg-toolbars-model.h \
  egg-toolbar-editor.h

noinst_HEADERS = \
  $(EGGHEADERS) \
  eggmarshalers.h \
  eggintl.h

eggmarshalers.h:
	cd $(srcdir) \
	&& $(GLIB_GENMARSHAL) --prefix=_egg_marshal eggmarshalers.list --header > xgen-emh \
	&& cp xgen-emh eggmarshalers.h \
	&& rm -f xgen-emh xgen-emh~

eggmarshalers.c:
	cd $(srcdir) \
	&& echo '#include "eggmarshalers.h"' > xgen-emc \
	&& echo '' >> xgen-emc \
	&& $(GLIB_GENMARSHAL) --prefix=_egg_marshal eggmarshalers.list --body >> xgen-emc \
	&& cp xgen-emc eggmarshalers.c \
	&& rm -f xgen-emc xgen-emc~

egg-marshal.c: eggmarshalers.h eggmarshalers.c

EXTRA_DIST=			\
	eggmarshalers.list

EGGFILES=$(EGGSOURCES) $(EGGHEADERS)
EGGDIR=$(srcdir)/../../../libegg/libegg

regenerate-built-sources:
	EGGFILES="$(EGGFILES) eggmarshalers.list" EGGDIR="$(EGGDIR)" $(srcdir)/update-from-egg.sh

