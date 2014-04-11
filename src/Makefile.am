NONGUI_SUBDIRS = \
  libqof \
  doc \
  pixmaps \
  core-utils \
  gnc-module \
  test-core \
  engine \
  backend \
  scm \
  quotes

GUI_SUBDIRS_1 = \
  calculation \
  tax \
  app-utils \
  gnome-utils \
  html \
  gnome-search

GUI_SUBDIRS_2 = \
  register \
  gnome \
  import-export \
  business \
  optional \
  bin

DIST_SUBDIRS = $(NONGUI_SUBDIRS) $(GUI_SUBDIRS_1) report $(GUI_SUBDIRS_2)

if GNUCASH_ENABLE_GUI
SUBDIRS = . $(DIST_SUBDIRS)
else 
SUBDIRS = . $(NONGUI_SUBDIRS) report
endif

noinst_HEADERS = \
  RecnWindow.h \
  swig-runtime.h \
  gnc-ui.h

bin_SCRIPTS = gnc-test-env

if BUILDING_FROM_SVN
swig-runtime.h:
	$(SWIG) -guile -external-runtime $@
endif
MAINTAINERCLEANFILES = swig-runtime.h

EXTRA_DIST = \
  base-typemaps.i \
  README.modules \
  gnc-test-env \
  valgrind-gnucash.supp \
  guile-mappings.h \
  glib-compat.h
