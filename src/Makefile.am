# These directories do not contain any gtk dependencies
NONGUI_SUBDIRS = \
  libqof \
  debug \
  doc \
  pixmaps \
  test-core \
  core-utils \
  gnc-module \
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
  plugins \
  bin


if GNUCASH_ENABLE_GUI
SUBDIRS = . $(NONGUI_SUBDIRS) $(GUI_SUBDIRS_1) report $(GUI_SUBDIRS_2)
else
SUBDIRS = . $(NONGUI_SUBDIRS) report
endif

noinst_HEADERS = \
  swig-runtime.h

noinst_DATA = gnc-test-env

if BUILDING_FROM_SVN
swig-runtime.h:
	$(SWIG) -guile -external-runtime $@
endif
MAINTAINERCLEANFILES = swig-runtime.h

EXTRA_DIST = \
  base-typemaps.i \
  README.modules \
  gnc-test-env \
  guile-mappings.h \
  platform.h
