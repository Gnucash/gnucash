DIST_SUBDIRS = \
  doc \
  pixmaps \
  bin 

NONGUI_SUBDIRS = \
  core-utils \
  gnc-module \
  test-core \
  engine \
  backend \
  report \
  scm \
  quotes 

GUI_SUBDIRS = \
  network-utils \
  calculation \
  tax \
  app-utils \
  gnome-utils \
  app-file \
  import-export \
  dialog-account \
  register \
  gnome \
  experimental \
  business \
  optional 

if GNUCASH_ENABLE_GUI
SUBDIRS = $(DIST_SUBDIRS) $(NONGUI_SUBDIRS) $(GUI_SUBDIRS)
else 
SUBDIRS = $(DIST_SUBDIRS) $(NONGUI_SUBDIRS)
endif

noinst_HEADERS = \
  AccWindow.h \
  RecnWindow.h \
  gnc-ui.h

bin_SCRIPTS = gnc-test-env

EXTRA_DIST = \
  .cvsignore \
  README.modules
