NONGUI_SUBDIRS = \
  doc \
  pixmaps \
  core-utils \
  gnc-module \
  test-core \
  engine \
  backend \
  scm \
  quotes \
  bin

GUI_SUBDIRS_1 = \
  network-utils \
  calculation \
  tax \
  app-utils \
  gnome-utils \
  gnome-search \
  app-file

GUI_SUBDIRS_2 = \
  dialog-account \
  register \
  import-export \
  gnome \
  experimental \
  business \
  optional 

DIST_SUBDIRS = $(NONGUI_SUBDIRS) $(GUI_SUBDIRS_1) report $(GUI_SUBDIRS_2)

if GNUCASH_ENABLE_GUI
SUBDIRS = $(DIST_SUBDIRS)
else 
SUBDIRS = $(NONGUI_SUBDIRS) report
endif

noinst_HEADERS = \
  AccWindow.h \
  RecnWindow.h \
  gnc-ui.h

bin_SCRIPTS = gnc-test-env

EXTRA_DIST = \
  .cvsignore \
  README.modules \
  gnc-test-env
