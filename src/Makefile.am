# These directories do not contain any gtk dependencies
NONGUI_SUBDIRS = \
  libqof \
  test-core \
  debug \
  doc \
  pixmaps \
  core-utils \
  gnc-module \
  engine \
  backend \
  scm \
  quotes

if WITH_PYTHON
   PYTHON_DIR = python
endif

# These directories should also not contain any gtk dependencies, but
# currently there is only single very little dependency in
# app-utils. It should go away in the long run, though.
ALMOST_NONGUI_SUBDIRS = \
  app-utils \
  tax \
  ${PYTHON_DIR}

# These directories contain the code with gtk dependency
GUI_SUBDIRS = \
  gnome-utils \
  html \
  gnome-search \
  report \
  register \
  gnome \
  import-export \
  business \
  optional \
  plugins \
  bin

# Need to include '.' in order to build swig-runtime.h
if GNUCASH_ENABLE_GUI
SUBDIRS = . $(NONGUI_SUBDIRS) $(ALMOST_NONGUI_SUBDIRS) $(GUI_SUBDIRS)
else
# Note: In non-GUI build, "optional" needs to be added as well, but
# cannot be added in the variable because in with-GUI mode it depends
# on other parts.
SUBDIRS = . $(NONGUI_SUBDIRS) $(ALMOST_NONGUI_SUBDIRS) optional
endif

noinst_HEADERS = \
  swig-runtime.h

noinst_DATA = gnc-test-env

if BUILDING_FROM_SCM
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
