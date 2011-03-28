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
  quotes \
  calculation \
  tax

# These directories should also not contain any gtk dependencies, but
# currently there is only single very little dependency in
# app-utils. It should go away in the long run, though.
ALMOST_NONGUI_SUBDIRS = \
  app-utils \
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
SUBDIRS = . $(NONGUI_SUBDIRS) $(ALMOST_NONGUI_SUBDIRS) $(GUI_SUBDIRS)
DIST_SUBDIRS = $(SUBDIRS) python

noinst_HEADERS = \
  swig-runtime.h

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
  guile-mappings.h \
  platform.h
