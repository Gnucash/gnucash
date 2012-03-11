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
  quotes \
  tax

# These directories should also not contain any gtk dependencies, but
# currently there is only single very little dependency in
# app-utils. It should go away in the long run, though.
if WITH_PYTHON
   PYTHON_DIR = python
endif

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
