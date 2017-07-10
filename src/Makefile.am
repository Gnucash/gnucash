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

if BUILDING_FROM_VCS
swig-runtime.h:
	$(SWIG) -guile -external-runtime $@
endif
MAINTAINERCLEANFILES = swig-runtime.h

EXTRA_DIST = \
  base-typemaps.i \
  cmake_modules/MacroAppendForeach.cmake \
  cmake_modules/GncAddSwigCommand.cmake \
  cmake_modules/GncAddTest.cmake \
  cmake_modules/MakeDist.cmake \
  cmake_modules/COPYING-CMAKE-SCRIPTS.txt \
  cmake_modules/MakeDistFiles.cmake \
  cmake_modules/MacroAddSourceFileCompileFlags.cmake \
  cmake_modules/MakeDistCheck.cmake \
  cmake_modules/GncConfigure.cmake \
  cmake_modules/GncAddSchemeTargets.cmake \
  cmake_modules/GncAddGSchemaTargets.cmake \
  cmake_modules/GncFindPkgConfig.cmake \
  cmake_modules/CMakeLists.txt \
  config.h.cmake.in \
  README.modules \
  gnc-test-env.pl \
  guile-mappings.h \
  platform.h \
  CMakeLists.txt
