# These directories do not contain any gtk dependencies
# Note the unusual ordering of some test directories. This is
# because test-core depends on engine and the test directories
# in turn depend on test-core.
SUBDIRS = \
  . \
  debug
# Note normally SUBDIRS should also include test-core. That directory
# however depends on libgnucash/engine and hence is included in
# libgnucash/Makefile.am for proper build oldering. This would not
# be needed if our Makefiles were structured to be included one
# in the other instead of for a recursive make invocation.

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
  cmake_modules/GncAddSchemeTargets.cmake \
  cmake_modules/GncAddGSchemaTargets.cmake \
  cmake_modules/GncFindPkgConfig.cmake \
  cmake_modules/CMakeLists.txt \
  config.h.cmake.in \
  gnc-test-env.pl \
  guile-mappings.h \
  platform.h \
  CMakeLists.txt
