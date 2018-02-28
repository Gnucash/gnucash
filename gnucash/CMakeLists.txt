# CMakeLists.txt for gnucash/

# Preparations for macro add_gschema_targets and gschemas.compiled target
set(SCHEMADIR_BUILD ${DATADIR_BUILD}/glib-2.0/schemas)
file(MAKE_DIRECTORY ${SCHEMADIR_BUILD})
unset(gschema_depends CACHE)
# Check for GDK_MODIFIER_INTENT_DEFAULT_MOD_MASK; it's Gtk3-14 and later.
include(CheckCSourceCompiles)
unset(have_mod_mask CACHE)
set(CMAKE_REQUIRED_INCLUDES ${GTK3_INCLUDE_DIRS})
check_c_source_compiles("
#include <gdk/gdk.h>

int main (int argc, char** argv) {
  GdkModifierIntent foo = GDK_MODIFIER_INTENT_DEFAULT_MOD_MASK;
  return 0;
}
"
 have_mod_mask)

# The subdirectories
ADD_SUBDIRECTORY (gnome)
ADD_SUBDIRECTORY (gnome-utils)
ADD_SUBDIRECTORY (gnome-search)
ADD_SUBDIRECTORY (gtkbuilder)
ADD_SUBDIRECTORY (html)
ADD_SUBDIRECTORY (import-export)
ADD_SUBDIRECTORY (python)
ADD_SUBDIRECTORY (register)
ADD_SUBDIRECTORY (report)
ADD_SUBDIRECTORY (ui)
ADD_SUBDIRECTORY (gschemas)

ADD_DEFINITIONS (-DHAVE_CONFIG_H)

# Some settings are platform dependent. Let's define them per platform.
IF (WIN32)
    # Windows specific settings go here:
    set (GNUCASH_RESOURCE_FILE gnucash.rc)
    configure_file(gnucash.rc.in gnucash.rc @ONLY NEWLINE_STYLE WIN32)

ELSE (WIN32)
  # All other platforms use these settings:
  SET (PLATFORM_FILES gnucash-valgrind)

ENDIF (WIN32)

SET (gnucash_SOURCES
  gnucash-bin.c
  gnucash-gresources.c
  ${GNUCASH_RESOURCE_FILE}
)

ADD_EXECUTABLE (gnucash
  ${gnucash_SOURCES}
)

ADD_DEPENDENCIES (gnucash gnucash-manpage)

TARGET_COMPILE_DEFINITIONS(gnucash PRIVATE -DG_LOG_DOMAIN=\"gnc.bin\")

TARGET_LINK_LIBRARIES (gnucash
   gncmod-ledger-core gncmod-report-gnome gnc-gnome gncmod-gnome-utils gncmod-app-utils
   gncmod-engine gnc-module gnc-core-utils gncmod-report-system
   ${GUILE_LDFLAGS} ${GLIB2_LDFLAGS} ${GTK3_LDFLAGS} ${GTK_MAC_LDFLAGS}
)

# Get glib executable for generating the gresource file
EXECUTE_PROCESS(
   COMMAND
       ${PKG_CONFIG_EXECUTABLE} gio-2.0 --variable glib_compile_resources
   OUTPUT_VARIABLE
       GLIB_COMPILE_RESOURCES_EXECUTABLE
   OUTPUT_STRIP_TRAILING_WHITESPACE
)

# Get the dependencies of the gresource
EXECUTE_PROCESS(
  OUTPUT_VARIABLE
      gr_files
  COMMAND ${GLIB_COMPILE_RESOURCES_EXECUTABLE}
     --sourcedir=${CMAKE_CURRENT_SOURCE_DIR}
     --generate-dependencies
     ${CMAKE_CURRENT_SOURCE_DIR}/gnucash-gresources.xml
)

STRING (REPLACE "\n" ";" gresource_files ${gr_files})

ADD_CUSTOM_COMMAND(
   OUTPUT gnucash-gresources.c
   COMMAND
    ${GLIB_COMPILE_RESOURCES_EXECUTABLE}
    --target=gnucash-gresources.c
    --sourcedir=${CMAKE_CURRENT_SOURCE_DIR}
    --generate-source
    ${CMAKE_CURRENT_SOURCE_DIR}/gnucash-gresources.xml
   DEPENDS
    ${CMAKE_CURRENT_SOURCE_DIR}/gnucash-gresources.xml ${gresource_files}
   WORKING_DIRECTORY
    ${CMAKE_CURRENT_BINARY_DIR}
)

IF (MAC_INTEGRATION)
  TARGET_COMPILE_OPTIONS(gnucash PRIVATE ${OSX_EXTRA_COMPILE_FLAGS})
  TARGET_LINK_LIBRARIES(gnucash ${OSX_EXTRA_LIBRARIES})
ENDIF(MAC_INTEGRATION)

INSTALL(TARGETS gnucash DESTINATION ${CMAKE_INSTALL_BINDIR})
# No headers to install.


SET(TOP_SRC_DIR ${CMAKE_SOURCE_DIR})
SET(GNUCASH_BIN_INSTALL_NAME "gnucash")

SET(VALGRIND_OUTDIR ${BINDIR_BUILD})

configure_file(gnucash-valgrind.in ${CMAKE_CURRENT_BINARY_DIR}${CMAKE_FILES_DIRECTORY}/gnucash-valgrind @ONLY)

FILE(COPY ${CMAKE_CURRENT_BINARY_DIR}${CMAKE_FILES_DIRECTORY}/gnucash-valgrind
          DESTINATION ${VALGRIND_OUTDIR}
          FILE_PERMISSIONS OWNER_READ OWNER_WRITE OWNER_EXECUTE GROUP_READ GROUP_EXECUTE WORLD_READ WORLD_EXECUTE
)

## Create the environment file

FILE(STRINGS ${CMAKE_CURRENT_SOURCE_DIR}/environment.in ENV_STRINGS_IN)

SET(ENV_STRINGS_LIST "")

FOREACH(line ${ENV_STRINGS_IN})
  STRING(REPLACE ";" "\;" line2 "${line}")
  IF(NOT "${line2}" MATCHES "@NOTE")
    LIST(APPEND ENV_STRINGS_LIST "${line2}\n")
  ENDIF()
ENDFOREACH()

STRING(CONCAT ENV_STRINGS ${ENV_STRINGS_LIST})
STRING(CONFIGURE "${ENV_STRINGS}" ENV_STRINGS_CONF @ONLY)

SET(ENV_FILE_OUT ${CMAKE_CURRENT_BINARY_DIR}${CMAKE_FILES_DIRECTORY}/environment)
SET(BUILD_ENV_FILE_OUT ${CMAKE_CURRENT_BINARY_DIR}${CMAKE_FILES_DIRECTORY}/environment.build)

FILE(WRITE ${ENV_FILE_OUT} "${ENV_STRINGS_CONF}")
FILE(WRITE ${BUILD_ENV_FILE_OUT} "${ENV_STRINGS_CONF}")

SET(XDG_TEXT "
# GnuCash was not installed in the default location.
# XDG_DATA_DIRS will be set so that our documentation
# and gsettings schema are found.\n"
)

IF (NOT(${CMAKE_INSTALL_FULL_DATADIR} STREQUAL "/usr/share") AND NOT(${CMAKE_INSTALL_FULL_DATADIR} STREQUAL "/usr/local/share"))
  FILE(APPEND ${ENV_FILE_OUT} ${XDG_TEXT})
  FILE(APPEND ${ENV_FILE_OUT} "XDG_DATA_DIRS={GNC_HOME}/share;{XDG_DATA_DIRS};" "${GNC_SYSTEM_XDG_DATA_DIRS}\n")
ENDIF()
FILE(APPEND ${BUILD_ENV_FILE_OUT} ${XDG_TEXT})
FILE(APPEND ${BUILD_ENV_FILE_OUT} "XDG_DATA_DIRS=${DATADIR_BUILD};{XDG_DATA_DIRS};" "${GNC_SYSTEM_XDG_DATA_DIRS}\n")

if (LIBDBI_LIBRARY AND LIBDBI_DRIVERS_DIR)
  get_filename_component(libdir ${LIBDBI_LIBRARY} DIRECTORY)
  string(FIND ${LIBDBI_DRIVERS_DIR} ${libdir} is_subdir)
  if (NOT is_subdir EQUAL 0)
    FILE(APPEND ${BUILD_ENV_FILE_OUT} "GNC_DBD_DIR=${LIBDBI_DRIVERS_DIR}")
  endif()
ENDIF()

IF (WITH_PYTHON)
  SET(PYTHON_TEXT "
# Define PYTHONPATH for non default installation path.\n"
 )
  IF (NOT(${CMAKE_INSTALL_PREFIX} STREQUAL "/usr") AND NOT(${CMAKE_INSTALL_PREFIX} STREQUAL "/usr/local"))

    FILE(APPEND ${ENV_FILE_OUT} ${PYTHON_TEXT})
    FILE(APPEND ${ENV_FILE_OUT} "PYTHONPATH=${PYTHON_SYSCONFIG_OUTPUT};{PYTHONPATH}\n")
  ENDIF()

  FILE(APPEND ${BUILD_ENV_FILE_OUT} ${PYTHON_TEXT})
  FILE(APPEND ${BUILD_ENV_FILE_OUT} "PYTHONPATH=${PYTHON_SYSCONFIG_BUILD};{PYTHONPATH}\n")
ENDIF()

IF (MAC_INTEGRATION)
  file(APPEND ${ENV_FILE_OUT} "XDG_CONFIG_HOME={HOME}/Library/Application Support/Gnucash/config\n")
  file(APPEND ${ENV_FILE_OUT} "GDK_PIXBUF_MODULE_FILE={SYS_LIB}/gdk-pixbuf-2.0/2.10.0/loaders.cache\n")
  file(APPEND ${ENV_FILE_OUT} "FONTCONFIG_FILE={GNC_HOME}/etc/fonts/fonts.conf\n")
ENDIF()

FILE(COPY ${BUILD_ENV_FILE_OUT}
  DESTINATION ${SYSCONFDIR_BUILD}/gnucash
  FILE_PERMISSIONS OWNER_READ OWNER_WRITE OWNER_EXECUTE GROUP_READ GROUP_EXECUTE WORLD_READ WORLD_EXECUTE
)
FILE(RENAME
  ${SYSCONFDIR_BUILD}/gnucash/environment.build
  ${SYSCONFDIR_BUILD}/gnucash/environment
)

SET(ENVIRONMENT_FILE_DIR ${CMAKE_CURRENT_BINARY_DIR})
FILE(COPY ${ENV_FILE_OUT}
  DESTINATION ${ENVIRONMENT_FILE_DIR}
  FILE_PERMISSIONS OWNER_READ OWNER_WRITE OWNER_EXECUTE GROUP_READ GROUP_EXECUTE WORLD_READ WORLD_EXECUTE
)

INSTALL(FILES ${SCRIPT_LIST} ${VALGRIND_OUTDIR}/gnucash-valgrind DESTINATION ${CMAKE_INSTALL_BINDIR})
INSTALL(FILES ${ENVIRONMENT_FILE_DIR}/environment DESTINATION
  ${CMAKE_INSTALL_FULL_SYSCONFDIR}/gnucash)

IF (WIN32)
  # Write out a command script for windows
  SET(lib_directories boost enchant libsoup mysql pgsql libxslt)
  SET(bin_directories mingw gnutls libgsf pcre gnome guile webkit regex aqbanking gwenhywfar libofx opensp
    libdbi sqlite3 mysql pgsql enchant libsoup libxslt)

  SET(CMD_LINES "")
  SET(BUILD_CMD_LINES "")
  FOREACH(dir bin lib lib/gnucash)
    FILE(TO_NATIVE_PATH ${CMAKE_INSTALL_PREFIX}/${dir} INSTALL_PATH_ITEM)
    FILE(TO_NATIVE_PATH ${CMAKE_BINARY_DIR}/${dir} BUILD_PATH_ITEM)
    LIST(APPEND CMD_LINES "set PATH=${INSTALL_PATH_ITEM}\;%PATH%\n")
    LIST(APPEND BUILD_CMD_LINES "set PATH=${BUILD_PATH_ITEM}\;%PATH%\n")
  ENDFOREACH(dir)
  IF (NOT ${MINGW64})
    FOREACH(dir ${lib_directories})
      FILE(TO_NATIVE_PATH ${CMAKE_PREFIX_PATH}/${dir}/lib PATH_ITEM)
      LIST(APPEND CMD_LINES "set PATH=${PATH_ITEM}\;%PATH%\n")
    ENDFOREACH(dir)

    FOREACH(dir ${bin_directories})
      FILE(TO_NATIVE_PATH ${CMAKE_PREFIX_PATH}/${dir}/bin PATH_ITEM)
      LIST(APPEND CMD_LINES "set PATH=${PATH_ITEM}\;%PATH%\n")
    ENDFOREACH(dir)
  ENDIF (NOT ${MINGW64})
  SET(CMD_FILE ${CMAKE_CURRENT_BINARY_DIR}/gnucash-launcher.cmd)
  FILE(WRITE ${CMD_FILE} "@echo off\nsetlocal\n\n")
  FOREACH(line ${CMD_LINES})
    FILE(APPEND ${CMD_FILE} "${line}")
  ENDFOREACH(line)
  FILE(APPEND ${CMD_FILE} "\nstart gnucash %*\n")

  SET(BUILD_CMD_FILE ${CMAKE_BINARY_DIR}/bin/gnucash-launcher.cmd)
  FILE(WRITE ${BUILD_CMD_FILE} "@echo off\nsetlocal\n\n")
  FOREACH(line ${CMD_LINES})
     FILE(APPEND ${BUILD_CMD_FILE} "${line}")
  ENDFOREACH(line)
  FILE(APPEND ${BUILD_CMD_FILE} "\nstart gnucash %*\n")

  INSTALL(PROGRAMS ${CMD_FILE} DESTINATION  ${CMAKE_INSTALL_BINDIR})
ENDIF(WIN32)

# The GResource Files are absolute paths but SET_LOCAL_DIST requires
# relative paths.
FOREACH(gres_file ${gresource_files})
  FILE(RELATIVE_PATH rel_file ${CMAKE_CURRENT_SOURCE_DIR} ${gres_file})
  LIST(REMOVE_ITEM gresource_files ${gres_file})
  LIST(APPEND gresource_files ${rel_file})
ENDFOREACH()

SET_LOCAL_DIST(gnucash_DIST_local CMakeLists.txt environment.in generate-gnc-script
               gnucash-bin.c gnucash.rc.in gnucash-valgrind.in gnucash-gresources.xml ${gresource_files}
               ${gnucash_EXTRA_DIST})

set (gnucash_DIST ${gnucash_DIST_local} ${gnome_DIST} ${gnome_search_DIST} ${gnome_utils_DIST}
    ${gschemas_DIST} ${gtkbuilder_DIST} ${html_DIST} ${import_export_DIST} ${python_DIST} ${register_DIST}
    ${report_DIST} ${overrides_DIST} ${test_bin_DIST} ${ui_DIST} PARENT_SCOPE)
