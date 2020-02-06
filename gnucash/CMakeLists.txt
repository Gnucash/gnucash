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
add_subdirectory (gnome)
add_subdirectory (gnome-utils)
add_subdirectory (gnome-search)
add_subdirectory (gtkbuilder)
add_subdirectory (html)
add_subdirectory (import-export)
add_subdirectory (python)
add_subdirectory (register)
add_subdirectory (report)
add_subdirectory (ui)
add_subdirectory (gschemas)

add_definitions (-DHAVE_CONFIG_H)

# Some settings are platform dependent. Let's define them per platform.
if (WIN32)
    # Windows specific settings go here:
    set (GNUCASH_RESOURCE_FILE ${CMAKE_CURRENT_BINARY_DIR}/gnucash.rc)
    configure_file(gnucash.rc.in gnucash.rc @ONLY NEWLINE_STYLE WIN32)

else()
  # All other platforms use these settings:
  set (PLATFORM_FILES gnucash-valgrind)

endif()

set (gnucash_SOURCES
  gnucash-bin.c
  gnucash-gresources.c
  ${GNUCASH_RESOURCE_FILE}
)

add_executable (gnucash
  ${gnucash_SOURCES}
)

add_dependencies (gnucash gnucash-manpage)

target_compile_definitions(gnucash PRIVATE -DG_LOG_DOMAIN=\"gnc.bin\")

if (BUILDING_FROM_VCS)
  target_compile_definitions(gnucash PRIVATE -DGNC_VCS=\"git\")
  target_compile_definitions(gncmod-gnome-utils PRIVATE -DGNC_VCS=\"git\")
endif()

target_link_libraries (gnucash
   gncmod-ledger-core gncmod-report-gnome gnc-gnome gncmod-gnome-utils
   gncmod-app-utils gncmod-engine gnc-module gnc-core-utils gncmod-report-system
   PkgConfig::GTK3 ${GUILE_LDFLAGS} ${GLIB2_LDFLAGS} ${GTK_MAC_LDFLAGS}
)

# Get glib executable for generating the gresource file
execute_process(
   COMMAND
       ${PKG_CONFIG_EXECUTABLE} gio-2.0 --variable glib_compile_resources
   OUTPUT_VARIABLE
       GLIB_COMPILE_RESOURCES_NAME
   OUTPUT_STRIP_TRAILING_WHITESPACE
)

find_program(GLIB_COMPILE_RESOURCES_EXECUTABLE ${GLIB_COMPILE_RESOURCES_NAME})

# Get the dependencies of the gresource

execute_process(
  OUTPUT_VARIABLE
      gr_files
  COMMAND "${GLIB_COMPILE_RESOURCES_EXECUTABLE}"
     --sourcedir=${CMAKE_CURRENT_SOURCE_DIR}
     --generate-dependencies
     ${CMAKE_CURRENT_SOURCE_DIR}/gnucash-gresources.xml
)

string (REPLACE "\n" ";" gresource_files ${gr_files})

add_custom_command(
   OUTPUT gnucash-gresources.c
   COMMAND
    "${GLIB_COMPILE_RESOURCES_EXECUTABLE}"
    --target=gnucash-gresources.c
    --sourcedir=${CMAKE_CURRENT_SOURCE_DIR}
    --generate-source
    ${CMAKE_CURRENT_SOURCE_DIR}/gnucash-gresources.xml
   DEPENDS
    ${CMAKE_CURRENT_SOURCE_DIR}/gnucash-gresources.xml ${gresource_files}
   WORKING_DIRECTORY
    ${CMAKE_CURRENT_BINARY_DIR}
)

if (MAC_INTEGRATION)
  target_compile_options(gnucash PRIVATE ${OSX_EXTRA_COMPILE_FLAGS})
  target_link_libraries(gnucash ${OSX_EXTRA_LIBRARIES})
endif()

install(TARGETS gnucash DESTINATION ${CMAKE_INSTALL_BINDIR})
# No headers to install.


set(TOP_SRC_DIR ${CMAKE_SOURCE_DIR})
set(GNUCASH_BIN_INSTALL_NAME "gnucash")

set(VALGRIND_OUTDIR ${BINDIR_BUILD})

configure_file(gnucash-valgrind.in ${CMAKE_CURRENT_BINARY_DIR}${CMAKE_FILES_DIRECTORY}/gnucash-valgrind @ONLY)

file(COPY ${CMAKE_CURRENT_BINARY_DIR}${CMAKE_FILES_DIRECTORY}/gnucash-valgrind
          DESTINATION ${VALGRIND_OUTDIR}
          FILE_PERMISSIONS OWNER_READ OWNER_WRITE OWNER_EXECUTE GROUP_READ GROUP_EXECUTE WORLD_READ WORLD_EXECUTE
)

## Create the environment file

file(STRINGS ${CMAKE_CURRENT_SOURCE_DIR}/environment.in ENV_STRINGS_IN)

set(ENV_STRINGS_LIST "")

foreach(line ${ENV_STRINGS_IN})
  string(REPLACE ";" "\;" line2 "${line}")
  if(NOT "${line2}" MATCHES "@NOTE")
    list(APPEND ENV_STRINGS_LIST "${line2}\n")
  endif()
endforeach()

string(CONCAT ENV_STRINGS ${ENV_STRINGS_LIST})
string(CONFIGURE "${ENV_STRINGS}" ENV_STRINGS_CONF @ONLY)

set(ENV_FILE_OUT ${CMAKE_CURRENT_BINARY_DIR}${CMAKE_FILES_DIRECTORY}/environment)
set(BUILD_ENV_FILE_OUT ${CMAKE_CURRENT_BINARY_DIR}${CMAKE_FILES_DIRECTORY}/environment.build)

file(WRITE ${ENV_FILE_OUT} "${ENV_STRINGS_CONF}")
file(WRITE ${BUILD_ENV_FILE_OUT} "${ENV_STRINGS_CONF}")

set(XDG_TEXT "
# GnuCash was not installed in the default location.
# XDG_DATA_DIRS will be set so that our documentation
# and gsettings schema are found.\n"
)

if (NOT(${CMAKE_INSTALL_FULL_DATADIR} STREQUAL "/usr/share") AND NOT(${CMAKE_INSTALL_FULL_DATADIR} STREQUAL "/usr/local/share"))
  file(APPEND ${ENV_FILE_OUT} ${XDG_TEXT})
  file(APPEND ${ENV_FILE_OUT} "XDG_DATA_DIRS={GNC_HOME}/share;{XDG_DATA_DIRS};" "${GNC_SYSTEM_XDG_DATA_DIRS}\n")
endif()
file(APPEND ${BUILD_ENV_FILE_OUT} ${XDG_TEXT})
file(APPEND ${BUILD_ENV_FILE_OUT} "XDG_DATA_DIRS=${DATADIR_BUILD};{XDG_DATA_DIRS};" "${GNC_SYSTEM_XDG_DATA_DIRS}\n")

if (LIBDBI_LIBRARY AND LIBDBI_DRIVERS_DIR)
  get_filename_component(libdir ${LIBDBI_LIBRARY} DIRECTORY)
  string(FIND ${LIBDBI_DRIVERS_DIR} ${libdir} is_subdir)
  if (NOT is_subdir EQUAL 0)
    file(APPEND ${BUILD_ENV_FILE_OUT} "GNC_DBD_DIR=${LIBDBI_DRIVERS_DIR}")
  endif()
endif()

if (WITH_PYTHON)
  set(PYTHON_TEXT "
# Define PYTHONPATH for non default installation path.\n"
 )
  if (NOT(${CMAKE_INSTALL_PREFIX} STREQUAL "/usr") AND NOT(${CMAKE_INSTALL_PREFIX} STREQUAL "/usr/local"))

    file(APPEND ${ENV_FILE_OUT} ${PYTHON_TEXT})
    file(APPEND ${ENV_FILE_OUT} "PYTHONPATH=${PYTHON_SYSCONFIG_OUTPUT};{PYTHONPATH}\n")
  endif()

  file(APPEND ${BUILD_ENV_FILE_OUT} ${PYTHON_TEXT})
  file(APPEND ${BUILD_ENV_FILE_OUT} "PYTHONPATH=${PYTHON_SYSCONFIG_BUILD};{PYTHONPATH}\n")
endif()

if (MAC_INTEGRATION)
  file(APPEND ${ENV_FILE_OUT} "XDG_CONFIG_HOME={HOME}/Library/Application Support/${PACKAGE_NAME}/config\n")
  file(APPEND ${ENV_FILE_OUT} "GDK_PIXBUF_MODULE_FILE={SYS_LIB}/gdk-pixbuf-2.0/2.10.0/loaders.cache\n")
  file(APPEND ${ENV_FILE_OUT} "FONTCONFIG_FILE={GNC_HOME}/etc/fonts/fonts.conf\n")
  file(APPEND ${ENV_FILE_OUT} "OFX_DTD_PATH={GNC_HOME}/share/libofx/dtd\n")
  file(APPEND ${ENV_FILE_OUT} "GNC_DBD_DIR={SYS_LIB}/dbd\n")
  file(APPEND ${ENV_FILE_OUT} "GTK_IM_MODULE_FILE={GNC_HOME}/lib/gtk-3.0/3.0.0/immodules.cache\n")
endif()

file(COPY ${BUILD_ENV_FILE_OUT}
  DESTINATION ${SYSCONFDIR_BUILD}/gnucash
  FILE_PERMISSIONS OWNER_READ OWNER_WRITE OWNER_EXECUTE GROUP_READ GROUP_EXECUTE WORLD_READ WORLD_EXECUTE
)
file(RENAME
  ${SYSCONFDIR_BUILD}/gnucash/environment.build
  ${SYSCONFDIR_BUILD}/gnucash/environment
)

set(ENVIRONMENT_FILE_DIR ${CMAKE_CURRENT_BINARY_DIR})
file(COPY ${ENV_FILE_OUT}
  DESTINATION ${ENVIRONMENT_FILE_DIR}
  FILE_PERMISSIONS OWNER_READ OWNER_WRITE OWNER_EXECUTE GROUP_READ GROUP_EXECUTE WORLD_READ WORLD_EXECUTE
)

install(FILES ${SCRIPT_LIST} ${VALGRIND_OUTDIR}/gnucash-valgrind DESTINATION ${CMAKE_INSTALL_BINDIR})
install(FILES ${ENVIRONMENT_FILE_DIR}/environment DESTINATION
  ${CMAKE_INSTALL_FULL_SYSCONFDIR}/gnucash)

if (WIN32)
  # Write out a command script for windows
  set(lib_directories boost enchant libsoup mysql pgsql libxslt)
  set(bin_directories mingw gnutls libgsf pcre gnome guile webkit regex aqbanking gwenhywfar libofx opensp
    libdbi sqlite3 mysql pgsql enchant libsoup libxslt)

  set(CMD_LINES "")
  set(BUILD_CMD_LINES "")
  foreach(dir bin lib lib/gnucash)
    file(TO_NATIVE_PATH ${CMAKE_INSTALL_PREFIX}/${dir} INSTALL_PATH_ITEM)
    file(TO_NATIVE_PATH ${CMAKE_BINARY_DIR}/${dir} BUILD_PATH_ITEM)
    list(APPEND CMD_LINES "set PATH=${INSTALL_PATH_ITEM}\;%PATH%\n")
    list(APPEND BUILD_CMD_LINES "set PATH=${BUILD_PATH_ITEM}\;%PATH%\n")
  endforeach(dir)
  if (NOT ${MINGW64})
    foreach(dir ${lib_directories})
      file(TO_NATIVE_PATH ${CMAKE_PREFIX_PATH}/${dir}/lib PATH_ITEM)
      list(APPEND CMD_LINES "set PATH=${PATH_ITEM}\;%PATH%\n")
    endforeach(dir)

    foreach(dir ${bin_directories})
      file(TO_NATIVE_PATH ${CMAKE_PREFIX_PATH}/${dir}/bin PATH_ITEM)
      list(APPEND CMD_LINES "set PATH=${PATH_ITEM}\;%PATH%\n")
    endforeach(dir)
  endif()
  set(CMD_FILE ${CMAKE_CURRENT_BINARY_DIR}/gnucash-launcher.cmd)
  file(WRITE ${CMD_FILE} "@echo off\nsetlocal\n\n")
  foreach(line ${CMD_LINES})
    file(APPEND ${CMD_FILE} "${line}")
  endforeach(line)
  file(APPEND ${CMD_FILE} "\nstart gnucash %*\n")

  set(BUILD_CMD_FILE ${CMAKE_BINARY_DIR}/bin/gnucash-launcher.cmd)
  file(WRITE ${BUILD_CMD_FILE} "@echo off\nsetlocal\n\n")
  foreach(line ${CMD_LINES})
     file(APPEND ${BUILD_CMD_FILE} "${line}")
  endforeach(line)
  file(APPEND ${BUILD_CMD_FILE} "\nstart gnucash %*\n")

  install(PROGRAMS ${CMD_FILE} DESTINATION  ${CMAKE_INSTALL_BINDIR})
endif()

# The GResource Files are absolute paths but set_local_dist requires
# relative paths.
foreach(gres_file ${gresource_files})
  file(RELATIVE_PATH rel_file ${CMAKE_CURRENT_SOURCE_DIR} ${gres_file})
  list(REMOVE_ITEM gresource_files ${gres_file})
  list(APPEND gresource_files ${rel_file})
endforeach()

set_local_dist(gnucash_DIST_local CMakeLists.txt environment.in generate-gnc-script
               gnucash-bin.c gnucash.rc.in gnucash-valgrind.in gnucash-gresources.xml ${gresource_files}
               ${gnucash_EXTRA_DIST})

set (gnucash_DIST ${gnucash_DIST_local} ${gnome_DIST} ${gnome_search_DIST} ${gnome_utils_DIST}
    ${gschemas_DIST} ${gtkbuilder_DIST} ${html_DIST} ${import_export_DIST} ${python_DIST} ${register_DIST}
    ${report_DIST} ${overrides_DIST} ${test_bin_DIST} ${ui_DIST} PARENT_SCOPE)
