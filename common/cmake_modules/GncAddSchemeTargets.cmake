# GncAddSchemeTargets.cmake Define a command to compile Scheme programs with Guile
# Copyright (c) 2015, Rob Gowin
# Copyright 2017 John Ralls <jralls@ceridwen.us>
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program; if not, contact:
# Free Software Foundation           Voice:  +1-617-542-5942
# 51 Franklin Street, Fifth Floor    Fax:    +1-617-542-2652
# Boston, MA  02110-1301,  USA       gnu@gnu.org

#Guile and ltdl require MSYS paths on MinGW-w64; this function transforms them.
function(make_unix_path PATH)
    string(REGEX REPLACE "^([A-Za-z]):" "/\\1" newpath ${${PATH}})
    string(REGEX REPLACE "\\\\" "/" newpath ${newpath})
    set(${PATH} ${newpath} PARENT_SCOPE)
endfunction()

#PATH variables in the environment are separated by colons, but CMake lists are separated by semicolons. This function transforms the separators.
function(make_unix_path_list PATH)
    string(REPLACE ";" ":" newpath "${${PATH}}")
    set(${PATH} ${newpath} PARENT_SCOPE)
endfunction()

function(gnc_add_scheme_targets _TARGET _SOURCE_FILES _OUTPUT_DIR _GUILE_DEPENDS
				MAKE_LINKS)
  set(__DEBUG FALSE)
  if (__DEBUG)
    message("Parameters to COMPILE_SCHEME for target ${_TARGET}")
    message("   SOURCE_FILES: ${_SOURCE_FILES}")
    message("   GUILE_DEPENDS: ${_GUILE_DEPENDS}")
    message("   DIRECTORIES: ${BINDIR_BUILD}, ${LIBDIR_BUILD}, ${DATADIR_BUILD}")
  endif(__DEBUG)
  set(_CMD "create_symlink")
  if(WIN32)
    set(_CMD "copy")
  endif(WIN32)
  set(current_srcdir ${CMAKE_CURRENT_SOURCE_DIR})
  set(current_bindir ${CMAKE_CURRENT_BINARY_DIR})
  set(build_bindir ${BINDIR_BUILD})
  set(build_libdir ${LIBDIR_BUILD})
  set(build_datadir ${DATADIR_BUILD})
  if(MINGW64)
    make_unix_path(build_bindir)
    make_unix_path(build_libdir)
    make_unix_path(build_datadir)
    make_unix_path(current_bindir)
    make_unix_path(current_srcdir)
    make_unix_path(CMAKE_BINARY_DIR)
    make_unix_path(CMAKE_SOURCE_DIR)
  endif(MINGW64)

  # If links are requested, we simple link (or copy, for Windows) each source file to the dest directory
  if(MAKE_LINKS)
    set(_LINK_DIR ${DATADIR_BUILD}/gnucash/scm/${_OUTPUT_DIR})
    file(MAKE_DIRECTORY ${_LINK_DIR})
    set(_SCHEME_LINKS "")
    foreach(scheme_file ${_SOURCE_FILES})
      set(_SOURCE_FILE ${current_srcdir}/${scheme_file})
      if(IS_ABSOLUTE ${scheme_file})
        set(_SOURCE_FILE ${scheme_file})
      endif()
      get_filename_component(name ${scheme_file} NAME)
      set(_OUTPUT_FILE ${_LINK_DIR}/${name})
      if(NOT EXISTS ${_OUTPUT_FILE})
        list(APPEND _SCHEME_LINKS ${_OUTPUT_FILE})
        add_custom_command(
            OUTPUT ${_OUTPUT_FILE}
            COMMAND ${CMAKE_COMMAND} -E ${_CMD} ${_SOURCE_FILE} ${_OUTPUT_FILE}
        )
      endif()
    endforeach(scheme_file)
    add_custom_target(${_TARGET}-links ALL DEPENDS ${_SCHEME_LINKS})
  endif(MAKE_LINKS)

  # Construct the guile source and compiled load paths

  set(_GUILE_LOAD_PATH "${current_srcdir}"
      "${current_bindir}" "${CMAKE_BINARY_DIR}/libgnucash/scm")  # to pick up generated build-config.scm
  set(_GUILE_LOAD_COMPILED_PATH "${current_bindir}")

  set(_GUILE_CACHE_DIR ${LIBDIR_BUILD}/gnucash/scm/ccache/${GUILE_EFFECTIVE_VERSION})
  set(_GUILE_LOAD_PATH "${current_srcdir}")
  if (MAKE_LINKS)
      list(APPEND _GUILE_LOAD_PATH "${build_datadir}/gnucash/scm")
  endif()
  set(_GUILE_LOAD_COMPILED_PATH ${build_libdir}/gnucash/scm/ccache/${GUILE_EFFECTIVE_VERSION})

  set(_TARGET_FILES "")

  foreach(source_file ${_SOURCE_FILES})
      set(guile_depends ${_GUILE_DEPENDS})
      get_filename_component(basename ${source_file} NAME_WE)

      set(output_file ${basename}.go)
      set(_TMP_OUTPUT_DIR ${_OUTPUT_DIR})
      if (_TMP_OUTPUT_DIR)
        set(output_file ${_OUTPUT_DIR}/${basename}.go)
      endif()
      set(output_file ${_GUILE_CACHE_DIR}/${output_file})
      list(APPEND _TARGET_FILES ${output_file})

      set(source_file_abs_path ${CMAKE_CURRENT_SOURCE_DIR}/${source_file})
      if (IS_ABSOLUTE ${source_file})
        set(source_file_abs_path ${source_file})
      endif()
      if (__DEBUG)
        message("add_custom_command: output = ${output_file}")
      endif()
      set(CMAKE_COMMMAND_TMP "")
      if (${CMAKE_VERSION} VERSION_GREATER 3.1)
        set(CMAKE_COMMAND_TMP ${CMAKE_COMMAND} -E env)
      endif()
      if (MINGW64)
        set(fpath "")
        foreach(dir $ENV{PATH})
            make_unix_path(dir)
            set(fpath "${fpath}${dir}:")
        endforeach(dir)
        set(LIBRARY_PATH "PATH=\"${build_bindir}:${fpath}\"")
      else (MINGW64)
        set (LIBRARY_PATH "LD_LIBRARY_PATH=${LIBDIR_BUILD}:${LIBDIR_BUILD}/gnucash:${_GUILE_LD_LIBRARY_PATH}")
      endif (MINGW64)
      if (APPLE)
        set (LIBRARY_PATH "DYLD_LIBRARY_PATH=${LIBDIR_BUILD}:${LIBDIR_BUILD}/gnucash:${_GUILE_LD_LIBRARY_PATH}")
      endif (APPLE)
      set(_GNC_MODULE_PATH "")
      if(MINGW64)
        set(_GNC_MODULE_PATH "${build_bindir}")
      else(MINGW64)
        set(_GNC_MODULE_PATH "${LIBDIR_BUILD}" "${LIBDIR_BUILD}/gnucash" "${GNC_MODULE_PATH}")
      endif(MINGW64)
      make_unix_path_list(_GUILE_LOAD_PATH)
      make_unix_path_list(_GUILE_LOAD_COMPILED_PATH)
      make_unix_path_list(_GUILE_LD_LIBRARY_PATH)
      make_unix_path_list(_GNC_MODULE_PATH)
      if (__DEBUG)
        message("  ")
        message("   LIBRARY_PATH: ${LIBRARY_PATH}")
        message("   GUILE_LOAD_PATH: ${_GUILE_LOAD_PATH}")
        message("   GUILE_LOAD_COMPILED_PATH: ${_GUILE_LOAD_COMPILED_PATH}")
        message("   GNC_MODULE_PATH: ${_GNC_MODULE_PATH}")
      endif(__DEBUG)
      add_custom_command(
        OUTPUT ${output_file}
        COMMAND ${CMAKE_COMMAND_TMP}
            ${LIBRARY_PATH}
            GNC_UNINSTALLED=YES
            GNC_BUILDDIR=${CMAKE_BINARY_DIR}
            GUILE_LOAD_PATH=${_GUILE_LOAD_PATH}
            GUILE_LOAD_COMPILED_PATH=${_GUILE_LOAD_COMPILED_PATH}
            GNC_MODULE_PATH=${_GNC_MODULE_PATH}
            ${GUILE_EXECUTABLE} -e '\(@@ \(guild\) main\)' -s ${GUILD_EXECUTABLE} compile -o ${output_file} ${source_file_abs_path}
        DEPENDS ${guile_depends}
        MAIN_DEPENDENCY ${source_file_abs_path}
        )
  endforeach(source_file)
  if (__DEBUG)
    message("TARGET_FILES are ${_TARGET_FILES}")
  endif(__DEBUG)
  add_custom_target(${_TARGET} ALL DEPENDS ${_TARGET_FILES})
  install(FILES ${_TARGET_FILES} DESTINATION ${SCHEME_INSTALLED_CACHE_DIR}/${_OUTPUT_DIR})
  install(FILES ${_SOURCE_FILES} DESTINATION ${SCHEME_INSTALLED_SOURCE_DIR}/${_OUTPUT_DIR})
endfunction(gnc_add_scheme_targets)
