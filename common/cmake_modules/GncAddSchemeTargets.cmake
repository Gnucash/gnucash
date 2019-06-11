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

# Guile and ltdl require MSYS paths on MinGW-w64; this function transforms them.
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

# This function will set two or four environment variables to a directory in the parent PARENT_SCOPE
# * _DIRCLASS (eg "prefix", "sitedir" is used to construct the variable name(s) and in error messages
# * _DIRCMD is the guile command to run to get the path for the given _DIRCLASS (eg "(display (%site-dir))")
# * _PREFIX: if set will be used to calculate paths relative to this prefix and
#   set two more environment variable with this relative path.
# When run successfully this function will set following variables in the parent scope:
# * GUILE_${CMDCLASS} and GUILE_UNIX_${CMDCLASS} - the latter is the former transformed
#   into an msys compatible format (c:\some\directory => /c/some/directory)
# * If _PREFIX was set: GUILE_${CMDCLASS} and GUILE_REL_UNIX_${CMDCLASS}
#   which are the former two variables with _PREFIX removed
#   (_PREFIX=/usr, GUILE_${CMDCLASS} = /usr/share/something
#    => GUILE_REL_${CMDCLASS} = share/something
function(find_one_guile_dir _DIRCLASS _DIRCMD _PREFIX)

    string(TOUPPER ${_DIRCLASS} CLASS_UPPER)
    string(TOLOWER ${_DIRCLASS} CLASS_LOWER)
    execute_process(
        COMMAND ${GUILE_EXECUTABLE} -c ${_DIRCMD}
        RESULT_VARIABLE CMD_RESULT
        OUTPUT_VARIABLE CMD_OUTPUT
        ERROR_VARIABLE CMD_ERROR
        OUTPUT_STRIP_TRAILING_WHITESPACE
        ERROR_STRIP_TRAILING_WHITESPACE
    )
    if (CMD_RESULT)
        message(SEND_ERROR "Could not determine Guile ${CLASS_LOWER}:\n${CMD_ERROR}")
    endif()

    set(GUILE_${CLASS_UPPER} ${CMD_OUTPUT} PARENT_SCOPE)
    set(CMD_UNIX_OUTPUT  ${CMD_OUTPUT})
    make_unix_path(CMD_UNIX_OUTPUT)
    set(GUILE_UNIX_${CLASS_UPPER} ${CMD_UNIX_OUTPUT} PARENT_SCOPE)

    if (_PREFIX)
        string(REGEX REPLACE "^${_PREFIX}[\\/]*" "" CMD_REL_OUTPUT ${CMD_OUTPUT})
        set(GUILE_REL_${CLASS_UPPER} ${CMD_REL_OUTPUT} PARENT_SCOPE)
        set(CMD_REL_UNIX_OUTPUT  ${CMD_REL_OUTPUT})
        make_unix_path(CMD_REL_UNIX_OUTPUT)
        set(GUILE_REL_UNIX_${CLASS_UPPER} ${CMD_REL_UNIX_OUTPUT} PARENT_SCOPE)
    endif()

endfunction(find_one_guile_dir)

# Query the guile executable for path information. We're interested in guile's
# datadir, libdir, sitedir, ccachedir and siteccachedir
macro(find_guile_dirs)
    # Get GUILE_PREFIX and GUILE_UNIX_PREFIX
    find_one_guile_dir("prefix" "(display (assoc-ref %guile-build-info 'prefix))" "")
    # Get GUILE_DATADIR, GUILE_UNIX_DATADIR, GUILE_REL_DATADIR, GUILE_REL_UNIX_DATADIR
    find_one_guile_dir("datadir" "(display (%package-data-dir))" ${GUILE_PREFIX})
    # Get GUILE_LIBDIR, GUILE_UNIX_LIBDIR, GUILE_REL_LIBDIR, GUILE_REL_UNIX_LIBDIR
    find_one_guile_dir("libdir" "(display (%library-dir))" ${GUILE_PREFIX})
    # Get GUILE_CCACHEDIR, GUILE_UNIX_CCACHEDIR, GUILE_REL_CCACHEDIR, GUILE_REL_UNIX_CCACHEDIR
    find_one_guile_dir("ccachedir" "(display (assoc-ref %guile-build-info 'ccachedir))" ${GUILE_PREFIX})
    # Get GUILE_SITEDIR, GUILE_UNIX_SITEDIR, GUILE_REL_SITEDIR, GUILE_REL_UNIX_SITEDIR
    find_one_guile_dir("sitedir" "(display (%site-dir))" ${GUILE_PREFIX})
    # Get GUILE_SITECCACHEDIR, GUILE_UNIX_SITECCACHEDIR, GUILE_REL_SITECCACHEDIR, GUILE_REL_UNIX_SITECCACHEDIR
    find_one_guile_dir("siteccachedir" "(display (%site-ccache-dir))" ${GUILE_PREFIX})
    string(REGEX REPLACE "[/\\]*${GUILE_EFFECTIVE_VERSION}$" "" GUILE_REL_TOP_SITEDIR ${GUILE_REL_SITEDIR})
    string(REGEX REPLACE "[/]*${GUILE_EFFECTIVE_VERSION}$" "" GUILE_REL_UNIX_TOP_SITEDIR ${GUILE_REL_UNIX_SITEDIR})

    # Generate replacement strings for use in environment file. The paths used are
    # the paths found in %load-path and %load-compiled-path by default but
    # rebased on {GNC_HOME} (which is the runtime resolved variable to where gnucash
    # gets installed).
    set (GNC_GUILE_LOAD_PATH
            "{GNC_HOME}/${GUILE_REL_UNIX_LIBDIR}"
            "{GNC_HOME}/${GUILE_REL_UNIX_SITEDIR}"
            "{GNC_HOME}/${GUILE_REL_UNIX_TOP_SITEDIR}"
            "{GNC_HOME}/${GUILE_REL_UNIX_DATADIR}")

    set (GNC_GUILE_LOAD_COMPILED_PATH
            "{GNC_HOME}/${GUILE_REL_UNIX_CCACHEDIR}"
            "{GNC_HOME}/${GUILE_REL_UNIX_SITECCACHEDIR}")
endmacro(find_guile_dirs)

function(make_scheme_targets _TARGET _SOURCE_FILES _OUTPUT_DIR _GUILE_DEPENDS
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
  if(MINGW64 AND ${GUILE_EFFECTIVE_VERSION} VERSION_LESS 2.2)
    make_unix_path(build_bindir)
    make_unix_path(build_libdir)
    make_unix_path(build_datadir)
    make_unix_path(current_bindir)
    make_unix_path(current_srcdir)
    make_unix_path(CMAKE_BINARY_DIR)
    make_unix_path(CMAKE_SOURCE_DIR)
  endif()

  # If links are requested, we simply link (or copy, for Windows) each source file to the dest directory
  if(MAKE_LINKS)
    set(_LINK_DIR ${CMAKE_BINARY_DIR}/${GUILE_REL_UNIX_SITEDIR}/${_OUTPUT_DIR})
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
  set(_GUILE_LOAD_PATH "${current_srcdir}" "${current_bindir}")
  set(_GUILE_LOAD_COMPILED_PATH "${current_bindir}")
  # VERSION_GREATER_EQUAL introduced in CMake 3.7.
  if(MINGW64 AND (${GUILE_EFFECTIVE_VERSION} VERSION_GREATER 2.2 OR
	${GUILE_EFFECTIVE_VERSION} VERSION_EQUAL 2.2))
    file(TO_CMAKE_PATH $ENV{GUILE_LOAD_PATH} guile_load_path)
    file(TO_CMAKE_PATH $ENV{GUILE_LOAD_COMPILED_PATH} guile_load_compiled_path)
    list(APPEND _GUILE_LOAD_PATH ${guile_load_path})
    list(APPEND _GUILE_LOAD_COMPILED_PATH ${guile_load_compiled_path})
  endif()
  set(_GUILE_CACHE_DIR ${CMAKE_BINARY_DIR}/${GUILE_REL_UNIX_SITECCACHEDIR})
  list(APPEND _GUILE_LOAD_PATH "${CMAKE_BINARY_DIR}/${GUILE_REL_UNIX_SITEDIR}")
  list(APPEND _GUILE_LOAD_COMPILED_PATH ${_GUILE_CACHE_DIR})

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
      if (MINGW64)
        set(fpath "")
        file(TO_CMAKE_PATH "$ENV{PATH}" fpath)
        set(LIBRARY_PATH "PATH=\"${BINDIR_BUILD};${fpath}\"")
      else (MINGW64)
        set (LIBRARY_PATH "LD_LIBRARY_PATH=${LIBDIR_BUILD}:${LIBDIR_BUILD}/gnucash")
      endif (MINGW64)
      if (APPLE)
        set (LIBRARY_PATH "DYLD_LIBRARY_PATH=${LIBDIR_BUILD}:${LIBDIR_BUILD}/gnucash")
      endif (APPLE)
      set(_GNC_MODULE_PATH "")
      if(MINGW64)
        set(_GNC_MODULE_PATH "${build_bindir}")
      else(MINGW64)
        set(_GNC_MODULE_PATH "${LIBDIR_BUILD}" "${LIBDIR_BUILD}/gnucash" "${GNC_MODULE_PATH}")
      endif(MINGW64)
      if(NOT MINGW64 OR ${GUILE_EFFECTIVE_VERSION} VERSION_LESS 2.2)
        make_unix_path_list(_GUILE_LOAD_PATH)
        make_unix_path_list(_GUILE_LOAD_COMPILED_PATH)
      endif()
      make_unix_path_list(_GNC_MODULE_PATH)
      if (__DEBUG)
        message("  ")
        message("   LIBRARY_PATH: ${LIBRARY_PATH}")
        message("   GUILE_LOAD_PATH: ${_GUILE_LOAD_PATH}")
        message("   GUILE_LOAD_COMPILED_PATH: ${_GUILE_LOAD_COMPILED_PATH}")
        message("   GNC_MODULE_PATH: ${_GNC_MODULE_PATH}")
      endif(__DEBUG)
      #We quote the arguments to stop CMake stripping the path separators.
      add_custom_command(
        OUTPUT ${output_file}
        COMMAND ${CMAKE_COMMAND} -E env
            "${LIBRARY_PATH}"
            "GNC_UNINSTALLED=YES"
            "GNC_BUILDDIR=\"${CMAKE_BINARY_DIR}\""
            "GUILE_LOAD_PATH=\"${_GUILE_LOAD_PATH}\""
            "GUILE_LOAD_COMPILED_PATH=\"${_GUILE_LOAD_COMPILED_PATH}\""
            "GNC_MODULE_PATH=\"${_GNC_MODULE_PATH}\""
            ${GUILE_EXECUTABLE} -e '\(@@ \(guild\) main\)' -s ${GUILD_EXECUTABLE} compile -o ${output_file} ${source_file_abs_path}
        DEPENDS ${guile_depends}
        MAIN_DEPENDENCY ${source_file_abs_path}
        )
  endforeach(source_file)
  if (__DEBUG)
    message("TARGET_FILES are ${_TARGET_FILES}")
  endif(__DEBUG)
  add_custom_target(${_TARGET} ALL DEPENDS ${_TARGET_FILES})
  set(_TARGET_FILES "${_TARGET_FILES}" PARENT_SCOPE)
endfunction(make_scheme_targets)

function(gnc_add_scheme_targets _TARGET _SOURCE_FILES _OUTPUT_DIR _GUILE_DEPENDS
    MAKE_LINKS)
  make_scheme_targets("${_TARGET}" "${_SOURCE_FILES}" "${_OUTPUT_DIR}"
                      "${_GUILE_DEPENDS}" "${MAKE_LINKS}")
  install(FILES ${_TARGET_FILES} DESTINATION ${CMAKE_INSTALL_PREFIX}/${GUILE_REL_SITECCACHEDIR}/${_OUTPUT_DIR})
  install(FILES ${_SOURCE_FILES} DESTINATION ${CMAKE_INSTALL_PREFIX}/${GUILE_REL_SITEDIR}/${_OUTPUT_DIR})
endfunction(gnc_add_scheme_targets)

function(gnc_add_scheme_test_targets _TARGET _SOURCE_FILES _OUTPUT_DIR _GUILE_DEPENDS
    MAKE_LINKS)
  make_scheme_targets("${_TARGET}" "${_SOURCE_FILES}" "${_OUTPUT_DIR}"
                      "${_GUILE_DEPENDS}" "${MAKE_LINKS}")
  add_dependencies(check ${_TARGET})
endfunction(gnc_add_scheme_test_targets)
