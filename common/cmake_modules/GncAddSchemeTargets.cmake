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

endfunction()

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
            "{GNC_HOME}/${GUILE_REL_UNIX_SITEDIR}/gnucash/deprecated" # Path to gnucash' deprecated modules
            "{GNC_HOME}/${GUILE_REL_UNIX_DATADIR}")

    set (GNC_GUILE_LOAD_COMPILED_PATH
            "{GNC_HOME}/${GUILE_REL_UNIX_CCACHEDIR}"
            "{GNC_HOME}/${GUILE_REL_UNIX_CCACHEDIR}/gnucash/deprecated"
            "{GNC_HOME}/${GUILE_REL_UNIX_SITECCACHEDIR}")
endmacro(find_guile_dirs)

# gnc_add_scheme_targets (target
#                         SOURCES source1 source2 ...
#                         OUTPUT_DIR directory
#                         [DEPENDS depedency1 dependency2 ...]
#                         [MAKE_LINKS] [TEST])
#
#̉ Use this function to add scheme targets, that is *.scm files to
# compile into their corresponding *.go files.
#
# SOURCES is a list of scm source files to compile.
#
# The resulting *.go binaries will be generated in OUTPUT_DIR directory.
# This directory is interpreted as a path relative to the build's guile compiled directory
# directory. For example if guile binaries in the build directory are located in
# $HOME/gnucash/build/lib/x86_64-linux-gnu/guile/2.0/site-cache and OUTPUT_DIR is "gnucash"
# the binary .go files will go into
# $HOME/gnucash/build/lib/x86_64-linux-gnu/guile/2.0/site-cache/gnucash
#
# If cmake targets are provided via the DEPENDS keyword those will be added to
# the guile targets as dependencies.
#
# If MAKE_LINKS is set links (or copies on Windows) will be set up
# from the source directory to the build's guile sources directory.
# For example if guile source path in the build directory is
# $HOME/gnucash/build/share/guile/site/2.0 and OUTPUT_DIR is "gnucash"
# the links or copies will go into
# $HOME/gnucash/build/share/guile/site/2.0/gnucash
#
# If keyword TEST is specified this target will be treated as a test target.
# That is its compiled files won't be installed and will be added to the set
# of tests to run via the "check" target. If TEST is not set the targets are
# considered normal targets and will be added to the list of files to install.
# They will be installed in the guile compiled directory relative to the prefix
# set up for this build, with the OUTPUT_DIR appended to it. For example:
# /usr/local/lib/x86_64-linux-gnu/guile/2.0/site-cache/gnucash
function(gnc_add_scheme_targets _TARGET)
  set(noValues MAKE_LINKS TEST)
  set(singleValues OUTPUT_DIR)
  set(multiValues SOURCES DEPENDS)
  cmake_parse_arguments(SCHEME_TGT "${noValues}" "${singleValues}" "${multiValues}" ${ARGN})
  
  set(__DEBUG FALSE)
  if (__DEBUG)
    message("Parameters to COMPILE_SCHEME for target ${_TARGET}")
    message("   SOURCE_FILES: ${SCHEME_TGT_SOURCES}")
    message("   GUILE_DEPENDS: ${SCHEME_TGT_DEPENDS}")
    message("   MAKE_LINKS: ${SCHEME_TGT_MAKE_LINKS}")
    message("   TEST: ${SCHEME_TGT_TEST}")
    message("   DIRECTORIES: ${BINDIR_BUILD}, ${LIBDIR_BUILD}, ${DATADIR_BUILD}, ${SCHEME_TGT_OUTPUT_DIR}")
  endif()
  set(_CMD "create_symlink")
  if(WIN32)
    set(_CMD "copy")
  endif()
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
  set(TARGET_LINKS "")
  if(SCHEME_TGT_MAKE_LINKS)
    set(_LINK_DIR ${CMAKE_BINARY_DIR}/${GUILE_REL_UNIX_SITEDIR}/${SCHEME_TGT_OUTPUT_DIR})
    file(MAKE_DIRECTORY ${_LINK_DIR})
    set(_SCHEME_LINKS "")
    foreach(scheme_file ${SCHEME_TGT_SOURCES})
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
    set(TARGET_LINKS ${_TARGET}-links)
    add_custom_target(${TARGET_LINKS} ALL DEPENDS ${_SCHEME_LINKS})
  endif()

  # Construct the guile source and compiled load paths
  set(_GUILE_LOAD_PATH "${current_srcdir}" "${current_bindir}" "${current_bindir}/deprecated")
  set(_GUILE_LOAD_COMPILED_PATH "${current_bindir}")
  # VERSION_GREATER_EQUAL introduced in CMake 3.7.
  if(MINGW64 AND (${GUILE_EFFECTIVE_VERSION} VERSION_GREATER_EQUAL 2.2))
    if (NOT (DEFINED ENV{GUILE_LOAD_PATH} AND DEFINED ENV{GUILE_LOAD_COMPILED_PATH}))
      message(FATAL_ERROR "$GUILE_LOAD_PATH and $GUILE_LOAD_COMPILED_PATH must be defined in the environment to configure GnuCash on Microsoft Windows.")
    endif()
    file(TO_CMAKE_PATH $ENV{GUILE_LOAD_PATH} guile_load_path)
    file(TO_CMAKE_PATH $ENV{GUILE_LOAD_COMPILED_PATH} guile_load_compiled_path)
    list(APPEND _GUILE_LOAD_PATH ${guile_load_path})
    list(APPEND _GUILE_LOAD_COMPILED_PATH ${guile_load_compiled_path})
  endif()
  set(_GUILE_CACHE_DIR "${CMAKE_BINARY_DIR}/${GUILE_REL_UNIX_SITECCACHEDIR}")
  list(APPEND _GUILE_LOAD_PATH "${CMAKE_BINARY_DIR}/${GUILE_REL_UNIX_SITEDIR}")
  list(APPEND _GUILE_LOAD_COMPILED_PATH ${_GUILE_CACHE_DIR}
              "${CMAKE_BINARY_DIR}/${GUILE_REL_UNIX_SITECCACHEDIR}/gnucash/deprecated")

  set(_TARGET_FILES "")

  foreach(source_file ${SCHEME_TGT_SOURCES})
      set(guile_depends ${SCHEME_TGT_DEPENDS})
      get_filename_component(basename ${source_file} NAME_WE)

      set(output_file ${basename}.go)
      set(_TMP_OUTPUT_DIR ${SCHEME_TGT_OUTPUT_DIR})
      if (_TMP_OUTPUT_DIR)
        set(output_file ${SCHEME_TGT_OUTPUT_DIR}/${basename}.go)
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
        set(LIBRARY_PATH "PATH=${BINDIR_BUILD};${fpath}")
      else()
        set (LIBRARY_PATH "LD_LIBRARY_PATH=${LIBDIR_BUILD}:${LIBDIR_BUILD}/gnucash:$ENV{LD_LIBRARY_PATH}")
      endif()
      if (APPLE)
        set (LIBRARY_PATH "DYLD_LIBRARY_PATH=${LIBDIR_BUILD}:${LIBDIR_BUILD}/gnucash:$ENV{DYLD_LIBRARY_PATH}")
      endif()
      set(_GNC_MODULE_PATH "")
      if(MINGW64)
        set(_GNC_MODULE_PATH "${build_bindir}")
      else()
        set(_GNC_MODULE_PATH "${LIBDIR_BUILD}" "${LIBDIR_BUILD}/gnucash" "${GNC_MODULE_PATH}")
      endif()
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
      endif()
      #We quote the arguments to stop CMake stripping the path separators.
      set (GUILE_ENV
        "${LIBRARY_PATH}"
        "GNC_UNINSTALLED=YES"
        "GNC_BUILDDIR=${CMAKE_BINARY_DIR}"
        "GUILE_LOAD_PATH=${_GUILE_LOAD_PATH}"
        "GUILE_LOAD_COMPILED_PATH=${_GUILE_LOAD_COMPILED_PATH}"
        "GNC_MODULE_PATH=${_GNC_MODULE_PATH}"
      )

      add_custom_command(
        OUTPUT ${output_file}
        COMMAND ${CMAKE_COMMAND} -E env
            "${GUILE_ENV}$<$<CONFIG:Asan>:;${ASAN_DYNAMIC_LIB_ENV};ASAN_OPTIONS=${ASAN_BUILD_OPTIONS}>"
            ${GUILE_EXECUTABLE} -e "\(@@ \(guild\) main\)" -s ${GUILD_EXECUTABLE} compile -o ${output_file} ${source_file_abs_path}
        DEPENDS ${guile_depends}
        MAIN_DEPENDENCY ${source_file_abs_path}
        COMMAND_EXPAND_LISTS
        VERBATIM
        )
  endforeach(source_file)
  if (__DEBUG)
    message("TARGET_FILES are ${_TARGET_FILES}")
  endif()
  add_custom_target(${_TARGET} ALL DEPENDS ${_TARGET_FILES} ${TARGET_LINKS})

  set(_TARGET_FILES "${_TARGET_FILES}" PARENT_SCOPE)

  if(SCHEME_TGT_TEST)
    add_dependencies(check ${_TARGET})
  else()
    install(FILES ${_TARGET_FILES} DESTINATION ${CMAKE_INSTALL_PREFIX}/${GUILE_REL_SITECCACHEDIR}/${SCHEME_TGT_OUTPUT_DIR})
    install(FILES ${SCHEME_TGT_SOURCES} DESTINATION ${CMAKE_INSTALL_PREFIX}/${GUILE_REL_SITEDIR}/${SCHEME_TGT_OUTPUT_DIR})
  endif()
endfunction()


# gnc_add_scheme_test_targets (target
#                              SOURCES source1 source2 ...
#                              OUTPUT_DIR directory
#                              [DEPENDS depedency1 dependency2 ...]
#                              [MAKE_LINKS])
#
# Calls gnc_add_scheme_targets with the TEST keyword set
# See that function's description for more details.
function(gnc_add_scheme_test_targets _TARGET)
  gnc_add_scheme_targets(${_TARGET} ${ARGN} TEST)
endfunction()

# gnc_add_scheme_deprecated_module (OLD_MODULE old_module_name
#                                   [NEW_MODULE new_module_name
#                                    DEPENDS new_module_target]
#                                   [MESSAGE msg_string])
#
# Function to write boilerplate code for deprecated guile modules
# such that invocation of the old module will emit a deprecation warning
# message.
#
# All but the OLD_MODULE keyword are optional
#
# OLD_MODULE and NEW_MODULE should be passed in the form
# "gnucash mod parts"
#
# If NEW_MODULE is set that module will be loaded instead of the
# deprecated module.
# If NEW_MODULE is set, DEPENDS should be set to the target for which
# that module is a source file.
# For example module (gnucash reports standard transaction)
# is defined in transaction.scm, which is a source file for
# cmake target scm-reports-standard so that should be set as DEPENDS.
#
# If MESSAGE is left blank, the module will emit a generic message.
# Otherwise MESSAGE will be emitted.
function(gnc_add_scheme_deprecated_module)

    set(singleValues OLD_MODULE NEW_MODULE MESSAGE)
    set(multiValues DEPENDS)
    cmake_parse_arguments(DM "" "${singleValues}" "${multiValues}" ${ARGN})

    string(STRIP DM_OLD_MODULE "${DM_OLD_MODULE}")
    string(REPLACE " " "-" _TARGET ${DM_OLD_MODULE})
    set(_TARGET "scm-deprecated-${_TARGET}")

    string(REPLACE " " ";" MODPARTS "${DM_OLD_MODULE}")
    list(GET MODPARTS -1 DEPFILENAME)
    set(SOURCEFILE "${CMAKE_CURRENT_BINARY_DIR}/deprecated/${DEPFILENAME}.scm")

    string(FIND "${DM_OLD_MODULE}" ${DEPFILENAME} POS REVERSE)
    if (${POS} LESS 2)
        set(MODPATH "gnucash/deprecated")
    else()
        list(REMOVE_AT MODPARTS -1)
        string(REPLACE ";" "/" MODPATH "${MODPARTS}")
        set(MODPATH "gnucash/deprecated/${MODPATH}")
    endif()

    set(DEPPREFIX "* WARN <gnc-guile-deprecation> *: ")
    if (DM_MESSAGE)
        set(DEPWARNING "(issue-deprecation-warning \"${DEPPREFIX}${DM_MESSAGE}\")")
    else()
        set(DEPWARNING
            "(issue-deprecation-warning \"${DEPPREFIX}Module '(${DM_OLD_MODULE})' has been deprecated and will be removed in the future.\")")
        if (DM_NEW_MODULE)
            set(DEPWARNING "${DEPWARNING}
                (issue-deprecation-warning \"${DEPPREFIX}Use module '(${DM_NEW_MODULE})' instead.\")")
        endif()
    endif()

    # Write the stub file
    file(WRITE ${SOURCEFILE} "
;; ${DEPFILENAME}.scm
;; Compatibility module for deprecated (${DM_OLD_MODULE}).
;; This file is autogenerated, do not modify by hand.

(define-module (${DM_OLD_MODULE}))

${DEPWARNING}
")

    if (DM_NEW_MODULE)
        file(APPEND ${SOURCEFILE} "
(use-modules (${DM_NEW_MODULE}))

(let ((i (module-public-interface (current-module))))
     (module-use! i (resolve-interface '(${DM_NEW_MODULE}))))")
    endif()

    gnc_add_scheme_targets("${_TARGET}"
                           SOURCES "${SOURCEFILE}"
                           OUTPUT_DIR "${MODPATH}"
                           DEPENDS "${DM_DEPENDS}")
endfunction()
