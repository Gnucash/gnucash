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
FUNCTION(MAKE_UNIX_PATH PATH)
    STRING(REGEX REPLACE "^([A-Za-z]):" "/\\1" newpath ${${PATH}})
    string(REGEX REPLACE "\\\\" "/" newpath ${newpath})
    SET(${PATH} ${newpath} PARENT_SCOPE)
ENDFUNCTION()

#PATH variables in the environment are separated by colons, but CMake lists are separated by semicolons. This function transforms the separators.
FUNCTION(MAKE_UNIX_PATH_LIST PATH)
    STRING(REPLACE ";" ":" newpath "${${PATH}}")
    SET(${PATH} ${newpath} PARENT_SCOPE)
ENDFUNCTION()

FUNCTION(GNC_ADD_SCHEME_TARGETS _TARGET _SOURCE_FILES _OUTPUT_DIR _GUILE_DEPENDS
				MAKE_LINKS)
  SET(__DEBUG FALSE)
  IF (__DEBUG)
    MESSAGE("Parameters to COMPILE_SCHEME for target ${_TARGET}")
    MESSAGE("   SOURCE_FILES: ${_SOURCE_FILES}")
    MESSAGE("   GUILE_DEPENDS: ${_GUILE_DEPENDS}")
    MESSAGE("   DIRECTORIES: ${BINDIR_BUILD}, ${LIBDIR_BUILD}, ${DATADIR_BUILD}")
  ENDIF(__DEBUG)
  SET(_CMD "create_symlink")
  IF(WIN32)
    SET(_CMD "copy")
  ENDIF(WIN32)
  SET(current_srcdir ${CMAKE_CURRENT_SOURCE_DIR})
  SET(current_bindir ${CMAKE_CURRENT_BINARY_DIR})
  SET(build_bindir ${BINDIR_BUILD})
  SET(build_libdir ${LIBDIR_BUILD})
  SET(build_datadir ${DATADIR_BUILD})
  IF(MINGW64)
    MAKE_UNIX_PATH(build_bindir)
    MAKE_UNIX_PATH(build_libdir)
    MAKE_UNIX_PATH(build_datadir)
    MAKE_UNIX_PATH(current_bindir)
    MAKE_UNIX_PATH(current_srcdir)
    MAKE_UNIX_PATH(CMAKE_BINARY_DIR)
    MAKE_UNIX_PATH(CMAKE_SOURCE_DIR)
  ENDIF(MINGW64)

  # If links are requested, we simple link (or copy, for Windows) each source file to the dest directory
  IF(MAKE_LINKS)
    SET(_LINK_DIR ${DATADIR_BUILD}/gnucash/scm/${_OUTPUT_DIR})
    FILE(MAKE_DIRECTORY ${_LINK_DIR})
    SET(_SCHEME_LINKS "")
    FOREACH(scheme_file ${_SOURCE_FILES})
      SET(_SOURCE_FILE ${current_srcdir}/${scheme_file})
      IF(IS_ABSOLUTE ${scheme_file})
        SET(_SOURCE_FILE ${scheme_file})
      ENDIF()
      GET_FILENAME_COMPONENT(name ${scheme_file} NAME)
      SET(_OUTPUT_FILE ${_LINK_DIR}/${name})
      IF(NOT EXISTS ${_OUTPUT_FILE})
        LIST(APPEND _SCHEME_LINKS ${_OUTPUT_FILE})
        ADD_CUSTOM_COMMAND(
            OUTPUT ${_OUTPUT_FILE}
            COMMAND ${CMAKE_COMMAND} -E ${_CMD} ${_SOURCE_FILE} ${_OUTPUT_FILE}
        )
      ENDIF()
    ENDFOREACH(scheme_file)
    ADD_CUSTOM_TARGET(${_TARGET}-links ALL DEPENDS ${_SCHEME_LINKS})
  ENDIF(MAKE_LINKS)

  # Construct the guile source and compiled load paths

  SET(_GUILE_LOAD_PATH "${current_srcdir}"
      "${current_bindir}" "${CMAKE_BINARY_DIR}/libgnucash/scm")  # to pick up generated build-config.scm
  SET(_GUILE_LOAD_COMPILED_PATH "${current_bindir}")

  SET(_GUILE_CACHE_DIR ${LIBDIR_BUILD}/gnucash/scm/ccache/${GUILE_EFFECTIVE_VERSION})
  SET(_GUILE_LOAD_PATH "${current_srcdir}")
  IF (MAKE_LINKS)
      LIST(APPEND _GUILE_LOAD_PATH "${build_datadir}/gnucash/scm")
  ENDIF()
  SET(_GUILE_LOAD_COMPILED_PATH ${build_libdir}/gnucash/scm/ccache/${GUILE_EFFECTIVE_VERSION})

  SET(_TARGET_FILES "")

  FOREACH(source_file ${_SOURCE_FILES})
      SET(guile_depends ${_GUILE_DEPENDS})
      GET_FILENAME_COMPONENT(basename ${source_file} NAME_WE)

      SET(output_file ${basename}.go)
      SET(_TMP_OUTPUT_DIR ${_OUTPUT_DIR})
      IF (_TMP_OUTPUT_DIR)
        SET(output_file ${_OUTPUT_DIR}/${basename}.go)
      ENDIF()
      SET(output_file ${_GUILE_CACHE_DIR}/${output_file})
      LIST(APPEND _TARGET_FILES ${output_file})

      SET(source_file_abs_path ${CMAKE_CURRENT_SOURCE_DIR}/${source_file})
      IF (IS_ABSOLUTE ${source_file})
        SET(source_file_abs_path ${source_file})
      ENDIF()
      IF (__DEBUG)
        MESSAGE("ADD_CUSTOM_COMMAND: output = ${output_file}")
      ENDIF()
      SET(CMAKE_COMMMAND_TMP "")
      IF (${CMAKE_VERSION} VERSION_GREATER 3.1)
        SET(CMAKE_COMMAND_TMP ${CMAKE_COMMAND} -E env)
      ENDIF()
      IF (MINGW64)
        set(fpath "")
        foreach(dir $ENV{PATH})
            MAKE_UNIX_PATH(dir)
            set(fpath "${fpath}${dir}:")
        endforeach(dir)
        SET(LIBRARY_PATH "PATH=\"${build_bindir}:${fpath}\"")
      ELSE (MINGW64)
        SET (LIBRARY_PATH "LD_LIBRARY_PATH=${LIBDIR_BUILD}:${LIBDIR_BUILD}/gnucash:${_GUILE_LD_LIBRARY_PATH}")
      ENDIF (MINGW64)
      IF (APPLE)
        SET (LIBRARY_PATH "DYLD_LIBRARY_PATH=${LIBDIR_BUILD}:${LIBDIR_BUILD}/gnucash:${_GUILE_LD_LIBRARY_PATH}")
      ENDIF (APPLE)
      SET(_GNC_MODULE_PATH "")
      IF(MINGW64)
        SET(_GNC_MODULE_PATH "${build_bindir}")
      ELSE(MINGW64)
        SET(_GNC_MODULE_PATH "${LIBDIR_BUILD}" "${LIBDIR_BUILD}/gnucash" "${GNC_MODULE_PATH}")
      ENDIF(MINGW64)
      MAKE_UNIX_PATH_LIST(_GUILE_LOAD_PATH)
      MAKE_UNIX_PATH_LIST(_GUILE_LOAD_COMPILED_PATH)
      MAKE_UNIX_PATH_LIST(_GUILE_LD_LIBRARY_PATH)
      MAKE_UNIX_PATH_LIST(_GNC_MODULE_PATH)
      IF (__DEBUG)
        MESSAGE("  ")
        MESSAGE("   LIBRARY_PATH: ${LIBRARY_PATH}")
        MESSAGE("   GUILE_LOAD_PATH: ${_GUILE_LOAD_PATH}")
        MESSAGE("   GUILE_LOAD_COMPILED_PATH: ${_GUILE_LOAD_COMPILED_PATH}")
        MESSAGE("   GNC_MODULE_PATH: ${_GNC_MODULE_PATH}")
      ENDIF(__DEBUG)
      ADD_CUSTOM_COMMAND(
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
  ENDFOREACH(source_file)
  IF (__DEBUG)
    MESSAGE("TARGET_FILES are ${_TARGET_FILES}")
  ENDIF(__DEBUG)
  ADD_CUSTOM_TARGET(${_TARGET} ALL DEPENDS ${_TARGET_FILES})
  INSTALL(FILES ${_TARGET_FILES} DESTINATION ${SCHEME_INSTALLED_CACHE_DIR}/${_OUTPUT_DIR})
  INSTALL(FILES ${_SOURCE_FILES} DESTINATION ${SCHEME_INSTALLED_SOURCE_DIR}/${_OUTPUT_DIR})
ENDFUNCTION(GNC_ADD_SCHEME_TARGETS)
