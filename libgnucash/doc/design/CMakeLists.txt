
set(gnucash_design_TEXINFOS
  component-manager.texi
  concept-index.texi
  engine.texi
  fdl.texi
  function-index.texi
  intro.texi
  register.texi
  reports.texi
  top-level.texi
  type-index.texi
  user-preferences.texi
)

  execute_process(COMMAND date +%e OUTPUT_VARIABLE CURRENT_DATE OUTPUT_STRIP_TRAILING_WHITESPACE)
  execute_process(COMMAND date +%B OUTPUT_VARIABLE CURRENT_MONTH OUTPUT_STRIP_TRAILING_WHITESPACE)
  execute_process(COMMAND date +%Y OUTPUT_VARIABLE CURRENT_YEAR OUTPUT_STRIP_TRAILING_WHITESPACE)
set(UPDATED "${CURRENT_DATE} ${CURRENT_MONTH} ${CURRENT_YEAR}")
set(UPDATED_MONTH "${CURRENT_MONTH} ${CURRENT_YEAR}")

set(VERSION_TEXI_IN
  "@set UPDATED ${UPDATED}
@set UPDATED-MONTH ${UPDATED_MONTH}
@set EDITION ${PROJECT_VERSION}
@set VERSION ${PROJECT_VERSION}
")

file(WRITE ${CMAKE_CURRENT_BINARY_DIR}/version.texi ${VERSION_TEXI_IN})
file(WRITE ${CMAKE_CURRENT_BINARY_DIR}/stamp-vti ${VERSION_TEXI_IN})

file(TO_NATIVE_PATH "${CMAKE_CURRENT_BINARY_DIR}" TEXI_BINARY_DIR)
file(TO_NATIVE_PATH "${CMAKE_CURRENT_SOURCE_DIR}/gnucash-design.texi" TEXI_SOURCE_FILE)
find_program(MAKEINFO makeinfo)
if (NOT ${MAKEINFO} STREQUAL "MAKEINFO-NOTFOUND")
  add_custom_command(
    OUTPUT gnucash-design.info
    COMMAND ${MAKEINFO} -I ${TEXI_BINARY_DIR} ${TEXI_SOURCE_FILE}
    DEPENDS ${gnucash_design_TEXINFOS}
  )
endif()
dist_add_generated (${BUILDING_FROM_VCS} gnucash-design.info)

set_dist_list(doc_design_DIST CMakeLists.txt gnucash-design.texi ${gnucash_design_TEXINFOS})
