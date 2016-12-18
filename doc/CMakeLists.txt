ADD_SUBDIRECTORY(examples)

SET(doc_DATA
  README.francais
  README.german
  README-de.win32-bin.txt
  README-fr.win32-bin.txt
  README-it.win32-bin.txt
  README-lv.win32-bin.txt
  README-nl.win32-bin.txt
  README-zh_CN.win32-bin.txt
  README-zh_TW.win32-bin.txt
  README.win32-bin.txt
  guile-hackers.txt
  projects.html
)

INSTALL(FILES ${doc_DATA} DESTINATION share/doc/gnucash)

IF (GNC_BUILD_AS_INSTALL)
  FILE(COPY ${doc_DATA} DESTINATION ${DATADIR_BUILD}/doc/gnucash)
ENDIF()

IF (${CMAKE_VERSION} VERSION_GREATER 3.1)
  EXECUTE_PROCESS(
    COMMAND ${CMAKE_COMMAND} -E  env date +"%B %Y"
    OUTPUT_VARIABLE DATE
    OUTPUT_STRIP_TRAILING_WHITESPACE
  )
ELSE()
  EXECUTE_PROCESS(
    COMMAND date +"%B %Y"
    OUTPUT_VARIABLE DATE
    OUTPUT_STRIP_TRAILING_WHITESPACE
  )
ENDIF()

# Generate the tip of the day file.

EXECUTE_PROCESS(
    COMMAND ${CMAKE_C_COMPILER} -E -P -x c -DN_\(x\)=x -o ${CMAKE_CURRENT_BINARY_DIR}/tip_of_the_day.list.tmp ${CMAKE_CURRENT_SOURCE_DIR}/tip_of_the_day.list.in
)

FILE(STRINGS ${CMAKE_CURRENT_BINARY_DIR}/tip_of_the_day.list.tmp TIP_OF_THE_DAY_LINES)

SET(TOTD_OUTPUT "")
FOREACH(line ${TIP_OF_THE_DAY_LINES})
  STRING(REGEX REPLACE "^ *\"" "" line2 "${line}")
  STRING(REGEX REPLACE "\" *$" "" line3 "${line2}")
  LIST(APPEND TOTD_OUTPUT "${line3}\n")
ENDFOREACH()

LIST(APPEND TOTD_OUTPUT "\n")

STRING(CONCAT FINAL_TOTD ${TOTD_OUTPUT})

FILE(WRITE ${CMAKE_CURRENT_BINARY_DIR}/tip_of_the_day.list "${FINAL_TOTD}")

INSTALL(FILES ${CMAKE_CURRENT_BINARY_DIR}/tip_of_the_day.list DESTINATION share/gnucash)

IF (GNC_BUILD_AS_INSTALL)
  FILE(COPY ${CMAKE_CURRENT_BINARY_DIR}/tip_of_the_day.list
    DESTINATION ${DATADIR_BUILD}/gnucash)
ENDIF()

# ---

GNC_CONFIGURE(gnucash.1.in gnucash.1 )
INSTALL(FILES ${CMAKE_CURRENT_BINARY_DIR}/gnucash.1 DESTINATION share/man/man1)

IF (GNC_BUILD_AS_INSTALL)
  FILE(COPY ${CMAKE_CURRENT_BINARY_DIR}/gnucash.1
    DESTINATION ${DATADIR_BUILD}/gnucash)
ENDIF()

