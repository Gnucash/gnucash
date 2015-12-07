

# Set of available languages.
SET (TP_LINGUAS az ca cs da eu fa ja nl pt rw sk sr sv tr uk zh_CN)
# already marked as external at TP:
SET (GC_LINGUAS ar bg de el en_GB es fi fr gu he hi hu it kn ko lt lv mr nb ne pl pt_BR ro ru ta te ur vi zh_TW)
# not marked or no TP team:
SET (NEW_LINGUAS as brx doi es_NI kok kok@latin ks mai mni mni@bengali)

SET (ALL_LINGUAS ${TP_LINGUAS} ${GC_LINGUAS} ${NEW_LINGUAS})

SET (CATALOGS "")
SET (BUILD_CATALOGS "")
FOREACH(lingua ${ALL_LINGUAS})
  SET(_OUTPUT_FILE ${lingua}.gmo)
  LIST(APPEND CATALOGS ${_OUTPUT_FILE})
  ADD_CUSTOM_COMMAND(
      OUTPUT ${_OUTPUT_FILE}
      COMMAND ${CMAKE_COMMAND} -E env ${MSGFMT} -o ${_OUTPUT_FILE} ${CMAKE_CURRENT_SOURCE_DIR}/${lingua}.po
  )
  IF (GNC_BUILD_AS_INSTALL)
    SET(_BUILD_FILE_DIR ${DATADIR_BUILD}/locale/${lingua}/LC_MESSAGES)
    MAKE_DIRECTORY(${_BUILD_FILE_DIR})
    SET(_BUILD_FILE ${_BUILD_FILE_DIR}/gnucash.gmo)
    LIST(APPEND BUILD_CATALOGS ${_BUILD_FILE})
    #ADD_CUSTOM_COMMAND(
    #  OUTPUT ${_BUILD_FILE}
    #  COMMAND ${CMAKE_COMMAND} make_directory ${_BUILD_FILE_DIR}
    #
    #)
    ADD_CUSTOM_COMMAND(
      OUTPUT ${_BUILD_FILE}
      COMMAND ${CMAKE_COMMAND} -E copy ${CMAKE_CURRENT_BINARY_DIR}/${lingua}.gmo ${_BUILD_FILE}
      DEPENDS ${CMAKE_CURRENT_BINARY_DIR}/${_OUTPUT_FILE}
      #APPEND
    )
  ENDIF()
ENDFOREACH(lingua)

ADD_CUSTOM_TARGET(po-gmo ALL DEPENDS ${CATALOGS})
IF (GNC_BUILD_AS_INSTALL)
  ADD_CUSTOM_TARGET(po-gmo-build ALL DEPENDS ${BUILD_CATALOGS})
ENDIF()


FOREACH(lingua ${ALL_LINGUAS})
  INSTALL(FILES ${CMAKE_CURRENT_BINARY_DIR}/${lingua}.gmo RENAME gnucash.mo DESTINATION share/locale/${lingua}/LC_MESSAGES)
ENDFOREACH(lingua)