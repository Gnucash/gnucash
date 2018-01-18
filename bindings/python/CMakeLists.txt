ADD_SUBDIRECTORY(example_scripts)
ADD_SUBDIRECTORY(tests)

SET(PYEXEC_FILES  __init__.py function_class.py gnucash_business.py gnucash_core.py)

IF (BUILDING_FROM_VCS)
  SET(SWIG_FILES ${CMAKE_CURRENT_SOURCE_DIR}/gnucash_core.i ${CMAKE_CURRENT_SOURCE_DIR}/timespec.i)
  SET(GNUCASH_CORE_C_INCLUDES
    ${CONFIG_H}
    ${CMAKE_SOURCE_DIR}/libgnucash/engine/qofsession.h
    ${CMAKE_SOURCE_DIR}/libgnucash/engine/qofbook.h
    ${CMAKE_SOURCE_DIR}/libgnucash/engine/qofbackend.h
    ${CMAKE_SOURCE_DIR}/libgnucash/engine/qoflog.h
    ${CMAKE_SOURCE_DIR}/libgnucash/engine/qofutil.h
    ${CMAKE_SOURCE_DIR}/libgnucash/engine/qofid.h
    ${CMAKE_SOURCE_DIR}/libgnucash/engine/guid.h
    ${CMAKE_SOURCE_DIR}/libgnucash/gnc-module/gnc-module.h
    ${CMAKE_SOURCE_DIR}/libgnucash/engine/gnc-engine.h
    ${CMAKE_SOURCE_DIR}/libgnucash/engine/Transaction.h
    ${CMAKE_SOURCE_DIR}/libgnucash/engine/Split.h
    ${CMAKE_SOURCE_DIR}/libgnucash/engine/Account.h
    ${CMAKE_SOURCE_DIR}/libgnucash/engine/gnc-commodity.h
    ${CMAKE_SOURCE_DIR}/libgnucash/engine/gnc-lot.h
    ${CMAKE_SOURCE_DIR}/libgnucash/engine/gnc-numeric.h
    ${CMAKE_SOURCE_DIR}/libgnucash/engine/gncCustomer.h
    ${CMAKE_SOURCE_DIR}/libgnucash/engine/gncEmployee.h
    ${CMAKE_SOURCE_DIR}/libgnucash/engine/gncVendor.h
    ${CMAKE_SOURCE_DIR}/libgnucash/engine/gncAddress.h
    ${CMAKE_SOURCE_DIR}/libgnucash/engine/gncBillTerm.h
    ${CMAKE_SOURCE_DIR}/libgnucash/engine/gncOwner.h
    ${CMAKE_SOURCE_DIR}/libgnucash/engine/gncInvoice.h
    ${CMAKE_SOURCE_DIR}/libgnucash/engine/gncJob.h
    ${CMAKE_SOURCE_DIR}/libgnucash/engine/gncEntry.h
    ${CMAKE_SOURCE_DIR}/libgnucash/engine/gncTaxTable.h
    ${CMAKE_SOURCE_DIR}/libgnucash/engine/gncIDSearch.h
    ${CMAKE_SOURCE_DIR}/libgnucash/engine/gnc-pricedb.h
    ${CMAKE_SOURCE_DIR}/libgnucash/app-utils/gnc-prefs-utils.h
  )

  SET (SWIG_GNUCASH_CORE_C ${CMAKE_CURRENT_BINARY_DIR}/gnucash_core.c)
  SET (SWIG_GNUCASH_CORE_C_PY ${CMAKE_CURRENT_BINARY_DIR}/gnucash_core_c.py)

  GNC_ADD_SWIG_PYTHON_COMMAND (swig-gnucash-core ${SWIG_GNUCASH_CORE_C}
    ${SWIG_FILES}
    ${CMAKE_SOURCE_DIR}/common/base-typemaps.i
    ${CMAKE_SOURCE_DIR}/libgnucash/engine/engine-common.i
    ${GNUCASH_CORE_C_INCLUDES}
  )
ELSE()
  SET (SWIG_GNUCASH_CORE_C gnucash_core.c)
  SET (SWIG_GNUCASH_CORE_C_PY ${CMAKE_CURRENT_SOURCE_DIR}/gnucash_core_c.py)
ENDIF()

IF(WITH_PYTHON)

  SET(gnucash_core_c_INCLUDE_DIRS
    ${CMAKE_SOURCE_DIR}/libgnucash
    ${CMAKE_SOURCE_DIR}/libgnucash/engine
    ${CMAKE_SOURCE_DIR}/gnucash/gnome-utils
    ${CMAKE_SOURCE_DIR}/libgnucash/app-utils
    ${CMAKE_SOURCE_DIR}/libgnucash/gnc-module
    ${CMAKE_SOURCE_DIR}/gnucash/gnome
    ${CMAKE_SOURCE_DIR}/libgnucash/core-utils
    ${CMAKE_SOURCE_DIR}/libgnucash/gnc-module
    ${GLIB_INCLUDE_DIRS}
    ${PYTHON_INCLUDE_DIRS}
  )

  ADD_LIBRARY(gnucash_core_c MODULE ${SWIG_GNUCASH_CORE_C})
  TARGET_INCLUDE_DIRECTORIES(gnucash_core_c PRIVATE ${gnucash_core_c_INCLUDE_DIRS})

  TARGET_LINK_LIBRARIES(gnucash_core_c gncmod-app-utils gncmod-engine gnc-module ${GLIB_LIBS} ${PYTHON_LIBRARIES})
  SET_TARGET_PROPERTIES(gnucash_core_c PROPERTIES PREFIX "_")
  TARGET_COMPILE_OPTIONS(gnucash_core_c PRIVATE -Wno-implicit -Wno-missing-prototypes -Wno-declaration-after-statement -Wno-missing-declarations)

  ADD_EXECUTABLE(sqlite3test EXCLUDE_FROM_ALL sqlite3test.c ${SWIG_GNUCASH_CORE_C})
  TARGET_LINK_LIBRARIES(sqlite3test gncmod-app-utils gncmod-engine gnc-module ${GLIB_LIBS} ${PYTHON_LIBRARIES})
  TARGET_INCLUDE_DIRECTORIES(sqlite3test PRIVATE ${gnucash_core_c_INCLUDE_DIRS})
  TARGET_COMPILE_OPTIONS(sqlite3test PRIVATE -Wno-implicit -Wno-missing-prototypes -Wno-declaration-after-statement -Wno-missing-declarations)

  ADD_TEST(NAME sqlite3test COMMAND sqlite3test)
  ADD_DEPENDENCIES(check sqlite3test)

  INSTALL(TARGETS gnucash_core_c
    LIBRARY DESTINATION ${PYTHON_SYSCONFIG_OUTPUT}/gnucash
    ARCHIVE DESTINATION ${PYTHON_SYSCONFIG_OUTPUT}/gnucash
  )
  INSTALL(FILES ${PYEXEC_FILES} ${SWIG_GNUCASH_CORE_C_PY}
    DESTINATION ${PYTHON_SYSCONFIG_OUTPUT}/gnucash
  )

  FILE(COPY ${PYEXEC_FILES} DESTINATION ${PYTHON_SYSCONFIG_BUILD}/gnucash)

  ADD_CUSTOM_TARGET(gnucash-core-c-py ALL
    COMMAND ${CMAKE_COMMAND} -E copy ${SWIG_GNUCASH_CORE_C_PY} ${PYTHON_SYSCONFIG_BUILD}/gnucash
    DEPENDS ${SWIG_GNUCASH_CORE_C})

  ADD_CUSTOM_TARGET(gnucash-core-c-build ALL
    COMMAND ${CMAKE_COMMAND} -E copy ${LIBDIR_BUILD}/gnucash/_gnucash_core_c${CMAKE_SHARED_MODULE_SUFFIX} ${PYTHON_SYSCONFIG_BUILD}/gnucash
    DEPENDS gnucash_core_c)
    
ENDIF()

SET(python_bindings_DATA ${PYEXEC_FILES}
  gnucash_core.i
  sqlite3test.c
  timespec.i)

SET_LOCAL_DIST(python_bindings_DIST_local CMakeLists.txt 
  ${python_bindings_DATA})

SET(python_bindings_DIST ${python_bindings_DIST_local}
  ${test_python_bindings_DIST} ${example_scripts_DIST} PARENT_SCOPE)
