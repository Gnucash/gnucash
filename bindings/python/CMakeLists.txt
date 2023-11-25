add_subdirectory(example_scripts)
add_subdirectory(tests)

set(PYEXEC_FILES  __init__.py function_class.py gnucash_business.py gnucash_core.py app_utils.py deprecation.py)

set(SWIG_FILES ${CMAKE_CURRENT_SOURCE_DIR}/gnucash_core.i ${CMAKE_CURRENT_SOURCE_DIR}/time64.i)
set(GNUCASH_CORE_C_INCLUDES
    ${CONFIG_H}
    ${CMAKE_SOURCE_DIR}/libgnucash/engine/qofsession.h
    ${CMAKE_SOURCE_DIR}/libgnucash/engine/qofbook.h
    ${CMAKE_SOURCE_DIR}/libgnucash/engine/qofbackend.h
    ${CMAKE_SOURCE_DIR}/libgnucash/engine/qoflog.h
    ${CMAKE_SOURCE_DIR}/libgnucash/engine/qofutil.h
    ${CMAKE_SOURCE_DIR}/libgnucash/engine/qofid.h
    ${CMAKE_SOURCE_DIR}/libgnucash/engine/guid.h
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

gnc_add_swig_python_command (swig-gnucash-core
    SWIG_GNUCASH_CORE_C SWIG_GNUCASH_CORE_PY
    gnucash_core.c gnucash_core_c.py
    ${CMAKE_CURRENT_SOURCE_DIR}/gnucash_core.i
    ${CMAKE_SOURCE_DIR}/bindings
    ${CMAKE_CURRENT_SOURCE_DIR}/time64.i
    ${CMAKE_SOURCE_DIR}/common/base-typemaps.i
    ${CMAKE_SOURCE_DIR}/bindings/engine-common.i
    ${GNUCASH_CORE_C_INCLUDES}
)

# Command to generate the swig-core-utils-python.c wrapper file
gnc_swig_extract_header_files (gnc-core-utils CORE_UTILS_HEADERS)
gnc_add_swig_python_command (swig-core-utils-python
    SWIG_CORE_UTILS_PYTHON_C SWIG_CORE_UTILS_PYTHON_PY
    swig-core-utils-python.c sw_core_utils.py
    ${CMAKE_SOURCE_DIR}/bindings/core-utils.i ${CMAKE_SOURCE_DIR}/libgnucash/core-utils
    ${CORE_UTILS_HEADERS}
)

# Command to generate the swig-app-utils-python.c wrapper file
gnc_add_swig_python_command (swig-app-utils-python
        SWIG_APP_UTILS_PYTHON_C SWIG_APP_UTILS_PYTHON_PY
        swig-app-utils-python.c sw_app_utils.py
        ${CMAKE_CURRENT_SOURCE_DIR}/../app-utils.i ""
        )

if(WITH_PYTHON)

  ### gnucash_core_c

  set(gnucash_core_c_INCLUDE_DIRS
    ${CMAKE_SOURCE_DIR}/libgnucash
    ${CMAKE_SOURCE_DIR}/libgnucash/engine
    ${CMAKE_SOURCE_DIR}/gnucash/gnome-utils
    ${CMAKE_SOURCE_DIR}/libgnucash/app-utils
    ${CMAKE_SOURCE_DIR}/gnucash/gnome
    ${CMAKE_SOURCE_DIR}/libgnucash/core-utils
    ${Python3_INCLUDE_DIRS}
  )

  add_library(gnucash_core_c MODULE ${SWIG_GNUCASH_CORE_C})
  target_include_directories(gnucash_core_c PRIVATE ${gnucash_core_c_INCLUDE_DIRS})

  target_link_libraries(gnucash_core_c gnc-app-utils gnc-engine ${Python3_LIBRARIES})
  set_target_properties(gnucash_core_c PROPERTIES PREFIX "_")
  target_compile_options(gnucash_core_c PRIVATE -Wno-implicit -Wno-missing-prototypes -Wno-declaration-after-statement -Wno-missing-declarations)
  if (HAVE_STRINGOP_TRUNCATION)
    target_compile_options(gnucash_core_c PRIVATE -Wno-error=stringop-truncation)
  endif()

  add_executable(sqlite3test EXCLUDE_FROM_ALL sqlite3test.c ${SWIG_GNUCASH_CORE_C})
  target_link_libraries(sqlite3test gnc-app-utils gnc-engine ${Python3_LIBRARIES})
  target_include_directories(sqlite3test PRIVATE ${gnucash_core_c_INCLUDE_DIRS})
  target_compile_options(sqlite3test PRIVATE -Wno-implicit -Wno-missing-prototypes -Wno-declaration-after-statement -Wno-missing-declarations)
  if (HAVE_STRINGOP_TRUNCATION)
    target_compile_options(sqlite3test PRIVATE -Wno-error=stringop-truncation)
  endif()

  add_test(NAME sqlite3test COMMAND sqlite3test)
  add_dependencies(check sqlite3test)
  set_tests_properties(sqlite3test PROPERTIES ENVIRONMENT "$<$<CONFIG:Asan>:;ASAN_OPTIONS=${ASAN_TEST_OPTIONS}>")

  install(TARGETS gnucash_core_c
    LIBRARY DESTINATION ${PYTHON_SYSCONFIG_OUTPUT}/gnucash
    ARCHIVE DESTINATION ${PYTHON_SYSCONFIG_OUTPUT}/gnucash
  )
  install(FILES ${PYEXEC_FILES} ${SWIG_GNUCASH_CORE_PY}
    DESTINATION ${PYTHON_SYSCONFIG_OUTPUT}/gnucash
  )

  foreach(PYEXEC_FILE ${PYEXEC_FILES})
      configure_file(${PYEXEC_FILE} ${PYTHON_SYSCONFIG_BUILD}/gnucash/${PYEXEC_FILE} COPYONLY)
  endforeach()

  add_custom_target(gnucash-core-c-py ALL
    COMMAND ${CMAKE_COMMAND} -E copy ${SWIG_GNUCASH_CORE_PY} ${PYTHON_SYSCONFIG_BUILD}/gnucash
    DEPENDS ${SWIG_GNUCASH_CORE_C})

  add_custom_target(gnucash-core-c-build ALL
    COMMAND ${CMAKE_COMMAND} -E copy ${LIBDIR_BUILD}/gnucash/_gnucash_core_c${CMAKE_SHARED_MODULE_SUFFIX} ${PYTHON_SYSCONFIG_BUILD}/gnucash
    DEPENDS gnucash_core_c)


  ### sw_core_utils

  add_library (sw_core_utils MODULE ${SWIG_CORE_UTILS_PYTHON_C})

  target_link_libraries(sw_core_utils gnc-core-utils ${Python3_LIBRARIES})
  set_target_properties(sw_core_utils PROPERTIES PREFIX "_")

  if (HAVE_STRINGOP_TRUNCATION)
    target_compile_options(sw_core_utils PRIVATE -Wno-error=stringop-truncation)
  endif()

  target_include_directories (sw_core_utils
    PUBLIC ${CMAKE_CURRENT_SOURCE_DIR}
    PRIVATE ${Python3_INCLUDE_DIRS}
  )
  target_compile_definitions (sw_core_utils PRIVATE -DG_LOG_DOMAIN=\"gnc.core-utils\")

  if (APPLE)
    set_target_properties (sw_core_utils PROPERTIES INSTALL_NAME_DIR "${CMAKE_INSTALL_FULL_LIBDIR}/gnucash")
  endif()

  install(TARGETS sw_core_utils
    LIBRARY DESTINATION ${PYTHON_SYSCONFIG_OUTPUT}/gnucash
    ARCHIVE DESTINATION ${PYTHON_SYSCONFIG_OUTPUT}/gnucash
  )

  add_custom_target(sw-core-utils-py ALL
    COMMAND ${CMAKE_COMMAND} -E copy ${SWIG_CORE_UTILS_PYTHON_PY} ${PYTHON_SYSCONFIG_BUILD}/gnucash
    DEPENDS ${SWIG_CORE_UTILS_PYTHON_C})

  add_custom_target(sw-core-utils-build ALL
    COMMAND ${CMAKE_COMMAND} -E copy ${LIBDIR_BUILD}/gnucash/_sw_core_utils${CMAKE_SHARED_MODULE_SUFFIX} ${PYTHON_SYSCONFIG_BUILD}/gnucash
    DEPENDS sw_core_utils)

  ### App-utils
  set(PYEXEC_FILES sw_app_utils.py)
  add_library (sw_app_utils MODULE ${SWIG_APP_UTILS_PYTHON_C})

  target_link_libraries(sw_app_utils gnc-app-utils ${app_utils_ALL_LIBRARIES} ${Python3_LIBRARIES})
  set_target_properties(sw_app_utils PROPERTIES PREFIX "_")
  if (HAVE_STRINGOP_TRUNCATION)
    target_compile_options(sw_app_utils PRIVATE -Wno-error=stringop-truncation)
  endif()

  add_custom_target(sw-app-utils-py ALL
          COMMAND ${CMAKE_COMMAND} -E copy ${SWIG_APP_UTILS_PYTHON_PY} ${PYTHON_SYSCONFIG_BUILD}/gnucash
          DEPENDS ${SWIG_APP_UTILS_PYTHON_C})

  target_include_directories (sw_app_utils
          PUBLIC ${CMAKE_CURRENT_SOURCE_DIR}
          PRIVATE ${app_utils_ALL_INCLUDES} ${Python3_INCLUDE_DIRS}
          )
  target_compile_definitions (sw_app_utils PRIVATE -DG_LOG_DOMAIN=\"gnc.app-utils\")

  if (APPLE)
    set_target_properties (sw_app_utils PROPERTIES INSTALL_NAME_DIR "${CMAKE_INSTALL_FULL_LIBDIR}")
  endif()

  install(TARGETS sw_app_utils
          LIBRARY DESTINATION ${PYTHON_SYSCONFIG_OUTPUT}/gnucash
          ARCHIVE DESTINATION ${PYTHON_SYSCONFIG_OUTPUT}/gnucash
          )

  add_custom_target(sw-app-utils-build ALL
          COMMAND ${CMAKE_COMMAND} -E copy ${LIBDIR_BUILD}/gnucash/_sw_app_utils${CMAKE_SHARED_MODULE_SUFFIX} ${PYTHON_SYSCONFIG_BUILD}/gnucash
          DEPENDS sw_app_utils)

endif()

set(python_bindings_DATA
  app_utils.py
  deprecation.py
  function_class.py
  gnucash_business.py
  gnucash_core.i
  gnucash_core.py
  __init__.py
  sqlite3test.c
  time64.i)

set_local_dist(python_bindings_DIST_local CMakeLists.txt
  ${python_bindings_DATA})

set(python_bindings_DIST ${python_bindings_DIST_local}
  ${test_python_bindings_DIST} ${example_scripts_DIST} PARENT_SCOPE)
