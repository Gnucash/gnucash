add_subdirectory(example_scripts)
add_subdirectory(tests)

set(PYEXEC_FILES  __init__.py function_class.py gnucash_business.py gnucash_core.py app_utils.py)

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

gnc_add_swig_python_command (swig-gnucash-core
    SWIG_GNUCASH_CORE_C SWIG_GNUCASH_CORE_PY
    gnucash_core.c gnucash_core_c.py
    ${SWIG_FILES}
    ${CMAKE_SOURCE_DIR}/common/base-typemaps.i
    ${CMAKE_SOURCE_DIR}/libgnucash/engine/engine-common.i
    ${GNUCASH_CORE_C_INCLUDES}
)

if(WITH_PYTHON)

  set(gnucash_core_c_INCLUDE_DIRS
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

  add_library(gnucash_core_c MODULE ${SWIG_GNUCASH_CORE_C})
  target_include_directories(gnucash_core_c PRIVATE ${gnucash_core_c_INCLUDE_DIRS})

  target_link_libraries(gnucash_core_c gncmod-app-utils gncmod-engine gnc-module ${GLIB_LIBS} ${PYTHON_LIBRARIES})
  set_target_properties(gnucash_core_c PROPERTIES PREFIX "_")
  target_compile_options(gnucash_core_c PRIVATE -Wno-implicit -Wno-missing-prototypes -Wno-declaration-after-statement -Wno-missing-declarations)
  if (HAVE_STRINGOP_TRUNCATION)
    target_compile_options(gnucash_core_c PRIVATE -Wno-error=stringop-truncation)
  endif()

  add_executable(sqlite3test EXCLUDE_FROM_ALL sqlite3test.c ${SWIG_GNUCASH_CORE_C})
  target_link_libraries(sqlite3test gncmod-app-utils gncmod-engine gnc-module ${GLIB_LIBS} ${PYTHON_LIBRARIES})
  target_include_directories(sqlite3test PRIVATE ${gnucash_core_c_INCLUDE_DIRS})
  target_compile_options(sqlite3test PRIVATE -Wno-implicit -Wno-missing-prototypes -Wno-declaration-after-statement -Wno-missing-declarations)
  if (HAVE_STRINGOP_TRUNCATION)
    target_compile_options(sqlite3test PRIVATE -Wno-error=stringop-truncation)
  endif()

  add_test(NAME sqlite3test COMMAND sqlite3test)
  add_dependencies(check sqlite3test)

  install(TARGETS gnucash_core_c
    LIBRARY DESTINATION ${PYTHON_SYSCONFIG_OUTPUT}/gnucash
    ARCHIVE DESTINATION ${PYTHON_SYSCONFIG_OUTPUT}/gnucash
  )
  install(FILES ${PYEXEC_FILES} ${SWIG_GNUCASH_CORE_PY}
    DESTINATION ${PYTHON_SYSCONFIG_OUTPUT}/gnucash
  )

  file(COPY ${PYEXEC_FILES} DESTINATION ${PYTHON_SYSCONFIG_BUILD}/gnucash)

  add_custom_target(gnucash-core-c-py ALL
    COMMAND ${CMAKE_COMMAND} -E copy ${SWIG_GNUCASH_CORE_PY} ${PYTHON_SYSCONFIG_BUILD}/gnucash
    DEPENDS ${SWIG_GNUCASH_CORE_C})

  add_custom_target(gnucash-core-c-build ALL
    COMMAND ${CMAKE_COMMAND} -E copy ${LIBDIR_BUILD}/gnucash/_gnucash_core_c${CMAKE_SHARED_MODULE_SUFFIX} ${PYTHON_SYSCONFIG_BUILD}/gnucash
    DEPENDS gnucash_core_c)

endif()

set(python_bindings_DATA ${PYEXEC_FILES}
  gnucash_core.i
  sqlite3test.c
  time64.i)

set_local_dist(python_bindings_DIST_local CMakeLists.txt
  ${python_bindings_DATA})

set(python_bindings_DIST ${python_bindings_DIST_local}
  ${test_python_bindings_DIST} ${example_scripts_DIST} PARENT_SCOPE)
