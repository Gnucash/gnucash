
IF (WITH_PYTHON)
  find_path(test_core_dir "unittest_support.py" PATHS ${CMAKE_BINARY_DIR}/common/test-core ${CMAKE_SOURCE_DIR}/common/test-core)
  if (${test_core_dir} STREQUAL "test_core_dir-NOTFOUND")
    # Because it hasn't been built yet
    set(test_core_dir ${CMAKE_BINARY_DIR}/common/test-core)
  endif()
  add_custom_target(test-python-bindings ALL DEPENDS unittest_support gnucash-core-c-build gnucash-core-c-py)
  add_dependencies(check test-python-bindings)
  ADD_TEST(python-bindings ${PYTHON_EXECUTABLE} ${CMAKE_CURRENT_SOURCE_DIR}/runTests.py.in)
  SET_PROPERTY(TEST python-bindings PROPERTY ENVIRONMENT
    GNC_BUILDDIR=${CMAKE_BINARY_DIR}
    PYTHONPATH=${PYTHON_SYSCONFIG_BUILD}:${LIBDIR_BUILD}/gnucash:${test_core_dir}
  )
ENDIF()

SET(test_python_bindings_DATA
        runTests.py.in
        test_account.py
        test_book.py
        test_business.py
        test_commodity.py
        test_numeric.py
        test_split.py
        test_transaction.py)

SET_DIST_LIST(test_python_bindings_DIST CMakeLists.txt ${test_python_bindings_DATA})
