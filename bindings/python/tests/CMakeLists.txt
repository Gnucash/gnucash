
IF (WITH_PYTHON)
  ADD_TEST(python-bindings ${PYTHON_EXECUTABLE} ${CMAKE_CURRENT_SOURCE_DIR}/runTests.py.in)
  SET_PROPERTY(TEST python-bindings PROPERTY ENVIRONMENT
    GNC_BUILDDIR=${CMAKE_BINARY_DIR}
    PYTHONPATH=${PYTHON_SYSCONFIG_BUILD}:${LIBDIR_BUILD}/gnucash:${CMAKE_BINARY_DIR}/common/test-core
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
