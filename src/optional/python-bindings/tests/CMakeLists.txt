
SET(test_python_bindings_DATA
        runTests.py.in
        test_account.py
        test_book.py
        test_business.py
        test_split.py
        test_transaction.py)

SET_DIST_LIST(test_python_bindings_DIST CMakeLists.txt Makefile.am ${test_python_bindings_DATA})