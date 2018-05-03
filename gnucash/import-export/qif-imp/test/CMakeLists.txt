

set(QIF_IMP_TEST_INCLUDE_DIRS)
set(QIF_IMP_TEST_LIBS)

gnc_add_test(test-link-qif-imp test-link.c QIF_IMP_TEST_INCLUDE_DIRS QIF_IMP_TEST_LIBS)

set_dist_list(test_qif_import_DIST CMakeLists.txt test-link.c)