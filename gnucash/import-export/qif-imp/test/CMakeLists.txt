

SET(QIF_IMP_TEST_INCLUDE_DIRS)
SET(QIF_IMP_TEST_LIBS)

GNC_ADD_TEST(test-link-qif-imp test-link.c QIF_IMP_TEST_INCLUDE_DIRS QIF_IMP_TEST_LIBS)

SET_DIST_LIST(test_qif_import_DIST CMakeLists.txt test-link.c)