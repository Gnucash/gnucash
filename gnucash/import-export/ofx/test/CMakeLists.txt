

if(WITH_OFX)
  set(OFX_TEST_INCLUDE_DIRS ${LIBOFX_INCLUDE_DIRS})
  set(OFX_TEST_LIBS)

  gnc_add_test(test-link-ofx test-link.c OFX_TEST_INCLUDE_DIRS OFX_TEST_LIBS)
endif()

set_dist_list(test_ofx_DIST CMakeLists.txt test-link.c)