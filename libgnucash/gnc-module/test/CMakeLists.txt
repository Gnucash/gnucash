
add_subdirectory(misc-mods)
add_subdirectory(mod-ordinary)
add_subdirectory(mod-withdep)

set(GNC_MODULE_TEST_INCLUDE_DIRS
  ${CMAKE_BINARY_DIR}/common # for config.h
  ${CMAKE_SOURCE_DIR}/libgnucash/gnc-module
  ${CMAKE_SOURCE_DIR}/common/test-core
)

set(GNC_MODULE_TEST_LIBS
  gnc-module
  test-core
  gncmod-agedver
  gncmod-futuremodsys
  gncmod-incompatdep
  gncmod-ordinary
  gncmod-withdep
  )

gnc_add_test(test-load-c
    test-load-c.c
    GNC_MODULE_TEST_INCLUDE_DIRS
    GNC_MODULE_TEST_LIBS
    "GNC_MODULE_PATH=${LIBDIR_BUILD}/gnucash/test"
)

gnc_add_test(test-modsysver
    test-modsysver.c
    GNC_MODULE_TEST_INCLUDE_DIRS
    GNC_MODULE_TEST_LIBS
    "GNC_MODULE_PATH=${LIBDIR_BUILD}/gnucash/test/future"
)
gnc_add_test(test-incompatdep
    test-incompatdep.c
    GNC_MODULE_TEST_INCLUDE_DIRS
    GNC_MODULE_TEST_LIBS
    "GNC_MODULE_PATH=${LIBDIR_BUILD}/gnucash/test"
)
gnc_add_test(test-agedver
    test-agedver.c
    GNC_MODULE_TEST_INCLUDE_DIRS
    GNC_MODULE_TEST_LIBS
    "GNC_MODULE_PATH=${LIBDIR_BUILD}/gnucash/test"
  )

set(_LIBDIR ${LIBDIR_BUILD})
if (WIN32)
  set(_LIBDIR ${CMAKE_BINARY_DIR}/bin)
endif()
gnc_add_test(test-dynload
    test-dynload.c
    GNC_MODULE_TEST_INCLUDE_DIRS
    GNC_MODULE_TEST_LIBS
    LIBDIR=${_LIBDIR}
)

set(test_gnc_module_SOURCE_DIST
  test-agedver.c
  test-dynload.c
  test-incompatdep.c
  test-load-c.c
  test-modsysver.c
)

set_local_dist(test_gnc_module_DIST_local CMakeLists.txt README ${test_gnc_module_SOURCE_DIST})

set(test_gnc_module_DIST ${test_gnc_module_DIST_local} ${misc_mods_DIST}
                         ${mod_ordinary_DIST} ${mod_withdep_DIST} PARENT_SCOPE)
