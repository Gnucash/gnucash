
add_subdirectory(mod-foo)
add_subdirectory(mod-bar)
add_subdirectory(mod-baz)
add_subdirectory(misc-mods)

set(GNC_MODULE_TEST_INCLUDE_DIRS
  ${CMAKE_BINARY_DIR}/common # for config.h
  ${CMAKE_SOURCE_DIR}/libgnucash/gnc-module
  ${CMAKE_SOURCE_DIR}/common/test-core
  ${GLIB2_INCLUDE_DIRS}
  ${GUILE_INCLUDE_DIRS}
)

set(GNC_MODULE_TEST_LIBS
  gnc-module
  test-core
  gncmod-foo
  gncmod-bar
  gncmod-baz
  gncmod-agedver
  gncmod-futuremodsys
  gncmod-incompatdep
  )

gnc_add_test_with_guile(test-load-c test-load-c.c GNC_MODULE_TEST_INCLUDE_DIRS GNC_MODULE_TEST_LIBS "GNC_MODULE_PATH=${LIBDIR_BUILD}/gnucash/test")

gnc_add_test_with_guile(test-modsysver test-modsysver.c
  GNC_MODULE_TEST_INCLUDE_DIRS GNC_MODULE_TEST_LIBS
)
gnc_add_test_with_guile(test-incompatdep test-incompatdep.c
  GNC_MODULE_TEST_INCLUDE_DIRS GNC_MODULE_TEST_LIBS
)
gnc_add_test_with_guile(test-agedver test-agedver.c
  GNC_MODULE_TEST_INCLUDE_DIRS GNC_MODULE_TEST_LIBS
  "GNC_MODULE_PATH=${LIBDIR_BUILD}/gnucash/test"
  )

set(_LIBDIR ${LIBDIR_BUILD})
if (WIN32)
  set(_LIBDIR ${CMAKE_BINARY_DIR}/bin)
endif()
gnc_add_test(test-dynload test-dynload.c
  GNC_MODULE_TEST_INCLUDE_DIRS GNC_MODULE_TEST_LIBS
  LIBDIR=${_LIBDIR}
)

set(test_gnc_module_SCHEME
  test-gnc-module-scm-init.scm
  test-gnc-module-load-scm.scm
  test-gnc-module-swigged-c.scm
  test-gnc-module-load-deps.scm
  test-gnc-module-scm-dynload.scm
  test-gnc-module-scm-module.scm
  test-gnc-module-scm-multi.scm
  )

set(CORE_GUILE_DEPENDS
  gnc-module
  scm-test-core
  scm-mod-foo
  gncmod-bar
  scm-mod-baz
  gncmod-agedver
  gncmod-incompatdep
  gncmod-futuremodsys
  )

gnc_add_scheme_targets(test-gnc-modules-scm
  "${test_gnc_module_SCHEME}"
  "gnucash/gnc-modules/test"
  "${CORE_GUILE_DEPENDS}"
  TRUE
  )

gnc_add_scheme_tests("${test_gnc_module_SCHEME}")
if(NOT WIN32)
  # This little dance is needed because gnc_module_init will assert if
  # it finds libgncmod-futuremod.so outside of a test that expects it.
  get_guile_env()
  set(_GNC_MODULE_PATH "${LIBDIR_BUILD}:${LIBDIR_BUILD}/gnucash:${LIBDIR_BUILD}/gnucash/test")
  foreach(test_file ${test_gnc_module_SCHEME})
    get_filename_component(basename ${test_file} NAME_WE)
    set_tests_properties(${basename} PROPERTIES ENVIRONMENT "${GUILE_ENV};GNC_MODULE_PATH=${_GNC_MODULE_PATH}")
  endforeach()
endif()

set(test_gnc_module_SOURCE_DIST
  test-agedver.c
  test-dynload.c
  test-incompatdep.c
  test-load-c.c
  test-modsysver.c
)

set(test_gnc_module_EXTRA_DIST
  README
  ${test_gnc_module_SCHEME}
)

set_local_dist(test_gnc_module_DIST_local CMakeLists.txt README ${test_gnc_module_SOURCE_DIST} ${test_gnc_module_EXTRA_DIST})

set(test_gnc_module_DIST ${test_gnc_module_DIST_local} ${misc_mods_DIST}
                         ${mod_bar_DIST} ${mod_baz_DIST} ${mod_foo_DIST} PARENT_SCOPE)
