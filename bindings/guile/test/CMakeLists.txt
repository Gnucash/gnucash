# C(++) tests requiring a proper guile environment set up

set(ENGINE_TEST_INCLUDE_DIRS
    ${CMAKE_SOURCE_DIR}/libgnucash/backend/xml
    ${CMAKE_SOURCE_DIR}/libgnucash/engine
    ${CMAKE_SOURCE_DIR}/libgnucash/engine/test-core
    ${CMAKE_BINARY_DIR}/common # for config.h
    ${CMAKE_SOURCE_DIR}/common/test-core  # for unittest-support.h
    ${CMAKE_SOURCE_DIR}/libgnucash/app-utils
    ${CMAKE_SOURCE_DIR}/bindings/guile # for gnc-engine-guile.h
    ${GUILE_INCLUDE_DIRS}
)

set(ENGINE_TEST_LIBS
    gnc-engine
    gnc-app-utils
    gnc-test-engine
    test-core
    gnucash-guile
    ${GIO_LDFLAGS}
    ${GUILE_LDFLAGS}
    ${LIBXML2_LDFLAGS}
    -lm)

gnc_add_test_with_guile(test-scm-query test-scm-query.cpp ENGINE_TEST_INCLUDE_DIRS ENGINE_TEST_LIBS)


# Scheme tests


set(bindings_test_SCHEME
    test-create-account.scm
)

#list(APPEND engine_test_SCHEME  test-scm-query-import.scm) Fails

set(GUILE_DEPENDS
    scm-app-utils
    scm-core-utils
    scm-engine)

gnc_add_scheme_test_targets(scm-test-engine-extras
    SOURCES "test-engine-extras.scm"
    OUTPUT_DIR "tests"
    DEPENDS "${GUILE_DEPENDS}")

gnc_add_scheme_test_targets(scm-test-engine
    SOURCES "${bindings_test_SCHEME}"
    OUTPUT_DIR "tests"
    DEPENDS "${GUILE_DEPENDS};scm-test-engine-extras")

gnc_add_scheme_tests("${bindings_test_SCHEME}")

add_dependencies(check scm-test-engine)
gnc_add_scheme_tests("${engine_test_SCHEME}")

set (scm_tests_with_srfi64_SOURCES
  test-core-utils.scm
  test-business-core.scm
  test-scm-engine.scm
  test-scm-kvpvalue.scm
  test-options.scm
  )

if (HAVE_SRFI64)
    gnc_add_scheme_test_targets (scm-srfi64-extras
        SOURCES "srfi64-extras.scm"
        OUTPUT_DIR "tests"
        DEPENDS "${GUILE_DEPENDS};scm-test-engine-extras;scm-srfi64-extras")

    gnc_add_scheme_test_targets (scm-test-with-srfi64
        SOURCES "${scm_tests_with_srfi64_SOURCES}"
        OUTPUT_DIR "tests"
        DEPENDS "${GUILE_DEPENDS};scm-test-engine-extras;scm-srfi64-extras;gnc-engine")

    gnc_add_scheme_tests("${scm_tests_with_srfi64_SOURCES}")
    add_dependencies(check scm-srfi64-extras)
endif()


set(test_scm_SCHEME
    test-scm-utilities.scm
    test-module-deprecation.scm
)

set(GUILE_DEPENDS
    scm-app-utils
    scm-engine
    scm-srfi64-extras
    scm-deprecated-gnucash-deprecated-module
    scm-deprecated-gnucash-superseded-module
)

if (HAVE_SRFI64)
  gnc_add_scheme_test_targets(scm-test-scm
    SOURCES "${test_scm_SCHEME}"
    OUTPUT_DIR "tests"
    DEPENDS "${GUILE_DEPENDS}")

  gnc_add_scheme_tests("${test_scm_SCHEME}")

endif()

set(test_engine_SCHEME_DIST
    srfi64-extras.scm
    ${bindings_test_SCHEME}
    ${scm_tests_with_srfi64_SOURCES}
    test-engine-extras.scm
    test-scm-query-import.scm
    test-business-core.scm
)

set(test_app_utils_scheme_SOURCES
        test-c-interface.scm
        test-load-app-utils-module.scm
        )

set (test_app_utils_scheme_SRFI64_SOURCES
        test-date-utilities.scm
        )

gnc_add_scheme_test_targets(scm-test-load-app-utils-module
        SOURCES "test-load-app-utils-module.scm"
        OUTPUT_DIR "tests"
        DEPENDS "${GUILE_DEPENDS}")

gnc_add_scheme_test_targets(scm-test-c-interface
        SOURCES "test-c-interface.scm"
        OUTPUT_DIR "tests"
        DEPENDS "${GUILE_DEPENDS}")

gnc_add_scheme_tests("${test_app_utils_scheme_SOURCES}")

if (HAVE_SRFI64)
    gnc_add_scheme_test_targets(scm-test-app-utils-srfi64
            SOURCES "${test_app_utils_scheme_SRFI64_SOURCES}"
            OUTPUT_DIR "tests"
            DEPENDS "${GUILE_DEPENDS};scm-srfi64-extras")

    gnc_add_scheme_test_targets(scm-test-gnc-optiondb
            SOURCES "test-gnc-optiondb.scm" "test-gnc-option-scheme-output.scm"
            OUTPUT_DIR "tests"
            DEPENDS "swig-apputils-guile-cpp;scm-srfi64-extras")
    gnc_add_scheme_tests("test-gnc-optiondb.scm")
    gnc_add_scheme_tests("test-gnc-option-scheme-output.scm")
    gnc_add_scheme_tests("${test_app_utils_scheme_SRFI64_SOURCES}")
endif()

gnc_add_test_with_guile(test-print-queries test-print-queries.cpp ENGINE_TEST_INCLUDE_DIRS ENGINE_TEST_LIBS)
gnc_add_test_with_guile(test-scm-query-string test-scm-query-string.cpp
        ENGINE_TEST_INCLUDE_DIRS ENGINE_TEST_LIBS
        )

set_local_dist(test_guile_DIST_local
    CMakeLists.txt
    test-print-queries.cpp
    test-scm-query.cpp
    test-scm-query-string.cpp
    test-options.scm
    test-gnc-optiondb.scm
    test-gnc-option-scheme-output.scm
    ${test_engine_SCHEME_DIST}
    ${test_scm_SCHEME}
    ${test_app_utils_scheme_SOURCES}
    ${test_app_utils_scheme_SRFI64_SOURCES}
)
set(test_guile_DIST
    ${test_guile_DIST_local}
    PARENT_SCOPE
)

# Define two imaginary deprecated guile modules to test the compat file generation code
# See commit bbcffa3ec for old code using this function.
gnc_add_scheme_deprecated_module (OLD_MODULE "gnucash deprecated-module")
gnc_add_scheme_deprecated_module (
    OLD_MODULE "gnucash superseded-module"
    NEW_MODULE "gnucash utilities"
    DEPENDS "scm-engine")
