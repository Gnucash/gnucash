ADD_SUBDIRECTORY(gnumeric)

SET (BUILD_CONFIG_SCM ${CMAKE_CURRENT_BINARY_DIR}/build-config.scm)

SET (scm_SCHEME_4
    substring-search.scm
    xml-generator.scm
)

# We need to replace a couple of variables in build-config.scm. Normally, I 
# would use CONFIGURE_FILE to do this. But this scheme files is using a different
# replacement marking scheme ("@-", "-@") than the usual one with just "@" 
# that CONFIGURE_FILE expects.

# FIXME: ${BUILD_CONFIG_SCM} needs a rule to cause it to be generated, I think.

IF (HAVE_GUILE1)
  GNC_CONFIGURE(build-config.scm.in build-config.scm)
ELSE()
  GNC_CONFIGURE2(build-config-scm build-config.scm.in build-config.scm)
ENDIF()

# CONFIGURE_FILE(build-config.scm.in ${BUILD_CONFIG_SCM})

SET(GUILE_MODULES      "")
SET(GUILE_LOAD_DIRS    src/core-utils src/gnc-module)
SET(GUILE_LIBRARY_DIRS src/core-utils src/gnc-module)
SET(GUILE_DEPENDS      scm-core-utils scm-gnc-module)

SET(scm_SCHEME_0
  fin.scm
  string.scm
  ${BUILD_CONFIG_SCM}
  substring-search.scm
  xml-generator.scm
)

GNC_ADD_SCHEME_TARGETS(scm-scm-0 "${scm_SCHEME_0}" "" "" "" "" "" FALSE)

GNC_ADD_SCHEME_TARGETS(scm-scm-1 "printf.scm" gnucash "" "" "" "" FALSE)

GNC_ADD_SCHEME_TARGETS(scm-scm-2
  main.scm
  gnucash
  "${GUILE_MODULES}"
  "${GUILE_LOAD_DIRS}"
  "${GUILE_LIBRARY_DIRS}"
  "scm-scm-1;${GUILE_DEPENDS}"  # requires printf.scm from scm-scm-1 and modules from GUILE_DEPENDS
  FALSE
)

# depends on main.scm
GNC_ADD_SCHEME_TARGETS(scm-scm-3
  price-quotes.scm
  gnucash
  "${GUILE_MODULES}"
  "${GUILE_LOAD_DIRS}"
  "${GUILE_LIBRARY_DIRS}"
  "scm-scm-2;scm-scm-0"  # depends on build_config.scm
  FALSE)

ADD_CUSTOM_TARGET(scm-scm ALL DEPENDS scm-scm-3 scm-scm-2 scm-scm-1 scm-scm-0 scm-gnumeric)

INSTALL(FILES config DESTINATION etc/gnucash)

SET_LOCAL_DIST(scm_DIST_local config CMakeLists.txt Makefile.am fin.scm string.scm build-config.scm.in substring-search.scm
                              xml-generator.scm main.scm price-quotes.scm printf.scm ${scm_SCHEME_4})
SET(scm_DIST ${scm_DIST_local} ${scm_gnumeric_DIST} PARENT_SCOPE)


