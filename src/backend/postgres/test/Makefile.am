TESTS=test-load-module 

TESTS_ENVIRONMENT=\
  GNC_MODULE_PATH=${top_srcdir}/src/engine:${top_srcdir}/src/backend/postgres \
  GUILE_LOAD_PATH=${G_WRAP_MODULE_DIR}:..:${top_srcdir}/src/gnc-module \
  LTDL_LIBRARY_PATH=${top_srcdir}/src/gnc-module \
  LD_LIBRARY_PATH=${top_srcdir}/src/gnc-module:${top_srcdir}/src/gnc-module/.libs:${top_srcdir}/src/engine:${top_srcdir}/src/engine/.libs
