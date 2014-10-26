GNC_TEST_DEPS = --gnc-module-dir ${top_builddir}/src/engine \
  --gnc-module-dir ${top_builddir}/src/app-utils \
  --guile-load-dir ${top_builddir}/src/core-utils \
  --guile-load-dir ${top_builddir}/src/gnc-module \
  --guile-load-dir ${top_builddir}/src/engine \
  --guile-load-dir ${top_builddir}/src/scm \
  --guile-load-dir ${top_builddir}/src/app-utils \
  --library-dir    ${top_builddir}/src/libqof/qof \
  --library-dir    ${top_builddir}/src/core-utils \
  --library-dir    ${top_builddir}/src/gnc-module \
  --library-dir    ${top_builddir}/src/engine \
  --library-dir    ${top_builddir}/src/backend/xml \
  --library-dir    ${top_builddir}/src/backend/sql \
  --library-dir    ${top_builddir}/src/app-utils \
  --library-dir    ${top_builddir}/src/test-core
  
TESTS_ENVIRONMENT = \
  GNC_BUILDDIR="${abs_top_builddir}" \
  PYTHON=${PYTHON} \
  PYTHONPATH=$$PYTHONPATH:$(top_builddir)/src/optional/python-bindings \
  PYTHONPATH=$$PYTHONPATH:$(top_builddir)/src/optional/python-bindings/.libs \
  PYTHONPATH=$$PYTHONPATH:$(top_srcdir)/src/optional/python-bindings/tests \
  PYTHONPATH=$$PYTHONPATH:$(top_builddir)/src/test-core/ \
  PYTHONPATH=$$PYTHONPATH:$(top_srcdir)/src/test-core/ \
  PYTHONPATH=$$PYTHONPATH:$(top_builddir)/src/test-core/.libs \
  $(shell $(top_builddir)/src/gnc-test-env --no-exports ${GNC_TEST_DEPS})
  
## We borrow guile's convention and use @-...-@ as the substitution
## brackets below, instead of the usual @...@.  This prevents autoconf
## from substituting the values directly into the left-hand sides of
## the sed substitutions.  *sigh*

runTests.py: runTests.py.in ${top_builddir}/config.status Makefile
	rm -f $@.tmp
	sed < $< > $@.tmp \
	    -e 's#@-PYTHON-@#${PYTHON}#'
	mv $@.tmp $@
	chmod u+x $@

CLEANFILES = runTests.py

TESTS = runTests.py

clean-local:
	rm -f translog.*

EXTRA_DIST = \
  runTests.py.in \
  test_account.py \
  test_book.py \
  test_split.py \
  test_transaction.py \
  test_business.py
