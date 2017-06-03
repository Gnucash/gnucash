GNC_TEST_DEPS = --gnc-module-dir ${top_builddir}/libgnucash/engine \
  --gnc-module-dir ${top_builddir}/libgnucash/app-utils \
  --guile-load-dir ${top_builddir}/libgnucash/core-utils \
  --guile-load-dir ${top_builddir}/libgnucash/gnc-module \
  --guile-load-dir ${top_builddir}/libgnucash/engine \
  --guile-load-dir ${top_builddir}/libgnucash/scm \
  --guile-load-dir ${top_builddir}/libgnucash/app-utils \
  --library-dir    ${top_builddir}/libgnucash/core-utils \
  --library-dir    ${top_builddir}/libgnucash/gnc-module \
  --library-dir    ${top_builddir}/libgnucash/engine \
  --library-dir    ${top_builddir}/libgnucash/backend/xml \
  --library-dir    ${top_builddir}/libgnucash/backend/sql \
  --library-dir    ${top_builddir}/libgnucash/app-utils \
  --library-dir    ${top_builddir}/common/test-core
  
TESTS_ENVIRONMENT = \
  GNC_BUILDDIR="${abs_top_builddir}" \
  PYTHON=${PYTHON} \
  PYTHONPATH=$$PYTHONPATH:$(top_builddir)/bindings/python \
  PYTHONPATH=$$PYTHONPATH:$(top_builddir)/bindings/python/.libs \
  PYTHONPATH=$$PYTHONPATH:$(top_srcdir)/bindings/python/tests \
  PYTHONPATH=$$PYTHONPATH:$(top_builddir)/common/test-core/ \
  PYTHONPATH=$$PYTHONPATH:$(top_srcdir)/common/test-core/ \
  PYTHONPATH=$$PYTHONPATH:$(top_builddir)/common/test-core/.libs \
  $(shell ${abs_top_srcdir}/common/gnc-test-env.pl --noexports ${GNC_TEST_DEPS})
  
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
  test_commodity.py \
  test_numeric.py \
  test_transaction.py \
  test_business.py \
  CMakeLists.txt
