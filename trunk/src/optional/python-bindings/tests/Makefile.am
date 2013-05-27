
TESTS_ENVIRONMENT = \
  GNC_BUILDDIR=`\cd ${top_builddir} && pwd` \
  PYTHONPATH=$(PYTHONPATH):$(top_builddir)/src/optional/python-bindings \
  PYTHONPATH=$$PYTHONPATH:$(top_builddir)/src/optional/python-bindings/.libs \
  PYTHONPATH=$$PYTHONPATH:$(top_builddir)/src/test-core/ \
  PYTHONPATH=$$PYTHONPATH:$(top_srcdir)/src/test-core/ \
  PYTHONPATH=$$PYTHONPATH:$(top_builddir)/src/test-core/.libs \
  $(top_builddir)/src/bin/overrides/gnucash-build-env $(PYTHON)
TESTS = runTests.py

clean-local:
	rm -f translog.*

EXTRA_DIST = \
  runTests.py \
  test_account.py \
  test_book.py \
  test_split.py \
  test_transaction.py \
  test_business.py
