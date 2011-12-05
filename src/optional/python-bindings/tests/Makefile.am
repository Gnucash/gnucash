

TESTS_ENVIRONMENT = \
  PYTHONPATH=$(PYTHONPATH):$(top_builddir)/src/optional/python-bindings \
  PYTHONPATH=$$PYTHONPATH:$(top_builddir)/src/optional/python-bindings/.libs \
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
