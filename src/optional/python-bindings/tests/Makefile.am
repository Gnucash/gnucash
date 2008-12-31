TESTS_ENVIRONMENT = PYTHONPATH=$(PYTHONPATH):$(pythondir) $(top_builddir)/src/bin/gnucash-env $(PYTHON)
TESTS = runTests.py

clean-local:
	rm -f translog.*

EXTRA_DIST = \
  runTests.py \
  test_account.py \
  test_book.py \
  test_split.py \
  test_transaction.py
