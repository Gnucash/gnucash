
TESTS = test-version test-run-script test-guile-env-override

# Some of the things we do here may seem a little awkward, but we want
# to make sure we're testing in a setup as close to the install
# arrangement as possible.

TEST_PWD := $(shell pwd)
TESTS_ENVIRONMENT := \
  SRCDIR=${srcdir} \
  PATH="${TEST_PWD}/overrides:${PATH}"

noinst_DATA = overrides.stamp

overrides.stamp:
	rm -rf overrides
	mkdir overrides
	cd overrides && ln -s ../../overrides/gnucash .
	cd overrides && ln -s ../../overrides/gnucash-make-guids .
	cd overrides && ln -s ../../overrides/gnucash-run-script .
	cd overrides && ln -s ../../overrides/guile .
	cd overrides && ln -s ../../overrides/gnucash-build-env gnucash-env
	touch overrides.stamp
CLEANFILES = overrides.stamp

clean-local:
	rm -rf overrides
EXTRA_DIST = \
  fake-guile test-version test-run-script test-guile-env-override
