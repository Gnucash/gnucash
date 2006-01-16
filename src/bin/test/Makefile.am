
TESTS = test-version test-run-script

# Some of the things we do here may seem a little awkward, but we want
# to make sure we're testing in a setup as close to the install
# arrangement as possible.

GNC_TEST_DEPS := @GNC_TEST_SRFI_LOAD_CMD@ \
  --guile-load-dir ${top_srcdir}/src/engine \
  --guile-load-dir ${top_srcdir}/src/scm \
  --guile-load-dir ${top_srcdir}/src/app-utils \
  --guile-load-dir ${G_WRAP_MODULE_DIR} \
  --library-dir    ${G_WRAP_LIB_DIR}

TEST_PWD := $(shell pwd)
TESTS_ENVIRONMENT := \
  SRCDIR=${srcdir} \
  PATH="${TEST_PWD}/overrides:${PATH}" \
  $(shell ${top_srcdir}/src/gnc-test-env --no-exports ${GNC_TEST_DEPS})

noinst_DATA = overrides.stamp

overrides.stamp:
	rm -rf overrides
	mkdir overrides
	cd overrides && ln -s ../${top_srcdir}/src/bin/overrides/gnucash-run-script .
	cd overrides && ln -s ../../overrides/gnucash-make-guids .
	cd overrides && ln -s ../../overrides/guile .
	cd overrides && ln -s ../../overrides/gnucash-build-env gnucash-env
	touch overrides.stamp
CLEANFILES = overrides.stamp

clean-local:
	rm -rf overrides
EXTRA_DIST = test-version test-run-script
