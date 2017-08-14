# These directories do not contain any gtk dependencies
# Note the unusual ordering of some test directories. This is
# because test-core depends on engine and the test directories
# in turn depend on test-core. This would not have been necessary
# if we had only one big Makefile instead of a recursive build.
# As we intend to move to cmake it's not worth fixing this any more.
SUBDIRS = \
  doc \
  core-utils \
  gnc-module \
  engine \
  ../common/test-core \
  core-utils/test \
  gnc-module/test \
  engine/test \
  backend \
  scm \
  quotes \
  app-utils \
  tax
