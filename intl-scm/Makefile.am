abs_top_srcdir=@abs_top_srcdir@

EXTRA_DIST = \
  guile-strings.c \
  xgettext.scm

# Cause automake to generate correct rules
# for maintainers
noinst_HEADERS = guile-strings.c

BUILT_SOURCES = guile-strings.c

SCMFILES = $(shell find ${abs_top_srcdir}/src -name test -prune -o -name '*.scm' -print | sort)

# This needs to be generated in srcdir
guile-strings.c: $(SCMFILES)
	rm -f guile-strings.c
	cd ${srcdir} && rm -f guile-strings.c \
	&& ${GUILE} -s ./xgettext.scm ${abs_top_srcdir} $(SCMFILES)

