EXTRA_DIST = \
  .cvsignore \
  guile-strings.c \
  xgettext.scm

# Cause automake to generate correct rules
# for maintainers
noinst_HEADERS = guile-strings.c

BUILT_SOURCES = ${srcdir}/guile-strings.c

SCMFILES = $(shell find ${top_srcdir} -name '*\.scm' | egrep -v "/${top_buildir}/" )

# This needs to be generated in srcdir
${srcdir}/guile-strings.c: $(SCMFILES)
	cd ${srcdir} && rm -f guile-strings.c && \
	guile -s ./xgettext.scm $(SCMFILES)

