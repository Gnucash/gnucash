EXTRA_DIST = \
  .cvsignore \
  guile-strings.c \
  xgettext.scm

guile-strings.c: $(find ${top_srcdir}/src -name '*.scm')
	rm -f guile-strings.c
	guile -s ./xgettext.scm `find "${top_srcdir}"/src -name '*.scm'`
CLEANFILES = guile-strings.c

all-local: guile-strings.c
