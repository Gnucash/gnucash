EXTRA_DIST = \
  .cvsignore \
  guile-strings.c \
  xgettext.scm

guile-strings.c:
	rm -f guile-strings.c
	guile -s ./xgettext.scm `find ../src -name '*.scm'`
CLEANFILES += guile-strings.c

all-local:
	rm -f guile-strings.c

