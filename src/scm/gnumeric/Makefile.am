
gncscmdir = ${GNC_SCM_INSTALL_DIR}/gnumeric

gncscm_DATA = \
  gnumeric-utilities.scm \
  table-utils.scm

EXTRA_DIST = \
  .cvsignore \
  ${gncscm_DATA}

guile-strings.c: ${gncscm_DATA}
	rm -f guile-strings.c
	guile -s ../xgettext.scm ${gncscm_DATA}
CLEANFILES = guile-strings.c

all-local: guile-strings.c
