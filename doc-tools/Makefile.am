
noinst_PROGRAMS = dbadd

LDADD = \
  ${GNOMEUI_LIBS} \
  ${GNOME_LIBDIR} \
  ${GNOME_PRINT_LIBS} \
  ${G_WRAP_LINK_ARGS} \
  ${GUILE_LIBS} \
  ${GTKHTML_LIBS} \
  ${GHTTP_LIBS} \
  ${GUPPI_LIBS} \
  ${DB_LIBS} \
  ${INTLLIBS}

dbadd_SOURCES = \
  dbadd.c

EXTRA_DIST = \
  .cvsignore \
  build-help-index.in

noinst_DATA = build-help-index

## We borrow guile's convention and use @-...-@ as the substitution
## brackets here, instead of the usual @...@.  This prevents autoconf
## from substituting the values directly into the left-hand sides of
## the sed substitutions.
build-help-index: build-help-index.in
	rm -f $@.tmp
	sed < $@.in > $@.tmp \
            -e 's:@-PERL-@:${PERL}:g'
	chmod +x $@.tmp
	mv $@.tmp $@

CLEANFILES += build-help-index
