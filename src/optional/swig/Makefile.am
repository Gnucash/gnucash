SUBDIRS = examples


SWIG = swig

docdir = ${GNC_DOC_INSTALL_DIR}
doc_DATA = \
  gnucash.engine_wrap.doc

perlsharedir = ${GNC_SHAREDIR}/perl
perlshare_DATA = gnucash.pm

perllibdir = ${GNC_LIBDIR}/perl

perllib_LTLIBRARIES = libgncswig.la

libgncswig_la_SOURCES = \
  helperfuncs.c \
  gnucash-engine-perl5_wrap.c

libgncswig_la_LDFLAGS = -version-info 2:0:2

noinst_HEADERS = \
  helperfuncs.h

EXTRA_DIST = \
  .cvsignore \
  examples/scan-acct.pl \
  perl5_typemaps.i \
  gnucash-swig-defines.c

AM_CFLAGS = \
  -I@PERLINCL@/CORE \
  ${GNOME_INCLUDEDIR} \
  -I${top_srcdir}/src/engine

#
# technically, the engine is independent of the gnome libs, however,
# we need to specify the inlcude path for glibconfig.h, and so this is the way

libgncswig_la_LIBADD = \
  -L${prefix}/lib/gnucash \
  -lgncmod-engine \
  ${GLIB_LIBS} 


SWIG_INPUT_HDRS := \
  ${top_srcdir}/src/engine/Account.h \
  ${top_srcdir}/src/engine/DateUtils.h \
  ${top_srcdir}/src/engine/GNCId.h \
  ${top_srcdir}/src/engine/Group.h \
  ${top_srcdir}/src/engine/Query.h \
  ${top_srcdir}/src/engine/Scrub.h \
  ${top_srcdir}/src/engine/Transaction.h \
  ${top_srcdir}/src/engine/date.h \
  ${top_srcdir}/src/engine/gnc-book.h \
  ${top_srcdir}/src/engine/gnc-engine.h \
  ${top_srcdir}/src/engine/gnc-numeric.h \
  ${top_srcdir}/src/engine/gnc-session.h

## This is kinda ugly, but swig insists that the .so file be named the
## same as the .pm file, and automake won't tolerate libraries that
## aren't named lib*.so...
install-data-local: libgncswig.la
	cd ${DESTDIR}${perllibdir} && rm -f gnucash.so
	if [ -f ${DESTDIR}${perllibdir}/libgncswig.so ]; then \
		cd ${DESTDIR}${perllibdir} && ln -s libgncswig.so gnucash.so; \
	elif [ -f ${DESTDIR}${perllibdir}/libgncswig.so.0 ]; then \
		cd ${DESTDIR}${perllibdir} && \
                ln -s libgncswig.so.0 gnucash.so; \
	else \
		echo "Can't find libgncswig shared library!" \
		exit 1; \
	fi

uninstall-local:
	rm -f ${DESTDIR}${perllibdir}/gnucash.so

gnucash.engine.i: ${SWIG_INPUT_HDRS} perl5_typemaps.i
	@echo Making $@.
	@echo "%module gnucash" > $@
	@echo "%include perl5_typemaps.i" >> $@
	@echo "%{" >> $@
	@for file in ${SWIG_INPUT_HDRS}; \
        do \
          echo "#include <`basename $$file`>" >> $@; \
	done
	@echo "%}" >> $@
	@for file in ${SWIG_INPUT_HDRS}; \
        do \
          echo "%include \"`basename $$file`\"" >> $@; \
	done

CLEANFILES = gnucash.engine.i

gnucash.engine_wrap.doc gnucash-engine-perl5_wrap_int.c: gnucash.engine.i
	${SWIG} -perl5 -I.. -I${srcdir} -I${top_srcdir}/src/engine -o $@ gnucash.engine.i

CLEANFILES += gnucash.engine_wrap.doc gnucash-engine-perl5_wrap_int.c

gnucash-engine-perl5_wrap.c: gnucash-swig-defines.c \
                             gnucash-engine-perl5_wrap_int.c
	cat $^ > $@

CLEANFILES += gnucash-engine-perl5_wrap.c

CLEANFILES += gnucash.pm gnucash.so

# We have to do this because otherwise automake insists on putting
# these files into the dist tarfile.  If there's a a better way,
# by all means, let us know...
dist-hook:
	rm -f ${distdir}/gnucash-engine-perl5_wrap.c
