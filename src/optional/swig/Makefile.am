
perlsharedir = ${GNC_SHAREDIR}/perl
perlshare_DATA = gnucash.pm

perllibdir = ${GNC_LIBDIR}/perl

perllib_LTLIBRARIES = libgncswig.la

libgncswig_la_SOURCES = \
  helperfuncs.c \
  gnucash-engine-perl5_wrap.c

libgncswig_la_LDFLAGS = -version-info 1:1:1

CFLAGS += ${GNOME_CFLAGS} -Dbool=char

INCLUDES = \
  -I@PERLINCL@/CORE \
  -I${top_srcdir}/src/engine

#  -I.. \
#  -I../engine \
#  -I../guile

LDADD = \
  ${top_srcdir}/src/engine/libgncengine.a

SWIG_INPUT_HDRS := \
  ${top_srcdir}/src/engine/AccInfo.h \
  ${top_srcdir}/src/engine/Account.h \
  ${top_srcdir}/src/engine/DateUtils.h \
  ${top_srcdir}/src/engine/FileIO.h \
  ${top_srcdir}/src/engine/GNCId.h \
  ${top_srcdir}/src/engine/Group.h \
  ${top_srcdir}/src/engine/LedgerUtils.h \
  ${top_srcdir}/src/engine/Query.h \
  ${top_srcdir}/src/engine/Queue.h \
  ${top_srcdir}/src/engine/Scrub.h \
  ${top_srcdir}/src/engine/Session.h \
  ${top_srcdir}/src/engine/TransLog.h \
  ${top_srcdir}/src/engine/Transaction.h \
  ${top_srcdir}/src/engine/date.h

## This is kinda ugly, but swig insists that the .so file be named the
## same as the .pm file, and automake won't tolerate libraries that
## aren't named lib*.so...
install-data-local: libgncswig.la
	cd ${perllibdir} && rm -f gnucash.so
	cd ${perllibdir} && ln -s libgncswig.so gnucash.so

uninstall-local:
	rm -f ${perllibdir}/gnucash.so

gnucash.engine.i: ${SWIG_INPUT_HDRS}  perl5_swig_annotations.i
	@echo Making $@.
	@echo "%module gnucash" > $@
	@echo "%include perl5_swig_annotations.i" >> $@
	@echo "%{" >> $@
	@for file in ${SWIG_INPUT_HDRS}; \
        do \
          echo "#include <`basename $$file`>" >> $@; \
	done
	@echo "%}" >> $@
	@for file in ${SWIG_INPUT_HDRS}; \
        do \
          echo "%include `basename $$file`" >> $@; \
	done

CLEANFILES += gnucash.engine.i gnucash.engine_wrap.doc

gnucash-engine-perl5_wrap.c: gnucash.engine.i
	${SWIG} -stat -perl5 -I.. -I${top_srcdir}/src/engine -o $@ $< 

CLEANFILES += gnucash-engine-perl5_wrap.c
CLEANFILES += gnucash.pm gnucash.so
