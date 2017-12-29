SUBDIRS = examples

doc_DATA = \
  README.francais \
  README.german \
  README-ca.win32-bin.txt \
  README-de.win32-bin.txt \
  README-fr.win32-bin.txt \
  README-it.win32-bin.txt \
  README-lv.win32-bin.txt \
  README-nl.win32-bin.txt \
  README-zh_CN.win32-bin.txt \
  README-zh_TW.win32-bin.txt \
  README.win32-bin.txt \
  guile-hackers.txt \
  projects.html 

dist_man_MANS = \
  gnucash.1 \
  gnc-fq-dump.1 \
  gnc-fq-helper.1 

tipsdir = $(GNC_SHAREDIR)
tips_DATA = tip_of_the_day.list

EXTRA_DIST = \
  ${doc_DATA} \
  README.build-system \
  README.HBCI \
  README.OFX \
  README.translator.txt \
  TRANSLATION_HOWTO \
  build-aix.txt \
  build-solaris.txt \
  gnucash.1.in \
  gtkrc-2.0.gnucash \
  misc-notes.txt \
  tip_of_the_day.list.in \
  CMakeLists.txt

if BUILDING_FROM_VCS
top_basedir = $(top_builddir)
else
top_basedir = $(top_srcdir)
endif

gnucash.1: gnucash.1.in Makefile
	rm -f $@.tmp
	GNC_VCS_REV_Y_M=`perl -ne 'print if $$_=~s/.*GNC_VCS_REV_Y_M "(.*)"/$$1/' ${top_basedir}/libgnucash/core-utils/gnc-vcs-info.h` ; \
	${SED} < $< > $@.tmp \
	-e 's:[@]VERSION[@]:${VERSION}:g' \
	-e 's:[@]DATE[@]:'$${GNC_VCS_REV_Y_M}':g'
	chmod +x $@.tmp
	mv $@.tmp $@

tip_of_the_day.list: tip_of_the_day.list.in Makefile
	${CC} -E -P -x c -D'N_(x)=x' -o $@.tmp $<
	cat -s $@.tmp | ${SED} -e 's/^ *"//' \
	                       -e 's/"* *[|] */|/' > $@
	rm -f $@.tmp
if PLATFORM_WIN32
	perl -pi.bak -e 's/" *$$//' $@
else
	perl -pi.bak -e 's/" *$$/\n/' $@
endif

DISTCLEANFILES = \
	gnucash.1 \
	tip_of_the_day.list \
	tip_of_the_day.list.bak
