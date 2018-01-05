bin_SCRIPTS = gnc-fq-check gnc-fq-dump gnc-fq-helper gnc-fq-update

man_MANS = \
  gnc-fq-dump.1 \
  gnc-fq-helper.1

EXTRA_DIST = \
  Quote_example.pl \
  gnc-fq-check.in \
  gnc-fq-dump \
  gnc-fq-helper.in \
  gnc-fq-update.in \
  CMakeLists.txt

gnc-fq-helper: gnc-fq-helper.in Makefile
	rm -f $@.tmp
	sed < $< > $@.tmp \
            -e 's:[@]PERL[@]:${PERL}:g' \
            -e 's:[@]PERLINCL[@]:${PERLINCL}:g'
	chmod +x $@.tmp
	mv $@.tmp $@

gnc-fq-check: gnc-fq-check.in Makefile
	rm -f $@.tmp
	sed < $< > $@.tmp \
            -e 's:[@]PERL[@]:${PERL}:g' \
            -e 's:[@]PERLINCL[@]:${PERLINCL}:g'
	chmod +x $@.tmp
	mv $@.tmp $@

gnc-fq-update: gnc-fq-update.in Makefile
	rm -f $@.tmp
	sed < $< > $@.tmp \
            -e 's:[@]PERL[@]:${PERL}:g' \
            -e 's:[@]PERLINCL[@]:${PERLINCL}:g'
	chmod +x $@.tmp
	mv $@.tmp $@

gnc-fq-dump.1: gnc-fq-dump Makefile
	pod2man $< > $@

gnc-fq-helper.1: gnc-fq-helper Makefile
	pod2man $< > $@

CLEANFILES = gnc-fq-helper gnc-fq-check gnc-fq-update

DISTCLEANFILES = gnc-fq-dump.1 gnc-fq-helper.1
