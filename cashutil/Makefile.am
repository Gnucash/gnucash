
SUBDIRS = src . po doc

noinst_DATA = make-potfiles

ACLOCAL_AMFLAGS = -I m4

make-potfiles: make-potfiles.in Makefile
	rm -f $@.tmp
	 sed < $< > $@.tmp \
             -e 's:@-PERL-@:${PERL}:g'
	 chmod +x $@.tmp
	 mv $@.tmp $@

EXTRA_DIST =  \
  make-potfiles.in \
  intltool-extract.in \
  intltool-merge.in \
  intltool-update.in

DISTCLEANFILES = \
  intltool-extract intltool-update intltool-merge\
  make-potfiles po/.intltool-merge-cache

