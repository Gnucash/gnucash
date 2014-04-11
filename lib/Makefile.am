SUBDIRS = libc guile-www srfi
DIST_SUBDIRS = libc guile-www srfi libgsf-1.12.3 goffice-0.0.4 libqof

if !HAVE_GOFFICE
if !HAVE_LIBGSF
SUBDIRS += libgsf-1.12.3
endif
SUBDIRS += goffice-0.0.4
endif

if USE_LIBQOF
SUBDIRS += libqof
endif

EXTRA_DIST = README

