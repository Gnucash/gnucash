SUBDIRS = libc glib26 guile-www srfi

if !HAVE_GOFFICE
if !HAVE_LIBGSF
SUBDIRS += libgsf-1.12.3
endif
SUBDIRS += goffice-0.0.4
endif

EXTRA_DIST = README guppi-legend.patch
