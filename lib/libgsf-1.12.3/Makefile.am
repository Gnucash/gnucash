pkgconfigdir = $(libdir)/pkgconfig
pkgconfig_DATA = libgsf-1.pc

if WITH_LIBGSF_GNOME
  pkgconfig_DATA += libgsf-gnome-1.pc
endif
if PLATFORM_WIN32
  pkgconfig_DATA += libgsf-win32-1.pc
endif

SUBDIRS = gsf gsf-gnome gsf-win32 tests
# doc (before tests)
# po 

if WITH_PYTHON
  SUBDIRS += python
endif

EXTRA_DIST = BUGS HACKING	\
	intltool-extract.in	\
	intltool-merge.in	\
	intltool-update.in	\
	libgsf-1.spec		\
	libgsf-gnome-1.spec

DISTCLEANFILES = intltool-extract intltool-merge intltool-update

#DISTCHECK_CONFIGURE_FLAGS = --enable-gtk-doc
