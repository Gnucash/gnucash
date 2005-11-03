if BUILD_XML
SUBDIRS = xml man
else
SUBDIRS = man
endif

EXTRA_DIST = doxygen.cfg.in

all:

doc: doxygen.cfg
	rm -rf html refman.pdf
	doxygen doxygen.cfg
