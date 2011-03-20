noinst_LTLIBRARIES = libc-missing.la

# All header files must be listed.
noinst_HEADERS = \
  gmtime_r.h \
  localtime_r.h \
  setenv.h \
  strptime.h \
  pow.h

# No sources should be listed.
libc_missing_la_SOURCES = libc-missing-noop.c

EXTRA_libc_missing_la_SOURCES = \
  gmtime_r.c \
  localtime_r.c \
  setenv.c \
  strptime.c

# This will automatically be filled in with the necessary object file
# names.  Configure does this based upon the AC_REPLACE_FUNCS macros.
LIBOBJS = @LIBOBJS@
LTLIBOBJS = $(shell echo "$(LIBOBJS)" | sed '@LIBOBJS_SEDSCRIPT@' | \
	sed 's,\.[^.]* ,.lo ,g;s,\.[^.]*$$,.lo,')

libc_missing_la_LIBADD = $(LTLIBOBJS)

if OS_WIN32
  AM_CPPFLAGS = -DOS_WIN32
endif

# Not currently used.  If added to AC_REPLACE_FUNCS then this line
# should be removed.
EXTRA_DIST = scm_strptime.c

print_libobjs: Makefile
	@echo "LIBOBJS = $(LIBOBJS)"
	@echo "LTLIBOBJS = $(LTLIBOBJS)"
