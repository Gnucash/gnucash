noinst_LTLIBRARIES = libc-missing.la

# All header files must be listed.
noinst_HEADERS = \
  localtime_r.h strptime.h

# No sources should be listed.
libc_missing_la_SOURCES = libc-missing-noop.c

# This will automatically be filled in with the necessary object file
# names.  Configure does this based upon the AC_REPLACE_FUNCS macros.
libc_missing_la_LIBADD = @LIBOBJS@

# Not currently used.  If added to AC_REPLACE_FUNCS then this line
# should be removed.
EXTRA_DIST = scm_strptime.c
