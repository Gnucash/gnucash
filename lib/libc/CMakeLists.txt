SET(libc_SOURCES gmtime_r.c libc-missing-noop.c localtime_r.c setenv.c strfmon.c strptime.c)
SET(libc_HEADERS gmtime_r.h localtime_r.h pow.h setenv.h strfmon.h strptime.h)

SET_DIST_LIST(libc_DIST CMakeLists.txt Makefile.am ${libc_SOURCES} ${libc_HEADERS})