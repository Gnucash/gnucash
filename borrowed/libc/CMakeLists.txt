SET(libc_SOURCES libc-missing-noop.c setenv.c strfmon.c strptime.c)
SET(libc_HEADERS pow.h setenv.h strfmon.h strptime.h)

SET_DIST_LIST(libc_DIST CMakeLists.txt ${libc_SOURCES} ${libc_HEADERS})