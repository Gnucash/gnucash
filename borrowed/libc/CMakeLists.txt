set(libc_SOURCES libc-missing-noop.c setenv.c strptime.c)
set(libc_HEADERS pow.h setenv.h strptime.h)

set_dist_list(libc_DIST CMakeLists.txt ${libc_SOURCES} ${libc_HEADERS})
