set(valgrind_FILES
        valgrind-gdk.supp
        valgrind-glib.supp
        valgrind-gnucash.supp
        valgrind-libfontconfig.supp
        valgrind-libgda.supp
        valgrind-libguile.supp
        valgrind-x11.supp
        )

set_dist_list(valgrind_DIST CMakeLists.txt ${valgrind_FILES})