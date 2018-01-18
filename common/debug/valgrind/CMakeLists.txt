SET(valgrind_FILES
        valgrind-gdk.supp
        valgrind-glib.supp
        valgrind-gnucash.supp
        valgrind-libfontconfig.supp
        valgrind-libgda.supp
        valgrind-libguile.supp
        valgrind-x11.supp
        )

SET_DIST_LIST(valgrind_DIST CMakeLists.txt ${valgrind_FILES})