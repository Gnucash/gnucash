set(doc_FILES
        constderv.html
        finderv.html
        finutil.html
        README
        )

set_local_dist(doc_DIST_local CMakeLists.txt ${doc_FILES})
set(doc_DIST ${doc_DIST_local} ${doc_design_DIST} PARENT_SCOPE)
