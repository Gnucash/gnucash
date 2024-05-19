if (NOT WIN32)
  add_subdirectory(design)
endif()
add_subdirectory(xml)

set(doc_FILES
        callgrind.txt
        constderv.html
        doxygen.cfg.in
        doxygen_main_page.c
        finderv.html
        finutil.html
        loans.txt
        lots.txt
        python-bindings-doxygen.py
        README
        )

set_local_dist(doc_DIST_local CMakeLists.txt ${doc_FILES})
set(doc_DIST ${doc_DIST_local} ${doc_design_DIST} ${doc_xml_DIST} PARENT_SCOPE)

find_program(DOXYGEN doxygen)
if (NOT ${DOXYGEN} STREQUAL "DOXYGEN-NOTFOUND")
  configure_file(doxygen.cfg.in doxygen.cfg)
  add_custom_target(doc ${DOXYGEN} doxygen.cfg)
endif()
