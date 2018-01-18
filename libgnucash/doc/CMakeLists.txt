if (NOT WIN32)
  ADD_SUBDIRECTORY(design)
endif()
ADD_SUBDIRECTORY(xml)

SET(doc_FILES
        backup.txt
        books.txt
        budget.txt
#        business.txt
        callgrind.txt
        constderv.html
#        constraints.txt
        currencies.txt
        doxygen.cfg.in
        doxygen_main_page.c
#        engine.txt
        finderv.html
        finutil.html
#        g2-architecture.txt
        generic-druid-framework.txt
#        gnc-numeric-example.txt
        guid.txt
        loans.txt
        lots.txt
        multicurrency-discussion.txt
#        prices.txt
        python-bindings-doxygen.py
        qif.txt
        README
#        sx.rst
        tax.txt
        TODO-schedxactions
        TODO-sixtp
        user-prefs-howto.txt
        )

SET_LOCAL_DIST(doc_DIST_local CMakeLists.txt ${doc_FILES})
SET(doc_DIST ${doc_DIST_local} ${doc_design_DIST} ${doc_xml_DIST} PARENT_SCOPE)

