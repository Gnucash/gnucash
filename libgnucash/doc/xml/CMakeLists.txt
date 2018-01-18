SET(doc_xml_FILES
        account-v2.dtd
        commodity-v2.dtd
        defaults-v1.dtd
#        gnucash-v2.rnc
        io-gncxml-version1.dtd
        io-gncxml-version2.dtd
        preferences-v1.dtd
        prices-v1.dtd
        README
        transactions-v2.dtd
        types.dtd
)

SET_DIST_LIST(doc_xml_DIST CMakeLists.txt ${doc_xml_FILES})