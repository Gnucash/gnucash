set(backend_xml_DTD_FILES
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

set_dist_list(backend_xml_DTD_DIST CMakeLists.txt ${backend_xml_DTD_FILES})
