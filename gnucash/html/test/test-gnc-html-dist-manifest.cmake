set(required_html_backend_files
  gnucash/html/gnc-html-webkit2.cpp
  gnucash/html/gnc-html-webkit2.hpp
  gnucash/html/gnc-html-webview2.cpp
  gnucash/html/gnc-html-webview2.hpp
  gnucash/html/gnc-html-webview2-loader-state.hpp
  gnucash/html/gnc-html-native-widget-lifecycle.hpp
  gnucash/html/gnc-html-wkwebview.mm
  gnucash/html/gnc-html-wkwebview.hpp
)

if (NOT EXISTS "${DIST_MANIFEST}")
  message(FATAL_ERROR "Missing dist manifest: ${DIST_MANIFEST}")
endif()

file(STRINGS "${DIST_MANIFEST}" dist_files)
foreach(required_file IN LISTS required_html_backend_files)
  list(FIND dist_files "${required_file}" required_file_index)
  if (required_file_index EQUAL -1)
    message(FATAL_ERROR "Missing ${required_file} from the distribution manifest")
  endif()
endforeach()
