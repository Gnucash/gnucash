
if (WITH_OFX)
  set(ofx_GSCHEMA org.gnucash.dialogs.import.ofx.gschema.xml)

  add_gschema_targets("${ofx_GSCHEMA}")
endif()

set_dist_list(ofx_gschema_DIST CMakeLists.txt org.gnucash.dialogs.import.ofx.gschema.xml.in)
