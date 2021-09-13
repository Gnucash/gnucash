
if (WITH_OFX)
  set(ofx_GSCHEMA org.gnucash.GnuCash.dialogs.import.ofx.gschema.xml)

  add_gschema_targets("${ofx_GSCHEMA}")
endif()

set_dist_list(ofx_gschema_DIST CMakeLists.txt org.gnucash.GnuCash.dialogs.import.ofx.gschema.xml.in)
