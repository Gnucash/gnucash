
FILE(COPY .
  DESTINATION ${DATADIR_BUILD}/gnucash/gtkbuilder
  PATTERN Makefile.* EXCLUDE
  PATTERN CMakeLists.txt EXCLUDE)

INSTALL(DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}
  DESTINATION share/gnucash
  PATTERN Makefile.* EXCLUDE
  PATTERN CMakeLists.txt EXCLUDE)

SET(business_gnome_GLADE business-options-gnome.glade business-prefs.glade dialog-billterms.glade dialog-choose-owner.glade
        dialog-customer.glade dialog-date-close.glade dialog-employee.glade dialog-invoice.glade dialog-job.glade
        dialog-order.glade dialog-payment.glade dialog-vendor.glade)

SET_DIST_LIST(business_gnome_gtkbuilder_DIST CMakeLists.txt Makefile.am ${business_gnome_GLADE})