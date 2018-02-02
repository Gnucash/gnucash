
SET(gnome_GSCHEMA
  org.gnucash.dialogs.gschema.xml
  org.gnucash.dialogs.commodities.gschema.xml
  org.gnucash.dialogs.checkprinting.gschema.xml
  org.gnucash.dialogs.reconcile.gschema.xml
  org.gnucash.dialogs.sxs.gschema.xml
  org.gnucash.dialogs.totd.gschema.xml
  org.gnucash.gschema.xml
  org.gnucash.general.finance-quote.gschema.xml
  org.gnucash.window.pages.account.tree.gschema.xml
  org.gnucash.window.pages.gschema.xml
)

# If you edit one of the above files, you need to rerun cmake.
# When we can use CONFIGURE_FILE() instead of GNC_CONFIGURE()
# this issue will go away.


ADD_GSCHEMA_TARGETS(gnome-gschema "${gnome_GSCHEMA}")

SET(gnome_gschema_DIST_local "")
FOREACH(file ${gnome_GSCHEMA})
    LIST(APPEND gnome_gschema_DIST_local ${file}.in.in)
ENDFOREACH()
SET_DIST_LIST(gnome_gschema_DIST CMakeLists.txt Makefile.am ${gnome_gschema_DIST_local})
