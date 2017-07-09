gncpixmapdir = ${GNC_PIXMAP_DIR}
gncpixmap_DATA = \
  gnucash-icon.ico \
  gnucash-icon-48x48.bmp \
  gnucash_splash.png

gncicon16dir = ${pkgdatadir}/icons/hicolor/16x16/actions
gncicon16_DATA = \
  hicolor/16x16/actions/gnc-account.png \
  hicolor/16x16/actions/gnc-account-delete.png \
  hicolor/16x16/actions/gnc-account-edit.png \
  hicolor/16x16/actions/gnc-account-new.png \
  hicolor/16x16/actions/gnc-account-open.png \
  hicolor/16x16/actions/gnc-account-report.png \
  hicolor/16x16/actions/gnc-gnome-pdf.png \
  hicolor/16x16/actions/gnc-invoice.png \
  hicolor/16x16/actions/gnc-invoice-duplicate.png \
  hicolor/16x16/actions/gnc-invoice-edit.png \
  hicolor/16x16/actions/gnc-invoice-new.png \
  hicolor/16x16/actions/gnc-invoice-pay.png \
  hicolor/16x16/actions/gnc-invoice-post.png \
  hicolor/16x16/actions/gnc-invoice-unpost.png \
  hicolor/16x16/actions/gnc-jumpto.png \
  hicolor/16x16/actions/gnc-split-trans.png \
  hicolor/16x16/actions/gnc-sx-new.png \
  hicolor/16x16/actions/gnc-transfer.png

gncicon24dir = ${pkgdatadir}/icons/hicolor/24x24/actions
gncicon24_DATA = \
  hicolor/24x24/actions/gnc-account.png \
  hicolor/24x24/actions/gnc-account-delete.png \
  hicolor/24x24/actions/gnc-account-edit.png \
  hicolor/24x24/actions/gnc-account-new.png \
  hicolor/24x24/actions/gnc-account-open.png \
  hicolor/24x24/actions/gnc-account-report.png \
  hicolor/24x24/actions/gnc-gnome-pdf.png \
  hicolor/24x24/actions/gnc-invoice.png \
  hicolor/24x24/actions/gnc-invoice-duplicate.png \
  hicolor/24x24/actions/gnc-invoice-edit.png \
  hicolor/24x24/actions/gnc-invoice-new.png \
  hicolor/24x24/actions/gnc-invoice-pay.png \
  hicolor/24x24/actions/gnc-invoice-post.png \
  hicolor/24x24/actions/gnc-invoice-unpost.png \
  hicolor/24x24/actions/gnc-jumpto.png \
  hicolor/24x24/actions/gnc-split-trans.png \
  hicolor/24x24/actions/gnc-sx-new.png \
  hicolor/24x24/actions/gnc-transfer.png

gnchuge256icondir = ${datadir}/icons/hicolor/256x256/apps
gnchuge256icon_DATA = hicolor/256x256/apps/gnucash-icon.png

gnchuge128icondir = ${datadir}/icons/hicolor/128x128/apps
gnchuge128icon_DATA = hicolor/128x128/apps/gnucash-icon.png

gnclarge96icondir = ${datadir}/icons/hicolor/96x96/apps
gnclarge96icon_DATA = hicolor/96x96/apps/gnucash-icon.png

gnclarge64icondir = ${datadir}/icons/hicolor/64x64/apps
gnclarge64icon_DATA = hicolor/64x64/apps/gnucash-icon.png

gnclarge48icondir = ${datadir}/icons/hicolor/48x48/apps
gnclarge48icon_DATA = hicolor/48x48/apps/gnucash-icon.png

gncmediumicondir = ${datadir}/icons/hicolor/32x32/apps
gncmediumicon_DATA = hicolor/32x32/apps/gnucash-icon.png

gncsmalltwfouricondir = ${datadir}/icons/hicolor/24x24/apps
gncsmalltwfouricon_DATA = hicolor/24x24/apps/gnucash-icon.png

gncsmalltwtwoicondir = ${datadir}/icons/hicolor/22x22/apps
gncsmalltwtwoicon_DATA = hicolor/22x22/apps/gnucash-icon.png

gncextrasmallicondir = ${datadir}/icons/hicolor/16x16/apps
gncextrasmallicon_DATA = hicolor/16x16/apps/gnucash-icon.png

gncscalableicondir = ${datadir}/icons/hicolor/scalable/apps
gncscalableicon_DATA = hicolor/scalable/apps/gnucash-icon.svg

# As suggested by <http://live.gnome.org/GnomeGoals/AppIcon>
if !OS_WIN32
gtk_update_icon_cache = gtk-update-icon-cache -f -t $(datadir)/icons/hicolor
install-data-hook: update-icon-cache
uninstall-hook: update-icon-cache
update-icon-cache:
	@-if test -z "$(DESTDIR)"; then \
		echo "Updating Gtk icon cache."; \
		$(gtk_update_icon_cache); \
	else \
		echo "*** Icon cache not updated.  After (un)install, run this:"; \
		echo "***   $(gtk_update_icon_cache)"; \
	fi
endif

ico_files_hr = \
  ${gnchuge256icon_DATA} \
  ${gnchuge128icon_DATA} \
  ${gnclarge96icon_DATA} \
  ${gnclarge64icon_DATA} \
  ${gnclarge48icon_DATA}

ico_files_lr = \
  ${gncmediumicon_DATA} \
  ${gncsmalltwfouricon_DATA} \
  ${gncsmalltwtwoicon_DATA} \
  ${gncextrasmallicon_DATA}

EXTRA_DIST = \
  ${gncpixmap_DATA} \
  ${gncicon16_DATA} \
  ${gncicon24_DATA} \
  ${ico_files_hr} \
  ${ico_files_lr} \
  ${gncscalableicon_DATA}

# Create gnucash-icon.ico
.PHONY: ico
ico: ${ico_files_hr} ${ico_files_lr}
	convert ${ico_files_lr} ${ico_files_hr} tmp.ico; \
	mv tmp.ico gnucash-icon.ico
