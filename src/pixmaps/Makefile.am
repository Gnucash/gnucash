gncpixmapdir = ${GNC_PIXMAP_DIR}

gncpixmap_DATA = \
  gnc-account-16.png \
  gnc-account-delete-16.png \
  gnc-account-delete.png \
  gnc-account-edit-16.png \
  gnc-account-edit.png \
  gnc-account-new-16.png \
  gnc-account-new.png \
  gnc-account-open-16.png \
  gnc-account-open.png \
  gnc-account.png \
  gnc-invoice-16.png \
  gnc-invoice-edit-16.png \
  gnc-invoice-edit.png \
  gnc-invoice-post-16.png \
  gnc-invoice-post.png \
  gnc-invoice-unpost-16.png \
  gnc-invoice-unpost.png \
  gnc-invoice.png \
  gnc-jumpto-16.png \
  gnc-jumpto.png \
  gnc-split-trans-16.png \
  gnc-split-trans.png \
  gnc-sx-new-16.png \
  gnc-sx-new.png \
  gnc-transfer-16.png \
  gnc-transfer.png \
  gnucash-icon.ico \
  gnucash-icon-16x16.png \
  gnucash-icon-32x32.png \
  gnucash-icon-48x48.bmp \
  gnucash-icon-48x48.png \
  gnucash_splash.png \
  stock_split_title.png \
  stock_split_watermark.png

gnclargeicondir = ${datadir}/icons/hicolor/48x48/apps
gnclargeicon_DATA = 48x48/gnucash-icon.png

gncmediumicondir = ${datadir}/icons/hicolor/32x32/apps
gncmediumicon_DATA = 32x32/gnucash-icon.png

gncsmalltwfouricondir = ${datadir}/icons/hicolor/24x24/apps
gncsmalltwfouricon_DATA = 24x24/gnucash-icon.png

gncsmalltwtwoicondir = ${datadir}/icons/hicolor/22x22/apps
gncsmalltwtwoicon_DATA = 22x22/gnucash-icon.png

gncextrasmallicondir = ${datadir}/icons/hicolor/16x16/apps
gncextrasmallicon_DATA = 16x16/gnucash-icon.png

gncscalableicondir = ${datadir}/icons/hicolor/scalable/apps
gncscalableicon_DATA = scalable/gnucash-icon.svg

if BUILDING_FROM_SVN
48x48/gnucash-icon.png: gnucash-icon-48x48.png
	-mkdir -p 48x48
	cp ${srcdir}/gnucash-icon-48x48.png 48x48/gnucash-icon.png

32x32/gnucash-icon.png: ${top_srcdir}/art/tango/32x32/gnucash.png
	-mkdir -p 32x32
	cp ${top_srcdir}/art/tango/32x32/gnucash.png 32x32/gnucash-icon.png

24x24/gnucash-icon.png: ${top_srcdir}/art/tango/22x22/gnucash-24x24.png
	-mkdir -p 24x24
	cp ${top_srcdir}/art/tango/22x22/gnucash-24x24.png 24x24/gnucash-icon.png

22x22/gnucash-icon.png: ${top_srcdir}/art/tango/22x22/gnucash-22x22.png
	-mkdir -p 22x22
	cp ${top_srcdir}/art/tango/22x22/gnucash-22x22.png 22x22/gnucash-icon.png

16x16/gnucash-icon.png: ${top_srcdir}/art/tango/16x16/gnucash.png
	-mkdir -p 16x16
	cp ${top_srcdir}/art/tango/16x16/gnucash.png 16x16/gnucash-icon.png

scalable/gnucash-icon.svg: ${top_srcdir}/art/tango/scalable/gnucash.svg
	-mkdir -p scalable
	cp ${top_srcdir}/art/tango/scalable/gnucash.svg scalable/gnucash-icon.svg
endif

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

EXTRA_DIST = \
  ${gncpixmap_DATA} \
  ${gnclargeicon_DATA} \
  ${gncmediumicon_DATA} \
  ${gncsmalltwfouricon_DATA} \
  ${gncsmalltwtwoicon_DATA} \
  ${gncextrasmallicon_DATA} \
  ${gncscalableicon_DATA}

clean-local:
	-rm -rf 48x48 32x32 24x24 22x22 16x16 scalable
