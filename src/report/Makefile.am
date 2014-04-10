NONGUI_SUBDIRS = \
  report-system \
  standard-reports \
  utility-reports \
  locale-specific \
  stylesheets

GUI_SUBDIRS = \
  report-gnome

if GNUCASH_ENABLE_GUI
SUBDIRS = $(NONGUI_SUBDIRS) $(GUI_SUBDIRS)
else 
SUBDIRS = $(NONGUI_SUBDIRS)
endif

DIST_SUBDIRS = $(NONGUI_SUBDIRS) $(GUI_SUBDIRS)
