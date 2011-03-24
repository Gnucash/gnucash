NONGUI_SUBDIRS = \
  report-system \
  standard-reports \
  utility-reports \
  business-reports \
  locale-specific \
  jqplot

GUI_SUBDIRS = \
  report-gnome \
  stylesheets

if GNUCASH_ENABLE_GUI
SUBDIRS = $(NONGUI_SUBDIRS) $(GUI_SUBDIRS)
else 
SUBDIRS = $(NONGUI_SUBDIRS)
endif

DIST_SUBDIRS = $(NONGUI_SUBDIRS) $(GUI_SUBDIRS)
