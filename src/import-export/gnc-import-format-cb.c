

#include "config.h"
#include "gnc-import-format-cb.h"
#include "gnc-basic-gobject.h"

GNC_BASIC_GOBJECT(GNCImportFormatCB, GNCImportFormatCBClass,
                  G_TYPE_GNC_DRUID_CB, NULL, NULL,
                  gnc_import_format_cb_get_type,
                  gnc_import_format_cb_new)
