

#include "config.h"
#include "import-prov-format-cb.h"
#include "gnc-basic-gobject.h"

GNC_BASIC_GOBJECT(GNCImportProvFormatCB, GNCImportProvFormatCBClass,
		  G_TYPE_GNC_DRUID_CB, NULL, NULL,
		  gnc_import_prov_format_cb_get_type, gnc_import_prov_format_cb_new)
