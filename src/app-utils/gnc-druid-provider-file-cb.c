

#include "config.h"
#include "gnc-druid-provider-file-cb.h"
#include "gnc-basic-gobject.h"

GNC_BASIC_GOBJECT(GNCDruidProviderFileCB, GNCDruidProviderFileCBClass,
		  G_TYPE_GNC_DRUID_CB, NULL, NULL,
		  gnc_druid_provider_file_cb_get_type,
		  gnc_druid_provider_file_cb_new)
