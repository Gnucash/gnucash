

#include "config.h"
#include "gnc-druid-cb.h"
#include "gnc-basic-gobject.h"

GNC_BASIC_GOBJECT(GNCDruidCB, GNCDruidCBClass, G_TYPE_OBJECT, NULL, NULL,
		  gnc_druid_cb_get_type, gnc_druid_cb_new)
