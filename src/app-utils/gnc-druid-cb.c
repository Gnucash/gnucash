

#include "config.h"
#include "gnc-druid-cb.h"

GType 
gnc_druid_cb_get_type(void)
{
  static GType type = 0;

  if (type == 0) {
    GTypeInfo type_info = {
      sizeof (GNCDruidCBClass),
      NULL,
      NULL,
      NULL,
      NULL,
      NULL,
      sizeof (GNCDruidCB),
      0,
      NULL,
    };

    type = g_type_register_static (G_TYPE_OBJECT, "GNCDruidCB", &type_info, 0);
  }
  
  return type;
}

GNCDruidCB*
gnc_druid_cb_new(void)
{
  return GNC_DRUID_CB(g_object_new(G_TYPE_GNC_DRUID_CB, NULL));
}
