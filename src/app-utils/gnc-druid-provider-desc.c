

#include "config.h"
#include "gnc-druid-provider-desc.h"

GType 
gnc_druid_provider_desc_get_type(void)
{
  static GType type = 0;

  if (type == 0) {
    GTypeInfo type_info = {
      sizeof (GNCDruidProviderDescClass),
      NULL,
      NULL,
      NULL,
      NULL,
      NULL,
      sizeof (GNCDruidProviderDesc),
      0,
      NULL,
    };

    type = g_type_register_static (G_TYPE_OBJECT, "GNCDruidProviderDesc", &type_info, 0);
  }
  
  return type;
}
