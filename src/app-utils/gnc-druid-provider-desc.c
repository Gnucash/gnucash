

#include "config.h"
#include "gnc-druid-provider-desc.h"

static void gnc_druid_provider_desc_class_init	(GNCDruidProviderDescClass *class);
static void gnc_druid_provider_desc_finalize	(GObject *obj);

static GObjectClass *parent_class;

GType 
gnc_druid_provider_desc_get_type(void)
{
  static GType type = 0;

  if (type == 0) {
    GTypeInfo type_info = {
      sizeof (GNCDruidProviderDescClass),
      NULL,
      NULL,
      (GClassInitFunc)gnc_druid_provider_desc_class_init,
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

static void
gnc_druid_provider_desc_class_init (GNCDruidProviderDescClass *klass)
{
  GObjectClass *object_class;
	
  object_class = G_OBJECT_CLASS (klass);
  parent_class = g_type_class_peek_parent (klass);

  object_class->finalize = gnc_druid_provider_desc_finalize;
}

static void
gnc_druid_provider_desc_finalize (GObject *obj)
{
  GNCDruidProviderDesc *desc = (GNCDruidProviderDesc *)obj;

  if (desc->title)
    g_free(desc->title);

  G_OBJECT_CLASS (parent_class)->finalize(obj);
}

void
gnc_druid_provider_desc_set_title(GNCDruidProviderDesc* desc, const gchar* title)
{
  g_return_if_fail(desc);
  g_return_if_fail(IS_GNC_DRUID_PROVIDER_DESC(desc));
  g_return_if_fail(title);

  if (desc->title)
    g_free(desc->title);
  desc->title = g_strdup(title);
}
