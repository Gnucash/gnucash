

#include "config.h"
#include "gnc-druid-provider-desc.h"
#include "gnc-basic-gobject.h"

static void gnc_druid_provider_desc_class_init	(GNCDruidProviderDescClass *class);
static void gnc_druid_provider_desc_finalize	(GObject *obj);

static GObjectClass *parent_class;

GNC_BASIC_GOBJECT_TYPE(GNCDruidProviderDesc, GNCDruidProviderDescClass,
		       G_TYPE_OBJECT, gnc_druid_provider_desc_class_init, NULL,
		       gnc_druid_provider_desc_get_type)

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
