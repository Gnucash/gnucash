

#include "config.h"
#include "gnc-druid-provider-desc-edge.h"

static void gnc_druid_provider_desc_edge_class_init	(GNCDruidProviderDescEdgeClass *class);
static void gnc_druid_provider_desc_edge_finalize	(GObject *obj);

static GNCDruidProviderDescClass *parent_class;

GType 
gnc_druid_provider_desc_edge_get_type(void)
{
  static GType type = 0;

  if (type == 0) {
    GTypeInfo type_info = {
      sizeof (GNCDruidProviderDescEdgeClass),
      NULL,
      NULL,
      (GClassInitFunc)gnc_druid_provider_desc_edge_class_init,
      NULL,
      NULL,
      sizeof (GNCDruidProviderDescEdge),
      0,
      NULL,
    };

    type = g_type_register_static (G_TYPE_GNC_DRUID_PROVIDER_DESC,
				   "GNCDruidProviderDescEdge", &type_info, 0);
  }
  
  return type;
}

static void
gnc_druid_provider_desc_edge_class_init (GNCDruidProviderDescEdgeClass *klass)
{
  GObjectClass *object_class;
	
  object_class = G_OBJECT_CLASS (klass);
  parent_class = g_type_class_peek_parent (klass);

  object_class->finalize = gnc_druid_provider_desc_edge_finalize;
}

static void
gnc_druid_provider_desc_edge_finalize (GObject *obj)
{
  GNCDruidProviderDescEdge *desc = (GNCDruidProviderDescEdge *)obj;

  if (desc->text)
    g_free(desc->text);

  G_OBJECT_CLASS (parent_class)->finalize(obj);
}

GNCDruidProviderDescEdge*
gnc_druid_provider_desc_edge_new(void)
{
  return GNC_DRUID_PROVIDER_DESC_EDGE(g_object_new(G_TYPE_GNC_DRUID_PROVIDER_DESC_EDGE, NULL));
}

void
gnc_druid_provider_desc_edge_set_text(GNCDruidProviderDescEdge* desc,
				      const gchar* text)
{
  g_return_if_fail(desc);
  g_return_if_fail(IS_GNC_DRUID_PROVIDER_DESC_EDGE(desc));
  g_return_if_fail(text);

  if (desc->text)
    g_free(desc->text);
  desc->text = g_strdup(text);
}

void
gnc_druid_provider_desc_edge_set_which(GNCDruidProviderDescEdge* desc,
				       GNCDruidProviderDescEdgeWhich which)
{
  g_return_if_fail(desc);
  g_return_if_fail(IS_GNC_DRUID_PROVIDER_DESC_EDGE(desc));

  desc->first_or_last = which;
}
