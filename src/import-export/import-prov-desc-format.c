

#include "config.h"
#include "import-prov-desc-format.h"
#include "gnc-basic-gobject.h"

static void gnc_import_prov_desc_format_class_init	(GNCImportProvDescFormatClass *class);
static void gnc_import_prov_desc_format_init		(GNCImportProvDescFormat *gspaper);
static void gnc_import_prov_desc_format_finalize	(GObject *obj);

static GNCDruidProviderDescClass *parent_class;

GNC_BASIC_GOBJECT(GNCImportProvDescFormat, GNCImportProvDescFormatClass,
		  G_TYPE_GNC_DRUID_PROVIDER_DESC,
		  gnc_import_prov_desc_format_class_init,
		  gnc_import_prov_desc_format_init,
		  gnc_import_prov_desc_format_get_type,
		  gnc_import_prov_desc_format_new)

static void
gnc_import_prov_desc_format_class_init (GNCImportProvDescFormatClass *klass)
{
  GObjectClass *object_class;
	
  object_class = G_OBJECT_CLASS (klass);
  parent_class = g_type_class_peek_parent (klass);

  object_class->finalize = gnc_import_prov_desc_format_finalize;
}

static void
gnc_import_prov_desc_format_finalize (GObject *obj)
{
  GNCImportProvDescFormat *desc = (GNCImportProvDescFormat *)obj;

  if (desc->instructions)
    g_free(desc->instructions);

  G_OBJECT_CLASS (parent_class)->finalize(obj);
}

static void
gnc_import_prov_desc_format_init (GNCImportProvDescFormat *o)
{
  o->parent.name = GNC_IMPORT_PROV_TYPE_FORMAT;
}

GNCImportProvDescFormat*
gnc_import_prov_desc_format_new_with_data(const gchar* title,
					  const gchar* instructions,
					  GncImportFormat (*get_formats)(gpointer),
					  const gchar* (*get_example)(gpointer),
					  GNCDruidProviderCB next_cb,
					  GNCDruidProviderCB prev_cb)
{
  GNCImportProvDescFormat* desc;
  GNCDruidProviderDesc* gdp_desc;

  desc = gnc_import_prov_desc_format_new();
  g_assert(desc);
  gdp_desc = GNC_DRUID_PROVIDER_DESC(desc);

  desc->get_formats = get_formats;
  desc->get_example = get_example;
  gdp_desc->next_cb = next_cb;
  gdp_desc->prev_cb = prev_cb;

  if (instructions)
    gnc_import_prov_desc_format_set_instructions(desc, instructions);
  if (title)
    gnc_druid_provider_desc_set_title(gdp_desc, title);

  return desc;
}

void
gnc_import_prov_desc_format_set_instructions(GNCImportProvDescFormat* desc,
					     const gchar* instructions)
{
  g_return_if_fail(desc);
  g_return_if_fail(IS_GNC_IMPORT_PROV_DESC_FORMAT(desc));
  g_return_if_fail(instructions);

  if (desc->instructions)
    g_free(desc->instructions);
  desc->instructions = g_strdup(instructions);
}
