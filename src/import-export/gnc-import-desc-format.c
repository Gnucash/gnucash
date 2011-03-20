

#include "config.h"
#include "gnc-import-desc-format.h"
#include "gnc-basic-gobject.h"

static void gnc_import_desc_format_class_init	(GNCImportDescFormatClass *class);
static void gnc_import_desc_format_init		(GNCImportDescFormat *gspaper);
static void gnc_import_desc_format_finalize	(GObject *obj);

static GNCDruidProviderDescClass *parent_class;

GNC_BASIC_GOBJECT(GNCImportDescFormat, GNCImportDescFormatClass,
                  G_TYPE_GNC_DRUID_PROVIDER_DESC,
                  gnc_import_desc_format_class_init,
                  gnc_import_desc_format_init,
                  gnc_import_desc_format_get_type,
                  gnc_import_desc_format_new)

static void
gnc_import_desc_format_class_init (GNCImportDescFormatClass *klass)
{
    GObjectClass *object_class;

    object_class = G_OBJECT_CLASS (klass);
    parent_class = g_type_class_peek_parent (klass);

    object_class->finalize = gnc_import_desc_format_finalize;
}

static void
gnc_import_desc_format_finalize (GObject *obj)
{
    GNCImportDescFormat *desc = (GNCImportDescFormat *)obj;

    if (desc->text)
        g_free(desc->text);

    G_OBJECT_CLASS (parent_class)->finalize(obj);
}

static void
gnc_import_desc_format_init (GNCImportDescFormat *o)
{
    o->parent.name = GNC_IMPORT_DESC_TYPE_FORMAT;
}

GNCImportDescFormat*
gnc_import_desc_format_new_with_data(const gchar* title,
                                     const gchar* text,
                                     gboolean (*next_cb)(GNCDruidCB*),
                                     GncImportFormat (*get_formats)(GNCImportFormatCB*),
                                     const gchar* (*get_sample)(GNCImportFormatCB*))
{
    GNCImportDescFormat* desc;

    desc = gnc_import_desc_format_new();
    g_assert(desc);

    desc->parent.next_cb = next_cb;
    desc->get_formats = get_formats;
    desc->get_sample = get_sample;

    if (text)
        gnc_import_desc_format_set_text(desc, text);
    if (title)
        gnc_druid_provider_desc_set_title(&(desc->parent), title);

    return desc;
}

void
gnc_import_desc_format_set_text(GNCImportDescFormat* desc, const gchar* text)
{
    g_return_if_fail(desc);
    g_return_if_fail(IS_GNC_IMPORT_DESC_FORMAT(desc));
    g_return_if_fail(text);

    if (desc->text)
        g_free(desc->text);
    desc->text = g_strdup(text);
}
