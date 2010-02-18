

#ifndef GNC_IMPORT_DESC_FORMAT_H
#define GNC_IMPORT_DESC_FORMAT_H

#ifdef __cplusplus
//extern "C" {
#endif

#include <glib.h>
#include <glib-object.h>

#define G_TYPE_GNC_IMPORT_DESC_FORMAT	gnc_import_desc_format_get_type()
#define GNC_IMPORT_DESC_FORMAT(obj)	G_TYPE_CHECK_INSTANCE_CAST((obj), G_TYPE_GNC_IMPORT_DESC_FORMAT, GNCImportDescFormat)
#define IS_GNC_IMPORT_DESC_FORMAT(obj)	G_TYPE_CHECK_INSTANCE_TYPE((obj), G_TYPE_GNC_IMPORT_DESC_FORMAT)

typedef struct _GNCImportDescFormat GNCImportDescFormat;
typedef struct _GNCImportDescFormatClass GNCImportDescFormatClass;

#include "gnc-druid.h"
#include "import-parse.h"

#include "gnc-import-format-cb.h"

#define GNC_IMPORT_DESC_TYPE_FORMAT	"import:format"

struct _GNCImportDescFormat
{
    GNCDruidProviderDesc parent;

    gchar* text;
    GncImportFormat (*get_formats)(GNCImportFormatCB*);
    const gchar* (*get_sample)(GNCImportFormatCB*);

};

struct _GNCImportDescFormatClass
{
    GNCDruidProviderDescClass parent;
};

GType	gnc_import_desc_format_get_type(void);
GNCImportDescFormat* gnc_import_desc_format_new(void);
GNCImportDescFormat*
gnc_import_desc_format_new_with_data(const gchar* title,
                                     const gchar* text,
                                     gboolean (*next_cb)(GNCDruidCB*),
                                     GncImportFormat (*get_formats)(GNCImportFormatCB*),
                                     const gchar* (*get_sample)(GNCImportFormatCB*));

void	gnc_import_desc_format_set_text(GNCImportDescFormat*,
                                        const gchar* text);

/* methods */

#ifdef __cplusplus
//}
#endif

#endif /* GNC_IMPORT_DESC_FORMAT_H */
