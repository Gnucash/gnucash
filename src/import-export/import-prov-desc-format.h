

#ifndef GNC_IMPORT_PROV_DESC_FORMAT_H
#define GNC_IMPORT_PROV_DESC_FORMAT_H

#ifdef __cplusplus
//extern "C" {
#endif

#include <glib.h>
#include <glib-object.h>

#define G_TYPE_GNC_IMPORT_PROV_DESC_FORMAT	gnc_import_prov_desc_format_get_type()
#define GNC_IMPORT_PROV_DESC_FORMAT(obj)	G_TYPE_CHECK_INSTANCE_CAST((obj), G_TYPE_GNC_IMPORT_PROV_DESC_FORMAT, GNCImportProvDescFormat)
#define IS_GNC_IMPORT_PROV_DESC_FORMAT(obj)	G_TYPE_CHECK_INSTANCE_TYPE((obj), G_TYPE_GNC_IMPORT_PROV_DESC_FORMAT)

typedef struct _GNCImportProvDescFormat GNCImportProvDescFormat;
typedef struct _GNCImportProvDescFormatClass GNCImportProvDescFormatClass;

#include "gnc-druid.h"
#include "import-prov-format-cb.h"

#define GNC_IMPORT_PROV_TYPE_FORMAT	"import:format"

struct _GNCImportProvDescFormat
{
  GNCDruidProviderDesc parent;

  gchar* instructions;
  GncImportFormat (*get_formats)(gpointer be_ctx);
  const gchar * (*get_example)(gpointer be_ctx);
};

struct _GNCImportProvDescFormatClass
{
  GNCDruidProviderDescClass parent;
};

GType	gnc_import_prov_desc_format_get_type(void);
GNCImportProvDescFormat* gnc_import_prov_desc_format_new(void);
GNCImportProvDescFormat*
gnc_import_prov_desc_format_new_with_data(const gchar* title,
					  const gchar* instructions,
					  GncImportFormat (*get_formats)(gpointer),
					  const gchar* (*get_example)(gpointer),
					  GNCDruidProviderCB next_cb,
					  GNCDruidProviderCB prev_cb);

void	gnc_import_prov_desc_format_set_instructions(GNCImportProvDescFormat*,
						     const gchar* inst);

/* methods */

#ifdef __cplusplus
//}
#endif

#endif /* GNC_IMPORT_PROV_DESC_FORMAT_H */
