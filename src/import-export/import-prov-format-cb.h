

#ifndef GNC_IMPORT_PROV_FORMAT_CB_H
#define GNC_IMPORT_PROV_FORMAT_CB_H

#ifdef __cplusplus
//extern "C" {
#endif

#include <glib.h>
#include <glib-object.h>

#define G_TYPE_GNC_IMPORT_PROV_FORMAT_CB	gnc_import_prov_format_cb_get_type()
#define GNC_IMPORT_PROV_FORMAT_CB(obj)	G_TYPE_CHECK_INSTANCE_CAST((obj), G_TYPE_GNC_IMPORT_PROV_FORMAT_CB, GNCImportProvFormatCB)
#define GNC_IMPORT_PROV_FORMAT_CB_CLASS(klass)	G_TYPE_CHECK_CLASS_CAST((klass), G_TYPE_GNC_IMPORT_PROV_FORMAT_CB, GNCImportProvFormatCBClass)
#define IS_GNC_IMPORT_PROV_FORMAT_CB(obj)	G_TYPE_CHECK_INSTANCE_TYPE((obj), G_TYPE_GNC_IMPORT_PROV_FORMAT_CB)
#define IS_GNC_IMPORT_PROV_FORMAT_CB_CLASS(klass)	G_TYPE_CHECK_CLASS_TYPE((klass), G_TYPE_GNC_IMPORT_PROV_FORMAT_CB)

typedef struct _GNCImportProvFormatCB GNCImportProvFormatCB;
typedef struct _GNCImportProvFormatCBClass GNCImportProvFormatCBClass;

#include "gnc-druid-cb.h"
#include "import-parse.h"

struct _GNCImportProvFormatCB
{
  GNCDruidCB parent;

  GncImportFormat	format;
};

struct _GNCImportProvFormatCBClass
{
  GNCDruidCBClass parent_class;
};

GType	gnc_import_prov_format_cb_get_type(void);
GNCImportProvFormatCB* gnc_import_prov_format_cb_new(void);

/* methods */

#ifdef __cplusplus
//}
#endif

#endif /* GNC_IMPORT_PROV_FORMAT_CB_H */
