

#ifndef GNC_DRUID_PROVIDER_FILE_CB_H
#define GNC_DRUID_PROVIDER_FILE_CB_H

#ifdef __cplusplus
//extern "C" {
#endif

#include <glib.h>
#include <glib-object.h>

#define G_TYPE_GNC_DRUID_PROVIDER_FILE_CB	gnc_druid_provider_file_cb_get_type()
#define GNC_DRUID_PROVIDER_FILE_CB(obj)	G_TYPE_CHECK_INSTANCE_CAST((obj), G_TYPE_GNC_DRUID_PROVIDER_FILE_CB, GNCDruidProviderFileCB)
#define GNC_DRUID_PROVIDER_FILE_CB_CLASS(klass)	G_TYPE_CHECK_CLASS_CAST((klass), G_TYPE_GNC_DRUID_PROVIDER_FILE_CB, GNCDruidProviderFileCBClass)
#define IS_GNC_DRUID_PROVIDER_FILE_CB(obj)	G_TYPE_CHECK_INSTANCE_TYPE((obj), G_TYPE_GNC_DRUID_PROVIDER_FILE_CB)
#define IS_GNC_DRUID_PROVIDER_FILE_CB_CLASS(klass)	G_TYPE_CHECK_CLASS_TYPE((klass), G_TYPE_GNC_DRUID_PROVIDER_FILE_CB)

typedef struct _GNCDruidProviderFileCB GNCDruidProviderFileCB;
typedef struct _GNCDruidProviderFileCBClass GNCDruidProviderFileCBClass;

#include "gnc-druid-cb.h"

struct _GNCDruidProviderFileCB
{
  GNCDruidCB parent;

  const gchar* filename;	/* owned by the provider */

  gpointer this_file;		/* set by the backend (return to the provider) */
};

struct _GNCDruidProviderFileCBClass
{
  GNCDruidCBClass parent_class;
};

GType	gnc_druid_provider_file_cb_get_type(void);
GNCDruidProviderFileCB* gnc_druid_provider_file_cb_new(void);

/* methods */

#ifdef __cplusplus
//}
#endif

#endif /* GNC_DRUID_PROVIDER_FILE_CB_H */
