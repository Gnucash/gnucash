

#ifndef GNC_DRUID_PROVIDER_DESC_H
#define GNC_DRUID_PROVIDER_DESC_H

#ifdef __cplusplus
//extern "C" {
#endif

#include <glib.h>
#include <glib-object.h>

#define G_TYPE_GNC_DRUID_PROVIDER_DESC	gnc_druid_provider_desc_get_type()
#define GNC_DRUID_PROVIDER_DESC(obj)	G_TYPE_CHECK_INSTANCE_CAST((obj), G_TYPE_GNC_DRUID_PROVIDER_DESC, GNCDruidProviderDesc)
#define IS_GNC_DRUID_PROVIDER_DESC(obj)	G_TYPE_CHECK_INSTANCE_TYPE((obj), G_TYPE_GNC_DRUID_PROVIDER_DESC)

typedef struct _GNCDruidProviderDesc GNCDruidProviderDesc;
typedef struct _GNCDruidProviderDescClass GNCDruidProviderDescClass;

#include "gnc-druid.h"

struct _GNCDruidProviderDesc
{
  GObject obj;
  gchar *name;

  gboolean (*provider_needed)(GNCDruidCB*);
  gboolean (*next_cb)(GNCDruidCB*);
  gboolean (*prev_cb)(GNCDruidCB*);
};

struct _GNCDruidProviderDescClass
{
  GObjectClass obj;
};

GType	gnc_druid_provider_desc_get_type(void);

/* methods */

#ifdef __cplusplus
//}
#endif

#endif /* GNC_DRUID_PROVIDER_DESC_H */
