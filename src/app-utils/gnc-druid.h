

#ifndef GNC_DRUID_H
#define GNC_DRUID_H

#ifdef __cplusplus
//extern "C" {
#endif

#include <glib.h>
#include <glib-object.h>

#define G_TYPE_GNC_DRUID	(gnc_druid_get_type())
#define GNC_DRUID(obj)		G_TYPE_CHECK_INSTANCE_CAST((obj), G_TYPE_GNC_DRUID, GNCDruid)
#define GNC_DRUID_CLASS(klass)	G_TYPE_CHECK_CLASS_CAST((klass), G_TYPE_GNC_DRUID, GNCDruidClass)
#define IS_GNC_DRUID(obj)	G_TYPE_CHECK_INSTANCE_TYPE((obj), G_TYPE_GNC_DRUID)
#define IS_GNC_DRUID_CLASS(klass) G_TYPE_CHECK_CLASS_TYPE((klass), G_TYPE_GNC_DRUID)
#define GNC_DRUID_GET_CLASS(o)	(G_TYPE_INSTANCE_GET_CLASS((o), G_TYPE_GNC_DRUID, GNCDruidClass))

typedef struct _GNCDruid GNCDruid;
typedef struct _GNCDruidClass GNCDruidClass;
typedef void GNCDruidPage;	/* completely opaque.Do we need to know anything? */

#include "gnc-druid-cb.h"
#include "gnc-druid-provider-desc.h"
#include "gnc-druid-provider.h"

struct _GNCDruid
{
  GObject obj;

  /* PROVIDED BY TOOLKIT IMPLEMENTATION... */
  GList *providers;		/* list of GNCProvider*; list is owned herein */
  GNCDruidPage *first_page;	/* the first page of the druid */
  GNCDruidPage *last_page;	/* the last page of the druid */

  /* PROVIDED HEREIN */
  GNCDruidProvider *provider;	/* current provider */

  /* Backend information */
  gpointer be_ctx;		/* backend context pointer */
  gboolean (*finish)(gpointer);	/* backend 'finish process' pointer */
  void (*cancel)(gpointer);	/* backend 'cancel process' pointer */
};

struct _GNCDruidClass
{
  GObjectClass parent_class;

  /* virtual methods */
  void	(*set_page)(GNCDruid*, GNCDruidPage*);
};

GType	gnc_druid_get_type(void);

typedef GNCDruid* (*GNCDruidNew)(const char* title, GList *providers);
void	gnc_druid_register_ui(const gchar* ui_type, GNCDruidNew new_druid);

/* methods */

void gnc_druid_set_page(GNCDruid*, GNCDruidPage*);
GNCDruidProvider* gnc_druid_next_provider(GNCDruid*);
GNCDruidProvider* gnc_druid_prev_provider(GNCDruid*);

/* Other functions */

/**
 * gnc_druid_new -- create a druid based on the list of providers
 *                  descriptors.  Hold onto the backend context and
 *                  the function to call when druid is finished.
 *
 * This will assume the "registered ui", or internally perform some
 * magic to figure out which "UI" to use..
 *
 * The provider list (and all the providerdesc objects) are owned by
 * the druid and will be freed by the druid.
 */
GNCDruid* gnc_druid_new(const gchar* title,
			GList *providers, gpointer backend_ctx,
			gboolean (*finish)(gpointer be_ctx),
			void (*cancel)(gpointer be_ctx));

#ifdef __cplusplus
//}
#endif

#endif /* GNC_DRUID_H */
