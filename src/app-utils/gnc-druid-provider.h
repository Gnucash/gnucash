

#ifndef GNC_DRUID_PROVIDER_H
#define GNC_DRUID_PROVIDER_H

#ifdef __cplusplus
//extern "C" {
#endif

#include <glib.h>
#include <glib-object.h>

#define G_TYPE_GNC_DRUID_PROVIDER	(gnc_druid_provider_get_type())
#define GNC_DRUID_PROVIDER(obj)		G_TYPE_CHECK_INSTANCE_CAST((obj), G_TYPE_GNC_DRUID_PROVIDER, GNCDruidProvider)
#define GNC_DRUID_PROVIDER_CLASS(klass)	G_TYPE_CHECK_CLASS_CAST((klass), G_TYPE_GNC_DRUID_PROVIDER, GNCDruidProviderClass)
#define IS_GNC_DRUID_PROVIDER(obj)	G_TYPE_CHECK_INSTANCE_TYPE((obj), G_TYPE_GNC_DRUID_PROVIDER)
#define IS_GNC_DRUID_PROVIDER_CLASS(klass) G_TYPE_CHECK_CLASS_TYPE((klass), G_TYPE_GNC_DRUID_PROVIDER)
#define GNC_DRUID_PROVIDER_GET_CLASS(o)	(G_TYPE_INSTANCE_GET_CLASS ((o), G_TYPE_GNC_DRUID_PROVIDER, GNCDruidProviderClass))

typedef struct _GNCDruidProvider GNCDruidProvider;
typedef struct _GNCDruidProviderClass GNCDruidProviderClass;

#include "gnc-druid.h"
#include "gnc-druid-provider-desc.h"

struct _GNCDruidProvider
{
  GObject obj;

  GNCDruid *druid;		/* The druid this object belongs to,
				 * inserted by this class. */
  GNCDruidProviderDesc *desc;	/* The description for this provider.
				 * inserted by this class,
				 * destroyed at finalize() */
  GList *pages;			/* list of ALL druid pages created by the
				 * subclass. destroyed at finalize() */
  GNCDruidPage* page;		/* The current page, used for reference */
};

struct _GNCDruidProviderClass
{
  GObjectClass parent_class;

  /* virtual methods */

  GNCDruidPage* (*first_page)(GNCDruidProvider*);
  GNCDruidPage* (*last_page)(GNCDruidProvider*);
  GNCDruidPage* (*next_page)(GNCDruidProvider*);
  GNCDruidPage* (*prev_page)(GNCDruidProvider*);
};

GType	gnc_druid_provider_get_type(void);
GNCDruidProvider* gnc_druid_provider_new(GNCDruid* druid,
					 GNCDruidProviderDesc* desc);


typedef GNCDruidProvider* (*GNCDruidProviderNew)(GNCDruid*, GNCDruidProviderDesc*);
void	gnc_druid_provider_register(const gchar* ui_type, const gchar* name,
				    GNCDruidProviderNew new_provider);

/* methods */

GNCDruidPage* gnc_druid_provider_first_page(GNCDruidProvider*);
GNCDruidPage* gnc_druid_provider_last_page(GNCDruidProvider*);
GNCDruidPage* gnc_druid_provider_next_page(GNCDruidProvider*);
GNCDruidPage* gnc_druid_provider_prev_page(GNCDruidProvider*);

GList* gnc_druid_provider_get_pages(GNCDruidProvider*);

#ifdef __cplusplus
//}
#endif

#endif /* GNC_DRUID_PROVIDER_H */
