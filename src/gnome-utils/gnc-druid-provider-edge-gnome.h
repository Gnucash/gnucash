

#ifndef GNC_DRUID_PROVIDER_EDGE_GNOME_H
#define GNC_DRUID_PROVIDER_EDGE_GNOME_H

#ifdef __cplusplus
//extern "C" {
#endif

#include <glib.h>
#include <glib-object.h>
#include <libgnomeui/libgnomeui.h>
#include "gnc-druid.h"

#define G_TYPE_GNC_DRUID_PROVIDER_EDGE_GNOME	(gnc_druid_provider_edge_gnome_get_type())
#define GNC_DRUID_PROVIDER_EDGE_GNOME(obj)		G_TYPE_CHECK_INSTANCE_CAST((obj), G_TYPE_GNC_DRUID_PROVIDER_EDGE_GNOME, GNCDruidProviderEdgeGnome)
#define GNC_DRUID_PROVIDER_EDGE_GNOME_CLASS(klass)	G_TYPE_CHECK_CLASS_CAST((klass), G_TYPE_GNC_DRUID_PROVIDER_EDGE_GNOME, GNCDruidProviderEdgeGnomeClass)
#define IS_GNC_DRUID_PROVIDER_EDGE_GNOME(obj)	G_TYPE_CHECK_INSTANCE_TYPE((obj), G_TYPE_GNC_DRUID_PROVIDER_EDGE_GNOME)
#define IS_GNC_DRUID_PROVIDER_EDGE_GNOME_CLASS(klass) G_TYPE_CHECK_CLASS_TYPE((klass), G_TYPE_GNC_DRUID_PROVIDER_EDGE_GNOME)
#define GNC_DRUID_PROVIDER_EDGE_GNOME_GET_CLASS(o)	(G_TYPE_INSTANCE_GET_CLASS((o), G_TYPE_GNC_DRUID_PROVIDER_EDGE_GNOME, GNCDruidProviderEdgeGnomeClass))

typedef struct _GNCDruidProviderEdgeGnome GNCDruidProviderEdgeGnome;
typedef struct _GNCDruidProviderEdgeGnomeClass GNCDruidProviderEdgeGnomeClass;

struct _GNCDruidProviderEdgeGnome
{
    GNCDruidProvider parent;

    GnomeDruidPageEdge*	page;
};

struct _GNCDruidProviderEdgeGnomeClass
{
    GNCDruidProviderClass parent_class;
};

GType	gnc_druid_provider_edge_gnome_get_type(void);
void	gnc_druid_provider_edge_gnome_register(void);

/* methods */

#ifdef __cplusplus
//}
#endif

#endif /* GNC_DRUID_PROVIDER_EDGE_GNOME_H */
