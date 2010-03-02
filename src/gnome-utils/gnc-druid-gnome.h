

#ifndef GNC_DRUID_GNOME_H
#define GNC_DRUID_GNOME_H

#ifdef __cplusplus
//extern "C" {
#endif

#include <glib.h>
#include <glib-object.h>
#include <libgnomeui/libgnomeui.h>
#include "gnc-druid.h"
#include "gnc-druid-gnome-ui.h"

#define G_TYPE_GNC_DRUID_GNOME	(gnc_druid_gnome_get_type())
#define GNC_DRUID_GNOME(obj)		G_TYPE_CHECK_INSTANCE_CAST((obj), G_TYPE_GNC_DRUID_GNOME, GNCDruidGnome)
#define GNC_DRUID_GNOME_CLASS(klass)	G_TYPE_CHECK_CLASS_CAST((klass), G_TYPE_GNC_DRUID_GNOME, GNCDruidGnomeClass)
#define IS_GNC_DRUID_GNOME(obj)	G_TYPE_CHECK_INSTANCE_TYPE((obj), G_TYPE_GNC_DRUID_GNOME)
#define IS_GNC_DRUID_GNOME_CLASS(klass) G_TYPE_CHECK_CLASS_TYPE((klass), G_TYPE_GNC_DRUID_GNOME)
#define GNC_DRUID_GNOME_GET_CLASS(o)	(G_TYPE_INSTANCE_GET_CLASS((o), G_TYPE_GNC_DRUID_GNOME, GNCDruidGnomeClass))

typedef struct _GNCDruidGnome GNCDruidGnome;
typedef struct _GNCDruidGnomeClass GNCDruidGnomeClass;

struct _GNCDruidGnome
{
    GNCDruid parent;

    GnomeDruid*	druid;
    GtkWidget*	window;
};

struct _GNCDruidGnomeClass
{
    GNCDruidClass parent_class;
};

GType	gnc_druid_gnome_get_type(void);
void	gnc_druid_gnome_register(void);

/* methods */

#ifdef __cplusplus
//}
#endif

#endif /* GNC_DRUID_GNOME_H */
