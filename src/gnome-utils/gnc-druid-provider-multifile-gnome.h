

#ifndef GNC_DRUID_PROVIDER_MULTIFILE_GNOME_H
#define GNC_DRUID_PROVIDER_MULTIFILE_GNOME_H

#ifdef __cplusplus
//extern "C" {
#endif

#include <glob.h>
#include <glib.h>
#include <glib-object.h>
#include <libgnomeui/libgnomeui.h>
#include "gnc-druid.h"

#define G_TYPE_GNC_DRUID_PROVIDER_MULTIFILE_GNOME	(gnc_druid_provider_multifile_gnome_get_type())
#define GNC_DRUID_PROVIDER_MULTIFILE_GNOME(obj)		G_TYPE_CHECK_INSTANCE_CAST((obj), G_TYPE_GNC_DRUID_PROVIDER_MULTIFILE_GNOME, GNCDruidProviderMultifileGnome)
#define GNC_DRUID_PROVIDER_MULTIFILE_GNOME_CLASS(klass)	G_TYPE_CHECK_CLASS_CAST((klass), G_TYPE_GNC_DRUID_PROVIDER_MULTIFILE_GNOME, GNCDruidProviderMultifileGnomeClass)
#define IS_GNC_DRUID_PROVIDER_MULTIFILE_GNOME(obj)	G_TYPE_CHECK_INSTANCE_TYPE((obj), G_TYPE_GNC_DRUID_PROVIDER_MULTIFILE_GNOME)
#define IS_GNC_DRUID_PROVIDER_MULTIFILE_GNOME_CLASS(klass) G_TYPE_CHECK_CLASS_TYPE((klass), G_TYPE_GNC_DRUID_PROVIDER_MULTIFILE_GNOME)
#define GNC_DRUID_PROVIDER_MULTIFILE_GNOME_GET_CLASS(o)	(G_TYPE_INSTANCE_GET_CLASS((o), G_TYPE_GNC_DRUID_PROVIDER_MULTIFILE_GNOME, GNCDruidProviderMultifileGnomeClass))

typedef struct _GNCDruidProviderMultifileGnome GNCDruidProviderMultifileGnome;
typedef struct _GNCDruidProviderMultifileGnomeClass GNCDruidProviderMultifileGnomeClass;

struct _GNCDruidProviderMultifileGnome
{
  GNCDruidProvider parent;

  GnomeDruidPage*	page;
  GtkWidget*		list;

  GNCDruidCB*		cb;
  gpointer		selected_file;
};

struct _GNCDruidProviderMultifileGnomeClass
{
  GNCDruidProviderClass parent_class;
};

GType	gnc_druid_provider_multifile_gnome_get_type(void);
void	gnc_druid_provider_multifile_gnome_register(void);

/* methods */

#ifdef __cplusplus
//}
#endif

#endif /* GNC_DRUID_PROVIDER_MULTIFILE_GNOME_H */
