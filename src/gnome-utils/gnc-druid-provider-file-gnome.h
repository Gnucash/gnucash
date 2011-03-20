

#ifndef GNC_DRUID_PROVIDER_FILE_GNOME_H
#define GNC_DRUID_PROVIDER_FILE_GNOME_H

#ifdef __cplusplus
//extern "C" {
#endif

#ifdef HAVE_GLOB_H
# include <glob.h>
#else
# include <stddef.h> /* for size_t */
typedef struct
{
    size_t gl_pathc;    /* Count of paths matched so far  */
    char **gl_pathv;    /* List of matched pathnames.  */
    size_t gl_offs;     /* Slots to reserve in `gl_pathv'.  */
} glob_t;
#endif

#include <glib.h>
#include <glib-object.h>
#include <libgnomeui/libgnomeui.h>
#include "gnc-druid.h"
#include "gnc-druid-provider-file-cb.h"

#define G_TYPE_GNC_DRUID_PROVIDER_FILE_GNOME	(gnc_druid_provider_file_gnome_get_type())
#define GNC_DRUID_PROVIDER_FILE_GNOME(obj)		G_TYPE_CHECK_INSTANCE_CAST((obj), G_TYPE_GNC_DRUID_PROVIDER_FILE_GNOME, GNCDruidProviderFileGnome)
#define GNC_DRUID_PROVIDER_FILE_GNOME_CLASS(klass)	G_TYPE_CHECK_CLASS_CAST((klass), G_TYPE_GNC_DRUID_PROVIDER_FILE_GNOME, GNCDruidProviderFileGnomeClass)
#define IS_GNC_DRUID_PROVIDER_FILE_GNOME(obj)	G_TYPE_CHECK_INSTANCE_TYPE((obj), G_TYPE_GNC_DRUID_PROVIDER_FILE_GNOME)
#define IS_GNC_DRUID_PROVIDER_FILE_GNOME_CLASS(klass) G_TYPE_CHECK_CLASS_TYPE((klass), G_TYPE_GNC_DRUID_PROVIDER_FILE_GNOME)
#define GNC_DRUID_PROVIDER_FILE_GNOME_GET_CLASS(o)	(G_TYPE_INSTANCE_GET_CLASS((o), G_TYPE_GNC_DRUID_PROVIDER_FILE_GNOME, GNCDruidProviderFileGnomeClass))

typedef struct _GNCDruidProviderFileGnome GNCDruidProviderFileGnome;
typedef struct _GNCDruidProviderFileGnomeClass GNCDruidProviderFileGnomeClass;

struct _GNCDruidProviderFileGnome
{
    GNCDruidProvider parent;

    GnomeDruidPage*	page;
    GtkFileChooser*       file_entry;

    GNCDruidProviderFileCB* cb;

    size_t		count;
    glob_t		glob;
    gboolean		globbed;
};

struct _GNCDruidProviderFileGnomeClass
{
    GNCDruidProviderClass parent_class;
};

GType	gnc_druid_provider_file_gnome_get_type(void);
void	gnc_druid_provider_file_gnome_register(void);

/* methods */

#ifdef __cplusplus
//}
#endif

#endif /* GNC_DRUID_PROVIDER_FILE_GNOME_H */
