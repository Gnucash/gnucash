

#ifndef GNC_IMPORT_FORMAT_GNOME_H
#define GNC_IMPORT_FORMAT_GNOME_H

#ifdef __cplusplus
//extern "C" {
#endif

#include <glib.h>
#include <glib-object.h>
#include <libgnomeui/libgnomeui.h>
#include "gnc-druid.h"
#include "gnc-import-format-cb.h"

#define G_TYPE_GNC_IMPORT_FORMAT_GNOME	(gnc_import_format_gnome_get_type())
#define GNC_IMPORT_FORMAT_GNOME(obj)		G_TYPE_CHECK_INSTANCE_CAST((obj), G_TYPE_GNC_IMPORT_FORMAT_GNOME, GNCImportProvFormatGnome)
#define GNC_IMPORT_FORMAT_GNOME_CLASS(klass)	G_TYPE_CHECK_CLASS_CAST((klass), G_TYPE_GNC_IMPORT_FORMAT_GNOME, GNCImportProvFormatGnomeClass)
#define IS_GNC_IMPORT_FORMAT_GNOME(obj)	G_TYPE_CHECK_INSTANCE_TYPE((obj), G_TYPE_GNC_IMPORT_FORMAT_GNOME)
#define IS_GNC_IMPORT_FORMAT_GNOME_CLASS(klass) G_TYPE_CHECK_CLASS_TYPE((klass), G_TYPE_GNC_IMPORT_FORMAT_GNOME)
#define GNC_IMPORT_FORMAT_GNOME_GET_CLASS(o)	(G_TYPE_INSTANCE_GET_CLASS((o), G_TYPE_GNC_IMPORT_FORMAT_GNOME, GNCImportProvFormatGnomeClass))

typedef struct _GNCImportProvFormatGnome GNCImportProvFormatGnome;
typedef struct _GNCImportProvFormatGnomeClass GNCImportProvFormatGnomeClass;

struct _GNCImportProvFormatGnome
{
    GNCDruidProvider parent;

    GnomeDruidPage*	page;
    GtkComboBox*		format_combo;
    GtkLabel*		sample_label;

    GncImportFormat	choice;

    GNCImportFormatCB* cb;
};

struct _GNCImportProvFormatGnomeClass
{
    GNCDruidProviderClass parent_class;
};

GType	gnc_import_format_gnome_get_type(void);
void	gnc_import_format_gnome_register(void);

/* methods */

#ifdef __cplusplus
//}
#endif

#endif /* GNC_IMPORT_FORMAT_GNOME_H */
