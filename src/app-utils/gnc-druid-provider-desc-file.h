/********************************************************************\
 * gnc-druid-provider-desc-file.h                                   *
 *                                                                  *
 * This program is free software; you can redistribute it and/or    *
 * modify it under the terms of the GNU General Public License as   *
 * published by the Free Software Foundation; either version 2 of   *
 * the License, or (at your option) any later version.              *
 *                                                                  *
 * This program is distributed in the hope that it will be useful,  *
 * but WITHOUT ANY WARRANTY; without even the implied warranty of   *
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the    *
 * GNU General Public License for more details.                     *
 *                                                                  *
 * You should have received a copy of the GNU General Public License*
 * along with this program; if not, write to the Free Software      *
 * Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.        *
\********************************************************************/


#ifndef GNC_DRUID_PROVIDER_DESC_FILE_H
#define GNC_DRUID_PROVIDER_DESC_FILE_H

#ifdef __cplusplus
//extern "C" {
#endif

#include <glib.h>
#include <glib-object.h>

#define G_TYPE_GNC_DRUID_PROVIDER_DESC_FILE	gnc_druid_provider_desc_file_get_type()
#define GNC_DRUID_PROVIDER_DESC_FILE(obj)	G_TYPE_CHECK_INSTANCE_CAST((obj), G_TYPE_GNC_DRUID_PROVIDER_DESC_FILE, GNCDruidProviderDescFile)
#define IS_GNC_DRUID_PROVIDER_DESC_FILE(obj)	G_TYPE_CHECK_INSTANCE_TYPE((obj), G_TYPE_GNC_DRUID_PROVIDER_DESC_FILE)

typedef struct _GNCDruidProviderDescFile GNCDruidProviderDescFile;
typedef struct _GNCDruidProviderDescFileClass GNCDruidProviderDescFileClass;

#include "gnc-druid.h"
#include "gnc-druid-provider-file-cb.h"
#include "gnc-druid-provider-desc-multifile.h"

#define GNC_DRUID_PROVIDER_TYPE_FILE	"file"

struct _GNCDruidProviderDescFile
{
    GNCDruidProviderDesc parent;

    gchar* text;
    gchar* last_dir;
    gchar* history_id;
    gboolean glob;
    void (*remove_file)(gpointer be_ctx, gpointer file_ctx);

    GNCDruidProviderDescMultifile *multifile_provider;
};

struct _GNCDruidProviderDescFileClass
{
    GNCDruidProviderDescClass parent;
};

GType	gnc_druid_provider_desc_file_get_type(void);
GNCDruidProviderDescFile* gnc_druid_provider_desc_file_new(void);
GNCDruidProviderDescFile*
gnc_druid_provider_desc_file_new_with_data(const gchar* title,
        const gchar* text,
        const gchar* history_id,
        const gchar* last_dir,
        gboolean glob,
        GNCDruidProviderCB next_cb,
        void (*remove_file)(gpointer, gpointer));

void	gnc_druid_provider_desc_file_set_text(GNCDruidProviderDescFile*,
        const gchar* text);
void	gnc_druid_provider_desc_file_set_last_dir(GNCDruidProviderDescFile*,
        const gchar* last_dir);
void	gnc_druid_provider_desc_file_set_history_id(GNCDruidProviderDescFile*,
        const gchar* history_id);

/* methods */

#ifdef __cplusplus
//}
#endif

#endif /* GNC_DRUID_PROVIDER_DESC_FILE_H */
