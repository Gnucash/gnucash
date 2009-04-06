/********************************************************************\
 * gnc-druid-provider-desc-multifile.h                              *
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


#ifndef GNC_DRUID_PROVIDER_DESC_MULTIFILE_H
#define GNC_DRUID_PROVIDER_DESC_MULTIFILE_H

#ifdef __cplusplus
//extern "C" {
#endif

#include <glib.h>
#include <glib-object.h>

#define G_TYPE_GNC_DRUID_PROVIDER_DESC_MULTIFILE	gnc_druid_provider_desc_multifile_get_type()
#define GNC_DRUID_PROVIDER_DESC_MULTIFILE(obj)	G_TYPE_CHECK_INSTANCE_CAST((obj), G_TYPE_GNC_DRUID_PROVIDER_DESC_MULTIFILE, GNCDruidProviderDescMultifile)
#define IS_GNC_DRUID_PROVIDER_DESC_MULTIFILE(obj)	G_TYPE_CHECK_INSTANCE_TYPE((obj), G_TYPE_GNC_DRUID_PROVIDER_DESC_MULTIFILE)

typedef struct _GNCDruidProviderDescMultifile GNCDruidProviderDescMultifile;
typedef struct _GNCDruidProviderDescMultifileClass GNCDruidProviderDescMultifileClass;

#include "gnc-druid.h"
#include "gnc-druid-provider-desc-file.h"

#define GNC_DRUID_PROVIDER_TYPE_MULTIFILE	"multifile"

struct _GNCDruidProviderDescMultifile
{
  GNCDruidProviderDesc parent;

  gchar* text;
  GNCDruidProviderDescFile* file_provider;
  GList* (*get_files)(gpointer be_ctx);
  const gchar* (*get_filename)(gpointer be_ctx, gpointer file_ctx);
};

struct _GNCDruidProviderDescMultifileClass
{
  GNCDruidProviderDescClass parent;
};

GType	gnc_druid_provider_desc_multifile_get_type(void);
GNCDruidProviderDescMultifile* gnc_druid_provider_desc_multifile_new(void);
GNCDruidProviderDescMultifile*
gnc_druid_provider_desc_multifile_new_with_data(const gchar* title,
						const gchar* text,
						GNCDruidProviderDescFile *file_prov,
						GNCDruidProviderCB next_cb,
						GList* (*get_files)(gpointer),
						const gchar* (*get_filename)(gpointer, gpointer));

void	gnc_druid_provider_desc_multifile_set_text(GNCDruidProviderDescMultifile*,
						   const gchar* text);

/* methods */

#ifdef __cplusplus
//}
#endif

#endif /* GNC_DRUID_PROVIDER_DESC_MULTIFILE_H */
