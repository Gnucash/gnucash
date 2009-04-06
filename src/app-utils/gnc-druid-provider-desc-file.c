/********************************************************************\
 * gnc-druid-provider-desc-file.c                                   *
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


#include "config.h"
#include "gnc-druid-provider-desc-file.h"
#include "gnc-basic-gobject.h"

static void gnc_druid_provider_desc_file_class_init	(GNCDruidProviderDescFileClass *class);
static void gnc_druid_provider_desc_file_init		(GNCDruidProviderDescFile *gspaper);
static void gnc_druid_provider_desc_file_finalize	(GObject *obj);

static GNCDruidProviderDescClass *parent_class;

GNC_BASIC_GOBJECT(GNCDruidProviderDescFile, GNCDruidProviderDescFileClass,
		  G_TYPE_GNC_DRUID_PROVIDER_DESC,
		  gnc_druid_provider_desc_file_class_init,
		  gnc_druid_provider_desc_file_init,
		  gnc_druid_provider_desc_file_get_type,
		  gnc_druid_provider_desc_file_new)

static void
gnc_druid_provider_desc_file_class_init (GNCDruidProviderDescFileClass *klass)
{
  GObjectClass *object_class;
	
  object_class = G_OBJECT_CLASS (klass);
  parent_class = g_type_class_peek_parent (klass);

  object_class->finalize = gnc_druid_provider_desc_file_finalize;
}

static void
gnc_druid_provider_desc_file_finalize (GObject *obj)
{
  GNCDruidProviderDescFile *desc = (GNCDruidProviderDescFile *)obj;

  if (desc->text)
    g_free(desc->text);
  if (desc->last_dir)
    g_free(desc->last_dir);
  if (desc->history_id)
    g_free(desc->history_id);

  G_OBJECT_CLASS (parent_class)->finalize(obj);
}

static void
gnc_druid_provider_desc_file_init (GNCDruidProviderDescFile *o)
{
  o->parent.name = GNC_DRUID_PROVIDER_TYPE_FILE;
}

GNCDruidProviderDescFile*
gnc_druid_provider_desc_file_new_with_data(const gchar* title,
					   const gchar* text,
					   const gchar* history_id,
					   const gchar* last_dir,
					   gboolean glob,
					   GNCDruidProviderCB next_cb,
					   void (*remove_file)(gpointer, gpointer))
{
  GNCDruidProviderDescFile* desc;

  desc = gnc_druid_provider_desc_file_new();
  g_assert(desc);

  desc->parent.next_cb = next_cb;
  desc->remove_file = remove_file;
  desc->glob = glob;

  if (text)
    gnc_druid_provider_desc_file_set_text(desc, text);
  if (history_id)
    gnc_druid_provider_desc_file_set_history_id(desc, history_id);
  if (last_dir)
    gnc_druid_provider_desc_file_set_last_dir(desc, last_dir);
  if (title)
    gnc_druid_provider_desc_set_title(&(desc->parent), title);

  return desc;
}

void
gnc_druid_provider_desc_file_set_text(GNCDruidProviderDescFile* desc,
				      const gchar* text)
{
  g_return_if_fail(desc);
  g_return_if_fail(IS_GNC_DRUID_PROVIDER_DESC_FILE(desc));
  g_return_if_fail(text);

  if (desc->text)
    g_free(desc->text);
  desc->text = g_strdup(text);
}

void
gnc_druid_provider_desc_file_set_last_dir(GNCDruidProviderDescFile* desc,
					  const gchar* last_dir)
{
  g_return_if_fail(desc);
  g_return_if_fail(IS_GNC_DRUID_PROVIDER_DESC_FILE(desc));
  g_return_if_fail(last_dir);

  if (desc->last_dir)
    g_free(desc->last_dir);
  desc->last_dir = g_strdup(last_dir);
}

void
gnc_druid_provider_desc_file_set_history_id(GNCDruidProviderDescFile* desc,
					    const gchar* history_id)
{
  g_return_if_fail(desc);
  g_return_if_fail(IS_GNC_DRUID_PROVIDER_DESC_FILE(desc));
  g_return_if_fail(history_id);

  if (desc->history_id)
    g_free(desc->history_id);
  desc->history_id = g_strdup(history_id);
}
