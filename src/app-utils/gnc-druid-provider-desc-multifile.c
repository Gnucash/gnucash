/********************************************************************\
 * gnc-druid-provider-desc-multifile.c                              *
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
#include "gnc-druid-provider-desc-multifile.h"
#include "gnc-basic-gobject.h"

static void gnc_druid_provider_desc_multifile_class_init	(GNCDruidProviderDescMultifileClass *class);
static void gnc_druid_provider_desc_multifile_init		(GNCDruidProviderDescMultifile *gspaper);
static void gnc_druid_provider_desc_multifile_finalize	(GObject *obj);

static GNCDruidProviderDescClass *parent_class;

GNC_BASIC_GOBJECT(GNCDruidProviderDescMultifile, GNCDruidProviderDescMultifileClass,
                  G_TYPE_GNC_DRUID_PROVIDER_DESC,
                  gnc_druid_provider_desc_multifile_class_init,
                  gnc_druid_provider_desc_multifile_init,
                  gnc_druid_provider_desc_multifile_get_type,
                  gnc_druid_provider_desc_multifile_new)

static void
gnc_druid_provider_desc_multifile_class_init (GNCDruidProviderDescMultifileClass *klass)
{
    GObjectClass *object_class;

    object_class = G_OBJECT_CLASS (klass);
    parent_class = g_type_class_peek_parent (klass);

    object_class->finalize = gnc_druid_provider_desc_multifile_finalize;
}

static void
gnc_druid_provider_desc_multifile_finalize (GObject *obj)
{
    GNCDruidProviderDescMultifile *desc = (GNCDruidProviderDescMultifile *)obj;

    if (desc->text)
        g_free(desc->text);

    G_OBJECT_CLASS (parent_class)->finalize(obj);
}

static void
gnc_druid_provider_desc_multifile_init (GNCDruidProviderDescMultifile *o)
{
    o->parent.name = GNC_DRUID_PROVIDER_TYPE_MULTIFILE;
}

GNCDruidProviderDescMultifile*
gnc_druid_provider_desc_multifile_new_with_data(const gchar* title,
        const gchar* text,
        GNCDruidProviderDescFile *file_prov,
        GNCDruidProviderCB next_cb,
        GList* (*get_files)(gpointer),
        const gchar* (*get_filename)(gpointer, gpointer))
{
    GNCDruidProviderDescMultifile* desc;

    desc = gnc_druid_provider_desc_multifile_new();
    g_assert(desc);

    desc->file_provider = file_prov;
    desc->get_files = get_files;
    desc->get_filename = get_filename;
    desc->parent.next_cb = next_cb;

    if (text)
        gnc_druid_provider_desc_multifile_set_text(desc, text);
    if (title)
        gnc_druid_provider_desc_set_title(&(desc->parent), title);

    return desc;
}

void
gnc_druid_provider_desc_multifile_set_text(GNCDruidProviderDescMultifile* desc,
        const gchar* text)
{
    g_return_if_fail(desc);
    g_return_if_fail(IS_GNC_DRUID_PROVIDER_DESC_MULTIFILE(desc));
    g_return_if_fail(text);

    if (desc->text)
        g_free(desc->text);
    desc->text = g_strdup(text);
}
