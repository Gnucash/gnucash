/********************************************************************\
 * gnc-druid-provider-desc-edge.c                                   *
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
#include "gnc-druid-provider-desc-edge.h"
#include "gnc-basic-gobject.h"

static void gnc_druid_provider_desc_edge_class_init	(GNCDruidProviderDescEdgeClass *class);
static void gnc_druid_provider_desc_edge_init		(GNCDruidProviderDescEdge *gspaper);
static void gnc_druid_provider_desc_edge_finalize	(GObject *obj);

static GNCDruidProviderDescClass *parent_class;

GNC_BASIC_GOBJECT(GNCDruidProviderDescEdge, GNCDruidProviderDescEdgeClass,
                  G_TYPE_GNC_DRUID_PROVIDER_DESC,
                  gnc_druid_provider_desc_edge_class_init,
                  gnc_druid_provider_desc_edge_init,
                  gnc_druid_provider_desc_edge_get_type,
                  gnc_druid_provider_desc_edge_new)

static void
gnc_druid_provider_desc_edge_class_init (GNCDruidProviderDescEdgeClass *klass)
{
    GObjectClass *object_class;

    object_class = G_OBJECT_CLASS (klass);
    parent_class = g_type_class_peek_parent (klass);

    object_class->finalize = gnc_druid_provider_desc_edge_finalize;
}

static void
gnc_druid_provider_desc_edge_finalize (GObject *obj)
{
    GNCDruidProviderDescEdge *desc = (GNCDruidProviderDescEdge *)obj;

    if (desc->text)
        g_free(desc->text);

    G_OBJECT_CLASS (parent_class)->finalize(obj);
}

static void
gnc_druid_provider_desc_edge_init (GNCDruidProviderDescEdge *o)
{
    o->parent.name = GNC_DRUID_PROVIDER_TYPE_EDGE;
}

GNCDruidProviderDescEdge*
gnc_druid_provider_desc_edge_new_with_data(GNCDruidProviderDescEdgeWhich which,
        const gchar* title,
        const gchar* text)
{
    GNCDruidProviderDescEdge* desc;

    desc = gnc_druid_provider_desc_edge_new();
    g_assert(desc);

    gnc_druid_provider_desc_edge_set_which(desc, which);
    if (text)
        gnc_druid_provider_desc_edge_set_text(desc, text);
    if (title)
        gnc_druid_provider_desc_set_title(&(desc->parent), title);

    return desc;
}

void
gnc_druid_provider_desc_edge_set_text(GNCDruidProviderDescEdge* desc,
                                      const gchar* text)
{
    g_return_if_fail(desc);
    g_return_if_fail(IS_GNC_DRUID_PROVIDER_DESC_EDGE(desc));
    g_return_if_fail(text);

    if (desc->text)
        g_free(desc->text);
    desc->text = g_strdup(text);
}

void
gnc_druid_provider_desc_edge_set_which(GNCDruidProviderDescEdge* desc,
                                       GNCDruidProviderDescEdgeWhich which)
{
    g_return_if_fail(desc);
    g_return_if_fail(IS_GNC_DRUID_PROVIDER_DESC_EDGE(desc));

    desc->first_or_last = which;
}
