/********************************************************************\
 * gnc-druid-provider-desc-edge.h                                   *
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


#ifndef GNC_DRUID_PROVIDER_DESC_EDGE_H
#define GNC_DRUID_PROVIDER_DESC_EDGE_H

#ifdef __cplusplus
//extern "C" {
#endif

#include <glib.h>
#include <glib-object.h>

#define G_TYPE_GNC_DRUID_PROVIDER_DESC_EDGE	gnc_druid_provider_desc_edge_get_type()
#define GNC_DRUID_PROVIDER_DESC_EDGE(obj)	G_TYPE_CHECK_INSTANCE_CAST((obj), G_TYPE_GNC_DRUID_PROVIDER_DESC_EDGE, GNCDruidProviderDescEdge)
#define IS_GNC_DRUID_PROVIDER_DESC_EDGE(obj)	G_TYPE_CHECK_INSTANCE_TYPE((obj), G_TYPE_GNC_DRUID_PROVIDER_DESC_EDGE)

typedef struct _GNCDruidProviderDescEdge GNCDruidProviderDescEdge;
typedef struct _GNCDruidProviderDescEdgeClass GNCDruidProviderDescEdgeClass;

#include "gnc-druid.h"

typedef enum {
  GNC_DPE_FIRST = 1,
  GNC_DPE_LAST
} GNCDruidProviderDescEdgeWhich;

#define GNC_DRUID_PROVIDER_TYPE_EDGE	"edge"

struct _GNCDruidProviderDescEdge
{
  GNCDruidProviderDesc parent;

  gchar* text;
  GNCDruidProviderDescEdgeWhich first_or_last;
};

struct _GNCDruidProviderDescEdgeClass
{
  GNCDruidProviderDescClass parent;
};

GType	gnc_druid_provider_desc_edge_get_type(void);
GNCDruidProviderDescEdge* gnc_druid_provider_desc_edge_new(void);
GNCDruidProviderDescEdge*
gnc_druid_provider_desc_edge_new_with_data(GNCDruidProviderDescEdgeWhich,
					   const gchar* title,
					   const gchar* text);

void	gnc_druid_provider_desc_edge_set_text(GNCDruidProviderDescEdge*,
					       const gchar* text);
void	gnc_druid_provider_desc_edge_set_which(GNCDruidProviderDescEdge*,
					       GNCDruidProviderDescEdgeWhich);


/* methods */

#ifdef __cplusplus
//}
#endif

#endif /* GNC_DRUID_PROVIDER_DESC_EDGE_H */
