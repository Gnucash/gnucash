/********************************************************************\
 * gnc-druid-provider-desc.h                                        *
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


#ifndef GNC_DRUID_PROVIDER_DESC_H
#define GNC_DRUID_PROVIDER_DESC_H

#ifdef __cplusplus
//extern "C" {
#endif

#include <glib.h>
#include <glib-object.h>

#define G_TYPE_GNC_DRUID_PROVIDER_DESC	gnc_druid_provider_desc_get_type()
#define GNC_DRUID_PROVIDER_DESC(obj)	G_TYPE_CHECK_INSTANCE_CAST((obj), G_TYPE_GNC_DRUID_PROVIDER_DESC, GNCDruidProviderDesc)
#define GNC_DRUID_PROVIDER_DESC_CLASS(klass)	G_TYPE_CHECK_CLASS_CAST((klass), G_TYPE_GNC_DRUID_PROVIDER_DESC, GNCDruidProviderDescClass)
#define IS_GNC_DRUID_PROVIDER_DESC(obj)	G_TYPE_CHECK_INSTANCE_TYPE((obj), G_TYPE_GNC_DRUID_PROVIDER_DESC)
#define IS_GNC_DRUID_PROVIDER_DESC_CLASS(klass)	G_TYPE_CHECK_CLASS_TYPE((klass), G_TYPE_GNC_DRUID_PROVIDER_DESC)

typedef struct _GNCDruidProviderDesc GNCDruidProviderDesc;
typedef struct _GNCDruidProviderDescClass GNCDruidProviderDescClass;

#include "gnc-druid.h"
#include "gnc-druid-provider.h"

/* return TRUE if the page should be changed, FALSE if it should not */
typedef gboolean (*GNCDruidProviderCB)(GNCDruidCB*);

struct _GNCDruidProviderDesc
{
  GObject obj;
  const gchar *name;		/* the (system-provided)name of this provider */
  gchar *title;			/* the (user-supplied) druid page title */

  /* Some providers require these, but not all. */
  GNCDruidProviderCB	next_cb;
  GNCDruidProviderCB	prev_cb;
  GNCDruidProviderCB	provider_needed;

  /* The following are set internally for use by the backend */
  GNCDruidProvider *	provider; /* a pointer to the provider */
};

struct _GNCDruidProviderDescClass
{
  GObjectClass obj;
};

GType	gnc_druid_provider_desc_get_type(void);

void	gnc_druid_provider_desc_set_title(GNCDruidProviderDesc*, const gchar*);

/* methods */

#ifdef __cplusplus
//}
#endif

#endif /* GNC_DRUID_PROVIDER_DESC_H */
