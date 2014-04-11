/********************************************************************\
 * gnc-druid-provider-file-cb.h                                     *
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


#ifndef GNC_DRUID_PROVIDER_FILE_CB_H
#define GNC_DRUID_PROVIDER_FILE_CB_H

#ifdef __cplusplus
//extern "C" {
#endif

#include <glib.h>
#include <glib-object.h>

#define G_TYPE_GNC_DRUID_PROVIDER_FILE_CB	gnc_druid_provider_file_cb_get_type()
#define GNC_DRUID_PROVIDER_FILE_CB(obj)	G_TYPE_CHECK_INSTANCE_CAST((obj), G_TYPE_GNC_DRUID_PROVIDER_FILE_CB, GNCDruidProviderFileCB)
#define GNC_DRUID_PROVIDER_FILE_CB_CLASS(klass)	G_TYPE_CHECK_CLASS_CAST((klass), G_TYPE_GNC_DRUID_PROVIDER_FILE_CB, GNCDruidProviderFileCBClass)
#define IS_GNC_DRUID_PROVIDER_FILE_CB(obj)	G_TYPE_CHECK_INSTANCE_TYPE((obj), G_TYPE_GNC_DRUID_PROVIDER_FILE_CB)
#define IS_GNC_DRUID_PROVIDER_FILE_CB_CLASS(klass)	G_TYPE_CHECK_CLASS_TYPE((klass), G_TYPE_GNC_DRUID_PROVIDER_FILE_CB)

typedef struct _GNCDruidProviderFileCB GNCDruidProviderFileCB;
typedef struct _GNCDruidProviderFileCBClass GNCDruidProviderFileCBClass;

#include "gnc-druid-cb.h"

struct _GNCDruidProviderFileCB
{
  GNCDruidCB parent;

  const gchar* filename;	/* owned by the provider */

  gpointer this_file;		/* set by the backend (return to the provider) */
};

struct _GNCDruidProviderFileCBClass
{
  GNCDruidCBClass parent_class;
};

GType	gnc_druid_provider_file_cb_get_type(void);
GNCDruidProviderFileCB* gnc_druid_provider_file_cb_new(void);

/* methods */

#ifdef __cplusplus
//}
#endif

#endif /* GNC_DRUID_PROVIDER_FILE_CB_H */
