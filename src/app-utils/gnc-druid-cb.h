/********************************************************************\
 * gnc-druid-cb.h                                                   *
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


#ifndef GNC_DRUID_CB_H
#define GNC_DRUID_CB_H

#ifdef __cplusplus
//extern "C" {
#endif

#include <glib.h>
#include <glib-object.h>

#define G_TYPE_GNC_DRUID_CB	gnc_druid_cb_get_type()
#define GNC_DRUID_CB(obj)	G_TYPE_CHECK_INSTANCE_CAST((obj), G_TYPE_GNC_DRUID_CB, GNCDruidCB)
#define GNC_DRUID_CB_CLASS(klass)	G_TYPE_CHECK_CLASS_CAST((klass), G_TYPE_GNC_DRUID_CB, GNCDruidCBClass)
#define IS_GNC_DRUID_CB(obj)	G_TYPE_CHECK_INSTANCE_TYPE((obj), G_TYPE_GNC_DRUID_CB)
#define IS_GNC_DRUID_CB_CLASS(klass)	G_TYPE_CHECK_CLASS_TYPE((klass), G_TYPE_GNC_DRUID_CB)

typedef struct _GNCDruidCB GNCDruidCB;
typedef struct _GNCDruidCBClass GNCDruidCBClass;

#include "gnc-druid.h"
#include "gnc-druid-provider.h"

struct _GNCDruidCB
{
  GObject obj;

  GNCDruid* druid_ctx;
  GNCDruidProvider* prov_ctx;
  gpointer be_ctx;
};

struct _GNCDruidCBClass
{
  GObjectClass parent_class;
};

GType	gnc_druid_cb_get_type(void);
GNCDruidCB* gnc_druid_cb_new(void);

/* methods */

#ifdef __cplusplus
//}
#endif

#endif /* GNC_DRUID_CB_H */
