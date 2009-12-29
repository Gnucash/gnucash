/********************************************************************\
 * gnc-druid.h                                                      *
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


#ifndef GNC_DRUID_H
#define GNC_DRUID_H

#ifdef __cplusplus
//extern "C" {
#endif

#include <glib.h>
#include <glib-object.h>

#define G_TYPE_GNC_DRUID	(gnc_druid_get_type())
#define GNC_DRUID(obj)		G_TYPE_CHECK_INSTANCE_CAST((obj), G_TYPE_GNC_DRUID, GNCDruid)
#define GNC_DRUID_CLASS(klass)	G_TYPE_CHECK_CLASS_CAST((klass), G_TYPE_GNC_DRUID, GNCDruidClass)
#define IS_GNC_DRUID(obj)	G_TYPE_CHECK_INSTANCE_TYPE((obj), G_TYPE_GNC_DRUID)
#define IS_GNC_DRUID_CLASS(klass) G_TYPE_CHECK_CLASS_TYPE((klass), G_TYPE_GNC_DRUID)
#define GNC_DRUID_GET_CLASS(o)	(G_TYPE_INSTANCE_GET_CLASS((o), G_TYPE_GNC_DRUID, GNCDruidClass))

typedef struct _GNCDruid GNCDruid;
typedef struct _GNCDruidClass GNCDruidClass;
typedef void GNCDruidPage;	/* completely opaque.Do we need to know anything? */

#include "gnc-druid-cb.h"
#include "gnc-druid-provider-desc.h"
#include "gnc-druid-provider.h"

struct _GNCDruid
{
    GObject obj;

    /* PROVIDED BY TOOLKIT IMPLEMENTATION... */
    const gchar *ui_type;

    /* PROVIDED HEREIN */
    GList *providers;		/* list of GNCProvider*; list is owned herein */
    GList *this_provider;		/* a pointer to the current provider */
    GNCDruidProvider *provider;	/* current provider */

    /* Backend information */
    gpointer be_ctx;		/* backend context pointer */
    gboolean (*finish)(gpointer);	/* backend 'finish process' pointer */
    void (*cancel)(gpointer);	/* backend 'cancel process' pointer */

    gint jump_count;
};

struct _GNCDruidClass
{
    GObjectClass parent_class;

    /* virtual methods */
    void	(*set_page)(GNCDruid*, GNCDruidPage*);
    void	(*append_provider)(GNCDruid*, GNCDruidProvider*);
};

GType	gnc_druid_get_type(void);

typedef GNCDruid* (*GNCDruidNew)(const char* title);
void	gnc_druid_register_ui(const gchar* ui_type, GNCDruidNew new_druid);

/* methods */

void gnc_druid_set_page(GNCDruid*, GNCDruidPage*);
GNCDruidProvider* gnc_druid_next_provider(GNCDruid*);
GNCDruidProvider* gnc_druid_prev_provider(GNCDruid*);

void gnc_druid_next_page(GNCDruid*);
void gnc_druid_prev_page(GNCDruid*);

/* Reset the druid page by jumping to the provider. */
void gnc_druid_jump_to_provider(GNCDruid*, GNCDruidProvider*);

/* Other functions */

/**
 * gnc_druid_new -- create a druid based on the list of providers
 *                  descriptors.  Hold onto the backend context and
 *                  the function to call when druid is finished.
 *
 * This will assume the "registered ui", or internally perform some
 * magic to figure out which "UI" to use..
 *
 * The provider list (and all the providerdesc objects) are owned by
 * the druid and will be freed by the druid.
 */
GNCDruid* gnc_druid_new(const gchar* title,
                        GList *providers, gpointer backend_ctx,
                        gboolean (*finish)(gpointer be_ctx),
                        void (*cancel)(gpointer be_ctx));

#ifdef __cplusplus
//}
#endif

#endif /* GNC_DRUID_H */
