/*
 * gnc-dense-cal-model.c
 *
 * Copyright (C) 2006 Joshua Sled <jsled@asynchronous.org>
 *
 * This program is free software; you can redistribute it and/or modify it
 * under the terms of version 2 of the GNU General Public License as
 * published by the Free Software Foundation.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License*
 * along with this program; if not, contact:
 *
 * Free Software Foundation           Voice:  +1-617-542-5942
 * 51 Franklin Street, Fifth Floor    Fax:    +1-617-542-2652
 * Boston, MA  02110-1301,  USA       gnu@gnu.org
 */


#include "config.h"
#include <glib.h>
#include <glib-object.h>
#include "gnc-dense-cal.h"
#include "gnc-dense-cal-model.h"

enum { GDCM_ADDED, GDCM_UPDATE, GDCM_REMOVE, LAST_SIGNAL };
static guint gnc_dense_cal_model_signals[LAST_SIGNAL] = { 0 };

static void
gnc_dense_cal_model_base_init(gpointer g_class)
{
     static gboolean initialized = FALSE;
     
     if (!initialized)
     {
          gnc_dense_cal_model_signals[GDCM_ADDED]
               = g_signal_new("added",
                              G_TYPE_FROM_CLASS(g_class),
                              G_SIGNAL_RUN_LAST | G_SIGNAL_NO_RECURSE | G_SIGNAL_NO_HOOKS,
                              0 /* default offset */,
                              NULL /* accumulator */,
                              NULL /* accum. data */,
                              g_cclosure_marshal_VOID__UINT,
                              G_TYPE_NONE /* return */,
                              1 /* n_params */,
                              G_TYPE_UINT /* param types */
                    );

          gnc_dense_cal_model_signals[GDCM_UPDATE]
               = g_signal_new("update",
                              G_TYPE_FROM_CLASS(g_class),
                              G_SIGNAL_RUN_LAST | G_SIGNAL_NO_RECURSE | G_SIGNAL_NO_HOOKS,
                              0 /* default offset */,
                              NULL /* accumulator */,
                              NULL /* accum. data */,
                              g_cclosure_marshal_VOID__UINT,
                              G_TYPE_NONE /* return */,
                              1 /* n_params */,
                              G_TYPE_UINT /* param types */
                    );

          gnc_dense_cal_model_signals[GDCM_REMOVE]
               = g_signal_new("removing",
                              G_TYPE_FROM_CLASS(g_class),
                              G_SIGNAL_RUN_LAST | G_SIGNAL_NO_RECURSE | G_SIGNAL_NO_HOOKS,
                              0 /* default offset */,
                              NULL /* accumulator */,
                              NULL /* accum. data */,
                              g_cclosure_marshal_VOID__UINT,
                              G_TYPE_NONE /* return */,
                              1 /* n_params */,
                              G_TYPE_UINT /* param types */
                    );

          initialized = TRUE;
     }
}

GType
gnc_dense_cal_model_get_type(void)
{
     static GType type = 0;
     if (type == 0) {
          static const GTypeInfo info = {
               sizeof(GncDenseCalModelIface),
               gnc_dense_cal_model_base_init,   /* base_init */
               NULL,   /* base_finalize */
               NULL,   /* class_init */
               NULL,   /* class_finalize */
               NULL,   /* class_data */
               0,
               0,      /* n_preallocs */
               NULL    /* instance_init */
          };
          type = g_type_register_static(G_TYPE_INTERFACE, "GncDenseCalModel", &info, 0);
     }
     return type;
}

GList*
gnc_dense_cal_model_get_contained(GncDenseCalModel *model)
{
     return (*GNC_DENSE_CAL_MODEL_GET_INTERFACE(model)->get_contained)(model);
}

gchar*
gnc_dense_cal_model_get_name(GncDenseCalModel *model, guint tag)
{
     return (*GNC_DENSE_CAL_MODEL_GET_INTERFACE(model)->get_name)(model, tag);
}

gchar*
gnc_dense_cal_model_get_info(GncDenseCalModel *model, guint tag)
{
     return (*GNC_DENSE_CAL_MODEL_GET_INTERFACE(model)->get_info)(model, tag);
}

gint
gnc_dense_cal_model_get_instance_count(GncDenseCalModel *model, guint tag)
{
     return (*GNC_DENSE_CAL_MODEL_GET_INTERFACE(model)->get_instance_count)(model, tag);
}

void
gnc_dense_cal_model_get_instance(GncDenseCalModel *model, guint tag, gint instance_index, GDate *date)
{
     return (*GNC_DENSE_CAL_MODEL_GET_INTERFACE(model)->get_instance)(model, tag, instance_index, date);
}
