/*
 * gnc-sx-instance-dense-cal-adapter.c
 *
 * Copyright (C) 2006 Josh Sled <jsled@asynchronous.org>
 *
 * This program is free software; you can redistribute it and/or
 * modify it under the terms of version 2 and/or version 3 of the GNU General Public
 * License as published by the Free Software Foundation.
 *
 * As a special exception, permission is granted to link the binary module
 * resultant from this code with the OpenSSL project's "OpenSSL" library (or
 * modified versions of it that use the same license as the "OpenSSL"
 * library), and distribute the linked executable.  You must obey the GNU
 * General Public License in all respects for all of the code used other than
 * "OpenSSL". If you modify this file, you may extend this exception to your
 * version of the file, but you are not obligated to do so. If you do not
 * wish to do so, delete this exception statement from your version of this
 * file.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, contact:
 *
 * Free Software Foundation           Voice:  +1-617-542-5942
 * 51 Franklin Street, Fifth Floor    Fax:    +1-617-542-2652
 * Boston, MA  02110-1301,  USA       gnu@gnu.org
 */

#include <config.h>
#include <glib.h>
#include "gnc-sx-instance-dense-cal-adapter.h"
#include "gnc-dense-cal.h"
#include <qoflog.h>

#undef G_LOG_DOMAIN
#define G_LOG_DOMAIN "gnc.gui.sx.adapter.sx-dense-cal"
static const QofLogModule log_module = G_LOG_DOMAIN;

static void gnc_sx_instance_dense_cal_adapter_interface_init(GncDenseCalModelInterface *iface);
static void gnc_sx_instance_dense_cal_adapter_dispose(GObject *obj);
static void gnc_sx_instance_dense_cal_adapter_finalize(GObject *obj);

static GList* gsidca_get_contained(GncDenseCalModel *model);
static gchar* gsidca_get_name(GncDenseCalModel *model, guint tag);
static gchar* gsidca_get_info(GncDenseCalModel *model, guint tag);
static gint gsidca_get_instance_count(GncDenseCalModel *model, guint tag);
static void gsidca_get_instance(GncDenseCalModel *model, guint tag, gint instance_index, GDate *date);

struct _GncSxInstanceDenseCalAdapter
{
    GObject parent;
    gboolean disposed;
    GncSxInstanceModel *instances;
};

G_DEFINE_TYPE_WITH_CODE (GncSxInstanceDenseCalAdapter, gnc_sx_instance_dense_cal_adapter, G_TYPE_OBJECT,
    G_IMPLEMENT_INTERFACE (GNC_TYPE_DENSE_CAL_MODEL, gnc_sx_instance_dense_cal_adapter_interface_init))

static void
gnc_sx_instance_dense_cal_adapter_class_init(GncSxInstanceDenseCalAdapterClass *klass)
{
    GObjectClass *obj_class = G_OBJECT_CLASS(klass);

    obj_class->dispose = gnc_sx_instance_dense_cal_adapter_dispose;
    obj_class->finalize = gnc_sx_instance_dense_cal_adapter_finalize;
}

static void
gnc_sx_instance_dense_cal_adapter_init(GncSxInstanceDenseCalAdapter *instance)
{
    /*GncSxInstanceDenseCalAdapter *adapter = GNC_SX_INSTANCE_DENSE_CAL_ADAPTER(instance);*/
    ; /* nop */
}

static void
gnc_sx_instance_dense_cal_adapter_interface_init(GncDenseCalModelInterface *iface)
{
    iface->get_contained = gsidca_get_contained;
    iface->get_name = gsidca_get_name;
    iface->get_info = gsidca_get_info;
    iface->get_instance_count = gsidca_get_instance_count;
    iface->get_instance = gsidca_get_instance;
}

static void
gsidca_instances_added_cb(GncSxInstanceModel *model, SchedXaction *sx_added, gpointer user_data)
{
    GncSxInstanceDenseCalAdapter *adapter = GNC_SX_INSTANCE_DENSE_CAL_ADAPTER(user_data);
    DEBUG("instance added\n");
    if (xaccSchedXactionGetEnabled(sx_added))
    {
        g_signal_emit_by_name(adapter, "added", GPOINTER_TO_UINT(sx_added));
    }
}

static void
gsidca_instances_updated_cb(GncSxInstanceModel *model, SchedXaction *sx_updated, gpointer user_data)
{
    GncSxInstanceDenseCalAdapter *adapter = GNC_SX_INSTANCE_DENSE_CAL_ADAPTER(user_data);
    gnc_sx_instance_model_update_sx_instances(model, sx_updated);
    DEBUG("instances updated\n");
    if (xaccSchedXactionGetEnabled(sx_updated))
    {
        g_signal_emit_by_name(adapter, "update", GPOINTER_TO_UINT((gpointer)sx_updated));
    }
    else
    {
        g_signal_emit_by_name(adapter, "removing", GPOINTER_TO_UINT((gpointer)sx_updated));
    }
}

static void
gsidca_instances_removing_cb(GncSxInstanceModel *model, SchedXaction *sx_to_be_removed, gpointer user_data)
{
    GncSxInstanceDenseCalAdapter *adapter = GNC_SX_INSTANCE_DENSE_CAL_ADAPTER(user_data);
    DEBUG("removing instance...\n");
    g_signal_emit_by_name(adapter, "removing", GPOINTER_TO_UINT(sx_to_be_removed));
    gnc_sx_instance_model_remove_sx_instances(model, sx_to_be_removed);
}

GncSxInstanceDenseCalAdapter*
gnc_sx_instance_dense_cal_adapter_new(GncSxInstanceModel *instances)
{
    GncSxInstanceDenseCalAdapter *adapter = g_object_new(GNC_TYPE_SX_INSTANCE_DENSE_CAL_ADAPTER, NULL);
    adapter->instances = instances;
    g_object_ref(G_OBJECT(adapter->instances));

    g_signal_connect(instances, "added", (GCallback)gsidca_instances_added_cb, adapter);
    g_signal_connect(instances, "updated", (GCallback)gsidca_instances_updated_cb, adapter);
    g_signal_connect(instances, "removing", (GCallback)gsidca_instances_removing_cb, adapter);
    return adapter;
}

static gint
gsidca_find_sx_with_tag(gconstpointer list_data,
                        gconstpointer find_data)
{
    GncSxInstances *sx_instances = (GncSxInstances*)list_data;
    return (GUINT_TO_POINTER(GPOINTER_TO_UINT(sx_instances->sx)) == find_data ? 0 : 1);
}

static GList*
gsidca_get_contained(GncDenseCalModel *model)
{
    GncSxInstanceDenseCalAdapter *adapter = GNC_SX_INSTANCE_DENSE_CAL_ADAPTER(model);
    //"removing return gnc_g_list_map(instances->sxes, sx_to_tag, null);
    GList *list = NULL, *sxes;
    for (sxes = gnc_sx_instance_model_get_sx_instances_list (adapter->instances); sxes != NULL; sxes = sxes->next)
    {
        GncSxInstances *sx_instances = (GncSxInstances*)sxes->data;
        if (xaccSchedXactionGetEnabled(sx_instances->sx))
            list = g_list_prepend (list, GUINT_TO_POINTER
                                   (GPOINTER_TO_UINT (sx_instances->sx)));
    }
    return g_list_reverse (list);
}

static gchar*
gsidca_get_name(GncDenseCalModel *model, guint tag)
{
    GncSxInstanceDenseCalAdapter *adapter = GNC_SX_INSTANCE_DENSE_CAL_ADAPTER(model);
    GncSxInstances *insts
    = (GncSxInstances*)g_list_find_custom(gnc_sx_instance_model_get_sx_instances_list (adapter->instances), GUINT_TO_POINTER(tag), gsidca_find_sx_with_tag)->data;
    if (insts == NULL)
        return NULL;
    return xaccSchedXactionGetName(insts->sx);
}

static gchar*
gsidca_get_info(GncDenseCalModel *model, guint tag)
{
    GncSxInstanceDenseCalAdapter *adapter = GNC_SX_INSTANCE_DENSE_CAL_ADAPTER(model);
    // g_list_find(instances->sxes, {sx_to_tag, tag}).get_freq_spec().get_freq_str();
    GList *schedule;
    gchar *schedule_str;
    GncSxInstances *insts
    = (GncSxInstances*)g_list_find_custom(gnc_sx_instance_model_get_sx_instances_list(adapter->instances), GUINT_TO_POINTER(tag), gsidca_find_sx_with_tag)->data;
    if (insts == NULL)
        return NULL;
    schedule = gnc_sx_get_schedule(insts->sx);
    schedule_str = recurrenceListToCompactString(schedule);
    return schedule_str;
}

static gint
gsidca_get_instance_count(GncDenseCalModel *model, guint tag)
{
    GncSxInstanceDenseCalAdapter *adapter = GNC_SX_INSTANCE_DENSE_CAL_ADAPTER(model);
    // g_list_find(instances->sxes, {sx_to_tag, tag}).length();
    GncSxInstances *insts
    = (GncSxInstances*)g_list_find_custom(gnc_sx_instance_model_get_sx_instances_list(adapter->instances), GUINT_TO_POINTER(tag), gsidca_find_sx_with_tag)->data;
    if (insts == NULL)
        return 0;
    return g_list_length(insts->instance_list);
}

static void
gsidca_get_instance(GncDenseCalModel *model, guint tag, gint instance_index, GDate *date)
{
    GncSxInstanceDenseCalAdapter *adapter = GNC_SX_INSTANCE_DENSE_CAL_ADAPTER(model);
    GncSxInstance *inst;
    GncSxInstances *insts
    = (GncSxInstances*)g_list_find_custom(gnc_sx_instance_model_get_sx_instances_list(adapter->instances), GUINT_TO_POINTER(tag), gsidca_find_sx_with_tag)->data;
    if (insts == NULL)
        return;
    inst = (GncSxInstance*)g_list_nth_data(insts->instance_list, instance_index);
    g_date_valid(&inst->date);
    *date = inst->date;
    g_date_valid(date);
}

static void
gnc_sx_instance_dense_cal_adapter_dispose(GObject *obj)
{
    GncSxInstanceDenseCalAdapter *adapter;
    g_return_if_fail(obj != NULL);
    adapter = GNC_SX_INSTANCE_DENSE_CAL_ADAPTER(obj);
    // g_return_if_fail(!adapter->disposed);
    if (adapter->disposed) return;
    adapter->disposed = TRUE;

    g_object_unref(G_OBJECT(adapter->instances));
    adapter->instances = NULL;

    G_OBJECT_CLASS(gnc_sx_instance_dense_cal_adapter_parent_class)->dispose(obj);
}

static void gnc_sx_instance_dense_cal_adapter_finalize(GObject *obj)
{
    g_return_if_fail(obj != NULL);
    // nop
    G_OBJECT_CLASS(gnc_sx_instance_dense_cal_adapter_parent_class)->finalize(obj);
}
