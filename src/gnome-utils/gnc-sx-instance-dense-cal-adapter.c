/* 
 * gnc-sx-instance-dense-cal-adapter.c
 *
 * Copyright (C) 2006 Josh Sled <jsled@asynchronous.org>
 *
 * This program is free software; you can redistribute it and/or
 * modify it under the terms of version 2 of the GNU General Public
 * License as published by the Free Software Foundation.
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

#include "config.h"
#include <glib.h>
#include "gnc-sx-instance-dense-cal-adapter.h"
#include "gnc-dense-cal.h"

static GList* gsidca_get_contained(GncDenseCalModel *model);
static gchar* gsidca_get_name(GncDenseCalModel *model, guint tag);
static gchar* gsidca_get_info(GncDenseCalModel *model, guint tag);
static gint gsidca_get_instance_count(GncDenseCalModel *model, guint tag);
static void gsidca_get_instance(GncDenseCalModel *model, guint tag, gint instance_index, GDate *date);

struct _GncSxInstanceDenseCalAdapterClass
{
  GObjectClass parent;
};

struct _GncSxInstanceDenseCalAdapter 
{
  GObject parent;
  GncSxInstanceModel *instances;
};

static void
gnc_sx_instance_dense_cal_adapter_class_init(GncSxInstanceDenseCalAdapterClass *klass)
{
     ; /* nop */
}

static void
gnc_sx_instance_dense_cal_adapter_init(GTypeInstance *instance, gpointer klass)
{
     /*GncSxInstanceDenseCalAdapter *adapter = GNC_SX_INSTANCE_DENSE_CAL_ADAPTER(instance);*/
     ; /* nop */
}

static void
gnc_sx_instance_dense_cal_adapter_interface_init(gpointer g_iface, gpointer iface_data)
{
     GncDenseCalModelIface *iface = (GncDenseCalModelIface*)g_iface;
     iface->get_contained = gsidca_get_contained;
     iface->get_name = gsidca_get_name;
     iface->get_info = gsidca_get_info;
     iface->get_instance_count = gsidca_get_instance_count;
     iface->get_instance = gsidca_get_instance;
}

static void
gsidca_instances_added_cb(GncSxInstanceModel *model, gpointer instance_added, gpointer user_data)
{
     GncSxInstanceDenseCalAdapter *adapter = GNC_SX_INSTANCE_DENSE_CAL_ADAPTER(user_data);
     g_signal_emit_by_name(adapter, "added", GPOINTER_TO_UINT(instance_added));
}

static void
gsidca_instances_updated_cb(GncSxInstanceModel *model, gpointer user_data)
{
     GncSxInstanceDenseCalAdapter *adapter = GNC_SX_INSTANCE_DENSE_CAL_ADAPTER(user_data);
     // @@fixme figure out which; emit appropriate signal.
     GList *exposed_tags;
     printf("instances updated\n");
     for (exposed_tags = gsidca_get_contained(GNC_DENSE_CAL_MODEL(adapter)); exposed_tags != NULL; exposed_tags = exposed_tags->next)
     {
          g_signal_emit_by_name(adapter, "update", GPOINTER_TO_UINT(exposed_tags->data));
     }
}

static void
gsidca_instances_removing_cb(GncSxInstanceModel *model, gpointer instance_to_be_removed, gpointer user_data)
{
     GncSxInstanceDenseCalAdapter *adapter = GNC_SX_INSTANCE_DENSE_CAL_ADAPTER(user_data);
     printf("removing instance...\n");
     g_signal_emit_by_name(adapter, "removing", GPOINTER_TO_UINT(instance_to_be_removed));
}

GncSxInstanceDenseCalAdapter*
gnc_sx_instance_dense_cal_adapter_new(GncSxInstanceModel *instances)
{
     GncSxInstanceDenseCalAdapter *adapter = g_object_new(GNC_TYPE_SX_INSTANCE_DENSE_CAL_ADAPTER, NULL);
     adapter->instances = instances;
     g_signal_connect(instances, "added", (GCallback)gsidca_instances_added_cb, adapter);
     g_signal_connect(instances, "updated", (GCallback)gsidca_instances_updated_cb, adapter);
     g_signal_connect(instances, "removing", (GCallback)gsidca_instances_removing_cb, adapter);
     return adapter;
}

GType
gnc_sx_instance_dense_cal_adapter_get_type(void)
{
     static GType type = 0;
     if (type == 0)
     {
          static const GTypeInfo info = {
               sizeof (GncSxInstanceDenseCalAdapterClass),
               NULL, /* base init */
               NULL, /* base finalize */
               (GClassInitFunc)gnc_sx_instance_dense_cal_adapter_class_init,
               NULL, /* class finalize */
               NULL, /* class data */
               sizeof(GncSxInstanceDenseCalAdapter),
               0, /* n_preallocs */
               (GInstanceInitFunc)gnc_sx_instance_dense_cal_adapter_init
          };
          static const GInterfaceInfo iDenseCalModelInfo = {
               (GInterfaceInitFunc)gnc_sx_instance_dense_cal_adapter_interface_init,
               NULL, /* interface finalize */
               NULL, /* interface data */
          };

          type = g_type_register_static (G_TYPE_OBJECT,
                                         "GncSxInstanceDenseCalAdapterType",
                                         &info, 0);
          g_type_add_interface_static(type,
                                      GNC_TYPE_DENSE_CAL_MODEL,
                                      &iDenseCalModelInfo);
     }
     return type;
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
     for (sxes = adapter->instances->sx_instance_list; sxes != NULL; sxes = sxes->next)
     {
          GncSxInstances *sx_instances = (GncSxInstances*)sxes->data;
          list = g_list_append(list, GUINT_TO_POINTER(GPOINTER_TO_UINT(sx_instances->sx)));
     }
     return list;
}

static gchar*
gsidca_get_name(GncDenseCalModel *model, guint tag)
{
     GncSxInstanceDenseCalAdapter *adapter = GNC_SX_INSTANCE_DENSE_CAL_ADAPTER(model);
     GncSxInstances *insts
          = (GncSxInstances*)g_list_find_custom(adapter->instances->sx_instance_list, GUINT_TO_POINTER(tag), gsidca_find_sx_with_tag)->data;
     if (insts == NULL)
          return NULL;
     return xaccSchedXactionGetName(insts->sx);
}

static gchar*
gsidca_get_info(GncDenseCalModel *model, guint tag)
{
     GncSxInstanceDenseCalAdapter *adapter = GNC_SX_INSTANCE_DENSE_CAL_ADAPTER(model);
     // g_list_find(instances->sxes, {sx_to_tag, tag}).get_freq_spec().get_freq_str();
     FreqSpec *spec;
     GString *info;
     gchar *info_str;
     GncSxInstances *insts
          = (GncSxInstances*)g_list_find_custom(adapter->instances->sx_instance_list, GUINT_TO_POINTER(tag), gsidca_find_sx_with_tag)->data;
     if (insts == NULL)
          return NULL;
     spec = xaccSchedXactionGetFreqSpec(insts->sx);
     info = g_string_sized_new(16);
     xaccFreqSpecGetFreqStr(spec, info);
     info_str = info->str; // @fixme leaked... :/
     g_string_free(info, FALSE);
     return info_str;
}

static gint
gsidca_get_instance_count(GncDenseCalModel *model, guint tag)
{
     GncSxInstanceDenseCalAdapter *adapter = GNC_SX_INSTANCE_DENSE_CAL_ADAPTER(model);
     // g_list_find(instances->sxes, {sx_to_tag, tag}).length();
     GncSxInstances *insts
          = (GncSxInstances*)g_list_find_custom(adapter->instances->sx_instance_list, GUINT_TO_POINTER(tag), gsidca_find_sx_with_tag)->data;
     if (insts == NULL)
          return 0;
     return g_list_length(insts->list);
}

static void
gsidca_get_instance(GncDenseCalModel *model, guint tag, gint instance_index, GDate *date)
{
     GncSxInstanceDenseCalAdapter *adapter = GNC_SX_INSTANCE_DENSE_CAL_ADAPTER(model);
     GncSxInstance *inst;
     GncSxInstances *insts
          = (GncSxInstances*)g_list_find_custom(adapter->instances->sx_instance_list, GUINT_TO_POINTER(tag), gsidca_find_sx_with_tag)->data;
     if (insts == NULL)
          return;
     inst = (GncSxInstance*)g_list_nth_data(insts->list, instance_index);
     g_date_valid(&inst->date);
     *date = inst->date;
     g_date_valid(date);
}
