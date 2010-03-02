/*
 * gnc-dense-cal-store.h
 *
 * Copyright (C) 2006 Joshua Sled <jsled@asynchronous.org>
 *
 * This program is free software; you can redistribute it and/or modify it
 * under the terms of version 2 of the GNU General Public License as
 * published by the Free Software Foundation.
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

#include "config.h"
#include <glib.h>
#include <glib-object.h>
#include "gnc-dense-cal.h"
#include "gnc-dense-cal-model.h"
#include "gnc-dense-cal-store.h"
#include "Recurrence.h"

struct _GncDenseCalStore
{
    GObject parent;

    GDate start_date;
    gdcs_end_type end_type;
    GDate end_date;
    gint n_occurrences;
    gchar *name;
    gchar *info;
    int num_marks;
    int num_real_marks;
    GDate **cal_marks;
};

struct _GncDenseCalStoreClass
{
    GObjectClass parent_class;
};

static GObjectClass *parent_class = NULL;

static void gnc_dense_cal_store_class_init(GncDenseCalStoreClass *klass);

static void gnc_dense_cal_store_finalize(GObject *obj);

static GList* gdcs_get_contained(GncDenseCalModel *model);
static gchar* gdcs_get_name(GncDenseCalModel *model, guint tag);
static gchar* gdcs_get_info(GncDenseCalModel *model, guint tag);
static gint gdcs_get_instance_count(GncDenseCalModel *model, guint tag);
static void gdcs_get_instance(GncDenseCalModel *model, guint tag, gint instance_index, GDate *date);

static void
gnc_dense_cal_store_class_init(GncDenseCalStoreClass *klass)
{
    GObjectClass *object_class = G_OBJECT_CLASS(klass);
    parent_class = g_type_class_peek_parent(klass);

    object_class->finalize = gnc_dense_cal_store_finalize;
}

static void
gnc_dense_cal_store_iface_init(gpointer g_iface, gpointer iface_data)
{
    GncDenseCalModelIface *iface = (GncDenseCalModelIface*)g_iface;
    iface->get_contained = gdcs_get_contained;
    iface->get_name = gdcs_get_name;
    iface->get_info = gdcs_get_info;
    iface->get_instance_count = gdcs_get_instance_count;
    iface->get_instance = gdcs_get_instance;
}

GType
gnc_dense_cal_store_get_type(void)
{
    static GType type = 0;
    if (type == 0)
    {
        static const GTypeInfo info =
        {
            sizeof (GncDenseCalStoreClass),
            NULL,   /* base_init */
            NULL,   /* base_finalize */
            (GClassInitFunc)gnc_dense_cal_store_class_init,   /* class_init */
            NULL,   /* class_finalize */
            NULL,   /* class_data */
            sizeof(GncDenseCalStore),
            0,      /* n_preallocs */
            NULL    /* instance_init */
        };
        static const GInterfaceInfo iDenseCalModelInfo =
        {
            (GInterfaceInitFunc)gnc_dense_cal_store_iface_init,
            NULL, /* interface finalize */
            NULL, /* interface data */
        };
        type = g_type_register_static(G_TYPE_OBJECT, "GncDenseCalStore", &info, 0);
        g_type_add_interface_static(type,
                                    GNC_TYPE_DENSE_CAL_MODEL,
                                    &iDenseCalModelInfo);
    }
    return type;
}

GncDenseCalStore*
gnc_dense_cal_store_new(int num_marks)
{
    GncDenseCalStore *model = g_object_new(GNC_TYPE_DENSE_CAL_STORE, NULL);
    model->num_marks = num_marks;
    model->cal_marks = g_new0(GDate*, num_marks);
    {
        int i = 0;
        for (i = 0; i < model->num_marks; i++)
        {
            model->cal_marks[i] = g_date_new();
        }
    }
    model->num_real_marks = 0;
    g_date_clear(&model->start_date, 1);
    g_date_set_time_t(&model->start_date, time(NULL));
    model->end_type = NEVER_END;
    g_date_clear(&model->end_date, 1);
    g_date_set_time_t(&model->end_date, time(NULL));
    model->n_occurrences = 0;
    return model;
}

void
gnc_dense_cal_store_clear(GncDenseCalStore *model)
{
    model->num_real_marks = 0;
    g_signal_emit_by_name(model, "update", GUINT_TO_POINTER(1));
}

void
gnc_dense_cal_store_update_name(GncDenseCalStore *model, gchar *name)
{
    if (model->name != NULL)
    {
        g_free(model->name);
    }
    model->name = g_strdup(name);
    //g_signal_emit_by_name(model, "update", GUINT_TO_POINTER(1));
}

void
gnc_dense_cal_store_update_info(GncDenseCalStore *model, gchar *info)
{
    if (model->info != NULL)
    {
        g_free(model->info);
    }
    model->info = g_strdup(info);
    //g_signal_emit_by_name(model, "update", GUINT_TO_POINTER(1));
}

static void
gdcs_generic_update_recurrences(GncDenseCalStore *trans, GDate *start, GList *recurrences)
{
    int i;
    GDate date, next;

    date = *start;
    /* go one day before what's in the box so we can get the correct start
     * date. */
    g_date_subtract_days(&date, 1);
    recurrenceListNextInstance(recurrences, &date, &next);

    i = 0;
    while ((i < trans->num_marks)
            && g_date_valid(&next)
            /* Do checking against end restriction. */
            && ((trans->end_type == NEVER_END)
                || (trans->end_type == END_ON_DATE
                    && g_date_compare(&next, &trans->end_date) <= 0)
                || (trans->end_type == END_AFTER_N_OCCS
                    && i < trans->n_occurrences)))
    {
        *trans->cal_marks[i++] = next;
        date = next;
        recurrenceListNextInstance(recurrences, &date, &next);
    }
    trans->num_real_marks = (i == 0 ? 0 : (i - 1));
    g_signal_emit_by_name(trans, "update", GUINT_TO_POINTER(1));
}

void
gnc_dense_cal_store_update_recurrences_no_end(GncDenseCalStore *model, GDate *start, GList *recurrences)
{
    model->end_type = NEVER_END;
    gdcs_generic_update_recurrences(model, start, recurrences);
}

void
gnc_dense_cal_store_update_recurrences_count_end(GncDenseCalStore *model, GDate *start, GList *recurrences, int num_occur)
{
    model->end_type = END_AFTER_N_OCCS;
    model->n_occurrences = num_occur;
    gdcs_generic_update_recurrences(model, start, recurrences);
}

void
gnc_dense_cal_store_update_recurrences_date_end(GncDenseCalStore *model, GDate *start, GList *recurrences, GDate *end_date)
{
    model->end_type = END_ON_DATE;
    model->end_date = *end_date;
    gdcs_generic_update_recurrences(model, start, recurrences);
}

static GList*
gdcs_get_contained(GncDenseCalModel *model)
{
    GList *rtn = NULL;
    rtn = g_list_append(rtn, GUINT_TO_POINTER(1));
    return rtn;
}

static gchar*
gdcs_get_name(GncDenseCalModel *model, guint tag)
{
    GncDenseCalStore *mdl = GNC_DENSE_CAL_STORE(model);
    // assert(tag == 1)
    return mdl->name;
}

static gchar*
gdcs_get_info(GncDenseCalModel *model, guint tag)
{
    GncDenseCalStore *mdl = GNC_DENSE_CAL_STORE(model);
    // assert(tag == 1)
    return g_strdup(mdl->info);
}

static gint
gdcs_get_instance_count(GncDenseCalModel *model, guint tag)
{
    GncDenseCalStore *mdl = GNC_DENSE_CAL_STORE(model);
    // assert(tag == 1)
    return mdl->num_real_marks;
}

static void
gdcs_get_instance(GncDenseCalModel *model, guint tag, gint instance_index, GDate *date)
{
    GncDenseCalStore *mdl = GNC_DENSE_CAL_STORE(model);
    // assert(tag == 1)
    // assert 0 < instance_index < model->num_marks;
    *date = *mdl->cal_marks[instance_index];
}

static void
gnc_dense_cal_store_finalize(GObject *obj)
{
    int i;
    GncDenseCalStore *store;
    g_return_if_fail(obj != NULL);

    store = GNC_DENSE_CAL_STORE(obj);

    if (store->name != NULL)
    {
        g_free(store->name);
        store->name = NULL;
    }

    if (store->info != NULL)
    {
        g_free(store->info);
        store->info = NULL;
    }

    for (i = 0; i < store->num_marks; i++)
    {
        g_free(store->cal_marks[i]);
        store->cal_marks[i] = NULL;
    }
    if (store->cal_marks != NULL)
    {
        g_free(store->cal_marks);
        store->cal_marks = NULL;
    }

    G_OBJECT_CLASS(parent_class)->finalize(obj);
}
