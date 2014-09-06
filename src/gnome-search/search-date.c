/*
 *  Copyright (C) 2002 Derek Atkins
 *
 *  Authors: Derek Atkins <warlord@MIT.EDU>
 *
 * Copyright (c) 2006 David Hampton <hampton@employees.org>
 *
 * This program is free software; you can redistribute it and/or
 * modify it under the terms of the GNU General Public License as
 * published by the Free Software Foundation; either version 2 of
 * the License, or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * General Public License for more details.
 *
 * You should have received a copy of the GNU General Public
 * License along with this program; if not, write to the
 * Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
 * Boston, MA 02110-1301, USA.
 */

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include <gtk/gtk.h>
#include <glib/gi18n.h>

#include "gnc-date.h"
#include "gnc-date-edit.h"
#include "qof.h"

#include "search-date.h"
#include "search-core-utils.h"

#define d(x)

static void editable_enters (GNCSearchCoreType *fe);
static void grab_focus (GNCSearchCoreType *fe);
static GNCSearchCoreType *gncs_clone(GNCSearchCoreType *fe);
static gboolean gncs_validate (GNCSearchCoreType *fe);
static GtkWidget *gncs_get_widget(GNCSearchCoreType *fe);
static QofQueryPredData* gncs_get_predicate (GNCSearchCoreType *fe);

static void gnc_search_date_class_init	(GNCSearchDateClass *klass);
static void gnc_search_date_init	(GNCSearchDate *gspaper);
static void gnc_search_date_finalize	(GObject *obj);

typedef struct _GNCSearchDatePrivate GNCSearchDatePrivate;

struct _GNCSearchDatePrivate
{
    GtkWidget *entry;
};

#define _PRIVATE(o) \
   (G_TYPE_INSTANCE_GET_PRIVATE ((o), GNC_TYPE_SEARCH_DATE, GNCSearchDatePrivate))

static GNCSearchCoreTypeClass *parent_class;

GType
gnc_search_date_get_type (void)
{
    static GType type = 0;

    if (!type)
    {
        GTypeInfo type_info =
        {
            sizeof(GNCSearchDateClass),       /* class_size */
            NULL,   				/* base_init */
            NULL,				/* base_finalize */
            (GClassInitFunc)gnc_search_date_class_init,
            NULL,				/* class_finalize */
            NULL,				/* class_data */
            sizeof(GNCSearchDate),		/* */
            0,				/* n_preallocs */
            (GInstanceInitFunc)gnc_search_date_init,
        };

        type = g_type_register_static (GNC_TYPE_SEARCH_CORE_TYPE,
                                       "GNCSearchDate",
                                       &type_info, 0);
    }

    return type;
}

static void
gnc_search_date_class_init (GNCSearchDateClass *klass)
{
    GObjectClass *object_class;
    GNCSearchCoreTypeClass *gnc_search_core_type = (GNCSearchCoreTypeClass *)klass;

    object_class = G_OBJECT_CLASS (klass);
    parent_class = g_type_class_peek_parent (klass);

    object_class->finalize = gnc_search_date_finalize;

    /* override methods */
    gnc_search_core_type->editable_enters = editable_enters;
    gnc_search_core_type->grab_focus = grab_focus;
    gnc_search_core_type->validate = gncs_validate;
    gnc_search_core_type->get_widget = gncs_get_widget;
    gnc_search_core_type->get_predicate = gncs_get_predicate;
    gnc_search_core_type->clone = gncs_clone;

    g_type_class_add_private(klass, sizeof(GNCSearchDatePrivate));
}

static void
gnc_search_date_init (GNCSearchDate *o)
{
    o->ts.tv_sec = gnc_time (NULL);
    o->how = QOF_COMPARE_LT;
}

static void
gnc_search_date_finalize (GObject *obj)
{
    GNCSearchDate *o;
    GNCSearchDatePrivate *priv;

    g_assert (IS_GNCSEARCH_DATE (obj));

    o = GNCSEARCH_DATE(obj);
    priv = _PRIVATE(o);
    if (priv->entry)
        gtk_widget_destroy (priv->entry);

    G_OBJECT_CLASS (parent_class)->finalize(obj);
}

/**
 * gnc_search_date_new:
 *
 * Create a new GNCSearchDate object.
 *
 * Return value: A new #GNCSearchDate object.
 **/
GNCSearchDate *
gnc_search_date_new (void)
{
    GNCSearchDate *o = g_object_new(GNC_TYPE_SEARCH_DATE, NULL);
    return o;
}

void
gnc_search_date_set_date (GNCSearchDate *fi, Timespec ts)
{
    g_return_if_fail (fi);
    g_return_if_fail (IS_GNCSEARCH_DATE (fi));

    fi->ts = ts;
}

void
gnc_search_date_set_how (GNCSearchDate *fi, QofQueryCompare how)
{
    g_return_if_fail (fi);
    g_return_if_fail (IS_GNCSEARCH_DATE (fi));
    fi->how = how;
}

static gboolean
gncs_validate (GNCSearchCoreType *fe)
{
    GNCSearchDate *fi = (GNCSearchDate *)fe;
    gboolean valid = TRUE;

    g_return_val_if_fail (fi, FALSE);
    g_return_val_if_fail (IS_GNCSEARCH_DATE (fi), FALSE);

    /* XXX */

    return valid;
}

static void
date_changed (GNCDateEdit *date_edit, GNCSearchDate *fe)
{
    fe->ts = gnc_date_edit_get_date_ts (date_edit);
}

static GtkWidget *
make_menu (GNCSearchCoreType *fe)
{
    GNCSearchDate *fi = (GNCSearchDate *)fe;
    GtkComboBox *combo;

    combo = GTK_COMBO_BOX(gnc_combo_box_new_search());

    gnc_combo_box_search_add(combo, _("is before"), QOF_COMPARE_LT);
    gnc_combo_box_search_add(combo, _("is before or on"), QOF_COMPARE_LTE);
    gnc_combo_box_search_add(combo, _("is on"), QOF_COMPARE_EQUAL);
    gnc_combo_box_search_add(combo, _("is not on"), QOF_COMPARE_NEQ);
    gnc_combo_box_search_add(combo, _("is after"), QOF_COMPARE_GT);
    gnc_combo_box_search_add(combo, _("is on or after"), QOF_COMPARE_GTE);
    gnc_combo_box_search_changed(combo, &fi->how);
    gnc_combo_box_search_set_active(combo, fi->how ? fi->how : QOF_COMPARE_LT);

    return GTK_WIDGET(combo);
}

static void
grab_focus (GNCSearchCoreType *fe)
{
    GNCSearchDate *fi = (GNCSearchDate *)fe;
    GNCSearchDatePrivate *priv;

    g_return_if_fail (fi);
    g_return_if_fail (IS_GNCSEARCH_DATE (fi));

    priv = _PRIVATE(fi);
    if (priv->entry)
        gtk_widget_grab_focus (GNC_DATE_EDIT(priv->entry)->date_entry);
}

static void
editable_enters (GNCSearchCoreType *fe)
{
    GNCSearchDate *fi = (GNCSearchDate *)fe;
    GNCSearchDatePrivate *priv;

    g_return_if_fail (fi);
    g_return_if_fail (IS_GNCSEARCH_DATE (fi));

    priv = _PRIVATE(fi);
    if (priv->entry)
        gnc_date_activates_default (GNC_DATE_EDIT (priv->entry), TRUE);
}

static GtkWidget *
gncs_get_widget (GNCSearchCoreType *fe)
{
    GtkWidget *entry, *menu, *box;
    GNCSearchDate *fi = (GNCSearchDate *)fe;
    GNCSearchDatePrivate *priv;

    g_return_val_if_fail (fi, NULL);
    g_return_val_if_fail (IS_GNCSEARCH_DATE (fi), NULL);

    priv = _PRIVATE(fi);
    box = gtk_hbox_new (FALSE, 3);

    /* Build and connect the option menu */
    menu = make_menu (fe);
    gtk_box_pack_start (GTK_BOX (box), menu, FALSE, FALSE, 3);

    /* Build and connect the date entry window */
    entry = gnc_date_edit_new_ts (fi->ts, FALSE, FALSE);
    g_signal_connect (G_OBJECT (entry), "date_changed", G_CALLBACK (date_changed), fe);
    gtk_box_pack_start (GTK_BOX (box), entry, FALSE, FALSE, 3);
    g_object_ref (entry);
    priv->entry = entry;

    /* And return the box */
    return box;
}

static QofQueryPredData* gncs_get_predicate (GNCSearchCoreType *fe)
{
    GNCSearchDate *fi = (GNCSearchDate *)fe;
    GNCSearchDatePrivate *priv;

    g_return_val_if_fail (fi, NULL);
    g_return_val_if_fail (IS_GNCSEARCH_DATE (fi), NULL);

    /* Make sure we actually use the currently-entered date */
    priv = _PRIVATE(fi);
    if (priv->entry)
        fi->ts = gnc_date_edit_get_date_ts (GNC_DATE_EDIT (priv->entry));

    return qof_query_date_predicate (fi->how, QOF_DATE_MATCH_NORMAL, fi->ts);
}

static GNCSearchCoreType *gncs_clone(GNCSearchCoreType *fe)
{
    GNCSearchDate *se, *fse = (GNCSearchDate *)fe;

    g_return_val_if_fail (fse, NULL);
    g_return_val_if_fail (IS_GNCSEARCH_DATE (fse), NULL);

    se = gnc_search_date_new ();
    gnc_search_date_set_date (se, fse->ts);
    gnc_search_date_set_how (se, fse->how);

    return (GNCSearchCoreType *)se;
}
