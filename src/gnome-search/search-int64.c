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

#include "gnc-amount-edit.h"
#include "qof.h"

#include "search-int64.h"
#include "search-core-utils.h"

#define d(x)

static void editable_enters (GNCSearchCoreType *fe);
static void grab_focus (GNCSearchCoreType *fe);
static GNCSearchCoreType *gncs_clone(GNCSearchCoreType *fe);
static gboolean gncs_validate (GNCSearchCoreType *fe);
static GtkWidget *gncs_get_widget(GNCSearchCoreType *fe);
static QofQueryPredData* gncs_get_predicate (GNCSearchCoreType *fe);

static void gnc_search_int64_class_init	(GNCSearchInt64Class *klass);
static void gnc_search_int64_init	(GNCSearchInt64 *gspaper);
static void gnc_search_int64_finalize	(GObject *obj);


typedef struct _GNCSearchInt64Private GNCSearchInt64Private;

struct _GNCSearchInt64Private
{
    GtkWidget *entry;
    GNCAmountEdit *gae;
};

#define _PRIVATE(o) \
   (G_TYPE_INSTANCE_GET_PRIVATE ((o), GNC_TYPE_SEARCH_INT64, GNCSearchInt64Private))

static GNCSearchCoreTypeClass *parent_class;

GType
gnc_search_int64_get_type (void)
{
    static GType type = 0;

    if (!type)
    {
        GTypeInfo type_info =
        {
            sizeof(GNCSearchInt64Class),      /* class_size */
            NULL,   				/* base_init */
            NULL,				/* base_finalize */
            (GClassInitFunc)gnc_search_int64_class_init,
            NULL,				/* class_finalize */
            NULL,				/* class_data */
            sizeof(GNCSearchInt64),		/* */
            0,				/* n_preallocs */
            (GInstanceInitFunc)gnc_search_int64_init,
        };

        type = g_type_register_static (GNC_TYPE_SEARCH_CORE_TYPE,
                                       "GNCSearchInt64",
                                       &type_info, 0);
    }

    return type;
}

static void
gnc_search_int64_class_init (GNCSearchInt64Class *klass)
{
    GObjectClass *object_class;
    GNCSearchCoreTypeClass *gnc_search_core_type = (GNCSearchCoreTypeClass *)klass;

    object_class = G_OBJECT_CLASS (klass);
    parent_class = g_type_class_peek_parent (klass);

    object_class->finalize = gnc_search_int64_finalize;

    /* override methods */
    gnc_search_core_type->editable_enters = editable_enters;
    gnc_search_core_type->grab_focus = grab_focus;
    gnc_search_core_type->validate = gncs_validate;
    gnc_search_core_type->get_widget = gncs_get_widget;
    gnc_search_core_type->get_predicate = gncs_get_predicate;
    gnc_search_core_type->clone = gncs_clone;

    g_type_class_add_private(klass, sizeof(GNCSearchInt64Private));
}

static void
gnc_search_int64_init (GNCSearchInt64 *o)
{
    o->how = QOF_COMPARE_EQUAL;
}

static void
gnc_search_int64_finalize (GObject *obj)
{
    GNCSearchInt64 *o = (GNCSearchInt64 *)obj;
    g_assert (IS_GNCSEARCH_INT64 (o));

    G_OBJECT_CLASS (parent_class)->finalize(obj);
}

/**
 * gnc_search_int64_new:
 *
 * Create a new GNCSearchInt64 object.
 *
 * Return value: A new #GNCSearchInt64 object.
 **/
GNCSearchInt64 *
gnc_search_int64_new (void)
{
    GNCSearchInt64 *o = g_object_new(GNC_TYPE_SEARCH_INT64, NULL);
    return o;
}

void
gnc_search_int64_set_value (GNCSearchInt64 *fi, gint64 value)
{
    g_return_if_fail (fi);
    g_return_if_fail (IS_GNCSEARCH_INT64 (fi));

    fi->value = value;
}

void
gnc_search_int64_set_how (GNCSearchInt64 *fi, QofQueryCompare how)
{
    g_return_if_fail (fi);
    g_return_if_fail (IS_GNCSEARCH_INT64 (fi));
    fi->how = how;
}

static gboolean
gncs_validate (GNCSearchCoreType *fe)
{
    GNCSearchInt64 *fi = (GNCSearchInt64 *)fe;
    gboolean valid = TRUE;

    g_return_val_if_fail (fi, FALSE);
    g_return_val_if_fail (IS_GNCSEARCH_INT64 (fi), FALSE);

    /* XXX */

    return valid;
}

static void
entry_changed (GNCAmountEdit *entry, GNCSearchInt64 *fe)
{
    gnc_numeric value = gnc_amount_edit_get_amount (entry);
    g_assert (value.denom == 1);
    fe->value = value.num;
}

static GtkWidget *
make_menu (GNCSearchCoreType *fe)
{
    GNCSearchInt64 *fi = (GNCSearchInt64 *)fe;
    GtkComboBox *combo;

    combo = GTK_COMBO_BOX(gnc_combo_box_new_search());
    gnc_combo_box_search_add(combo, _("is less than"), QOF_COMPARE_LT);
    gnc_combo_box_search_add(combo, _("is less than or equal to"), QOF_COMPARE_LTE);
    gnc_combo_box_search_add(combo, _("equals"), QOF_COMPARE_EQUAL);
    gnc_combo_box_search_add(combo, _("does not equal"), QOF_COMPARE_NEQ);
    gnc_combo_box_search_add(combo, _("is greater than"), QOF_COMPARE_GT);
    gnc_combo_box_search_add(combo, _("is greater than or equal to"), QOF_COMPARE_GTE);
    gnc_combo_box_search_changed(combo, &fi->how);
    gnc_combo_box_search_set_active(combo, fi->how ? fi->how : QOF_COMPARE_LT);

    return GTK_WIDGET(combo);
}

static void
grab_focus (GNCSearchCoreType *fe)
{
    GNCSearchInt64 *fi = (GNCSearchInt64 *)fe;
    GNCSearchInt64Private *priv;

    g_return_if_fail (fi);
    g_return_if_fail (IS_GNCSEARCH_INT64 (fi));

    priv = _PRIVATE(fi);
    if (priv->entry)
        gtk_widget_grab_focus (priv->entry);
}

static void
editable_enters (GNCSearchCoreType *fe)
{
    GNCSearchInt64 *fi = (GNCSearchInt64 *)fe;
    GNCSearchInt64Private *priv;

    g_return_if_fail (fi);
    g_return_if_fail (IS_GNCSEARCH_INT64 (fi));

    priv = _PRIVATE(fi);
    if (priv->entry)
        gtk_entry_set_activates_default(GTK_ENTRY (priv->entry), TRUE);
}

static GtkWidget *
gncs_get_widget (GNCSearchCoreType *fe)
{
    GtkWidget *entry, *menu, *box;
    GNCSearchInt64 *fi = (GNCSearchInt64 *)fe;
    GNCSearchInt64Private *priv;

    g_return_val_if_fail (fi, NULL);
    g_return_val_if_fail (IS_GNCSEARCH_INT64 (fi), NULL);

    priv = _PRIVATE(fi);
    box = gtk_hbox_new (FALSE, 3);

    /* Build and connect the option menu */
    menu = make_menu (fe);
    gtk_box_pack_start (GTK_BOX (box), menu, FALSE, FALSE, 3);

    /* Build and connect the entry window */
    entry = gnc_amount_edit_new ();
    gnc_amount_edit_set_print_info (GNC_AMOUNT_EDIT (entry),
                                    gnc_integral_print_info ());
    if (fi->value)
    {
        gnc_numeric value = gnc_numeric_create (fi->value, 1);
        gnc_amount_edit_set_amount (GNC_AMOUNT_EDIT (entry), value);
    }
    g_signal_connect (G_OBJECT (entry), "amount_changed", G_CALLBACK (entry_changed), fe);
    gtk_box_pack_start (GTK_BOX (box), entry, FALSE, FALSE, 3);
    priv->entry = gnc_amount_edit_gtk_entry (GNC_AMOUNT_EDIT (entry));
    priv->gae = GNC_AMOUNT_EDIT (entry);

    /* And return the box */
    return box;
}

static QofQueryPredData* gncs_get_predicate (GNCSearchCoreType *fe)
{
    GNCSearchInt64 *fi = (GNCSearchInt64 *)fe;
    GNCSearchInt64Private *priv;

    g_return_val_if_fail (fi, NULL);
    g_return_val_if_fail (IS_GNCSEARCH_INT64 (fi), NULL);

    /* force the computation of the entry, because we may not get the signal */
    priv = _PRIVATE(fi);
    entry_changed (priv->gae, fi);

    return qof_query_int64_predicate (fi->how, fi->value);
}

static GNCSearchCoreType *gncs_clone(GNCSearchCoreType *fe)
{
    GNCSearchInt64 *se, *fse = (GNCSearchInt64 *)fe;

    g_return_val_if_fail (fse, NULL);
    g_return_val_if_fail (IS_GNCSEARCH_INT64 (fse), NULL);

    se = gnc_search_int64_new ();
    gnc_search_int64_set_value (se, fse->value);
    gnc_search_int64_set_how (se, fse->how);

    return (GNCSearchCoreType *)se;
}
