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

#include "search-double.h"
#include "search-core-utils.h"

#define d(x)

static void editable_enters (GNCSearchCoreType *fe);
static void grab_focus (GNCSearchCoreType *fe);
static GNCSearchCoreType *gncs_clone(GNCSearchCoreType *fe);
static gboolean gncs_validate (GNCSearchCoreType *fe);
static GtkWidget *gncs_get_widget(GNCSearchCoreType *fe);
static QofQueryPredData* gncs_get_predicate (GNCSearchCoreType *fe);

static void gnc_search_double_class_init	(GNCSearchDoubleClass *klass);
static void gnc_search_double_init	(GNCSearchDouble *gspaper);
static void gnc_search_double_finalize	(GObject *obj);

typedef struct _GNCSearchDoublePrivate GNCSearchDoublePrivate;

struct _GNCSearchDoublePrivate
{
    GtkWidget * entry;
    GNCAmountEdit *gae;
};

#define _PRIVATE(o) \
   (G_TYPE_INSTANCE_GET_PRIVATE ((o), GNC_TYPE_SEARCH_DOUBLE, GNCSearchDoublePrivate))

static GNCSearchCoreTypeClass *parent_class;

GType
gnc_search_double_get_type (void)
{
    static GType type = 0;

    if (!type)
    {
        GTypeInfo type_info =
        {
            sizeof(GNCSearchDoubleClass),     /* class_size */
            NULL,   			        /* base_init */
            NULL,				/* base_finalize */
            (GClassInitFunc)gnc_search_double_class_init,
            NULL,				/* class_finalize */
            NULL,				/* class_data */
            sizeof(GNCSearchDouble),		/* */
            0,				/* n_preallocs */
            (GInstanceInitFunc)gnc_search_double_init,
        };

        type = g_type_register_static (GNC_TYPE_SEARCH_CORE_TYPE,
                                       "GNCSearchDouble",
                                       &type_info, 0);
    }

    return type;
}

static void
gnc_search_double_class_init (GNCSearchDoubleClass *klass)
{
    GObjectClass *object_class;
    GNCSearchCoreTypeClass *gnc_search_core_type = (GNCSearchCoreTypeClass *)klass;

    object_class = G_OBJECT_CLASS (klass);
    parent_class = g_type_class_peek_parent (klass);

    object_class->finalize = gnc_search_double_finalize;

    /* override methods */
    gnc_search_core_type->editable_enters = editable_enters;
    gnc_search_core_type->grab_focus = grab_focus;
    gnc_search_core_type->validate = gncs_validate;
    gnc_search_core_type->get_widget = gncs_get_widget;
    gnc_search_core_type->get_predicate = gncs_get_predicate;
    gnc_search_core_type->clone = gncs_clone;

    g_type_class_add_private(klass, sizeof(GNCSearchDoublePrivate));
}

static void
gnc_search_double_init (GNCSearchDouble *o)
{
    o->how = QOF_COMPARE_EQUAL;
}

static void
gnc_search_double_finalize (GObject *obj)
{
    GNCSearchDouble *o = (GNCSearchDouble *)obj;
    g_assert (IS_GNCSEARCH_DOUBLE (o));

    G_OBJECT_CLASS (parent_class)->finalize(obj);
}

/**
 * gnc_search_double_new:
 *
 * Create a new GNCSearchDouble object.
 *
 * Return value: A new #GNCSearchDouble object.
 **/
GNCSearchDouble *
gnc_search_double_new (void)
{
    GNCSearchDouble *o = g_object_new(GNC_TYPE_SEARCH_DOUBLE, NULL);
    return o;
}

void
gnc_search_double_set_value (GNCSearchDouble *fi, double value)
{
    g_return_if_fail (fi);
    g_return_if_fail (IS_GNCSEARCH_DOUBLE (fi));

    fi->value = value;
}

void
gnc_search_double_set_how (GNCSearchDouble *fi, QofQueryCompare how)
{
    g_return_if_fail (fi);
    g_return_if_fail (IS_GNCSEARCH_DOUBLE (fi));
    fi->how = how;
}

static gboolean
gncs_validate (GNCSearchCoreType *fe)
{
    GNCSearchDouble *fi = (GNCSearchDouble *)fe;
    gboolean valid = TRUE;

    g_return_val_if_fail (fi, FALSE);
    g_return_val_if_fail (IS_GNCSEARCH_DOUBLE (fi), FALSE);

    /* XXX */

    return valid;
}

static void
entry_changed (GNCAmountEdit *entry, GNCSearchDouble *fe)
{
    fe->value = gnc_amount_edit_get_damount (entry);
}

static GtkWidget *
make_menu (GNCSearchCoreType *fe)
{
    GNCSearchDouble *fi = (GNCSearchDouble *)fe;
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
    GNCSearchDouble *fi = (GNCSearchDouble *)fe;
    GNCSearchDoublePrivate *priv ;

    g_return_if_fail (fi);
    g_return_if_fail (IS_GNCSEARCH_DOUBLE (fi));

    priv = _PRIVATE(fi);
    if (priv->entry)
        gtk_widget_grab_focus (priv->entry);
}

static void
editable_enters (GNCSearchCoreType *fe)
{
    GNCSearchDouble *fi = (GNCSearchDouble *)fe;
    GNCSearchDoublePrivate *priv ;

    g_return_if_fail (fi);
    g_return_if_fail (IS_GNCSEARCH_DOUBLE (fi));

    priv = _PRIVATE(fi);
    if (priv->entry)
        gtk_entry_set_activates_default(GTK_ENTRY (priv->entry), TRUE);
}

static GtkWidget *
gncs_get_widget (GNCSearchCoreType *fe)
{
    GtkWidget *entry, *menu, *box;
    GNCSearchDouble *fi = (GNCSearchDouble *)fe;
    GNCSearchDoublePrivate *priv ;

    g_return_val_if_fail (fi, NULL);
    g_return_val_if_fail (IS_GNCSEARCH_DOUBLE (fi), NULL);

    priv = _PRIVATE(fi);
    box = gtk_hbox_new (FALSE, 3);

    /* Build and connect the option menu */
    menu = make_menu (fe);
    gtk_box_pack_start (GTK_BOX (box), menu, FALSE, FALSE, 3);

    /* Build and connect the entry window */
    entry = gnc_amount_edit_new ();
    if (fi->value)
        gnc_amount_edit_set_damount (GNC_AMOUNT_EDIT (entry), fi->value);
    g_signal_connect (G_OBJECT (entry), "amount_changed", G_CALLBACK (entry_changed), fe);
    gtk_box_pack_start (GTK_BOX (box), entry, FALSE, FALSE, 3);
    priv->entry = gnc_amount_edit_gtk_entry (GNC_AMOUNT_EDIT (entry));
    priv->gae = GNC_AMOUNT_EDIT (entry);

    /* And return the box */
    return box;
}

static QofQueryPredData* gncs_get_predicate (GNCSearchCoreType *fe)
{
    GNCSearchDouble *fi = (GNCSearchDouble *)fe;
    GNCSearchDoublePrivate *priv ;

    g_return_val_if_fail (fi, NULL);
    g_return_val_if_fail (IS_GNCSEARCH_DOUBLE (fi), NULL);

    /* force the computation of the entry, because we may not get the signal */
    priv = _PRIVATE(fi);
    entry_changed (priv->gae, fi);

    return qof_query_double_predicate (fi->how, fi->value);
}

static GNCSearchCoreType *gncs_clone(GNCSearchCoreType *fe)
{
    GNCSearchDouble *se, *fse = (GNCSearchDouble *)fe;

    g_return_val_if_fail (fse, NULL);
    g_return_val_if_fail (IS_GNCSEARCH_DOUBLE (fse), NULL);

    se = gnc_search_double_new ();
    gnc_search_double_set_value (se, fse->value);
    gnc_search_double_set_how (se, fse->how);

    return (GNCSearchCoreType *)se;
}
