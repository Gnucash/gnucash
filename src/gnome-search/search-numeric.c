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

#include "search-numeric.h"
#include "search-core-utils.h"

#define d(x)

static void editable_enters (GNCSearchCoreType *fe);
static void grab_focus (GNCSearchCoreType *fe);
static GNCSearchCoreType *gncs_clone(GNCSearchCoreType *fe);
static gboolean gncs_validate (GNCSearchCoreType *fe);
static GtkWidget *gncs_get_widget(GNCSearchCoreType *fe);
static QofQueryPredData* gncs_get_predicate (GNCSearchCoreType *fe);

static void gnc_search_numeric_class_init	(GNCSearchNumericClass *klass);
static void gnc_search_numeric_init	(GNCSearchNumeric *gspaper);
static void gnc_search_numeric_finalize	(GObject *obj);

typedef struct _GNCSearchNumericPrivate GNCSearchNumericPrivate;

struct _GNCSearchNumericPrivate
{
    gboolean	is_debcred;
    GtkWidget * 	entry;
    GNCAmountEdit *gae;
};

#define _PRIVATE(o) \
   (G_TYPE_INSTANCE_GET_PRIVATE ((o), GNC_TYPE_SEARCH_NUMERIC, GNCSearchNumericPrivate))

static GNCSearchCoreTypeClass *parent_class;

GType
gnc_search_numeric_get_type (void)
{
    static GType type = 0;

    if (!type)
    {
        GTypeInfo type_info =
        {
            sizeof(GNCSearchNumericClass),    /* class_size */
            NULL,   				/* base_init */
            NULL,				/* base_finalize */
            (GClassInitFunc)gnc_search_numeric_class_init,
            NULL,				/* class_finalize */
            NULL,				/* class_data */
            sizeof(GNCSearchNumeric),		/* */
            0,				/* n_preallocs */
            (GInstanceInitFunc)gnc_search_numeric_init,
        };

        type = g_type_register_static (GNC_TYPE_SEARCH_CORE_TYPE,
                                       "GNCSearchNumeric",
                                       &type_info, 0);
    }

    return type;
}

static void
gnc_search_numeric_class_init (GNCSearchNumericClass *klass)
{
    GObjectClass *object_class;
    GNCSearchCoreTypeClass *gnc_search_core_type = (GNCSearchCoreTypeClass *)klass;

    object_class = G_OBJECT_CLASS (klass);
    parent_class = g_type_class_peek_parent (klass);

    object_class->finalize = gnc_search_numeric_finalize;

    /* override methods */
    gnc_search_core_type->editable_enters = editable_enters;
    gnc_search_core_type->grab_focus = grab_focus;
    gnc_search_core_type->validate = gncs_validate;
    gnc_search_core_type->get_widget = gncs_get_widget;
    gnc_search_core_type->get_predicate = gncs_get_predicate;
    gnc_search_core_type->clone = gncs_clone;

    g_type_class_add_private(klass, sizeof(GNCSearchNumericPrivate));
}

static void
gnc_search_numeric_init (GNCSearchNumeric *o)
{
    o->value = gnc_numeric_zero ();
    o->how = QOF_COMPARE_EQUAL;
    o->option = QOF_NUMERIC_MATCH_ANY;
}

static void
gnc_search_numeric_finalize (GObject *obj)
{
    GNCSearchNumeric *o = (GNCSearchNumeric *)obj;
    g_assert (IS_GNCSEARCH_NUMERIC (o));

    G_OBJECT_CLASS (parent_class)->finalize(obj);
}

/**
 * gnc_search_numeric_new:
 *
 * Create a new GNCSearchNumeric object.
 *
 * Return value: A new #GNCSearchNumeric object.
 **/
GNCSearchNumeric *
gnc_search_numeric_new (void)
{
    GNCSearchNumeric *o = g_object_new(GNC_TYPE_SEARCH_NUMERIC, NULL);
    return o;
}

/**
 * gnc_search_numeric_debcred_new:
 *
 * Create a new GNCSearchNumeric object, configured for DebCred.
 *
 * Return value: A new #GNCSearchNumeric object.
 **/
GNCSearchNumeric *
gnc_search_numeric_debcred_new (void)
{
    GNCSearchNumeric *o;
    GNCSearchNumericPrivate *priv;

    o = g_object_new(GNC_TYPE_SEARCH_NUMERIC, NULL);
    priv = _PRIVATE(o);
    priv->is_debcred = TRUE;
    return o;
}

void
gnc_search_numeric_set_value (GNCSearchNumeric *fi, gnc_numeric value)
{
    g_return_if_fail (fi);
    g_return_if_fail (IS_GNCSEARCH_NUMERIC (fi));

    fi->value = value;
}

void
gnc_search_numeric_set_how (GNCSearchNumeric *fi, QofQueryCompare how)
{
    g_return_if_fail (fi);
    g_return_if_fail (IS_GNCSEARCH_NUMERIC (fi));
    fi->how = how;
}

void
gnc_search_numeric_set_option (GNCSearchNumeric *fi, QofNumericMatch option)
{
    g_return_if_fail (fi);
    g_return_if_fail (IS_GNCSEARCH_NUMERIC (fi));
    fi->option = option;
}

static gboolean
gncs_validate (GNCSearchCoreType *fe)
{
    GNCSearchNumeric *fi = (GNCSearchNumeric *)fe;
    gboolean valid = TRUE;

    g_return_val_if_fail (fi, FALSE);
    g_return_val_if_fail (IS_GNCSEARCH_NUMERIC (fi), FALSE);

    /* XXX */

    return valid;
}

static void
entry_changed (GNCAmountEdit *entry, GNCSearchNumeric *fe)
{
    fe->value = gnc_amount_edit_get_amount (entry);
}

static GtkWidget *
make_how_menu (GNCSearchCoreType *fe)
{
    GNCSearchNumeric *fi = (GNCSearchNumeric *)fe;
    GNCSearchNumericPrivate *priv;
    GtkComboBox *combo;

    priv = _PRIVATE(fi);

    combo = GTK_COMBO_BOX(gnc_combo_box_new_search());
    gnc_combo_box_search_add(combo, (priv->is_debcred ?
                                     _("less than") : _("is less than")),
                             QOF_COMPARE_LT);
    gnc_combo_box_search_add(combo, (priv->is_debcred ?
                                     _("less than or equal to") :
                                     _("is less than or equal to")),
                             QOF_COMPARE_LTE);
    gnc_combo_box_search_add(combo, (priv->is_debcred ?
                                     _("equal to") : _("equals")),
                             QOF_COMPARE_EQUAL);
    gnc_combo_box_search_add(combo, (priv->is_debcred ?
                                     _("not equal to") : _("does not equal")),
                             QOF_COMPARE_NEQ);
    gnc_combo_box_search_add(combo, (priv->is_debcred ?
                                     _("greater than") : _("is greater than")),
                             QOF_COMPARE_GT);
    gnc_combo_box_search_add(combo, (priv->is_debcred ?
                                     _("greater than or equal to") :
                                     _("is greater than or equal to")),
                             QOF_COMPARE_GTE);

    gnc_combo_box_search_changed(combo, &fi->how);
    gnc_combo_box_search_set_active(combo, fi->how ? fi->how : QOF_COMPARE_LT);

    return GTK_WIDGET(combo);
}

static GtkWidget *
make_option_menu (GNCSearchCoreType *fe)
{
    GNCSearchNumeric *fi = (GNCSearchNumeric *)fe;
    GtkComboBox *combo;

    combo = GTK_COMBO_BOX(gnc_combo_box_new_search());
    gnc_combo_box_search_add(combo, _("has credits or debits"), QOF_NUMERIC_MATCH_ANY);
    gnc_combo_box_search_add(combo, _("has debits"), QOF_NUMERIC_MATCH_DEBIT);
    gnc_combo_box_search_add(combo, _("has credits"), QOF_NUMERIC_MATCH_CREDIT);
    gnc_combo_box_search_changed(combo, &fi->option);
    gnc_combo_box_search_set_active(combo, fi->option ? fi->option : QOF_NUMERIC_MATCH_ANY);

    return GTK_WIDGET(combo);
}

static void
grab_focus (GNCSearchCoreType *fe)
{
    GNCSearchNumeric *fi = (GNCSearchNumeric *)fe;
    GNCSearchNumericPrivate *priv;

    g_return_if_fail (fi);
    g_return_if_fail (IS_GNCSEARCH_NUMERIC (fi));

    priv = _PRIVATE(fi);
    if (priv->entry)
        gtk_widget_grab_focus (priv->entry);
}

static void
editable_enters (GNCSearchCoreType *fe)
{
    GNCSearchNumeric *fi = (GNCSearchNumeric *)fe;
    GNCSearchNumericPrivate *priv;

    g_return_if_fail (fi);
    g_return_if_fail (IS_GNCSEARCH_NUMERIC (fi));

    priv = _PRIVATE(fi);
    if (priv->entry)
        gtk_entry_set_activates_default(GTK_ENTRY (priv->entry), TRUE);
}

static GtkWidget *
gncs_get_widget (GNCSearchCoreType *fe)
{
    GtkWidget *entry, *menu, *box;
    GNCSearchNumeric *fi = (GNCSearchNumeric *)fe;
    GNCSearchNumericPrivate *priv;

    g_return_val_if_fail (fi, NULL);
    g_return_val_if_fail (IS_GNCSEARCH_NUMERIC (fi), NULL);

    priv = _PRIVATE(fi);
    box = gtk_hbox_new (FALSE, 3);

    /* Build and connect the option menu(s) */
    if (priv->is_debcred)
    {
        menu = make_option_menu (fe);
        gtk_box_pack_start (GTK_BOX (box), menu, FALSE, FALSE, 3);
    }

    menu = make_how_menu (fe);
    gtk_box_pack_start (GTK_BOX (box), menu, FALSE, FALSE, 3);

    /* Build and connect the entry window */
    entry = gnc_amount_edit_new ();
    gnc_amount_edit_set_amount (GNC_AMOUNT_EDIT (entry), fi->value);
    g_signal_connect (G_OBJECT (entry), "amount_changed", G_CALLBACK (entry_changed), fe);
    gtk_box_pack_start (GTK_BOX (box), entry, FALSE, FALSE, 3);
    priv->gae = GNC_AMOUNT_EDIT (entry);
    priv->entry = gnc_amount_edit_gtk_entry (GNC_AMOUNT_EDIT (entry));

    /* And return the box */
    return box;
}

static QofQueryPredData* gncs_get_predicate (GNCSearchCoreType *fe)
{
    GNCSearchNumeric *fi = (GNCSearchNumeric *)fe;
    GNCSearchNumericPrivate *priv;

    g_return_val_if_fail (fi, NULL);
    g_return_val_if_fail (IS_GNCSEARCH_NUMERIC (fi), NULL);

    /* force the computation of the entry, because we may not get the signal */
    priv = _PRIVATE(fi);
    entry_changed (priv->gae, fi);

    return qof_query_numeric_predicate (fi->how, fi->option, fi->value);
}

static GNCSearchCoreType *gncs_clone(GNCSearchCoreType *fe)
{
    GNCSearchNumeric *se, *fse = (GNCSearchNumeric *)fe;
    GNCSearchNumericPrivate *se_priv, *fse_priv;

    g_return_val_if_fail (fse, NULL);
    g_return_val_if_fail (IS_GNCSEARCH_NUMERIC (fse), NULL);
    fse_priv = _PRIVATE(fse);

    se = gnc_search_numeric_new ();
    gnc_search_numeric_set_value (se, fse->value);
    gnc_search_numeric_set_how (se, fse->how);
    se_priv = _PRIVATE(se);
    gnc_search_numeric_set_option (se, fse->option);
    se_priv->is_debcred = fse_priv->is_debcred;

    return (GNCSearchCoreType *)se;
}
