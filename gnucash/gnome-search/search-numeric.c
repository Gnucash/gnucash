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
#include "gnc-gui-query.h"

#include "search-numeric.h"
#include "search-core-utils.h"

#define d(x)

static void pass_parent (GNCSearchCoreType *fe, gpointer parent);
static void editable_enters (GNCSearchCoreType *fe);
static void grab_focus (GNCSearchCoreType *fe);
static GNCSearchCoreType *gncs_clone(GNCSearchCoreType *fe);
static gboolean gncs_validate (GNCSearchCoreType *fe);
static GtkWidget *gncs_get_widget(GNCSearchCoreType *fe);
static QofQueryPredData* gncs_get_predicate (GNCSearchCoreType *fe);

static void gnc_search_numeric_finalize	(GObject *obj);

struct _GNCSearchNumeric
{
    GNCSearchCoreType parent_instance;

    QofQueryCompare	how;
    gnc_numeric		value;
    QofNumericMatch	option;

    gboolean	is_debcred;
    GtkWidget * 	entry;
    GNCAmountEdit *gae;
    GtkWindow *parent;
};

G_DEFINE_TYPE(GNCSearchNumeric, gnc_search_numeric, GNC_TYPE_SEARCH_CORE_TYPE)

static void
gnc_search_numeric_class_init (GNCSearchNumericClass *klass)
{
    GObjectClass *object_class;
    GNCSearchCoreTypeClass *gnc_search_core_type = (GNCSearchCoreTypeClass *)klass;

    object_class = G_OBJECT_CLASS (klass);

    object_class->finalize = gnc_search_numeric_finalize;

    /* override methods */
    gnc_search_core_type->pass_parent = pass_parent;
    gnc_search_core_type->editable_enters = editable_enters;
    gnc_search_core_type->grab_focus = grab_focus;
    gnc_search_core_type->validate = gncs_validate;
    gnc_search_core_type->get_widget = gncs_get_widget;
    gnc_search_core_type->get_predicate = gncs_get_predicate;
    gnc_search_core_type->clone = gncs_clone;
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
    g_assert (GNC_IS_SEARCH_NUMERIC (o));

    G_OBJECT_CLASS (gnc_search_numeric_parent_class)->finalize(obj);
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

    o = g_object_new(GNC_TYPE_SEARCH_NUMERIC, NULL);
    o->is_debcred = TRUE;
    return o;
}

void
gnc_search_numeric_set_value (GNCSearchNumeric *fi, gnc_numeric value)
{
    g_return_if_fail (fi);
    g_return_if_fail (GNC_IS_SEARCH_NUMERIC (fi));

    fi->value = value;
}

void
gnc_search_numeric_set_how (GNCSearchNumeric *fi, QofQueryCompare how)
{
    g_return_if_fail (fi);
    g_return_if_fail (GNC_IS_SEARCH_NUMERIC (fi));
    fi->how = how;
}

void
gnc_search_numeric_set_option (GNCSearchNumeric *fi, QofNumericMatch option)
{
    g_return_if_fail (fi);
    g_return_if_fail (GNC_IS_SEARCH_NUMERIC (fi));
    fi->option = option;
}

static void
pass_parent (GNCSearchCoreType *fe, gpointer parent)
{
    GNCSearchNumeric *fi = (GNCSearchNumeric *)fe;

    g_return_if_fail (fi);
    g_return_if_fail (GNC_IS_SEARCH_NUMERIC (fi));

    fi->parent = GTK_WINDOW(parent);
}

static gboolean
gncs_validate (GNCSearchCoreType *fe)
{
    GNCSearchNumeric *fi = (GNCSearchNumeric *)fe;
    gboolean valid = TRUE;
    GError *error = NULL;

    g_return_val_if_fail (fi, FALSE);
    g_return_val_if_fail (GNC_IS_SEARCH_NUMERIC (fi), FALSE);

    if (!gnc_amount_edit_evaluate (GNC_AMOUNT_EDIT(fi->gae), &error))
    {
        gnc_error_dialog (GTK_WINDOW(fi->parent), "%s", error->message);
        valid = FALSE;
        g_error_free (error);
    }
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
    GtkComboBox *combo;

    combo = GTK_COMBO_BOX(gnc_combo_box_new_search());
    gnc_combo_box_search_add(combo, (fi->is_debcred ?
                                     _("less than") : _("is less than")),
                             QOF_COMPARE_LT);
    gnc_combo_box_search_add(combo, (fi->is_debcred ?
                                     _("less than or equal to") :
                                     _("is less than or equal to")),
                             QOF_COMPARE_LTE);
    gnc_combo_box_search_add(combo, (fi->is_debcred ?
                                     _("equal to") : _("equals")),
                             QOF_COMPARE_EQUAL);
    gnc_combo_box_search_add(combo, (fi->is_debcred ?
                                     _("not equal to") : _("does not equal")),
                             QOF_COMPARE_NEQ);
    gnc_combo_box_search_add(combo, (fi->is_debcred ?
                                     _("greater than") : _("is greater than")),
                             QOF_COMPARE_GT);
    gnc_combo_box_search_add(combo, (fi->is_debcred ?
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

    g_return_if_fail (fi);
    g_return_if_fail (GNC_IS_SEARCH_NUMERIC (fi));

    if (fi->entry)
        gtk_widget_grab_focus (fi->entry);
}

static void
editable_enters (GNCSearchCoreType *fe)
{
    GNCSearchNumeric *fi = (GNCSearchNumeric *)fe;

    g_return_if_fail (fi);
    g_return_if_fail (GNC_IS_SEARCH_NUMERIC (fi));

    if (fi->entry)
        gtk_entry_set_activates_default(GTK_ENTRY (fi->entry), TRUE);
}

static GtkWidget *
gncs_get_widget (GNCSearchCoreType *fe)
{
    GtkWidget *entry, *menu, *box;
    GNCSearchNumeric *fi = (GNCSearchNumeric *)fe;

    g_return_val_if_fail (fi, NULL);
    g_return_val_if_fail (GNC_IS_SEARCH_NUMERIC (fi), NULL);

    box = gtk_box_new (GTK_ORIENTATION_HORIZONTAL, 3);
    gtk_box_set_homogeneous (GTK_BOX (box), FALSE);

    /* Build and connect the option menu(s) */
    if (fi->is_debcred)
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
    fi->gae = GNC_AMOUNT_EDIT (entry);
    fi->entry = gnc_amount_edit_gtk_entry (GNC_AMOUNT_EDIT (entry));

    /* And return the box */
    return box;
}

static QofQueryPredData* gncs_get_predicate (GNCSearchCoreType *fe)
{
    GNCSearchNumeric *fi = (GNCSearchNumeric *)fe;

    g_return_val_if_fail (fi, NULL);
    g_return_val_if_fail (GNC_IS_SEARCH_NUMERIC (fi), NULL);

    /* force the computation of the entry, because we may not get the signal */
    entry_changed (fi->gae, fi);

    return qof_query_numeric_predicate (fi->how, fi->option, fi->value);
}

static GNCSearchCoreType *gncs_clone(GNCSearchCoreType *fe)
{
    GNCSearchNumeric *se, *fse = (GNCSearchNumeric *)fe;

    g_return_val_if_fail (fse, NULL);
    g_return_val_if_fail (GNC_IS_SEARCH_NUMERIC (fse), NULL);

    se = gnc_search_numeric_new ();
    gnc_search_numeric_set_value (se, fse->value);
    gnc_search_numeric_set_how (se, fse->how);
    gnc_search_numeric_set_option (se, fse->option);
    se->is_debcred = fse->is_debcred;

    return (GNCSearchCoreType *)se;
}
