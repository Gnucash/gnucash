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

#include <stdint.h>
#include <gtk/gtk.h>
#include <glib/gi18n.h>

#include "qof.h"
#include "Transaction.h"	/* for ?REC */

#include "search-reconciled.h"
#include "search-core-utils.h"

#define d(x)

static void pass_parent (GNCSearchCoreType *fe, gpointer parent);
static GNCSearchCoreType *gncs_clone(GNCSearchCoreType *fe);
static gboolean gncs_validate (GNCSearchCoreType *fe);
static GtkWidget *gncs_get_widget(GNCSearchCoreType *fe);
static QofQueryPredData* gncs_get_predicate (GNCSearchCoreType *fe);

static void gnc_search_reconciled_class_init	(GNCSearchReconciledClass *klass);
static void gnc_search_reconciled_init	(GNCSearchReconciled *gspaper);
static void gnc_search_reconciled_finalize	(GObject *obj);

typedef struct _GNCSearchReconciledPrivate GNCSearchReconciledPrivate;

struct _GNCSearchReconciledPrivate
{
    GtkWindow *parent;
    gpointer dummy;
};

G_DEFINE_TYPE_WITH_PRIVATE(GNCSearchReconciled, gnc_search_reconciled, GNC_TYPE_SEARCH_CORE_TYPE)

#define _PRIVATE(o) \
   ((GNCSearchReconciledPrivate*)gnc_search_reconciled_get_instance_private((GNCSearchReconciled*)o))

static GNCSearchCoreTypeClass *parent_class;

static void
gnc_search_reconciled_class_init (GNCSearchReconciledClass *klass)
{
    GObjectClass *object_class;
    GNCSearchCoreTypeClass *gnc_search_core_type = (GNCSearchCoreTypeClass *)klass;

    object_class = G_OBJECT_CLASS (klass);
    parent_class = g_type_class_peek_parent (klass);

    object_class->finalize = gnc_search_reconciled_finalize;

    /* override methods */
    gnc_search_core_type->pass_parent = pass_parent;
    gnc_search_core_type->validate = gncs_validate;
    gnc_search_core_type->get_widget = gncs_get_widget;
    gnc_search_core_type->get_predicate = gncs_get_predicate;
    gnc_search_core_type->clone = gncs_clone;
}

static void
gnc_search_reconciled_init (GNCSearchReconciled *o)
{
    o->how = QOF_CHAR_MATCH_ANY;
    o->value = CLEARED_NO;
}

static void
gnc_search_reconciled_finalize (GObject *obj)
{
    GNCSearchReconciled *o = (GNCSearchReconciled *)obj;
    g_assert (IS_GNCSEARCH_RECONCILED (o));

    G_OBJECT_CLASS (parent_class)->finalize(obj);
}

/**
 * gnc_search_reconciled_new:
 *
 * Create a new GNCSearchReconciled object.
 *
 * Return value: A new #GNCSearchReconciled object.
 **/
GNCSearchReconciled *
gnc_search_reconciled_new (void)
{
    GNCSearchReconciled *o = g_object_new(GNC_TYPE_SEARCH_RECONCILED, NULL);
    return o;
}

void
gnc_search_reconciled_set_value (GNCSearchReconciled *fi, cleared_match_t value)
{
    g_return_if_fail (fi);
    g_return_if_fail (IS_GNCSEARCH_RECONCILED (fi));

    fi->value = value;
}

void
gnc_search_reconciled_set_how (GNCSearchReconciled *fi, QofCharMatch how)
{
    g_return_if_fail (fi);
    g_return_if_fail (IS_GNCSEARCH_RECONCILED (fi));
    fi->how = how;
}

static void
pass_parent (GNCSearchCoreType *fe, gpointer parent)
{
    GNCSearchReconciled *fi = (GNCSearchReconciled *)fe;
    GNCSearchReconciledPrivate *priv;

    g_return_if_fail (fi);
    g_return_if_fail (IS_GNCSEARCH_RECONCILED (fi));

    priv = _PRIVATE(fi);
    priv->parent = GTK_WINDOW(parent);
}

static gboolean
gncs_validate (GNCSearchCoreType *fe)
{
    GNCSearchReconciled *fi = (GNCSearchReconciled *)fe;
    gboolean valid = TRUE;

    g_return_val_if_fail (fi, FALSE);
    g_return_val_if_fail (IS_GNCSEARCH_RECONCILED (fi), FALSE);

    /* XXX */

    return valid;
}

static void
toggle_changed (GtkToggleButton *button, GNCSearchReconciled *fe)
{
    gboolean is_on = gtk_toggle_button_get_active (button);
#ifdef __LP64__
    cleared_match_t value =
        (cleared_match_t) ((uint64_t)g_object_get_data (G_OBJECT (button), "button-value") & 0xffffffff); // Binary mask to silence void-pointer-to-enum-cast warning.
#else
    cleared_match_t value =
	(cleared_match_t)g_object_get_data (G_OBJECT (button), "button-value");
#endif

    if (is_on)
        fe->value |= value;
    else
        fe->value &= ~value;
}

static GtkWidget *
make_menu (GNCSearchCoreType *fe)
{
    GNCSearchReconciled *fi = (GNCSearchReconciled *)fe;
    GtkComboBox *combo;

    combo = GTK_COMBO_BOX(gnc_combo_box_new_search());
    gnc_combo_box_search_add(combo, _("is"), QOF_CHAR_MATCH_ANY);
    gnc_combo_box_search_add(combo, _("is not"), QOF_CHAR_MATCH_NONE);
    gnc_combo_box_search_changed(combo, &fi->how);
    gnc_combo_box_search_set_active(combo, fi->how ? fi->how : QOF_CHAR_MATCH_ANY);

    return GTK_WIDGET(combo);
}

static GtkWidget *
make_toggle (GNCSearchReconciled *fi, char *label, cleared_match_t option)
{
    GtkWidget *toggle;

    toggle = gtk_check_button_new_with_label (label);
    gtk_toggle_button_set_active (GTK_TOGGLE_BUTTON (toggle), (fi->value & option));
    g_object_set_data (G_OBJECT (toggle), "button-value", (gpointer) option);
    g_signal_connect (G_OBJECT (toggle), "toggled", G_CALLBACK (toggle_changed), fi);

    return toggle;
}

static GtkWidget *
gncs_get_widget (GNCSearchCoreType *fe)
{
    GtkWidget *toggle, *menu, *box;
    GNCSearchReconciled *fi = (GNCSearchReconciled *)fe;

    g_return_val_if_fail (fi, NULL);
    g_return_val_if_fail (IS_GNCSEARCH_RECONCILED (fi), NULL);

    box = gtk_box_new (GTK_ORIENTATION_HORIZONTAL, 3);
    gtk_box_set_homogeneous (GTK_BOX (box), FALSE);

    /* Build and connect the option menu */
    menu = make_menu (fe);
    gtk_box_pack_start (GTK_BOX (box), menu, FALSE, FALSE, 3);

    /* Build and connect the toggles */
    toggle = make_toggle (fi, _("Not Cleared"), CLEARED_NO);
    gtk_box_pack_start (GTK_BOX (box), toggle, FALSE, FALSE, 3);

    toggle = make_toggle (fi, _("Cleared"), CLEARED_CLEARED);
    gtk_box_pack_start (GTK_BOX (box), toggle, FALSE, FALSE, 3);

    toggle = make_toggle (fi, _("Reconciled"), CLEARED_RECONCILED);
    gtk_box_pack_start (GTK_BOX (box), toggle, FALSE, FALSE, 3);

    toggle = make_toggle (fi, _("Frozen"), CLEARED_FROZEN);
    gtk_box_pack_start (GTK_BOX (box), toggle, FALSE, FALSE, 3);

    toggle = make_toggle (fi, _("Voided"), CLEARED_VOIDED);
    gtk_box_pack_start (GTK_BOX (box), toggle, FALSE, FALSE, 3);

    /* And return the box */
    return box;
}

static QofQueryPredData* gncs_get_predicate (GNCSearchCoreType *fe)
{
    GNCSearchReconciled *fi = (GNCSearchReconciled *)fe;
    char chars[6];
    cleared_match_t value;
    int i;

    g_return_val_if_fail (fi, NULL);
    g_return_val_if_fail (IS_GNCSEARCH_RECONCILED (fi), NULL);

    /* This code should look a lot like xaccQueryAddClearedMatch() */

    value = fi->value;
    i = 0;

    if (value & CLEARED_CLEARED)
        chars[i++] = CREC;
    if (value & CLEARED_RECONCILED)
        chars[i++] = YREC;
    if (value & CLEARED_FROZEN)
        chars[i++] = FREC;
    if (value & CLEARED_NO)
        chars[i++] = NREC;
    if (value & CLEARED_VOIDED)
        chars[i++] = VREC;
    chars[i] = '\0';

    return qof_query_char_predicate (fi->how, chars);
}

static GNCSearchCoreType *gncs_clone(GNCSearchCoreType *fe)
{
    GNCSearchReconciled *se, *fse = (GNCSearchReconciled *)fe;

    g_return_val_if_fail (fse, NULL);
    g_return_val_if_fail (IS_GNCSEARCH_RECONCILED (fse), NULL);

    se = gnc_search_reconciled_new ();
    gnc_search_reconciled_set_value (se, fse->value);
    gnc_search_reconciled_set_how (se, fse->how);

    return (GNCSearchCoreType *)se;
}
