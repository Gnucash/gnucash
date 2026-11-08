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

#include "qof.h"
#include "gnc-ui-util.h"
#include "gnc-gui-query.h"
#include "gncOwner.h"

#include "business-gnome-utils.h"
#include "search-owner.h"
#include "search-core-utils.h"

#define d(x)

static GNCSearchCoreType *gncs_clone(GNCSearchCoreType *fe);
static void pass_parent (GNCSearchCoreType *fe, gpointer parent);
static gboolean gncs_validate (GNCSearchCoreType *fe);
static GtkWidget *gncs_get_widget(GNCSearchCoreType *fe);
static QofQueryPredData* gncs_get_predicate (GNCSearchCoreType *fe);

static void gnc_search_owner_finalize   (GObject *obj);

struct _GNCSearchOwner
{
    GNCSearchCoreType parent_instance;

    QofGuidMatch    how;
    gboolean       job_only;
    gboolean       include_jobs;
    GncOwnerType   end_owner_type;

    GncOwner    owner;
    GtkWindow * parent;
    GtkWidget * owner_box;
    GtkWidget * owner_choice;
} GNCSearchOwnerPrivate;

G_DEFINE_TYPE(GNCSearchOwner, gnc_search_owner, GNC_TYPE_SEARCH_CORE_TYPE)

enum
{
    LAST_SIGNAL
};

#if LAST_SIGNAL > 0
static guint signals[LAST_SIGNAL] = { 0 };
#endif

static void
gnc_search_owner_class_init (GNCSearchOwnerClass *klass)
{
    GObjectClass *object_class;
    GNCSearchCoreTypeClass *gnc_search_core_type = (GNCSearchCoreTypeClass *)klass;

    object_class = G_OBJECT_CLASS (klass);

    object_class->finalize = gnc_search_owner_finalize;

    /* override methods */
    gnc_search_core_type->validate = gncs_validate;
    gnc_search_core_type->pass_parent = pass_parent;
    gnc_search_core_type->get_widget = gncs_get_widget;
    gnc_search_core_type->get_predicate = gncs_get_predicate;
    gnc_search_core_type->clone = gncs_clone;
}

static void
gnc_search_owner_init (GNCSearchOwner *o)
{
}

static void
gnc_search_owner_finalize (GObject *obj)
{
    g_assert (GNC_IS_SEARCH_OWNER (obj));

    G_OBJECT_CLASS (gnc_search_owner_parent_class)->finalize(obj);
}

/**
 * gnc_search_owner_new:
 *
 * Create a new GNCSearchOwner object.
 *
 * Return value: A new #GNCSearchOwner object.
 **/
GNCSearchOwner *
gnc_search_owner_new (void)
{
    GNCSearchOwner *o = g_object_new(gnc_search_owner_get_type (), NULL);
    return o;
}

static GNCSearchOwner *
search_document_owner_new (GncOwnerType type)
{
    GNCSearchOwner *o = gnc_search_owner_new ();
    o->include_jobs = TRUE;
    o->end_owner_type = type;
    o->owner.type = type;
    return o;
}

GNCSearchOwner *
gnc_search_invoice_owner_new (void)
{
    return search_document_owner_new (GNC_OWNER_CUSTOMER);
}

GNCSearchOwner *
gnc_search_bill_owner_new (void)
{
    return search_document_owner_new (GNC_OWNER_VENDOR);
}

GNCSearchOwner *
gnc_search_voucher_owner_new (void)
{
    return search_document_owner_new (GNC_OWNER_EMPLOYEE);
}

GNCSearchOwner *
gnc_search_job_new (void)
{
    GNCSearchOwner *o = gnc_search_owner_new ();
    o->job_only = TRUE;
    gncOwnerInitJob (&o->owner, NULL);
    return o;
}

static gboolean
gncs_validate (GNCSearchCoreType *fe)
{
    GNCSearchOwner *fi = (GNCSearchOwner *)fe;
    gboolean valid = TRUE;

    g_return_val_if_fail (fi, FALSE);
    g_return_val_if_fail (GNC_IS_SEARCH_OWNER (fi), FALSE);

    if (fi->owner.owner.undefined == NULL)
    {
        valid = FALSE;
        gnc_error_dialog (GTK_WINDOW(fi->parent), "%s", _("You have not selected an owner"));
    }

    if (valid && fi->end_owner_type != GNC_OWNER_NONE &&
        gncOwnerGetType (gncOwnerGetEndOwner (&fi->owner)) != fi->end_owner_type)
    {
        valid = FALSE;
        gnc_error_dialog (GTK_WINDOW(fi->parent), "%s",
                          _("The selected owner is not valid for this document type."));
    }

    return valid;
}

static int
owner_changed_cb (GtkWidget *widget, gpointer data)
{
    GNCSearchOwner *fe = data;

    gnc_owner_get_owner (fe->owner_choice, &(fe->owner));
    return FALSE;
}

static void
set_owner_widget (GNCSearchOwner *fe)
{
    /* Remove the old choice widget */
    if (fe->owner_choice)
        gtk_container_remove (GTK_CONTAINER (fe->owner_box), fe->owner_choice);

    /* Create a new choice widget */
    fe->owner_choice =
        gnc_owner_select_create (NULL, fe->owner_box,
                                 gnc_get_current_book(), &(fe->owner));

    /* Setup the "changed" callback */
    g_signal_connect (G_OBJECT (fe->owner_choice), "changed",
                      G_CALLBACK (owner_changed_cb), fe);

    gtk_widget_show_all (fe->owner_choice);
}

static void
type_combo_changed (GtkWidget *widget, GNCSearchOwner *fe)
{
    GncOwnerType type;

    g_return_if_fail(GTK_IS_COMBO_BOX(widget));

    type = gnc_combo_box_search_get_active(GTK_COMBO_BOX(widget));

    /* If the type changed or if we don't have a type create the owner_choice */
    if (type != gncOwnerGetType (&(fe->owner)))
    {
        fe->owner.type = type;
        fe->owner.owner.undefined = NULL;
        set_owner_widget (fe);
    }
    else if (fe->owner_choice == NULL)
        set_owner_widget (fe);
}

static void
owner_type_cell_data (GtkCellLayout *layout, GtkCellRenderer *cell,
                      GtkTreeModel *model, GtkTreeIter *iter, gpointer data)
{
    GNCSearchOwner *fi = data;
    guint type;
    gboolean sensitive;

    gtk_tree_model_get (model, iter, GNC_COMBO_SEARCH_COL_VALUE, &type, -1);
    sensitive = fi->end_owner_type == GNC_OWNER_NONE ||
                type == fi->end_owner_type ||
                (type == GNC_OWNER_JOB && fi->end_owner_type != GNC_OWNER_EMPLOYEE);
    g_object_set (cell, "sensitive", sensitive, NULL);
}

static GtkWidget *
make_type_menu (GNCSearchCoreType *fe)
{
    GNCSearchOwner *fi = (GNCSearchOwner *)fe;
    GtkComboBox *combo;
    GncOwnerType type;
    GList *cells;

    type = gncOwnerGetType (&(fi->owner));

    combo = GTK_COMBO_BOX(gnc_combo_box_new_search());
    gnc_combo_box_search_add(combo, _("Customer"), GNC_OWNER_CUSTOMER);
    gnc_combo_box_search_add(combo, _("Vendor"), GNC_OWNER_VENDOR);
    gnc_combo_box_search_add(combo, _("Employee"), GNC_OWNER_EMPLOYEE);
    gnc_combo_box_search_add(combo, _("Job"), GNC_OWNER_JOB);

    cells = gtk_cell_layout_get_cells (GTK_CELL_LAYOUT (combo));
    gtk_cell_layout_set_cell_data_func (GTK_CELL_LAYOUT (combo), cells->data,
                                        owner_type_cell_data, fi, NULL);
    g_list_free (cells);

    g_signal_connect (combo, "changed", G_CALLBACK (type_combo_changed), fe);
    gnc_combo_box_search_set_active(combo, type);

    return GTK_WIDGET(combo);


}

static GtkWidget *
make_how_menu (GNCSearchCoreType *fe)
{
    GNCSearchOwner *fi = (GNCSearchOwner *)fe;
    GtkComboBox *combo;

    combo = GTK_COMBO_BOX(gnc_combo_box_new_search());
    gnc_combo_box_search_add(combo, _("is"), QOF_GUID_MATCH_ANY);
    gnc_combo_box_search_add(combo, _("is not"), QOF_GUID_MATCH_NONE);
    gnc_combo_box_search_changed(combo, &fi->how);
    gnc_combo_box_search_set_active(combo, fi->how ? fi->how : QOF_GUID_MATCH_ANY);

    return GTK_WIDGET(combo);
}

static void
pass_parent (GNCSearchCoreType *fe, gpointer parent)
{
    GNCSearchOwner *fi = (GNCSearchOwner *)fe;

    g_return_if_fail (fi);
    g_return_if_fail (GNC_IS_SEARCH_OWNER (fi));

    fi->parent = GTK_WINDOW(parent);
}

static GtkWidget *
gncs_get_widget (GNCSearchCoreType *fe)
{
    GtkWidget *how_menu, *type_menu, *box;
    GNCSearchOwner *fi = (GNCSearchOwner *)fe;

    g_return_val_if_fail (fi, NULL);
    g_return_val_if_fail (GNC_IS_SEARCH_OWNER (fi), NULL);

    box = gtk_box_new (GTK_ORIENTATION_HORIZONTAL, 3);
    gtk_box_set_homogeneous (GTK_BOX (box), FALSE);

    /* Build and connect the "how" option menu. */
    how_menu = make_how_menu (fe);
    gtk_box_pack_start (GTK_BOX (box), how_menu, FALSE, FALSE, 3);

    /* Create the owner box */
    fi->owner_box = gtk_box_new (GTK_ORIENTATION_HORIZONTAL, 0);
    gtk_box_set_homogeneous (GTK_BOX (fi->owner_box), FALSE);

    /* Build and connect the "type" option menu.
     * Note that this will build the owner_choice and
     * put it in the owner_box we just created.
     */
    if (fi->job_only)
        set_owner_widget (fi);
    else
    {
        type_menu = make_type_menu (fe);
        gtk_box_pack_start (GTK_BOX (box), type_menu, FALSE, FALSE, 3);
    }

    /* connect the owner box */
    gtk_box_pack_start (GTK_BOX (box), fi->owner_box, FALSE, FALSE, 3);

    /* And return the box */
    return box;
}

static QofQueryPredData* gncs_get_predicate (GNCSearchCoreType *fe)
{
    GNCSearchOwner *fi = (GNCSearchOwner *)fe;
    const GncGUID *guid;
    GList *l = NULL;
    QofQueryPredData *predicate;

    g_return_val_if_fail (fi, NULL);
    g_return_val_if_fail (GNC_IS_SEARCH_OWNER (fi), NULL);

    guid = gncOwnerGetGUID (&(fi->owner));
    l = g_list_prepend (l, (gpointer)guid);

    if (fi->include_jobs &&
        (gncOwnerGetType (&fi->owner) == GNC_OWNER_CUSTOMER ||
         gncOwnerGetType (&fi->owner) == GNC_OWNER_VENDOR))
    {
        QofQuery *q = qof_query_create_for (GNC_ID_JOB);
        GList *jobs;

        qof_query_set_book (q, qof_instance_get_book (qofOwnerGetOwner (&fi->owner)));
        qof_query_add_guid_match (q, qof_query_build_param_list
                                  (JOB_OWNER, QOF_PARAM_GUID, NULL),
                                  guid, QOF_QUERY_AND);
        for (jobs = qof_query_run (q); jobs; jobs = jobs->next)
            l = g_list_prepend (l, (gpointer)gncJobGetGUID (jobs->data));
        qof_query_destroy (q);
    }

    predicate = qof_query_guid_predicate (fi->how, l);
    g_list_free (l);
    return predicate;
}

static GNCSearchCoreType *gncs_clone(GNCSearchCoreType *fe)
{
    GNCSearchOwner *se, *fse = (GNCSearchOwner *)fe;

    g_return_val_if_fail (fse, NULL);
    g_return_val_if_fail (GNC_IS_SEARCH_OWNER (fse), NULL);

    se = gnc_search_owner_new ();
    se->how = fse->how;
    se->job_only = fse->job_only;
    se->include_jobs = fse->include_jobs;
    se->end_owner_type = fse->end_owner_type;
    gncOwnerCopy (&(fse->owner), &(se->owner));

    return (GNCSearchCoreType *)se;
}
