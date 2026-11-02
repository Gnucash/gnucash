/*
 * Copyright (C) 2005, Chris Shoemaker <c.shoemaker@cox.net>
 *
 * This program is free software; you can redistribute it and/or
 * modify it under the terms of the GNU General Public License as
 * published by the Free Software Foundation; either version 2 of
 * the License, or (at your option) any later version.
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

/** @addtogroup budget
 *     @{ */

#include <config.h>

#include <gtk/gtk.h>
#include <glib/gi18n.h>
#include "gnc-tree-model-budget.h"
#include "gnc-budget.h"
#include "gnc-ui-util.h"
#include "gnc-session.h"

static void
budget_model_book_free (gpointer data)
{
    GWeakRef *book = data;
    g_weak_ref_clear (book);
    g_free (book);
}

/* Add the new budget object to the tree model.  */
static void add_budget_to_model(QofInstance* data, gpointer user_data )
{
    GtkTreeIter iter;
    GncBudget* budget = GNC_BUDGET(data);
    GtkTreeModel* treeModel = user_data;

    g_return_if_fail(GNC_IS_BUDGET(budget));
    g_return_if_fail(budget && treeModel);

    gtk_list_store_append (GTK_LIST_STORE(treeModel), &iter);
    gtk_list_store_set (GTK_LIST_STORE(treeModel), &iter,
                        BUDGET_GUID_COLUMN, gnc_budget_get_guid(budget),
                        BUDGET_NAME_COLUMN, gnc_budget_get_name(budget),
                        BUDGET_DESCRIPTION_COLUMN,
                        gnc_budget_get_description(budget), -1);
}

/* CAS: Even though it works, something feels not-quite-right with
 * this design.  The idea here is to _not_ provide yet another
 * implementation of GtkTreeModel, this time for budgets.  Instead,
 * right now, we're using the already implemented GtkListStore.  This
 * has a couple consequences: 1) We allocate a new store upon every
 * call, so the memory is owned by caller.  2) The model won't reflect
 * later updates to the book. Each row owns its GUID snapshot; lookup resolves
 * it only while the original book is still the active, open book. A budget
 * removed after the snapshot is therefore an empty selection, not a borrowed
 * pointer into a destroyed instance.
 *
 * If, for some reason, I decide I can't live with or remove those
 * consequences, I still think there must be some better way than
 * re-implementing GtkTreeModel.  One idea I'm toying with is to
 * implement a GtkTreeModel for QofCollections, which would offer only
 * the GncGUID as a field.  Then, TreeViews could add their own columns
 * with custom CellDataFuncs to display the object-specific fields.
 * Or, something like that.  :)
 *
 */
GtkTreeModel *
gnc_tree_model_budget_new(QofBook *book)
{
    GtkListStore* store;
    GWeakRef *original_book = g_new0 (GWeakRef, 1);

    store = gtk_list_store_new (BUDGET_LIST_NUM_COLS,
                                GNC_TYPE_GUID,
                                G_TYPE_STRING,
                                G_TYPE_STRING);
    g_weak_ref_init (original_book, book);
    g_object_set_data_full (G_OBJECT (store), "gnc-budget-original-book",
                            original_book, budget_model_book_free);

    qof_collection_foreach(qof_book_get_collection(book, GNC_ID_BUDGET),
                           add_budget_to_model, GTK_TREE_MODEL(store));

    return GTK_TREE_MODEL(store);
}

void
gnc_tree_view_budget_set_model(GtkTreeView *tv, GtkTreeModel *tm)
{
    GtkCellRenderer *renderer;
    GtkTreeViewColumn *column;

    gtk_tree_view_set_model (tv, tm);

    /* column for name */
    renderer = gtk_cell_renderer_text_new ();
    column = gtk_tree_view_column_new_with_attributes (
                 _("Name"), renderer, "text", BUDGET_NAME_COLUMN, NULL);
    gtk_tree_view_append_column (tv, column);

    /* column for description */
    renderer = gtk_cell_renderer_text_new ();
    column = gtk_tree_view_column_new_with_attributes (
                 _("Description"), renderer, "text", BUDGET_DESCRIPTION_COLUMN, NULL);
    gtk_tree_view_append_column (tv, column);

}

GncBudget *
gnc_tree_model_budget_get_budget(GtkTreeModel *tm, GtkTreeIter *iter)
{
    GncBudget *bgt;
    GncGUID *guid;
    GWeakRef *original_book = g_object_get_data (G_OBJECT (tm),
                                                 "gnc-budget-original-book");
    QofBook *book = original_book ? g_weak_ref_get (original_book) : NULL;

    if (!book || !qof_book_is_open (book) || qof_book_shutting_down (book) ||
        !gnc_current_session_exist () || book != gnc_get_current_book ())
    {
        g_clear_object (&book);
        return NULL;
    }

    gtk_tree_model_get (tm, iter, BUDGET_GUID_COLUMN, &guid, -1);
    bgt = guid ? gnc_budget_lookup (guid, book) : NULL;
    guid_free (guid);
    g_object_unref (book);
    return bgt;
}

gboolean
gnc_tree_model_budget_get_iter_for_budget(GtkTreeModel *tm, GtkTreeIter *iter,
        GncBudget *bgt)
{
    const GncGUID *guid1;
    GncGUID *guid2;

    g_return_val_if_fail(GNC_BUDGET(bgt), FALSE);

    guid1 = gnc_budget_get_guid(bgt);
    if (!gtk_tree_model_get_iter_first(tm, iter))
        return FALSE;
    while (gtk_list_store_iter_is_valid(GTK_LIST_STORE(tm), iter))
    {
        gtk_tree_model_get (tm, iter, BUDGET_GUID_COLUMN, &guid2, -1);

        gboolean matches = guid2 && guid_equal (guid1, guid2);
        guid_free (guid2);
        if (matches)
            return TRUE;

        if (!gtk_tree_model_iter_next(tm, iter))
            return FALSE;
    }
    return FALSE;
}

/** @} */
