/********************************************************************\
 * dialog-budget-list.c --  Implementation of the budget list       *
 *                          dialog.                                 *
 * Copyright (C) 08 sep 2003    Darin Willits <darin@willits.ca>    *
 *                                                                  *
 * This program is free software; you can redistribute it and/or    *
 * modify it under the terms of the GNU General Public License as   *
 * published by the Free Software Foundation; either version 2 of   *
 * the License, or (at your option) any later version.              *
 *                                                                  *
 * This program is distributed in the hope that it will be useful,  *
 * but WITHOUT ANY WARRANTY; without even the implied warranty of   *
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the    *
 * GNU General Public License for more details.                     *
 *                                                                  *
 * You should have received a copy of the GNU General Public License*
 * along with this program; if not, contact:                        *
 *                                                                  *
 * Free Software Foundation           Voice:  +1-617-542-5942       *
 * 59 Temple Place - Suite 330        Fax:    +1-617-542-2652       *
 * Boston, MA  02111-1307,  USA       gnu@gnu.org                   *
 *                                                                  *
\********************************************************************/

/** @addtogroup GUI
 *     @{ */
/** @addtogroup Budget_GUI
 *     @{ */
/** @file dialog-budget-list.c
 *  @brief Implementation of the budget list dialog.
 *  @author Created by Darin Willits 08 sep 2003 
 *  @author Copyright (c) 08 sep 2003 Darin Willits <darin@willits.ca>
 *
 * This dialog presents the user with a list of the available budget 
 * objects.  From here they can create a new budget or edit/delete
 * an existing one.
 */

// Includes
#include <glib.h>
#include <gnome.h>

#include "dialog-budget-list.h"
#include "dialog-budget-workbench.h"
#include "druid-budget-create.h"
#include "dialog-utils.h"
#include "gnc-ui-util.h"

#include "gnc-budget-gui.h"
#include "gnc-budget-book.h"
#include "gnc-budget.h"
#include "gnc-budget-list-tree-model.h"


/* The budget list dialog structure. */
typedef struct{
    GtkWidget* wnd;
    GladeXML* xml;
    
    GtkWidget* treeView;
    GtkTreeModel* treeModel;

} BudgetListDlg;

/* The budget list columns. */
enum{
    COLUMN_NAME,
    COLUMN_DESCRIPTION,
    BUDGET_LIST_NUM_COLS
};


/* Add the new budget object to the tree model.  
 * FIXME: This will be unnecessary once we create a tree model
 * which directly relates to the book's list of budgets but for 
 * now we have to hack around it.  This is ugly though and prone
 * to error so it should/must be fixed.
 */
void add_budget_to_model( gpointer data, gpointer user_data )
{
    GncBudget* budget;
    GtkTreeIter iter;
    GtkTreeModel* treeModel;
    /*BudgetListDlg* dlg;*/
    
    budget = data;
    treeModel = user_data;

    if((budget == NULL) || (treeModel == NULL)){
        return;
    }
    
    gtk_list_store_append (GTK_LIST_STORE(treeModel), &iter);
    gtk_list_store_set (GTK_LIST_STORE(treeModel), &iter,
			  COLUMN_NAME, gnc_budget_get_name(budget),
			  COLUMN_DESCRIPTION, gnc_budget_get_description(budget),
			  -1);
}

/* Initialize the tree model with the list of budgets from the
 * book.
 */
/*
static void fill_model(BudgetListDlg* dlg)
{
    GList *budgetList;
    GtkListStore* store;

    store = gtk_list_store_new (BUDGET_LIST_NUM_COLS,
			      G_TYPE_STRING,
			      G_TYPE_STRING);

    dlg->treeModel = GTK_TREE_MODEL(store);
    
    budgetList = gnc_book_get_budgets(gnc_get_current_book());
    g_list_foreach( budgetList, add_budget_to_model,  dlg->treeModel);
}
*/
/* Add the columns to the tree view. */
static void add_columns(GtkTreeView* treeView)
{
    GtkCellRenderer *renderer;
    GtkTreeViewColumn *column;

    /* column for name */
    renderer = gtk_cell_renderer_text_new ();
    column = gtk_tree_view_column_new_with_attributes ("Name",
						     renderer,
						     "text",
						     COLUMN_NAME,
						     NULL);
    gtk_tree_view_append_column (treeView, column);

    /* column for description */
    renderer = gtk_cell_renderer_text_new ();
    column = gtk_tree_view_column_new_with_attributes ("Description",
						     renderer,
						     "text",
						     COLUMN_DESCRIPTION,
						     NULL);
    gtk_tree_view_append_column (treeView, column);
}

/* Destroy the budget list dialog. */
static void budget_list_dialog_destroy(BudgetListDlg* dlg)
{
    if(dlg == NULL){
        return;
    }
    gtk_widget_destroy(dlg->wnd);
    g_free(dlg);
}

/* Event handler for the close button. */
static void close_button_clicked(GtkWidget* object, gpointer data)
{
    budget_list_dialog_destroy(data);
}

/* Event handler for the new button. */
static void new_button_clicked(GtkWidget* object, gpointer data)
{
    BudgetListDlg* dlg = data;

    gnc_budget_druid_create(dlg->treeModel);
}

/* Event handler for the edit button.  Create a budget workbench 
 * dialog with the given budget object. */
static void edit_button_clicked(GtkWidget* object, gpointer data)
{
    GtkTreeIter iter;
    BudgetListDlg* dlg = data;
    GtkTreeModel *model = gtk_tree_view_get_model (GTK_TREE_VIEW(dlg->treeView));
    GtkTreeSelection *selection = gtk_tree_view_get_selection (GTK_TREE_VIEW(dlg->treeView));

    /* If a budget is selected use it to create a budget workbench. */
    if (gtk_tree_selection_get_selected (selection, NULL, &iter)){
        gint i;
        GtkTreePath *path;
        GncBudget* budget;
        GList* budgetList;

        path = gtk_tree_model_get_path (model, &iter);
        i = gtk_tree_path_get_indices (path)[0];
    
        budgetList = gnc_book_get_budgets(gnc_get_current_book());
        budget = g_list_nth_data(budgetList, i);

        if(budget != NULL){
            printf("Creating Workbench: Budget: %p\n", budget);
            /* Create the workbench. */
            gnc_budget_workbench_dialog_create(budget);
        }
        
        gtk_tree_path_free (path);
    }
}

/* Create the budget list dialog. */
static void budget_create_list_dialog(BudgetListDlg* dlg)
{
    GladeXML *xml;
    GtkWidget *new_button;
    GtkWidget *edit_button;
    GtkWidget *close_button;
    
    /* Load the main dialog widget. */
    xml = gnc_glade_xml_new (GNC_BUDGET_GUI_FILE, "Budget List");
    dlg->wnd = glade_xml_get_widget(xml, "Budget List");
    dlg->xml = xml;
    
    /* Create and fill the model */
    /*fill_model(dlg);*/
    dlg->treeModel = gnc_budget_list_tree_model_new(gnc_get_current_book());
    
    dlg->treeView = glade_xml_get_widget(xml, "Budget List View");
    gtk_tree_view_set_model (GTK_TREE_VIEW(dlg->treeView), dlg->treeModel);

    add_columns(GTK_TREE_VIEW(dlg->treeView));

    
    /* Connect signals. */
    new_button = glade_xml_get_widget( dlg->xml, "new_button" );
    g_signal_connect( new_button, "clicked",
                        G_CALLBACK(new_button_clicked), dlg);
    edit_button = glade_xml_get_widget( dlg->xml, "edit_button" );
    g_signal_connect( edit_button, "clicked",
                        G_CALLBACK(edit_button_clicked), dlg);
    close_button = glade_xml_get_widget( dlg->xml, "close_button" );
    g_signal_connect( close_button, "clicked",
                        G_CALLBACK(close_button_clicked), dlg);
}

/* Public interface for creating a budget list dialog. */
void gnc_budget_list_dialog_create(void)
{
    BudgetListDlg* dlg;

    dlg = g_new0(BudgetListDlg, 1);

    budget_create_list_dialog(dlg);
    
    gtk_widget_show_all(dlg->wnd);
}

/** @} */
/** @} */
