/********************************************************************\
 * dialog-budget-workbench.c -- Implementation of the budget        *
 *                              workbench dialog.                   *
 * Copyright (C) 14 sep 2003    Darin Willits <darin@willits.ca>    *
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
/** @file dialog-budget-workbench.c
 *  @brief Implmentation of the budget workbench dialog.
 *  @author Created by Darin Willits 14 sep 2003 
 *  @author Copyright (c) 14 sep 2003 Darin Willits <darin@willits.ca>
 *
 * The Budget Workbench is the main budgeting window.  This window
 * allows the user to configure and track their budget.
 */

// Includes
#include <gnome.h>
#include "dialog-utils.h"
#include "gnc-ui-util.h"
#include "dialog-budget-category.h"
#include "gnc-general-select.h"
#include "gnc-budget-gui.h"
#include "gnc-ui-util.h"
#include "gnc-date-edit.h"
#include "misc-gnome-utils.h"
#include "gnc-frequency.h"

#include "dialog-budget-workbench.h"
#include "dialog-budget-category.h"
#include "gnc-budget-tree-model.h"
#include "gnc-budget-period.h"


/* The Budget Workbench dialog structure. */
typedef struct{
    GtkWidget* wnd;
    GladeXML* xml;

    GtkEntry* name_entry;
    GtkTextView* description_view;
    GNCDateEdit* start_date;
    GNCFrequency* freqWnd;
    GtkSpinButton*  length_button;
    GtkOptionMenu*  length_menu;
    GtkOptionMenu*  period_menu;
    GtkTreeView*    category_view;
    GtkTreeView*    balance_view;
    GtkTreeView*    track_view;
    GtkTreeModel*   category_model;
    GtkLabel*       period_label;

    int colNum;
    int active_period;
    
    GncBudget* budget;

} BudgetWorkbench;

/* Event handler for a cell having been edited. */
static void cell_edited (GtkCellRendererText *cell,
                	        const gchar         *path_string,
	                        const gchar         *new_text,
	                        gpointer             data)
{
    BudgetWorkbench* bench = data;
    GtkTreeModel *model = bench->category_model;
    GtkTreePath *path = gtk_tree_path_new_from_string (path_string);
    GtkTreeIter iter;
    GncBudgetCategory* category;

    gint column;
    gint index;
    gnc_numeric value;

    column = GPOINTER_TO_INT (g_object_get_data (G_OBJECT (cell), "column"));

    gtk_tree_model_get_iter (model, &iter, path);
    category = gnc_budget_tree_model_get_category(GNC_BUDGET_TREE_MODEL(model), &iter);

    if(category == NULL){
        printf("Cell_Edited: Category is NULL!\n");
        return;
    }

    index = column - GNC_BUDGET_TREE_MODEL_NUM_STATIC_COLUMNS;
    value = double_to_gnc_numeric(atof(new_text), 1, GNC_RND_NEVER);

    gnc_budget_category_set_period_value_by_index(category, index, value);

}

/* Add a single period column to the balance view tab. */
static void add_period_column(gpointer data, gpointer user_data)
{
    GncBudgetPeriod* period = data;
    GDate* startDate, *endDate;
    BudgetWorkbench* bench = user_data;
    GtkCellRenderer *renderer;
    GtkTreeViewColumn *column;
    char buffer[256];
    gint colnum;

    startDate = gnc_budget_period_get_start_date(period);
    endDate = gnc_budget_period_get_end_date(period);

    /* Format the period column title. */
    sprintf(buffer, "%d/%d/%d", g_date_get_day(startDate),
                                           g_date_get_month(startDate),
                                           g_date_get_year(startDate));

    /* column for severities */
    colnum = GNC_BUDGET_TREE_MODEL_NUM_STATIC_COLUMNS + bench->colNum;
    renderer = gtk_cell_renderer_text_new ();
    g_signal_connect (renderer, "edited",
		    G_CALLBACK (cell_edited), bench);
    g_object_set_data (G_OBJECT (renderer), "column", (gint *)colnum);
    column = gtk_tree_view_column_new_with_attributes (buffer,
						     renderer,
						     "text", colnum,
                             "editable", GNC_BUDGET_TREE_MODEL_COL_EDITABLE,
						     NULL);
    gtk_tree_view_append_column (bench->balance_view, column);

    bench->colNum++;
}

/* Iterates across the list of periods and adds a column for each
 * one to the balance budget view. */
static void add_balance_budget_columns(BudgetWorkbench* bench)
{
    GList* period_list;
    GtkCellRenderer *renderer;
    GtkTreeViewColumn *column;
    
    /* column for category name */
    renderer = gtk_cell_renderer_text_new ();
    column = gtk_tree_view_column_new_with_attributes ("Name",
						     renderer,
						     "text", GNC_BUDGET_TREE_MODEL_COL_NAME,
                             NULL);
    gtk_tree_view_append_column (bench->balance_view, column);
 
    
    bench->colNum = 0;
    period_list = gnc_budget_get_period_list(bench->budget);

    g_list_foreach(period_list, add_period_column, bench);
}

/* Remove a single column from the given TreeView. */
static void remove_column(gpointer data, gpointer user_data)
{
    GtkTreeView* treeView = user_data;
    GtkTreeViewColumn* column = data;

    gtk_tree_view_remove_column(treeView, column);
}

/* Removes all the columns from the balance view.  This is used to 
 * re-build the budget from category data. */
static void remove_balance_view_columns(BudgetWorkbench* bench)
{
    GList* columns;
    columns = gtk_tree_view_get_columns(bench->balance_view);

    g_list_foreach(columns, remove_column, bench->balance_view);

    g_list_free(columns);
}

/* Add the category columns to the given tree view. */
static void add_category_columns(GtkTreeView* treeView)
{
    GtkCellRenderer *renderer;
    GtkTreeViewColumn *column;

    /* column for category name */
    renderer = gtk_cell_renderer_text_new ();
    column = gtk_tree_view_column_new_with_attributes ("Name",
						     renderer,
						     "text", GNC_BUDGET_TREE_MODEL_COL_NAME,
						     NULL);
    gtk_tree_view_append_column (treeView, column);

    /* column for value */
    renderer = gtk_cell_renderer_text_new ();
    column = gtk_tree_view_column_new_with_attributes ("Value",
						     renderer,
						     "text", GNC_BUDGET_TREE_MODEL_COL_VALUE,
						     NULL);
    gtk_tree_view_append_column (treeView, column);
    
    /* column for freq */
    renderer = gtk_cell_renderer_text_new ();
    column = gtk_tree_view_column_new_with_attributes ("Frequency",
						     renderer,
						     "text", GNC_BUDGET_TREE_MODEL_COL_FREQ,
						     NULL);
    gtk_tree_view_append_column (treeView, column);
}

/* Add the columns for the track budget view. */
static void add_track_columns(GtkTreeView* treeView)
{
    GtkCellRenderer *renderer;
    GtkTreeViewColumn *column;

    /* column for category name */
    renderer = gtk_cell_renderer_text_new ();
    column = gtk_tree_view_column_new_with_attributes ("Name",
						     renderer,
						     "text", GNC_BUDGET_TREE_MODEL_COL_NAME,
						     NULL);
    gtk_tree_view_append_column (treeView, column);

    /* column for previous overflow value */
    renderer = gtk_cell_renderer_text_new ();
    column = gtk_tree_view_column_new_with_attributes ("Previous Overflow",
						     renderer,
						     "text", GNC_BUDGET_TREE_MODEL_COL_LAST_OVERFLOW,
						     NULL);
    gtk_tree_view_append_column (treeView, column);
    
    /* column for actual period value */
    renderer = gtk_cell_renderer_text_new ();
    column = gtk_tree_view_column_new_with_attributes ("Actual",
						     renderer,
						     "text", GNC_BUDGET_TREE_MODEL_COL_ACTUAL,
						     NULL);
    gtk_tree_view_append_column (treeView, column);    
    
    /* column for planned period value */
    renderer = gtk_cell_renderer_text_new ();
    column = gtk_tree_view_column_new_with_attributes ("Planned",
						     renderer,
						     "text", GNC_BUDGET_TREE_MODEL_COL_PLANNED,
						     NULL);
    gtk_tree_view_append_column (treeView, column);    
    
    /* column for period balance value */
    renderer = gtk_cell_renderer_text_new ();
    column = gtk_tree_view_column_new_with_attributes ("Period Balance",
						     renderer,
						     "text", GNC_BUDGET_TREE_MODEL_COL_CUR_OVERFLOW,
						     NULL);
    gtk_tree_view_append_column (treeView, column);    
}

/* Fill in the user interface from the budget object. */
static void workbench_from_budget(BudgetWorkbench* bench)
{
    /* Set the name and description.*/
    gtk_entry_set_text(bench->name_entry, gnc_budget_get_name(bench->budget));
    xxxgtk_textview_set_text(bench->description_view, 
            gnc_budget_get_description(bench->budget));
    
    /* Set the length value.  
     * FIXME: Default to showing length in months
     * cause we have no way of knowing which they choose before.
     * Maybe have to change this.
     */
    gtk_spin_button_set_value(bench->length_button, 
            gnc_budget_get_length_months(bench->budget));
    gtk_option_menu_set_history(bench->length_menu, 0);
}

/* Fill in the budget object from the user interface. */
static void workbench_to_budget(BudgetWorkbench* bench)
{
    const gchar *name, *description;
    FreqSpec* freqSpec;
    GDate* startDate;
    gint length;

    /* Set the name and description of the budget. */
    name = gtk_entry_get_text(bench->name_entry);
    description = xxxgtk_textview_get_text(bench->description_view);

    gnc_budget_set_name(bench->budget, name);
    gnc_budget_set_description(bench->budget, description);
    
    /* Set the length of the budget. */
    length = gtk_spin_button_get_value_as_int(bench->length_button);
    if(gtk_option_menu_get_history(bench->length_menu) == 1){
        gnc_budget_set_length_years(bench->budget, length);
    }
    else{
        gnc_budget_set_length_months(bench->budget, length);
    }
    
    /* Set the budget period. */
    freqSpec = gnc_budget_get_period_frequency(bench->budget);
    startDate = gnc_budget_get_start_date(bench->budget);
    if(freqSpec == NULL){
        freqSpec = xaccFreqSpecMalloc(gnc_budget_get_book(bench->budget));
    }
    gnc_frequency_save_state( bench->freqWnd, freqSpec, startDate);
    gnc_budget_set_period_frequency(bench->budget, freqSpec);

}

/* Destroy and cleanup the budget workbench dialog. */
static void budget_workbench_destroy(BudgetWorkbench* bench)
{
    if(bench == NULL){
        return;
    }
    gtk_widget_destroy(bench->wnd);
    g_free(bench);
}

/* Event handler for the ok button. */
static void ok_clicked(GtkWidget* object, gpointer data)
{
    BudgetWorkbench* bench = data;
    workbench_to_budget(bench);

    budget_workbench_destroy(bench);
}

/* Event handler for the apply button. */
static void apply_clicked(GtkWidget* object, gpointer data)
{
    BudgetWorkbench* bench = data;
    workbench_to_budget(bench);
}

/* Event handler for the cancel button. */
static void cancel_clicked(GtkWidget* object, gpointer data)
{
    BudgetWorkbench* bench = data;
    budget_workbench_destroy(bench);
}

/* Event handler for the new category button. */
static void new_category_clicked(GtkWidget* object, gpointer data)
{
    BudgetWorkbench* bench = data;

    printf("Creating new Category... Budget: %p\n", bench->budget);
    gnc_budget_category_new_dialog_create(bench->budget);
}

/* Event handler for the edit category button. */
static void edit_category_clicked(GtkWidget* object, gpointer data)
{
    BudgetWorkbench* bench = data;
    GtkTreeSelection *selection;
    GtkTreeIter iter;
    GncBudgetCategory *category = NULL;

    printf("Editing Category... Druid: %p Budget: %p\n",bench, bench->budget);
    selection = gtk_tree_view_get_selection (bench->category_view);
    if (!gtk_tree_selection_get_selected (selection, &bench->category_model, &iter)){
        printf("Nothing selected...\n");
        return;
    }

    category = gnc_budget_tree_model_get_category(GNC_BUDGET_TREE_MODEL(bench->category_model), &iter);

    gnc_budget_category_dialog_create(bench->budget, category);
}

/* Event handler for the delete category button. */
static void delete_category_clicked(GtkWidget* object, gpointer data)
{
    BudgetWorkbench* bench = data;
    GtkTreeSelection *selection;
    GtkTreeIter iter;
    GncBudgetCategory *category, *inflowCat, *outflowCat;

    printf("Deleting Category...\n");
    selection = gtk_tree_view_get_selection (bench->category_view);
    if (!gtk_tree_selection_get_selected (selection, &bench->category_model, &iter)){
        printf("Nothing selected...\n");
        return;
    }

    category = gnc_budget_tree_model_get_category(GNC_BUDGET_TREE_MODEL(bench->category_model), &iter);
    inflowCat = gnc_budget_get_inflow_category(bench->budget);
    outflowCat = gnc_budget_get_outflow_category(bench->budget);

    if((category != inflowCat) && (category != outflowCat)){
        gnc_budget_category_delete(category);
    }
    else{
        printf("Cannot delete this category...\n");
    }
}

/* Event handler for the regenerate button.  This will regenerate the 
 * values for each budget period from the category values. */
static void regenerate_clicked(GtkWidget* object, gpointer data)
{
    BudgetWorkbench* bench = data;

    remove_balance_view_columns(bench);
    gnc_budget_generate_periods(bench->budget);
    add_balance_budget_columns(bench);
}

/* Update the period label for the new active period on the track
 * budget tab. */
static void update_period_label(BudgetWorkbench* bench)
{
    GList* periodList;
    GncBudgetPeriod* period;
    GDate* startDate, *endDate;
    char buffer[256];
    gchar startBuffer[126];
    gchar endBuffer[126];

    periodList = gnc_budget_get_period_list(bench->budget);

    if(bench->active_period >= g_list_length(periodList)){
        bench->active_period = g_list_length(periodList) - 1;
    }
    else if(bench->active_period < 0){
        bench->active_period = 0;
    }

    period = g_list_nth_data(periodList, bench->active_period);
    startDate = gnc_budget_period_get_start_date(period);
    endDate = gnc_budget_period_get_end_date(period);

    g_date_strftime(startBuffer, 126, "%x", startDate);
    g_date_strftime(endBuffer, 126, "%x", endDate);
    
    sprintf(buffer, "%s - %s", startBuffer, endBuffer);
    gtk_label_set_text(bench->period_label, buffer);
}

/* Roll back the active period to the first one. */
static void first_period_clicked(GtkWidget* object, gpointer data)
{
    BudgetWorkbench* bench = data;
    bench->active_period = 0;
    update_period_label(bench);
}

/* Roll back the period to the previous one. */
static void back_period_clicked(GtkWidget* object, gpointer data)
{
    BudgetWorkbench* bench = data;
    if(bench->active_period > 0){
        bench->active_period -= 1;
    }
    update_period_label(bench);
}

/* Advance to the next period. */
static void next_period_clicked(GtkWidget* object, gpointer data)
{
    BudgetWorkbench* bench = data;
    if(bench->active_period < g_list_length(gnc_budget_get_period_list(bench->budget)) - 1){
        bench->active_period += 1;
    }
    update_period_label(bench);
}

/* Advance to the last period. */
static void last_period_clicked(GtkWidget* object, gpointer data)
{
    BudgetWorkbench* bench = data;
    bench->active_period = g_list_length(gnc_budget_get_period_list(bench->budget)) - 1;
    update_period_label(bench);
}

/* Create the budget workbench dialog. */
static void create_budget_workbench(BudgetWorkbench* bench)
{
    GtkWidget *box;
    GDate* startDate;
    GtkWidget* button;
     
    /* Load the widgets from the glade xml file. */
    bench->xml = gnc_glade_xml_new (GNC_BUDGET_GUI_FILE, "Budget Workbench");
    bench->wnd = glade_xml_get_widget(bench->xml, "Budget Workbench");

    bench->name_entry = 
        GTK_ENTRY(glade_xml_get_widget( bench->xml, "name_entry"));
    bench->description_view = 
        GTK_TEXT_VIEW(glade_xml_get_widget( bench->xml, "description_view"));
    bench->category_view = 
        GTK_TREE_VIEW(glade_xml_get_widget(bench->xml, "category_view"));
    bench->balance_view = 
        GTK_TREE_VIEW(glade_xml_get_widget(bench->xml, "balance_budget_view"));
    bench->track_view = 
        GTK_TREE_VIEW(glade_xml_get_widget(bench->xml, "track_view"));
    bench->length_button = 
        GTK_SPIN_BUTTON(glade_xml_get_widget(bench->xml, "length_button"));
    bench->length_menu = 
        GTK_OPTION_MENU(glade_xml_get_widget(bench->xml, "length_menu"));
    bench->period_menu = 
        GTK_OPTION_MENU(glade_xml_get_widget(bench->xml, "period_menu"));
    bench->period_label= 
        GTK_LABEL(glade_xml_get_widget(bench->xml, "period_label"));

    /* Set up the period frequency view. */
    startDate = gnc_budget_get_start_date(bench->budget);
    bench->freqWnd = GNC_FREQUENCY(
            gnc_frequency_new(gnc_budget_get_period_frequency(bench->budget), startDate));
    
    /* Change the text so that its more mainingful for this druid*/
    gnc_frequency_set_frequency_label_text(bench->freqWnd, _("Budget Period:"));
    gnc_frequency_set_date_label_text(bench->freqWnd, _("Starting Date:"));
        
    /* Reparent to the correct location */
    box = glade_xml_get_widget (bench->xml, "budget_freq_box");
    gtk_box_pack_start (GTK_BOX (box), GTK_WIDGET (bench->freqWnd),TRUE, TRUE, 0);
    
    /* Create and assign the model. */
    bench->category_model = gnc_budget_tree_model_new(bench->budget);
    gtk_tree_view_set_model(bench->category_view, bench->category_model);
    gtk_tree_view_set_model(bench->balance_view, bench->category_model);
    gtk_tree_view_set_model(bench->track_view, bench->category_model);

    /* Add the columns to the various tree views. */
    add_category_columns(bench->category_view);
    add_balance_budget_columns(bench);
    add_track_columns(bench->track_view);

    /* Connect the signal handlers. */
    button = glade_xml_get_widget( bench->xml, "ok_button" );
    g_signal_connect( button, "clicked",
                        G_CALLBACK(ok_clicked), bench);
    button = glade_xml_get_widget( bench->xml, "apply_button" );
    g_signal_connect( button, "clicked",
                        G_CALLBACK(apply_clicked), bench);
    button = glade_xml_get_widget( bench->xml, "cancel_button" );
    g_signal_connect( button, "clicked",
                        G_CALLBACK(cancel_clicked), bench);
    button = glade_xml_get_widget( bench->xml, "new_category_button" );
    g_signal_connect( button, "clicked",
                        G_CALLBACK(new_category_clicked), bench);
    button = glade_xml_get_widget( bench->xml, "edit_category_button" );
    g_signal_connect( button, "clicked",
                        G_CALLBACK(edit_category_clicked), bench);
    button = glade_xml_get_widget( bench->xml, "delete_category_button" );
    g_signal_connect( button, "clicked",
                        G_CALLBACK(delete_category_clicked), bench);
    button = glade_xml_get_widget( bench->xml, "regenerate_button" );
    g_signal_connect( button, "clicked",
                        G_CALLBACK(regenerate_clicked), bench);
    button = glade_xml_get_widget( bench->xml, "first_period_button" );
    g_signal_connect( button, "clicked",
                        G_CALLBACK(first_period_clicked), bench);
    button = glade_xml_get_widget( bench->xml, "back_period_button" );
    g_signal_connect( button, "clicked",
                        G_CALLBACK(back_period_clicked), bench);
    button = glade_xml_get_widget( bench->xml, "next_period_button" );
    g_signal_connect( button, "clicked",
                        G_CALLBACK(next_period_clicked), bench);
    button = glade_xml_get_widget( bench->xml, "last_period_button" );
    g_signal_connect( button, "clicked",
                        G_CALLBACK(last_period_clicked), bench);
    
    /* Fill in the values based on our budget. */
    workbench_from_budget(bench);
    update_period_label(bench);
}

/* Public interface to create a budget workbench dialog. */
void gnc_budget_workbench_dialog_create(GncBudget* budget)
{
    BudgetWorkbench* bench;

    bench = g_new0(BudgetWorkbench, 1);
    printf("Creating Workbench dialog: Budget: %p\n", budget);

    bench->budget = budget;
    bench->active_period = 0;
    create_budget_workbench(bench);

    printf("Showing Workbench dialog: Budget: %p\n", bench->budget);
    gtk_widget_show_all(bench->wnd);
}

/** @} */
/** @} */
