/********************************************************************\
 * druid-budget-create.c -- Implementation of the budget create     *
 *                          druid.                                  *
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
/** @file druid-budget-create.c
 *  @brief Implementation of the budget create druid.
 *  @author Created by Darin Willits 08 sep 2003 
 *  @author Copyright (c) 08 sep 2003 Darin Willits <darin@willits.ca>
 *
 */

// Includes
#include <glib.h>
#include <gnome.h>
#include <libgnomeui/libgnomeui.h>

#include "druid-budget-create.h"
#include "gnc-budget.h"
#include "dialog-budget-list.h"
#include "gnc-budget-tree-model.h"
#include "dialog-budget-category.h"
#include "dialog-budget-workbench.h"

#include "dialog-utils.h"
#include "gnc-ui-util.h"
#include "misc-gnome-utils.h"
#include "gnc-frequency.h"
#include "gnc-date-edit.h"

#include "gnc-budget-gui.h"
#include "gnc-budget-book.h"
#include "gnc-budget.h"


#define BUDGET_NAME_ENTRY           "name_entry"
#define BUDGET_DESC_VIEW            "description_view"
#define BUDGET_LENGTH_BUTTON        "length_button"
#define BUDGET_TIME_SCALE           "time_scale_menu"
#define BUDGET_TREE_VIEW            "budget_tree_view"
#define BUDGET_LENGTH_BUTTON        "length_button"
#define BUDGET_LENGTH_MENU          "length_menu"
#define BUDGET_PERIOD_MENU          "period_menu"

#define BUDGET_DRUID    "Budget Druid"
#define BUDGET_FINISH_PAGE "finish_page"



typedef struct{
    GtkWidget* wnd;
    GladeXML* xml;
    GtkTreeModel* budgetListModel;
    GtkTreeModel* budgetCatModel;

    GtkEntry* nameEntry;
    GtkTextView* descriptionView;
    GtkTreeView* budgetView;
    GtkSpinButton*  length_button;
    GtkOptionMenu*  length_menu;
    GNCFrequency* freqWnd;
    
    GncBudget* budget;
    
} BudgetDruid;


static void budget_druid_destroy(BudgetDruid* druid)
{
    if(druid == NULL){
        return;
    }
    gtk_widget_destroy(druid->wnd);
    g_free(druid);
}

static void cancel_clicked(GnomeDruid *druid, gpointer user_data)
{
    budget_druid_destroy(user_data);
}

static void apply_clicked(GnomeDruidPage *page, GtkWidget* arg1, gpointer data)
{
    QofBook* book;
    BudgetDruid* druid = data;
    const gchar *name, *description;
    FreqSpec* freqSpec;
    //time_t tmpTimeT;
    //GDate outDate;
    GDate* startDate;
    gint length;//, period;
    
    book = gnc_get_current_book();

    if(druid->budget == NULL){
        /* something terribly wrong. */
        return;
    }
    
    /* Set the name and description of the budget. */
    name = gtk_entry_get_text(druid->nameEntry);
    description = xxxgtk_textview_get_text(druid->descriptionView);

    gnc_budget_set_name(druid->budget, name);
    gnc_budget_set_description(druid->budget, description);
    
    /* Set the length of the budget. */
    length = gtk_spin_button_get_value_as_int(druid->length_button);
    if(gtk_option_menu_get_history(druid->length_menu) == 1){
        gnc_budget_set_length_years(druid->budget, length);
    }
    else{
        gnc_budget_set_length_months(druid->budget, length);
    }
    
    freqSpec = gnc_budget_get_period_frequency(druid->budget);
    startDate = gnc_budget_get_start_date(druid->budget);
    if(freqSpec == NULL){
        freqSpec = xaccFreqSpecMalloc(gnc_budget_get_book(druid->budget));
    }
    gnc_frequency_save_state( druid->freqWnd, freqSpec, startDate);
    gnc_budget_set_period_frequency(druid->budget, freqSpec);
 
    /* Add the budget to the book. */
    gnc_book_add_budget(book, druid->budget);
 
    /* FIXME: This is just a temporary mesure and won't be necessary once
     * we connect up to the gnc gui event system.  We will also need to be
     * generating the proper events from the budget engine structures so 
     * for now we will just do this.
     */
    /*add_budget_to_model(druid->budget, druid->budgetListModel);*/

    printf("Generating periods: Druid %p Budget: %p\n", druid, druid->budget);
    /* Generate the list of budget periods. */
    gnc_budget_generate_periods(druid->budget);
    
    budget_druid_destroy(druid);
}

static void add_columns(GtkTreeView* treeView)
{
    GtkCellRenderer *renderer;
    GtkTreeViewColumn *column;

    /* column for severities */
    renderer = gtk_cell_renderer_text_new ();
    column = gtk_tree_view_column_new_with_attributes ("Name",
						     renderer,
						     "text",
						     GNC_BUDGET_TREE_MODEL_COL_NAME,
						     NULL);
    gtk_tree_view_append_column (treeView, column);

    /* column for value */
    renderer = gtk_cell_renderer_text_new ();
    column = gtk_tree_view_column_new_with_attributes ("Value",
						     renderer,
						     "text",
						     GNC_BUDGET_TREE_MODEL_COL_VALUE,
						     NULL);
    gtk_tree_view_append_column (treeView, column);
    
    /* column for freq */
    renderer = gtk_cell_renderer_text_new ();
    column = gtk_tree_view_column_new_with_attributes ("Frequency",
						     renderer,
						     "text",
						     GNC_BUDGET_TREE_MODEL_COL_FREQ,
						     NULL);
    gtk_tree_view_append_column (treeView, column);
}

static void edit_category_clicked(GtkWidget* object, gpointer data)
{
    BudgetDruid* druid = data;
    GtkTreeSelection *selection;
    GtkTreeIter iter;
    GncBudgetCategory *category = NULL;

    printf("Editing Category... Druid: %p Budget: %p\n",druid, druid->budget);
    selection = gtk_tree_view_get_selection (druid->budgetView);
    if (!gtk_tree_selection_get_selected (selection, &druid->budgetCatModel, &iter)){
        printf("Nothing selected...\n");
        return;
    }

    category = gnc_budget_tree_model_get_category(GNC_BUDGET_TREE_MODEL(druid->budgetCatModel), &iter);

    gnc_budget_category_dialog_create(druid->budget, category);
}

static void new_category_clicked(GtkWidget* object, gpointer data)
{
    BudgetDruid* druid = data;

    printf("Creating new Category... Budget: %p\n", druid->budget);
    gnc_budget_category_new_dialog_create(druid->budget);
}

static void delete_category_clicked(GtkWidget* object, gpointer data)
{
    BudgetDruid* druid = data;
    GtkTreeSelection *selection;
    GtkTreeIter iter;
    GncBudgetCategory *category, *inflowCat, *outflowCat;

    printf("Deleting Category...\n");
    selection = gtk_tree_view_get_selection (druid->budgetView);
    if (!gtk_tree_selection_get_selected (selection, &druid->budgetCatModel, &iter)){
        printf("Nothing selected...\n");
        return;
    }

    category = gnc_budget_tree_model_get_category(GNC_BUDGET_TREE_MODEL(druid->budgetCatModel), &iter);
    inflowCat = gnc_budget_get_inflow_category(druid->budget);
    outflowCat = gnc_budget_get_outflow_category(druid->budget);

    if((category != inflowCat) && (category != outflowCat)){
        gnc_budget_category_delete(category);
    }
    else{
        printf("Cannot delete this category...\n");
    }
    
}

static void create_budget_druid(BudgetDruid* druid)
{
    GladeXML *xml;
    GtkWidget *box;
    GnomeDruid* budgetDruid;
    GtkWidget *final_page;
    GtkWidget* editCategoryButton;
    GtkWidget* newCategoryButton;
    GtkWidget* deleteCategoryButton;
     
    druid->budget = gnc_budget_new(gnc_get_current_book());

    xml = gnc_glade_xml_new (GNC_BUDGET_GUI_FILE, "New Budget Druid");
    druid->wnd = glade_xml_get_widget(xml, "New Budget Druid");
    druid->xml = xml;

    druid->nameEntry = 
        GTK_ENTRY(glade_xml_get_widget( druid->xml, BUDGET_NAME_ENTRY));
    druid->descriptionView = 
        GTK_TEXT_VIEW(glade_xml_get_widget( druid->xml, BUDGET_DESC_VIEW));
    druid->budgetView = 
        GTK_TREE_VIEW(glade_xml_get_widget(druid->xml, BUDGET_TREE_VIEW));
    druid->length_button = 
        GTK_SPIN_BUTTON(glade_xml_get_widget(druid->xml, BUDGET_LENGTH_BUTTON));
    druid->length_menu = 
        GTK_OPTION_MENU(glade_xml_get_widget(druid->xml, BUDGET_LENGTH_MENU));

    /* Set the default length to 1 year. */
    gtk_option_menu_set_history(druid->length_menu, 1);

    /* Set up the period frequency view. */
    druid->freqWnd = GNC_FREQUENCY(
            gnc_frequency_new(gnc_budget_get_period_frequency(druid->budget), 
                                gnc_budget_get_start_date(druid->budget)));
    
    /* Change the text so that its more mainingful for this druid*/
    gnc_frequency_set_frequency_label_text(druid->freqWnd, _("Budget Period:"));
    gnc_frequency_set_date_label_text(druid->freqWnd, _("Starting Date:"));
        
    /* Reparent to the correct location */
    box = glade_xml_get_widget (druid->xml, "budget_freq_box");
    gtk_box_pack_start (GTK_BOX (box), GTK_WIDGET (druid->freqWnd),TRUE, TRUE, 0);
 
    /* Create the budget Tree model. */
    druid->budgetCatModel = gnc_budget_tree_model_new(druid->budget);
    gtk_tree_view_set_model(druid->budgetView, druid->budgetCatModel);

    add_columns(druid->budgetView);
        
    /* Connect signals. */
    final_page = glade_xml_get_widget( druid->xml, BUDGET_FINISH_PAGE );
    g_signal_connect(final_page, "finish", G_CALLBACK(apply_clicked), druid);

    budgetDruid =  GNOME_DRUID(glade_xml_get_widget( druid->xml, BUDGET_DRUID ));
    g_signal_connect(budgetDruid, "cancel", G_CALLBACK(cancel_clicked), druid);

    editCategoryButton = glade_xml_get_widget( druid->xml, "edit_category_button" );
    g_signal_connect(editCategoryButton, "clicked", G_CALLBACK(edit_category_clicked), druid);
    
    newCategoryButton = glade_xml_get_widget( druid->xml, "new_category_button" );
    g_signal_connect(newCategoryButton, "clicked", G_CALLBACK(new_category_clicked), druid);
    
    deleteCategoryButton = glade_xml_get_widget( druid->xml, "delete_category_button" );
    g_signal_connect(deleteCategoryButton, "clicked", G_CALLBACK(delete_category_clicked), druid);
}

void gnc_budget_druid_create(GtkTreeModel* treeModel)
{
    BudgetDruid* druid;

    druid = g_new0(BudgetDruid, 1);

    druid->budgetListModel = treeModel;
    
    create_budget_druid(druid);
    
    gtk_widget_show_all(druid->wnd);
}

/** @} */
/** @} */
