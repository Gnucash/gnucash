/********************************************************************\
 * dialog-budget-category.c --  Implementation of the budget        *
 *                              category dialog.                    *
 * Copyright (C) 10 sep 2003    Darin Willits <darin@willits.ca>    *
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
/** @file dialog-budget-category.c
 *  @brief Implementation of the budget category dialog.
 *  @author Created by Darin Willits 10 sep 2003 
 *  @author Copyright (c) 10 sep 2003 Darin Willits <darin@willits.ca>
 *
 */

// Includes
#include "config.h"

#include <gnome.h>
#include "dialog-utils.h"
#include "gnc-ui-util.h"
#include "dialog-budget-category.h"
#include "FreqSpec.h"
#include "gnc-tree-model-account.h"
#include "gnc-tree-view-account.h"
#include "gnc-general-select.h"
#include "gnc-frequency.h"
#include "gnc-budget-gui.h"
#include "gnc-commodity-edit.h"
#include "dialog-commodity.h"
#include "gnc-ui-util.h"
#include "Group.h"


/* Budget Category Dialog structure definition. */
typedef struct{
    GtkWidget* wnd;
    GladeXML* xml;

    GtkWidget* commodityEdit;
    dialog_commodity_mode commodityMode;
    GtkTreeView* relatedView;
    GNCFrequency* freqWnd;
    GtkEntry* name_entry;
    GtkEntry* description_entry;
    GtkSpinButton* value_button;
    GtkRadioButton* inflow_button;
    GtkRadioButton* outflow_button;

    gboolean new_category;

    GncBudgetCategory* category;
    GncBudget* budget;

} BudgetCategoryDlg;

/* Fill in the Budget Category from the values displayed in the UI. */
static void category_from_ui(BudgetCategoryDlg* dlg)
{
    FreqSpec* freqSpec;
    GDate startDate;
    const gchar* name, *description;
    gnc_numeric value; 
    gnc_commodity* commodity;
    gint type;
    AccountList* selectedAccounts;

    /* Set the name and description. */
    name = gtk_entry_get_text(dlg->name_entry);
    description = gtk_entry_get_text(dlg->description_entry);

    gnc_budget_category_set_name(dlg->category, name);
    gnc_budget_category_set_description(dlg->category, description);
    
    /* Set the category's value. */
    value = double_to_gnc_numeric(
                gtk_spin_button_get_value_as_float(dlg->value_button), 
                1, GNC_RND_NEVER );
    gnc_budget_category_set_value(dlg->category, value);
    
    /* Set the frequency. */
    freqSpec = gnc_budget_category_get_frequency(dlg->category);
    if(freqSpec == NULL){
        freqSpec = xaccFreqSpecMalloc(gnc_budget_get_book(dlg->budget));
    }
    gnc_frequency_save_state( dlg->freqWnd, freqSpec, &startDate);
    gnc_budget_category_set_frequency(dlg->category, freqSpec);
    

    /* Set the commodity. */
    commodity = (gnc_commodity *)
        gnc_general_select_get_selected (GNC_GENERAL_SELECT (dlg->commodityEdit));
    gnc_budget_category_set_commodity(dlg->category, commodity);

    /* Set the category's type. */
    if(gtk_toggle_button_get_active(GTK_TOGGLE_BUTTON(dlg->inflow_button))){
        type = BUDGET_CATEGORY_INFLOW;
    }
    else{
        type = BUDGET_CATEGORY_OUTFLOW;
    }
   
    /* If this category has just been created we have to add it to either
     * the inflow or outflow categories.  We also have to check if the type 
     * has been changed and move it. */
    if(dlg->new_category == TRUE){
        if(type == BUDGET_CATEGORY_INFLOW){
            gnc_budget_add_inflow_category(dlg->budget, dlg->category);
        }
        else{
            gnc_budget_add_outflow_category(dlg->budget, dlg->category);
        }
        gnc_budget_category_set_type(dlg->category, type);
    }
    else if(type != gnc_budget_category_get_type(dlg->category)){
        if(type == BUDGET_CATEGORY_INFLOW){
            gnc_budget_remove_outflow_category(dlg->budget, dlg->category);
            gnc_budget_add_inflow_category(dlg->budget, dlg->category);
        }
        else{
            gnc_budget_remove_inflow_category(dlg->budget, dlg->category);
            gnc_budget_add_outflow_category(dlg->budget, dlg->category);
        }
        gnc_budget_category_set_type(dlg->category, type);
    }
    
    /* Set the related accounts. */
    selectedAccounts = 
        (AccountList*)gnc_tree_view_account_get_selected_accounts(
                                           GNC_TREE_VIEW_ACCOUNT(dlg->relatedView));
    gnc_budget_category_set_related_accounts(dlg->category, selectedAccounts);
}

/* Fill in the User Interface from the category object. */
static void category_to_ui(BudgetCategoryDlg* dlg)
{
    double value;
    gnc_commodity* commodity;
    GncBudgetCategoryType type;
    GList* relatedAccounts;
    
    /* Set the name and description. */
    gtk_entry_set_text(dlg->name_entry, 
            gnc_budget_category_get_name(dlg->category));
    
    if (gnc_budget_category_get_description(dlg->category)) {
      gtk_entry_set_text(dlg->description_entry, 
			 gnc_budget_category_get_description(dlg->category));
    }
    /* Set the value. */
    value = gnc_numeric_to_double(
                gnc_budget_category_get_value(dlg->category));
    gtk_spin_button_set_value(dlg->value_button, value);
    
    /* Set the commodity. */
    commodity = gnc_budget_category_get_commodity(dlg->category);
    gnc_general_select_set_selected (GNC_GENERAL_SELECT (dlg->commodityEdit),
                                    commodity);

    /* Set the categories type. */
    type = gnc_budget_category_get_type(dlg->category);
    if(type == BUDGET_CATEGORY_INFLOW){
        gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(dlg->inflow_button), TRUE);
    }
    else{
        gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(dlg->outflow_button), TRUE);
    }

    /* Set the tree view to show the related accounts. */
    relatedAccounts = (GList*)gnc_budget_category_get_related_accounts(dlg->category);
    printf("Related Accouts: %d\n", g_list_length(relatedAccounts));
    gnc_tree_view_account_set_selected_accounts(
            GNC_TREE_VIEW_ACCOUNT(dlg->relatedView), relatedAccounts, TRUE);
}


/* Destroy the dialog and cleanup. */
static void category_dialog_destroy(BudgetCategoryDlg* dlg)
{
    gtk_widget_destroy(dlg->wnd);
    g_free(dlg);
}

/* Ok Button event handler. */
static void ok_clicked(GtkWidget* object, gpointer* data)
{
    BudgetCategoryDlg* dlg = (BudgetCategoryDlg*)data;
    category_from_ui(dlg);
    category_dialog_destroy(dlg);
}

/* Cancel button event handler. */
static void cancel_clicked(GtkWidget* object, gpointer data)
{
    category_dialog_destroy((BudgetCategoryDlg*)data);
}

/* Create the Budget Category Dialog. */
static void category_dialog_create(BudgetCategoryDlg* dlg)
{
    GladeXML *xml;
    GtkWidget *box;
    GtkWidget* ok_button;
    GtkWidget* cancel_button;
    GtkTreeSelection* selection;
    
    /* Load the glade xml file and create the widget. */
    xml = gnc_glade_xml_new (GNC_BUDGET_GUI_FILE, "Modify Category Dialog");
    dlg->wnd = glade_xml_get_widget(xml, "Modify Category Dialog");
    dlg->xml = xml;
   
    /* Create the various component widgets. */
    dlg->name_entry = 
        GTK_ENTRY(glade_xml_get_widget(dlg->xml, "name_entry"));
    dlg->description_entry = 
        GTK_ENTRY(glade_xml_get_widget(dlg->xml, "description_entry"));
    dlg->value_button = 
        GTK_SPIN_BUTTON(glade_xml_get_widget(dlg->xml, "value_button"));
    dlg->inflow_button = 
        GTK_RADIO_BUTTON(glade_xml_get_widget(dlg->xml, "inflow_button"));
    dlg->outflow_button = 
        GTK_RADIO_BUTTON(glade_xml_get_widget(dlg->xml, "outflow_button"));

    
    /* Set up the commodity widget. */
    dlg->commodityMode = DIAG_COMM_CURRENCY;
    box = glade_xml_get_widget (dlg->xml, "commodity_box");
    dlg->commodityEdit = gnc_general_select_new (GNC_GENERAL_SELECT_TYPE_SELECT,
					       gnc_commodity_edit_get_string,
					       gnc_commodity_edit_new_select,
					       &dlg->commodityMode);
    gtk_box_pack_start(GTK_BOX(box), dlg->commodityEdit, TRUE, TRUE, 0);


    /* Set up the related accounts view. */
    box = glade_xml_get_widget (xml, "related_accounts_scroll");
    dlg->relatedView = gnc_tree_view_account_new(FALSE);
    gtk_container_add(GTK_CONTAINER(box), GTK_WIDGET(dlg->relatedView));
    
    gnc_tree_view_account_configure_columns(GNC_TREE_VIEW_ACCOUNT(dlg->relatedView), NULL);
    selection = gtk_tree_view_get_selection (GTK_TREE_VIEW(dlg->relatedView));
    gtk_tree_selection_set_mode (selection, GTK_SELECTION_MULTIPLE);
 

    /* Set up the frequency view. */
    dlg->freqWnd = GNC_FREQUENCY(
            gnc_frequency_new(gnc_budget_category_get_frequency(dlg->category), NULL));
    
    /* Reparent to the correct location */
    box = glade_xml_get_widget (xml, "frequency_box");
    gtk_box_pack_start (GTK_BOX (box), GTK_WIDGET (dlg->freqWnd),TRUE, TRUE, 0);
    
    /* Connect signals. */
    ok_button = glade_xml_get_widget( dlg->xml, "ok_button" );
    g_signal_connect( ok_button, "clicked",
                        G_CALLBACK(ok_clicked), dlg);
    cancel_button = glade_xml_get_widget( dlg->xml, "cancel_button" );
    g_signal_connect( cancel_button, "clicked",
                        G_CALLBACK(cancel_clicked), dlg);


    /* Fill in the ui from the category. */
    category_to_ui(dlg);
}

/* Public interface method to create the budget category dialog.
 * This method is used to create a dialog to edit an existing category. */
void gnc_budget_category_dialog_create(GncBudget* budget, GncBudgetCategory* category)
{
    BudgetCategoryDlg* dlg;

    dlg = g_new0(BudgetCategoryDlg, 1);

    dlg->category = category;
    dlg->budget = budget;
    category_dialog_create(dlg);

    gtk_widget_show_all(dlg->wnd);
}

/* Public interface method to create the budget category dialog.
 * This method is used to create a dialog to edit a newly created category. 
 * If the user clicks ok then the new category will be added to the budget,
 * otherwise the new category will be destroyed.
 * */
void gnc_budget_category_new_dialog_create(GncBudget* budget)
{
    BudgetCategoryDlg* dlg;
    GncBudgetCategory* category;

    dlg = g_new0(BudgetCategoryDlg, 1);

    category = gnc_budget_category_new(gnc_budget_get_book(budget),  budget);
    
    
    dlg->category = category;
    dlg->budget = budget;
    
    dlg->new_category = TRUE;
    
    category_dialog_create(dlg);

    gtk_widget_show_all(dlg->wnd);
}

/** @} */
/** @} */
