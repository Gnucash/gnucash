/********************************************************************\
 * gnc-budget.c -- Implementation of the top level Budgeting API's.     *
 * Copyright (C) 04 sep 2003    Darin Willits <darin@willits.ca>    *
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

/** @file gnc-budget.c
 *  @brief Implementation of the top level budgeting API's.
 *  @author Created by Darin Willits 04 sep 2003 
 *  @author Copyright (c) 2003 Darin Willits <darin@willits.ca>
 *
 *
 */

// Includes
#include "config.h"
#include <stdio.h>

#include "gnc-budget-p.h"
#include "qofbook.h"
#include "qofbook-p.h"
#include "qofid-p.h"
#include "gnc-engine.h"
#include "gnc-engine-util.h"
#include "gnc-event-p.h"
#include "Group.h"
#include "Account.h"
#include "gnc-budget-period.h"


static void budget_fill_categories(GncBudget* budget);

GncBudget* gnc_budget_new(QofBook *book)
{
    GncBudget* budget;
    GDate date;
    GString* freqStr;

    g_return_val_if_fail(book, NULL);

    budget = g_new0(GncBudget, 1);
    
    
    qof_instance_init (&budget->inst, GNC_ID_BUDGET, book);
    //budget->entity_table = qof_book_get_entity_table (book);
    //qof_entity_guid_new (budget->entity_table, &budget->guid);
    //qof_entity_store( budget->entity_table, budget,
    //                &budget->guid, GNC_ID_BUDGET );
    
    budget->book = book;

    /* Create the default period frequency.*/
    budget->period_freq = xaccFreqSpecMalloc(book);
    
    g_date_clear(&date, 1);
    g_date_set_time(&date, time(NULL));
    xaccFreqSpecSetMonthly(budget->period_freq, &date, 1);
    xaccFreqSpecSetUIType(budget->period_freq, UIFREQ_MONTHLY);

    freqStr = g_string_sized_new(16);
    xaccFreqSpecGetFreqStr(budget->period_freq, freqStr);
    //printf("Category Freq: %s\n", freqStr->str);

    
    
    /* fill the categories based on the current account hierarchy. 
     * FIXME: This should probably not be here but rather be a separate 
     * operation with a public interface.  For now it is convienent.*/
    budget_fill_categories(budget);


    gnc_engine_gen_event( &budget->inst.entity, GNC_EVENT_CREATE );

    return budget;
}

void gnc_budget_delete(GncBudget* budget)
{
    if(budget == NULL){
        return;
    }
    
    /* We first send the message that this object is about to be
     * destroyed so that any GUI elements can remove it before it is
     * actually gone.
     */
    gnc_engine_gen_event( &budget->inst.entity, GNC_EVENT_DESTROY);
    
    //qof_entity_remove(budget->entity_table, &budget->guid);

    if(budget->name){
        g_free(budget->name);
    }
    
    if(budget->description){
        g_free(budget->description);
    }

    qof_instance_release (&budget->inst);
    g_free(budget);
}

void gnc_budget_set_name(GncBudget* budget, const gchar* name)
{
    g_return_if_fail( name != NULL );
    if ( budget->name != NULL ) {
        g_free( budget->name );
        budget->name = NULL;
    }
    budget->name = g_strdup( name );
}

gchar* gnc_budget_get_name(GncBudget* budget)
{
    if(budget == NULL){
        return NULL;
    }
    return budget->name;
}

void gnc_budget_set_description(GncBudget* budget, const gchar* description)
{
    g_return_if_fail( description != NULL );
    if ( budget->description != NULL ) {
        g_free( budget->description);
        budget->description = NULL;
    }
    budget->description = g_strdup(description);
}

gchar* gnc_budget_get_description(GncBudget* budget)
{
    if(budget == NULL){
        return NULL;
    }
    return budget->description;
}




void gnc_budget_set_period_frequency(GncBudget* budget, FreqSpec* period)
{
    if(budget == NULL){
        return;
    }
    
    if((budget->period_freq == period) || (period == NULL)){
        return;
    }

    if(budget->period_freq != NULL){
        /* Delete the existing object before setting the new one. */
        xaccFreqSpecFree(budget->period_freq);
    }

    budget->period_freq = period;
}

FreqSpec* gnc_budget_get_period_frequency(GncBudget* budget)
{
    if(budget == NULL){
        return NULL;
    }

    return budget->period_freq;
}




void gnc_budget_set_start_date(GncBudget* budget, GDate* date)
{
    if(budget == NULL){
        return;
    }
    budget->start_date = *date;
}

GDate* gnc_budget_get_start_date(GncBudget* budget)
{
    if(budget == NULL){
        return NULL;
    }
    return &budget->start_date;
}






void gnc_budget_set_length_months(GncBudget* budget, gint months)
{
    if(budget == NULL){
        return;
    }

    budget->length = months;
}

void gnc_budget_set_length_years(GncBudget* budget, gint years)
{
    if(budget == NULL){
        return;
    }

    budget->length = years * 12;
}

gint gnc_budget_get_length_months(GncBudget* budget)
{
    if(budget == NULL){
        return 0;
    }

    return budget->length;
}

gint gnc_budget_get_length_years(GncBudget* budget)
{
    if(budget == NULL){
        return 0;
    }

    return budget->length / 12;
}






void gnc_budget_add_inflow_category(GncBudget* budget, GncBudgetCategory* category)
{
    if(budget == NULL){
        return;
    }
    gnc_budget_category_add_child(budget->inflow_category, category);
}

void gnc_budget_remove_inflow_category(GncBudget* budget, GncBudgetCategory* category)
{
    if(budget == NULL){
        return;
    }
    gnc_budget_category_remove_child(budget->inflow_category, category);
}

void gnc_budget_set_inflow_category(GncBudget* budget, GncBudgetCategory* inflow)
{
    if(budget == NULL){
        return;
    }
    budget->inflow_category = inflow;
}

GncBudgetCategory* gnc_budget_get_inflow_category(GncBudget* budget)
{
    if(budget == NULL){
        return NULL;
    }
    return budget->inflow_category;
}

void gnc_budget_set_outflow_category(GncBudget* budget, GncBudgetCategory* outflow)
{
    if(budget == NULL){
        return;
    }
    budget->outflow_category = outflow;
}

GncBudgetCategory* gnc_budget_get_outflow_category(GncBudget* budget)
{
    if(budget == NULL){
        return NULL;
    }
    return budget->outflow_category;
}

void gnc_budget_add_outflow_category(GncBudget* budget, GncBudgetCategory* category)
{
    if(budget == NULL){
        return;
    }
    gnc_budget_category_add_child(budget->outflow_category, category);
}

void gnc_budget_remove_outflow_category(GncBudget* budget, GncBudgetCategory* category)
{
    if(budget == NULL){
        return;
    }
    gnc_budget_category_remove_child(budget->inflow_category, category);
}

QofBook* gnc_budget_get_book(GncBudget* budget)
{
    if(budget == NULL){
        return NULL;
    }
    return budget->book;
}

static void free_budget_period(gpointer data, gpointer user_data)
{
    GncBudgetPeriod* period = data;
    g_free(period);
}

void gnc_budget_generate_periods(GncBudget* budget)
{
    GDate currentPeriod, outDate;
    GDate endDate;
    GDate periodStartDate;
    GncBudgetPeriod* budgetPeriod;

    if(budget == NULL){
        return;
    }

    /* TODO: Add in some error checking such that we don't regenerate
     * the periods if they are the same as before.  We may still need
     * to recalculate the values however.
     */
    if(budget->period_list != NULL){
        g_list_foreach(budget->period_list, free_budget_period, NULL);
        g_list_free(budget->period_list);
        budget->period_list = NULL;
    }
    
    /* Add the starting period.  Every budget should have at least
     * one period. */
    g_date_set_dmy(&periodStartDate, g_date_get_day(&budget->start_date),
                                     g_date_get_month(&budget->start_date),
                                     g_date_get_year(&budget->start_date));
    budgetPeriod = gnc_budget_period_new();
    gnc_budget_period_set_start_date(budgetPeriod, &periodStartDate);
    budget->period_list = g_list_append(budget->period_list, budgetPeriod);
 
    
    /* Setup the end Date */
    g_date_set_dmy(&endDate, g_date_get_day(&budget->start_date),
                             g_date_get_month(&budget->start_date),
                             g_date_get_year(&budget->start_date));
    g_date_add_months(&endDate, budget->length);
    

    g_date_set_dmy(&currentPeriod, g_date_get_day(&budget->start_date),
                                  g_date_get_month(&budget->start_date),
                                  g_date_get_year(&budget->start_date));
    do{
        xaccFreqSpecGetNextInstance(budget->period_freq, &currentPeriod, &outDate);
        if(g_date_valid(&outDate) == FALSE){
            break;
        }
        
        if(g_date_compare(&outDate, &endDate) >= 0){
            break;
        }
        else{
            /* Set the end date of the previous period. */
            gnc_budget_period_set_end_date(budgetPeriod, &outDate);

            /* Create the new period. */
            budgetPeriod = gnc_budget_period_new();
            gnc_budget_period_set_start_date(budgetPeriod, &outDate);
            
            budget->period_list = g_list_append(budget->period_list, budgetPeriod);
            g_date_set_dmy(&currentPeriod, g_date_get_day(&outDate),
                            g_date_get_month(&outDate),
                            g_date_get_year(&outDate));
        }
    }while(1);

    gnc_budget_period_set_end_date(budgetPeriod, &endDate);
    
    /* Now we should generate the list of values for each category.*/
    gnc_budget_category_generate_values(budget->inflow_category);
    gnc_budget_category_generate_values(budget->outflow_category);
}


gint gnc_budget_get_num_periods(GncBudget* budget)
{
    if(budget == NULL){
        return 0;
    }
    return g_list_length(budget->period_list);
}

GList* gnc_budget_get_period_list(GncBudget* budget)
{
    if(budget == NULL){
        return NULL;
    }
    return budget->period_list;
}



/* ==================================================================== */
/* Private member functions.
 *
 * ==================================================================== */

static void add_category_from_account(gpointer data, gpointer userdata)
{
    GncBudget* budget;
    Account* account;
    AccountList* children;
    GncBudgetCategory* category = NULL;
    gchar* name;
    AccountGroup* group;
    int numChildren;
    GNCAccountType accountType;

    account = data;
    budget = userdata;
    
    if(account == NULL){
        return;
    }

    //printf("Adding Category from account\n");
    //printf("Account: %s\n", xaccAccountGetName(account));
    group  = xaccAccountGetChildren(account);
    children = xaccGroupGetAccountList(group);
    numChildren = xaccGroupGetNumAccounts(group);
    //printf("NumChildren: %d\n", numChildren);

    if(numChildren == 0){
        name = xaccAccountGetFullName(account, ':');
        accountType = xaccAccountGetType(account);
        
        category = gnc_budget_category_new(budget->book, budget);
        gnc_budget_category_set_name(category, name);
        gnc_budget_category_add_account(category, account);
                
        if(accountType == INCOME){
            //printf("Inflow Category: %s\n", gnc_budget_category_get_name(category));
            gnc_budget_category_set_type(category, BUDGET_CATEGORY_INFLOW);
            gnc_budget_category_add_child(budget->inflow_category, category);
        }
        else{
            //printf("Outflow Category: %s\n", gnc_budget_category_get_name(category));
            gnc_budget_category_set_type(category, BUDGET_CATEGORY_OUTFLOW);
            gnc_budget_category_add_child(budget->outflow_category, category);
        }
        
        g_free(name);
    }

}

static void budget_fill_categories(GncBudget* budget)
{
    AccountList* children;
    if(budget == NULL){
        return;
    }
    
    //printf("Creating top level budget categories.\n");

    budget->inflow_category = gnc_budget_category_new(budget->book, budget);
    gnc_budget_category_set_name(budget->inflow_category, "Inflow");

    
    budget->outflow_category = gnc_budget_category_new(budget->book, budget);
    gnc_budget_category_set_name(budget->outflow_category, "Outflow");

    
    //printf("Filing budget Categories.\n");
    //children = xaccGroupGetAccountList(gnc_book_get_group(budget->book));
    children = xaccGroupGetSubAccounts(gnc_book_get_group(budget->book));
    g_list_foreach(children, add_category_from_account, budget);

    //printf("Number of inflow categories: %d\n", 
            //gnc_budget_category_get_num_children(budget->inflow_category));
    //printf("Number of outflow categories: %d\n", 
            //gnc_budget_category_get_num_children(budget->outflow_category));
}

GncBudget* gnc_budget_lookup (const GUID *guid, QofBook *book)
{
    QofCollection *col;
    
    if (!guid || !book){
        return NULL;
    }
        
    col = qof_book_get_collection (book, GNC_ID_BUDGET);
    return (GncBudget*)qof_collection_lookup_entity (col, guid);
}
