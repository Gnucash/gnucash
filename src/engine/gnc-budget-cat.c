/********************************************************************\
 * gnc-budget-cat.c --  Implementation of the budget category methods.  *
 * Copyright (C) 05 sep 2003    Darin Willits <darin@willits.ca>    *
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

/** @addtogroup Engine
 *     @{ */
/** @addtogroup Budget
 *     @{ */
/** @file gnc-budget-cat.c
 *  @brief Implementation of the budget category methods.
 *  @author Created by Darin Willits 05 sep 2003 
 *  @author Copyright (c) 05 sep 2003 Darin Willits <darin@willits.ca>
 *
 *
 */


// Includes
#include "config.h"
#include <stdio.h>
#include <time.h>

#include "gnc-budget-cat-p.h"
#include "qofid-p.h"
#include "gnc-trace.h"
#include "gnc-budget-period-value.h"
#include "gnc-budget-period.h"

static short module = MOD_GUI;

GncBudgetCategory *gnc_budget_category_new(QofBook* book, GncBudget* budget)
{
    GncBudgetCategory* category;
    GDate date;
    GString* freqStr;

    ENTER("book %p, budget %p", book, budget);
    g_return_val_if_fail(book, NULL);

    category = g_new0(GncBudgetCategory, 1);
   
    qof_instance_init (&category->inst, GNC_ID_BUDGET_CATEGORY, book);

    // The interface for this has been changed to the above line.
    //category->entity_table = qof_book_get_entity_table (book);
    //qof_entity_guid_new (category->entity_table, &category->guid);

    //qof_entity_store( category->entity_table, category,
    //                &category->guid, GNC_ID_BUDGET_CATEGORY );
 
    category->budget = budget;
    category->value = gnc_numeric_zero();
    
    category->freq = xaccFreqSpecMalloc(book);
    g_date_clear(&date, 1);
    g_date_set_time(&date, time(NULL));
    if(g_date_valid(&date) == FALSE){
        //printf("Invalid Date!\n");
    }
    xaccFreqSpecSetMonthly(category->freq, &date, 1);
    xaccFreqSpecSetUIType(category->freq, UIFREQ_MONTHLY);
    
    freqStr = g_string_sized_new(16);
    xaccFreqSpecGetFreqStr(category->freq, freqStr);
    //printf("Category Freq: %s\n", freqStr->str);


    LEAVE("category: %p", category);
    return category;
}

static void delete_child(gpointer data, gpointer user_data)
{
    GncBudgetCategory* category = (GncBudgetCategory*)data;
    gnc_budget_category_delete(category);
}


void gnc_budget_category_delete(GncBudgetCategory* category)
{
    if(category == NULL){
        return;
    }
    
    if(category->name){
        g_free(category->name);
    }
    if(category->description){
        g_free(category->description);
    }

    // Now use qof_instance_release.
    //qof_entity_remove(category->entity_table, &category->guid);
    g_list_free(category->accountList);

    xaccFreqSpecFree(category->freq);

    g_list_foreach(category->children, delete_child, NULL);
    

    /* Remove us from our parents list. */
    if(category->parent != NULL){
        gnc_budget_category_remove_child(category->parent, category);
    }
    
    qof_instance_release(&category->inst);
    g_free(category);
}

void gnc_budget_category_set_name(GncBudgetCategory* category, const gchar* name)
{
    g_return_if_fail( name != NULL );
    if ( category->name != NULL ) {
        g_free( category->name );
        category->name = NULL;
    }
    category->name = g_strdup( name );
}

gchar* gnc_budget_category_get_name(GncBudgetCategory* category)
{
    if(category == NULL){
        return NULL;
    }
    return category->name;
}

void gnc_budget_category_set_description(GncBudgetCategory* category, const gchar* description)
{
    g_return_if_fail(description != NULL);
    if(category->description != NULL){
        g_free(category->description);
        category->description = NULL;
    }
    category->description = g_strdup(description);
}

gchar* gnc_budget_category_get_description(GncBudgetCategory* category)
{
    if(category == NULL){
        return NULL;
    }
    return category->description;
}

void gnc_budget_category_set_value(GncBudgetCategory* category, gnc_numeric value)
{
    if(category == NULL){
        return;
    }
    category->value = value;
}

gnc_numeric gnc_budget_category_get_value(GncBudgetCategory* category)
{
    if(category == NULL){
        return gnc_numeric_zero();
    }
    return category->value;
}

void gnc_budget_category_add_account(GncBudgetCategory* category, Account* account)
{
    if(category == NULL){
        return;
    }
    category->accountList = g_list_append(category->accountList, account);

    /* If our commodity hasn't been set yet we might as well take it
     * from the account we have just been given. */
    if(category->commodity == NULL){
        category->commodity = xaccAccountGetCommodity(account);
    }
}

void gnc_budget_category_remove_account(GncBudgetCategory* category, Account* account)
{
    if(category == NULL){
        return;
    }
    category->accountList = g_list_remove(category->accountList, account);
}

AccountList* gnc_budget_category_get_related_accounts(GncBudgetCategory* category)
{
    if(category == NULL){
        return NULL;
    }
    return category->accountList;
}

void gnc_budget_category_set_related_accounts(GncBudgetCategory* category, AccountList* accounts)
{
    if(category == NULL){
        return;
    }

    if(category->accountList != NULL){
        g_list_free(category->accountList);   
        category->accountList = NULL;
    }

    category->accountList = accounts;
}

void gnc_budget_category_set_frequency(GncBudgetCategory* category, FreqSpec* freq)
{
    if(category == NULL){
        return;
    }
    category->freq = freq;
}

FreqSpec* gnc_budget_category_get_frequency(GncBudgetCategory* category)
{
    if(category == NULL){
        return NULL;
    }
    return category->freq;
}

void gnc_budget_category_set_type(GncBudgetCategory* category, GncBudgetCategoryType type)
{
    if(category == NULL){
        return;
    }
    category->type = type;
}

GncBudgetCategoryType gnc_budget_category_get_type(GncBudgetCategory* category)
{
    if(category == NULL){
        return -1;
    }
    return category->type;
}

void gnc_budget_category_set_commodity(GncBudgetCategory* category, gnc_commodity* commodity)
{
    if(category == NULL){
        return;
    }
    category->commodity = commodity;
}

gnc_commodity* gnc_budget_category_get_commodity(GncBudgetCategory* category)
{
    if(category == NULL){
        return NULL;
    }
    return category->commodity;
}

void gnc_budget_category_add_child(GncBudgetCategory* parent, GncBudgetCategory* child)
{
    if((parent == NULL) || (child == NULL)){
        return;
    }
    if(child->parent != NULL){
        /* Remove the child account from its previous parents list. */
        child->parent->children = g_list_remove(child->parent->children, child);
    }
    parent->children = g_list_append(parent->children, child);
    child->parent = parent;
}

void gnc_budget_category_remove_child(GncBudgetCategory* parent, GncBudgetCategory* child)
{
    if((parent == NULL) || (child == NULL)){
        return;
    }
    parent->children = g_list_remove(parent->children, child);
}

GncBudgetCategory* gnc_budget_category_get_child(GncBudgetCategory* parent, gint index)
{
    if(parent == NULL){
        return NULL;
    }
    return g_list_nth_data(parent->children, index);
}

GncBudgetCategoryList* gnc_budget_category_get_children(GncBudgetCategory* parent)
{
    if(parent == NULL){
        return NULL;
    }
    return parent->children;
}

gint gnc_budget_category_get_num_children(GncBudgetCategory* parent)
{
    if(parent == NULL){
        return -1;
    }
    return g_list_length(parent->children);
}

GncBudgetCategory* gnc_budget_category_get_parent(GncBudgetCategory* category)
{
    if(category == NULL){
        return NULL;
    }
    return category->parent;
}

gint gnc_budget_category_get_index_in_parent_list(GncBudgetCategory* category)
{
    if(category == NULL){
        return -1;
    }
    if(category->parent == NULL){
        return 0;
    }
    return g_list_index(category->parent->children, category);
}

/*
 * Nice idea but not necessary.
static gint calculate_mulitplier(FreqSpec* freq, GDate* start_date, GDate* end_date)
{
    gint multiplier = 0;
    gboolean done = FALSE;
    GDate* outDate, *inDate;
    inDate = g_date_new_dmy(   g_date_get_day(start_date),
                                g_date_get_month(start_date),
                                g_date_get_year(start_date));
    outDate = g_date_new();
    g_date_clear(outDate, 1);


    while(!done){
        xaccFreqSpecGetNextInstance(freq, inDate, outDate);
        if(!g_date_valid(outDate)){
            break;
        }
        
        if(g_date_compare(outDate, end_date) < 0){
            multiplier++;
            g_date_set_dmy(inDate, g_date_get_day(outDate),
                                    g_date_get_month(outDate),
                                    g_date_get_year(outDate));
        }
        else{
            done = TRUE;
        }
    }
    
    g_date_free(inDate);
    g_date_free(outDate);
    
    return multiplier;
}


GncBudgetValue* budget_value_new()
{
    GncBudgetValue* value;

    value = g_new0(GncBudgetValue, 1);
    value->value = gnc_numeric_zero();
    value->adjustment = gnc_numeric_zero();

    return value;
}
*/

/* Find out how many occurences of the given frequency spec fall within the 
 * given dates.  
 * TODO: This should probably be moved to the FreqSpec public interface.
 */
static gint find_num_occurences(FreqSpec* a, const GDate* startDate, const GDate* endDate)
{
    GDate outDate;
    GDate curDate;
    gint counter = 0;
    
    g_date_set_dmy(&curDate, g_date_day(startDate),
                             g_date_month(startDate),
                             g_date_year(startDate));

    do{
        xaccFreqSpecGetNextInstance(a, &curDate, &outDate);
        if(g_date_valid(&outDate) == FALSE){
            break;
        }

        if(g_date_compare(&outDate, endDate) <= 0){
            counter++;
            g_date_set_dmy(&curDate, g_date_day(&outDate),
                                    g_date_month(&outDate),
                                    g_date_year(&outDate));
        }
        else{
            break;
        }
    }while(1);

    return counter;
}

/* Generate the budget value for the given period.
 */
static void generate_budget_value_for_period(gpointer data, gpointer user_data)
{
    GncBudgetCategory* category = user_data;
    GncBudgetPeriod* period = data;
    GDate* periodStartDate, *periodEndDate;
    FreqSpec* periodFreq, *catFreq;
    int compare;
    GncBudgetPeriodValue* periodValue;
    gint multiplier;
    double value;
    
    periodFreq = gnc_budget_get_period_frequency(category->budget);
    catFreq = gnc_budget_category_get_frequency(category);
    
    periodStartDate = gnc_budget_period_get_start_date(period);
    periodEndDate = gnc_budget_period_get_end_date(period);
   
    compare = gnc_freq_spec_compare(periodFreq, catFreq);
    
    /* If the frequencies are the same we can just fill in the value 
     * from the categories value.  Otherwise we have to find out how many
     * of the categories frequencies fall within the given period and and 
     * multiply by the categories value.
     */
    if(compare == 0){
        value = gnc_numeric_to_double(category->value);
        //printf("Frequencies equal: %0.2f\n", value);
        periodValue = gnc_budget_period_value_new();
        gnc_budget_period_value_set_value_double(periodValue, value);
        category->period_values = g_list_append(category->period_values, periodValue);
    }
    else{
        multiplier = find_num_occurences(catFreq, periodStartDate, periodEndDate);
        value = gnc_numeric_to_double(category->value) * multiplier;
        //printf("Period Freq larger: %0.2f\n", value);
        periodValue = gnc_budget_period_value_new();
        gnc_budget_period_value_set_value_double(periodValue, value);
        category->period_values = g_list_append(category->period_values, periodValue);
    }
    
}

static void category_generate_values(gpointer data, gpointer user_data)
{
    GncBudgetCategory* category = data;
    
    gnc_budget_category_generate_values(category);
}

static void free_budget_value(gpointer data, gpointer user_data)
{
    GncBudgetPeriodValue* periodValue = data;
    gnc_budget_period_value_delete(periodValue);
}

void gnc_budget_category_generate_values(GncBudgetCategory* category)
{
    GList* period_list;
    double value;

    if(category == NULL){
        return;
    }

    /* Delete all the old values if there are any cause we are going to 
     * re-evaluate them all.
     */
    if(category->period_values != NULL){
        g_list_foreach(category->period_values, free_budget_value, NULL);
        g_list_free(category->period_values);
        category->period_values = NULL;
    }
    
    value = gnc_numeric_to_double(category->value);
    //printf("Category Value: %0.2f\n", value);

    period_list = gnc_budget_get_period_list(category->budget);
    
    g_list_foreach(period_list, generate_budget_value_for_period, category);

    /* Now generate the value lists for our kids.
     * Should this really do this?? Maybe categories with children
     * should have the sum of their kids values as their own ala the
     * accounts.  Have to think on this some more... for now the quick and 
     * dirty.
     */
    //printf("Doing children for category: %s\n", category->name);
    g_list_foreach(category->children, category_generate_values, NULL);
}

gnc_numeric gnc_budget_category_get_value_by_index(GncBudgetCategory* category, gint index)
{
    GncBudgetPeriodValue* periodValue;

    if(category == NULL){
        return gnc_numeric_zero();
    }
    
    periodValue = g_list_nth_data(category->period_values, index);

    if(periodValue == NULL){
        return gnc_numeric_zero();
    }
    
    return gnc_budget_period_value_get_value(periodValue); 
}

void gnc_budget_category_set_period_value_by_index(GncBudgetCategory* category, gint index, 
                                                    gnc_numeric value)
{
    GncBudgetPeriodValue* periodValue;

    if(category == NULL){
        return;
    }

    periodValue = g_list_nth_data(category->period_values, index);

    if(periodValue == NULL){
        return;
    }

    gnc_budget_period_value_set_value(periodValue, value);
}

/* This is a cheat method right now to give meaningfull values for the
 * display.  We should probably NEVER return doubles as we could then
 * potentially encounter rounding errors.  All calculations should be done
 * in terms of the gnc_numeric value.
 */
double gnc_budget_category_sum_child_values_by_index(GncBudgetCategory* category, gint index)
{
    double sum = 0.0;
    GList* childList;

    if(category == NULL){
        return 0.00;
    }

    childList = category->children;
    
    for(;childList; childList = childList->next){
        sum += gnc_numeric_to_double(gnc_budget_category_get_value_by_index(childList->data, index));
    }
    
    return sum;
}

/** @} */
/** @} */
