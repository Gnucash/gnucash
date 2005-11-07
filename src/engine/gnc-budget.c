/********************************************************************\
 * gnc-budget.c -- Implementation of the top level Budgeting API's. *
 * Copyright (C) 04 sep 2003    Darin Willits <darin@willits.ca>    *
 * Copyright (C) 2005  Chris Shoemaker <c.shoemaker@cox.net>        *
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

#include <glib.h>
#include <glib/gprintf.h>

#include "qof.h"

#include "Account.h"
#include "Group.h"

#include "gnc-budget.h"
#include "gnc-commodity.h"
#include "gnc-gdate-utils.h"

static QofLogModule log_module = GNC_MOD_ENGINE;

struct gnc_budget_private{
    QofInstance inst;

    gchar* name;
    gchar* description;
    Recurrence recurrence;
    guint  num_periods;
};

GncBudget*
gnc_budget_new(QofBook *book)
{
    GncBudget* budget;

    g_return_val_if_fail(book, NULL);

    ENTER(" ");
    budget = g_new0(GncBudget, 1);
    qof_instance_init (&budget->inst, GNC_ID_BUDGET, book);

    recurrenceSet(&budget->recurrence, 1, PERIOD_MONTH, NULL);

    gnc_budget_set_name(budget, "Unnamed Budget");
    gnc_budget_set_description(budget, "");
    gnc_budget_set_num_periods(budget, 12);

    gnc_engine_gen_event( &budget->inst.entity, GNC_EVENT_CREATE );

    LEAVE(" ");
    return budget;
}

static void
remove_bgt_line_items(QofEntity *act, gpointer bgt)
{
    KvpFrame *frame;
    const GUID *guid;
    gchar guidbuf[GUID_ENCODING_LENGTH+1];

    frame = qof_instance_get_slots(QOF_INSTANCE(bgt));
    guid = qof_entity_get_guid(QOF_ENTITY(act));
    guid_to_string_buff(guid, guidbuf);
    kvp_frame_delete(kvp_frame_get_frame(frame, guidbuf));
}

static void
gnc_budget_remove_all_line_items(GncBudget *budget)
{
    QofBook *book;
    QofCollection *col;

    g_return_if_fail(GNC_IS_BUDGET(budget));

    book = qof_instance_get_book(QOF_INSTANCE(budget));
    col = qof_book_get_collection(book, GNC_ID_ACCOUNT);
    qof_collection_foreach(col, remove_bgt_line_items, (gpointer) budget);
}

void
gnc_budget_free(GncBudget* budget)
{
    if (budget == NULL)
        return;

    g_return_if_fail(GNC_IS_BUDGET(budget));
    gnc_budget_remove_all_line_items(budget);

    /* We first send the message that this object is about to be
     * destroyed so that any GUI elements can remove it before it is
     * actually gone. */
    gnc_engine_gen_event( &budget->inst.entity, GNC_EVENT_DESTROY);

    if (budget->name)
        g_free(budget->name);

    if (budget->description)
        g_free(budget->description);

    qof_instance_release (&budget->inst);
    g_free(budget);
}

void
gnc_budget_set_name(GncBudget* budget, const gchar* name)
{
    g_return_if_fail(GNC_IS_BUDGET(budget));
    g_return_if_fail(name);

    if (budget->name)
        g_free(budget->name);
    budget->name = g_strdup(name);
    gnc_engine_gen_event( &budget->inst.entity, GNC_EVENT_MODIFY);
}

const gchar*
gnc_budget_get_name(GncBudget* budget)
{
    g_return_val_if_fail(GNC_IS_BUDGET(budget), NULL);
    return budget->name;
}

void
gnc_budget_set_description(GncBudget* budget, const gchar* description)
{
    g_return_if_fail(GNC_IS_BUDGET(budget));
    g_return_if_fail(description);

    if (budget->description)
        g_free( budget->description);
    budget->description = g_strdup(description);
    gnc_engine_gen_event( &budget->inst.entity, GNC_EVENT_MODIFY);
}

const gchar*
gnc_budget_get_description(GncBudget* budget)
{
    g_return_val_if_fail(GNC_IS_BUDGET(budget), NULL);
    return budget->description;
}

void
gnc_budget_set_recurrence(GncBudget *budget, const Recurrence *r)
{
    g_return_if_fail(budget && r);
    budget->recurrence = *r;
    gnc_engine_gen_event(&budget->inst.entity, GNC_EVENT_MODIFY);
}

const Recurrence *
gnc_budget_get_recurrence(GncBudget *budget)
{
    g_return_val_if_fail(budget, NULL);
    return (&budget->recurrence);
}

const GUID*
gnc_budget_get_guid(GncBudget* budget)
{
    g_return_val_if_fail(budget, NULL);
    g_return_val_if_fail(GNC_IS_BUDGET(budget), NULL);
    return qof_entity_get_guid(QOF_ENTITY(budget));
}

void
gnc_budget_set_num_periods(GncBudget* budget, guint num_periods)
{
    g_return_if_fail(GNC_IS_BUDGET(budget));
    budget->num_periods = num_periods;
    gnc_engine_gen_event( &budget->inst.entity, GNC_EVENT_MODIFY);
}

guint
gnc_budget_get_num_periods(GncBudget* budget)
{
    g_return_val_if_fail(GNC_IS_BUDGET(budget), 0);
    return budget->num_periods;
}

#define BUF_SIZE (10 + GUID_ENCODING_LENGTH + \
   GNC_BUDGET_MAX_NUM_PERIODS_DIGITS)

/* period_num is zero-based */
/* What happens when account is deleted, after we have an entry for it? */
void
gnc_budget_set_account_period_value(GncBudget *budget, Account *account,
                                    guint period_num, gnc_numeric val)
{
    const GUID *guid;
    KvpFrame *frame;
    gchar path[BUF_SIZE];
    gchar *bufend;

    frame = qof_instance_get_slots(QOF_INSTANCE(budget));
    guid = xaccAccountGetGUID(account);
    bufend = guid_to_string_buff(guid, path);
    g_sprintf(bufend, "/%d", period_num);

    kvp_frame_set_numeric(frame, path, val);
    gnc_engine_gen_event( &budget->inst.entity, GNC_EVENT_MODIFY);

}

/* We don't need these here, but maybe they're useful somewhere else?
   Maybe this should move to Account.h */
#if 0
static gpointer
is_same_commodity(Account *a, gpointer data)
{
    gnc_commodity *acct_comm;
    gnc_commodity *comm;

    g_return_val_if_fail(data, NULL);
    // What? No type-checking macro?
    comm = (gnc_commodity *) data;
    acct_comm = xaccAccountGetCommodity(a);

    return gnc_commodity_equal(comm, acct_comm) ? NULL : data;
}

static gboolean
xaccAccountChildrenHaveSameCommodity(Account *account)
{
    AccountGroup *grp;
    gpointer different;
    gnc_commodity *comm;

    comm = xaccAccountGetCommodity(account);
    grp = xaccAccountGetChildren(account);
    different = xaccGroupForEachAccount(
        grp, is_same_commodity, comm, TRUE);
    return (different == NULL);
}
#endif


/* In order to distinguish between a value of zero and an unset value,
   this function can return a gnc_numeric with GNC_ERROR_ARG set.
   Currently, it only does so for placeholder accounts with
   mixed-commodity subaccounts. */
gnc_numeric
gnc_budget_get_account_period_value(GncBudget *budget, Account *account,
                                    guint period_num)
{
    gnc_numeric numeric;
    gchar path[BUF_SIZE];
    gchar *bufend;
    const GUID *guid;
    KvpFrame *frame;

    numeric = gnc_numeric_zero();
    g_return_val_if_fail(GNC_IS_BUDGET(budget), numeric);
    g_return_val_if_fail(account, numeric);

    /* FIXME? check for unset?  Right now, returns zero on unset. */
    frame = qof_instance_get_slots(QOF_INSTANCE(budget));
    guid = xaccAccountGetGUID(account);
    bufend = guid_to_string_buff(guid, path);
    g_sprintf(bufend, "/%d", period_num);
    numeric = kvp_frame_get_numeric(frame, path);
    return numeric;
}

/* If 'end' is true, then get a time just before the beginning of the
   next period */
static time_t
gnc_budget_get_period_time(GncBudget *budget, guint period_num, gboolean end)
{
    GDate date;
    recurrenceNthInstance(&(budget->recurrence),
                          period_num + (end ? 1 : 0), &date);
    if (end) {
        g_date_subtract_days(&date, 1);
        return gnc_timet_get_day_end_gdate(&date);
    } else {
        return gnc_timet_get_day_start_gdate(&date);
    }

}

Timespec
gnc_budget_get_period_start_date(GncBudget *budget, guint period_num)
{
    Timespec ts;
    timespecFromTime_t(
        &ts,  gnc_budget_get_period_time(budget, period_num, FALSE));
    return ts;
}

gnc_numeric
gnc_budget_get_account_period_actual_value(
    GncBudget *budget, Account *account, guint period_num)
{
    gnc_numeric numeric, num1, num2;
    time_t t1, t2;

    // FIXME: maybe zero is not best error return val.
    g_return_val_if_fail(GNC_IS_BUDGET(budget) && account, gnc_numeric_zero());
    t1 = gnc_budget_get_period_time(budget, period_num, FALSE);
    t2 = gnc_budget_get_period_time(budget, period_num, TRUE);

    num1 = xaccAccountGetBalanceAsOfDateInCurrency(
        account, t1, NULL, TRUE);
    num2 = xaccAccountGetBalanceAsOfDateInCurrency(
        account, t2, NULL, TRUE);

    numeric = gnc_numeric_sub(num2, num1, GNC_DENOM_AUTO,
                              GNC_HOW_DENOM_FIXED);
    return numeric;
}

QofBook*
gnc_budget_get_book(GncBudget* budget)
{
    g_return_val_if_fail(GNC_IS_BUDGET(budget), NULL);
    return qof_instance_get_book(&budget->inst);
}

GncBudget*
gnc_budget_lookup (const GUID *guid, QofBook *book)
{
    QofCollection *col;

    g_return_val_if_fail(guid, NULL);
    g_return_val_if_fail(book, NULL);
    col = qof_book_get_collection (book, GNC_ID_BUDGET);
    return GNC_BUDGET(qof_collection_lookup_entity (col, guid));
}

static void just_get_one(QofEntity *ent, gpointer data)
{
    GncBudget **bgt = (GncBudget**)data;
    if (bgt && !*bgt) *bgt = GNC_BUDGET(ent);
}

GncBudget*
gnc_budget_get_default (QofBook *book)
{
    QofCollection *col;
    GncBudget *bgt = NULL;

    g_return_val_if_fail(book, NULL);
    col = qof_book_get_collection(book, GNC_ID_BUDGET);
    if (qof_collection_count(col) > 0) {
        qof_collection_foreach(col, just_get_one, &bgt);
    }
    return bgt;
}

/* Define the QofObject. */
/* TODO: Eventually, I'm think I'm going to have to check if this struct is
   complete.  Also, do we need one of those QofParam thingys? */
static QofObject budget_object_def =
{
    interface_version: QOF_OBJECT_VERSION,
    e_type:            GNC_ID_BUDGET,
    type_label:        "BUDGET",
    create:            (gpointer (*)(QofBook *)) gnc_budget_new,
    book_begin:        NULL,
    book_end:          NULL,
    is_dirty:          NULL,
    mark_clean:        NULL,
    foreach:           qof_collection_foreach,
    printable:         NULL,
    version_cmp:       (int (*)(gpointer, gpointer)) qof_instance_version_cmp,
};

/* Register ourselves with the engine. */
gboolean gnc_budget_register (void)
{
    return qof_object_register (&budget_object_def);
}
