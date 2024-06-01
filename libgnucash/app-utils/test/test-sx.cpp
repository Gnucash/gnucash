/********************************************************************\
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
 * 51 Franklin Street, Fifth Floor    Fax:    +1-617-542-2652       *
 * Boston, MA  02110-1301,  USA       gnu@gnu.org                   *
 *                                                                  *
\********************************************************************/

#include <config.h>
#include <glib.h>
#include <libguile.h>

#include <stdlib.h>
#include "SX-book.h"
#include "SX-ttinfo.hpp"
#include "gnc-date.h"
#include "gnc-session.h"
#include "gnc-sx-instance-model.h"
#include "gnc-ui-util.h"
#include "SchedXaction.hpp"

#include "test-stuff.h"
#include "test-engine-stuff.h"

static void
test_basic()
{
    GncSxInstanceModel *model;
    GDate yesterday, today, tomorrow, before_yesterday, after_tomorrow;
    SchedXaction *one_sx;

    g_date_clear(&today, 1);
    gnc_gdate_set_today (&today);

    yesterday = today;
    g_date_subtract_days(&yesterday, 1);
    before_yesterday = yesterday;
    g_date_subtract_days(&before_yesterday, 1);
    tomorrow = today;
    g_date_add_days(&tomorrow, 1);
    after_tomorrow = tomorrow;
    g_date_add_days(&after_tomorrow, 1);

    one_sx = add_daily_sx("foo", &yesterday, NULL, NULL);

    model = gnc_sx_get_instances(&tomorrow, TRUE);

    {
        GncSxInstances *insts;
        GList *iter;

        do_test(g_list_length(gnc_sx_instance_model_get_sx_instances_list(model)) == 1, "1 GncSxInstances");
        insts = (GncSxInstances*)gnc_sx_instance_model_get_sx_instances_list(model)->data;
        do_test(g_list_length(insts->instance_list) == 3, "yesterday, today and tomorrow");
        for (iter = insts->instance_list; iter != NULL; iter = iter->next)
        {
            GncSxInstance *inst = (GncSxInstance*)iter->data;
            do_test(inst->state == SX_INSTANCE_STATE_TO_CREATE, "to-create");
        }
    }

    xaccSchedXactionSetEndDate(one_sx, &tomorrow);

    do_test(gnc_sx_get_num_occur_daterange(one_sx, &before_yesterday, &before_yesterday) == 0, "0 occurrences before start");
    do_test(gnc_sx_get_num_occur_daterange(one_sx, &today, &today) == 1, "1 occurrence today");
    do_test(gnc_sx_get_num_occur_daterange(one_sx, &today, &tomorrow) == 2, "2 occurrence today and tomorrow");
    do_test(gnc_sx_get_num_occur_daterange(one_sx, &yesterday, &tomorrow) == 3, "3 occurrences from yesterday to tomorrow");
    do_test(gnc_sx_get_num_occur_daterange(one_sx, &tomorrow, &tomorrow) == 1, "1 occurrence tomorrow");
    do_test(gnc_sx_get_num_occur_daterange(one_sx, &after_tomorrow, &after_tomorrow) == 0, "0 occurrence after the SX has ended");

    g_object_unref(G_OBJECT(model));
    remove_sx(one_sx);
}

static void
test_empty()
{
    // no sxes should exist at this point.
    int way_in_the_future_year = 2038;
    GDate *end;
    GncSxInstanceModel *model;

    end = g_date_new_dmy(31, (GDateMonth)12, way_in_the_future_year);
    model = gnc_sx_get_instances(end, TRUE);
    do_test(g_list_length(gnc_sx_instance_model_get_sx_instances_list(model)) == 0, "no instances");
    g_object_unref(G_OBJECT(model));
    g_date_free(end);
    success("empty");
}

static void
test_once()
{
    SchedXaction *lonely;
    GDate *when, *end;
    int random_offset_within_one_year = 0;
    GncSxInstanceModel *model;
    GncSxInstances *instances;
    GncSxInstance *instance;

    when = g_date_new();
    g_date_clear(when, 1);
    gnc_gdate_set_today (when);
    while (random_offset_within_one_year == 0)
        random_offset_within_one_year = get_random_int_in_range(-365, 365);
    if (random_offset_within_one_year > 0)
        g_date_add_days(when, random_offset_within_one_year);
    else
        g_date_subtract_days(when, -random_offset_within_one_year);

    end = g_date_new();
    g_date_clear(end, 1);
    gnc_gdate_set_today (end);
    g_date_add_years(end, 1);

    lonely = add_once_sx("once", when);

    model = gnc_sx_get_instances(end, TRUE);

    do_test(g_list_length(gnc_sx_instance_model_get_sx_instances_list(model)) == 1, "1 instances");
    instances = (GncSxInstances*)gnc_sx_instance_model_get_sx_instances_list(model)->data;
    do_test(g_list_length(instances->instance_list) == 1, "1 instance");
    instance = (GncSxInstance*)instances->instance_list->data;
    do_test(g_date_compare(when, &instances->next_instance_date) == 0, "next instance is expected");
    do_test(g_date_compare(when, &instance->date) == 0, "instance date is expected");

    g_object_unref(model);
    success("model unref");
    remove_sx(lonely);
    g_date_free (when);
    g_date_free (end);
}

static GncSxInstance*
_nth_instance(GncSxInstances *instances, int i)
{
    return (GncSxInstance*)g_list_nth_data(instances->instance_list, i);
}

static void
test_state_changes()
{
    SchedXaction *foo;
    GDate *start, *end;
    GncSxInstanceModel *model;
    GncSxInstances *insts;
    GncSxInstance *inst;

    start = g_date_new();
    gnc_gdate_set_today (start);

    end = g_date_new();
    gnc_gdate_set_today (end);
    g_date_add_days(end, 3);

    foo = add_daily_sx("foo", start, NULL, NULL);
    model = gnc_sx_get_instances(end, TRUE);

    do_test(g_list_length(gnc_sx_instance_model_get_sx_instances_list(model)) == 1, "one sx");
    insts = (GncSxInstances*)g_list_nth_data(gnc_sx_instance_model_get_sx_instances_list(model), 0);
    do_test(g_list_length(insts->instance_list) == 4, "4 instances");

    inst = _nth_instance(insts, 2);
    gnc_sx_instance_model_change_instance_state(model, inst, SX_INSTANCE_STATE_TO_CREATE);
    {
        int i;
        for (i = 0; i < 4; i++)
        {
            inst = _nth_instance(insts, i);
            do_test(inst->state == SX_INSTANCE_STATE_TO_CREATE, "0 didn't change");
        }
        success("nothing else changed");
    }

    inst = _nth_instance(insts, 1);
    gnc_sx_instance_model_change_instance_state(model, inst, SX_INSTANCE_STATE_POSTPONED);
    {
        int i;
        inst = _nth_instance(insts, 1);
        do_test(inst->state == SX_INSTANCE_STATE_POSTPONED, "as we said");
        for (i = 0; i < 4; i++)
        {
            if (i == 1)
                continue; // skip
            inst = _nth_instance(insts, i);
            do_test(inst->state == SX_INSTANCE_STATE_TO_CREATE, "still to create");
        }
    }
    success("postponed changed what it needed to");

    inst = _nth_instance(insts, 1);
    gnc_sx_instance_model_change_instance_state(model, inst, SX_INSTANCE_STATE_REMINDER);
    success("changed to reminder");
    {
        int i;
        inst = _nth_instance(insts, 0);
        do_test(inst->state == SX_INSTANCE_STATE_TO_CREATE, "left alone");
        inst = _nth_instance(insts, 1);
        do_test(inst->state == SX_INSTANCE_STATE_REMINDER, "as we asked for");
        for (i = 2; i < 4; i++)
        {
            inst = _nth_instance(insts, i);
            do_test(inst->state == SX_INSTANCE_STATE_REMINDER, "changed as well");
        }
    }

    g_object_unref(model);
    remove_sx(foo);
    g_date_free (start);
    g_date_free (end);
}

static void
make_one_transaction_begin (TTInfoPtr& tti, Account **account1, Account **account2)
{
    QofBook *book = qof_session_get_book(gnc_get_current_session());

    *account1 = get_random_account(book);
    *account2 = get_random_account(book);
    tti = std::make_shared<TTInfo>();

    // Both accounts need to have the same currency
    xaccAccountBeginEdit(*account2);
    xaccAccountSetCommodity(*account2, xaccAccountGetCommodity(*account1));
    xaccAccountCommitEdit(*account2);

}

static void
make_one_transaction_end(TTInfoPtr& tti, SchedXaction *sx)
{
    QofBook *book = qof_session_get_book(gnc_get_current_session());
    xaccSchedXactionSetTemplateTrans (sx, { tti }, book);
}

static void
make_one_transaction_with_two_splits(SchedXaction *sx, const char *value1,
                                     const char *value2, int set_txcurr)
{
    TTInfoPtr tti;
    Account *account1;
    Account *account2;

    make_one_transaction_begin (tti, &account1, &account2);

    if (set_txcurr)
        tti->set_currency (xaccAccountGetCommodity(account1));

    TTSplitInfoPtr split1 = std::make_shared<TTSplitInfo>();
    TTSplitInfoPtr split2 = std::make_shared<TTSplitInfo>();

    split1->set_account (account1);
    split1->set_debit_formula (value1);
    tti->append_template_split (split1);

    split2->set_account (account2);
    split2->set_credit_formula (value2);
    tti->append_template_split (split2);

    make_one_transaction_end (tti, sx);
}

static void
make_one_transaction(SchedXaction *sx)
{
    make_one_transaction_with_two_splits(sx, "123", "123", FALSE);
}

static void
make_one_zero_transaction(SchedXaction *sx)
{
    make_one_transaction_with_two_splits(sx, "0", "0", FALSE);
}

static void
make_one_empty_transaction(SchedXaction *sx)
{
    make_one_transaction_with_two_splits(sx, "", "", FALSE);
}

static void
make_one_empty_transaction_with_txcurr(SchedXaction *sx)
{
    make_one_transaction_with_two_splits(sx, "", "", TRUE);
}

static void
test_auto_create_transactions(const char *name, void (*populate_sx)(SchedXaction*), unsigned int expected_txns)
{
    GncSxInstanceModel *model;
    GDate yesterday, today;
    SchedXaction *one_sx;
    GncSxSummary summary;
    GList *auto_created_txns = NULL;

    g_date_clear(&today, 1);
    gnc_gdate_set_today(&today);

    yesterday = today;
    g_date_subtract_days(&yesterday, 1);

    one_sx = add_daily_sx(name, &yesterday, NULL, NULL);

    xaccSchedXactionSetNumOccur(one_sx, 1);
    xaccSchedXactionSetRemOccur(one_sx, 1);
    xaccSchedXactionSetAutoCreate(one_sx, TRUE, FALSE);

    populate_sx(one_sx);

    model = gnc_sx_get_current_instances();
    gnc_sx_instance_model_summarize(model, &summary);
    gnc_sx_instance_model_effect_change(model, TRUE, &auto_created_txns, NULL);

    do_test_args(summary.need_dialog == 0, "Dialog not required",
        __FILE__, __LINE__, "for %s", name);
    do_test_args(summary.num_auto_create_no_notify_instances == 1, "1 automatically created instance",
        __FILE__, __LINE__, "for %s", name);
    do_test_args(g_list_length(auto_created_txns) == expected_txns, "Automatically created transactions",
        __FILE__, __LINE__, "for %s: auto_created_txns = %u, expected_txns = %u",
        name, g_list_length(auto_created_txns), expected_txns);

    g_list_free(auto_created_txns);
    g_object_unref(model);
    remove_sx(one_sx);
}

static void
real_main(void *closure, int argc, char **argv)
{
    g_setenv ("GNC_UNINSTALLED", "1", TRUE);
    qof_init();
    gnc_engine_init(0, NULL);

    test_empty();
    {
        int i;
        for (i = 0; i < 10; i++)
            test_once();
    }
    test_basic();
    test_state_changes();

    test_auto_create_transactions("make_one_transaction", make_one_transaction, 1);
    test_auto_create_transactions("make_one_zero_transaction", make_one_zero_transaction, 1);
    test_auto_create_transactions("make_one_empty_transaction", make_one_empty_transaction, 1);
    test_auto_create_transactions("make_one_empty_transaction_with_txcurr", make_one_empty_transaction_with_txcurr, 1);

    print_test_results();
    exit(get_rv());
}

int main(int argc, char **argv)
{
    /* do things this way so we can test scheme function calls from expressions */
    scm_boot_guile(argc, argv, real_main, NULL);
    return 0;
}
