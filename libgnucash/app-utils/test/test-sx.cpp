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

extern "C"
{
#include <config.h>
#include <stdlib.h>
#include <glib.h>
#include "SX-book.h"
#include "gnc-date.h"
#include "gnc-sx-instance-model.h"
#include "gnc-ui-util.h"

#include "test-stuff.h"
#include "test-engine-stuff.h"
}

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

        do_test(g_list_length(model->sx_instance_list) == 1, "1 GncSxInstances");
        insts = (GncSxInstances*)model->sx_instance_list->data;
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
    do_test(g_list_length(model->sx_instance_list) == 0, "no instances");
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
    g_date_add_days(when, random_offset_within_one_year);

    end = g_date_new();
    g_date_clear(end, 1);
    gnc_gdate_set_today (end);
    g_date_add_years(end, 1);

    lonely = add_once_sx("once", when);

    model = gnc_sx_get_instances(end, TRUE);

    do_test(g_list_length(model->sx_instance_list) == 1, "1 instances");
    instances = (GncSxInstances*)model->sx_instance_list->data;
    do_test(g_list_length(instances->instance_list) == 1, "1 instance");
    instance = (GncSxInstance*)instances->instance_list->data;
    do_test(g_date_compare(when, &instances->next_instance_date) == 0, "next instance is expected");
    do_test(g_date_compare(when, &instance->date) == 0, "instance date is expected");

    g_object_unref(model);
    success("model unref");
    remove_sx(lonely);
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

    do_test(g_list_length(model->sx_instance_list) == 1, "one sx");
    insts = (GncSxInstances*)g_list_nth_data(model->sx_instance_list, 0);
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
}

int
main(int argc, char **argv)
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

    print_test_results();
    exit(get_rv());
}
