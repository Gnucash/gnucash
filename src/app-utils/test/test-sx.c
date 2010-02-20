#include "config.h"
#include <stdlib.h>
#include <glib.h>
#include "SX-book.h"
#include "gnc-sx-instance-model.h"
#include "gnc-ui-util.h"

#include "test-stuff.h"
#include "test-engine-stuff.h"

static void
test_basic()
{
    GncSxInstanceModel *model;
    GDate *yesterday, *range_end_tomorrow;
    SchedXaction *foo;

    yesterday = g_date_new();
    g_date_clear(yesterday, 1);
    g_date_set_time_t(yesterday, time(NULL));
    g_date_subtract_days(yesterday, 1);

    foo = add_daily_sx("foo", yesterday, NULL, NULL);

    range_end_tomorrow = g_date_new();
    g_date_clear(range_end_tomorrow, 1);
    g_date_set_time_t(range_end_tomorrow, time(NULL));
    g_date_add_days(range_end_tomorrow, 1);
    model = gnc_sx_get_instances(range_end_tomorrow, TRUE);

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

    g_object_unref(G_OBJECT(model));
    remove_sx(foo);
}

static void
test_empty()
{
    // no sxes should exist at this point.
    int way_in_the_future_year = 2038;
    GDate *end;
    GncSxInstanceModel *model;

    end = g_date_new_dmy(31, 12, way_in_the_future_year);
    model = gnc_sx_get_instances(end, TRUE);
    do_test(g_list_length(model->sx_instance_list) == 0, "no instances");
    g_object_unref(G_OBJECT(model));
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
    g_date_set_time_t(when, time(NULL));
    while (random_offset_within_one_year == 0)
        random_offset_within_one_year = get_random_int_in_range(-365, 365);
    g_date_add_days(when, random_offset_within_one_year);

    end = g_date_new();
    g_date_clear(end, 1);
    g_date_set_time_t(end, time(NULL));
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
    g_date_set_time_t(start, time(NULL));

    end = g_date_new();
    g_date_set_time_t(end, time(NULL));
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
    g_type_init();
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
