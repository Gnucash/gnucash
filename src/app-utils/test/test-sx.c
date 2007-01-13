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
     model = gnc_sx_get_instances(range_end_tomorrow);

     {
          GncSxInstances *insts;
          GList *iter;

          do_test(g_list_length(model->sx_instance_list) == 1, "1 GncSxInstances");
          insts = (GncSxInstances*)model->sx_instance_list->data;
          do_test(g_list_length(insts->list) == 3, "yesterday, today and tomorrow");
          for (iter = insts->list; iter != NULL; iter = iter->next)
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
     model = gnc_sx_get_instances(end);
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

     model = gnc_sx_get_instances(end);

     do_test(g_list_length(model->sx_instance_list) == 1, "1 instances");
     instances = (GncSxInstances*)model->sx_instance_list->data;
     do_test(g_list_length(instances->list) == 1, "1 instance");
     instance = (GncSxInstance*)instances->list->data;
     do_test(g_date_compare(when, &instances->next_instance_date) == 0, "next instance is expected");
     do_test(g_date_compare(when, &instance->date) == 0, "instance date is expected");

     g_object_unref(model);
     success("model unref");
     remove_sx(lonely);
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

     print_test_results();
     exit(get_rv());
}
