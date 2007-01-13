#include "config.h"
#include <stdlib.h>
#include <glib.h>
#include "SX-book.h"
#include "gnc-sx-instance-model.h"
#include "gnc-ui-util.h"

#include "test-stuff.h"
#include "test-engine-stuff.h"

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
     success("empty");
}

static FreqSpec*
daily_freq(GDate* start, int multiplier)
{
     FreqSpec *freq = xaccFreqSpecMalloc(gnc_get_current_book());
     xaccFreqSpecSetDaily(freq, start, multiplier);
     return freq;
}

static void
add_sx(gchar *name, GDate *start, GDate *end, GDate *last_occur, FreqSpec *fs)
{
     SchedXaction *sx = xaccSchedXactionMalloc(gnc_get_current_book());
     xaccSchedXactionSetName(sx, name);
     xaccSchedXactionSetStartDate(sx, start);
     if (end != NULL)
          xaccSchedXactionSetEndDate(sx, end);
     if (last_occur != NULL)
          xaccSchedXactionSetLastOccurDate(sx, last_occur);
     xaccSchedXactionSetFreqSpec(sx, fs);

     gnc_sxes_add_sx(gnc_book_get_schedxactions(gnc_get_current_book()), sx);
}

static void
add_daily_sx(gchar *name, GDate *start, GDate *end, GDate *last_occur)
{
     add_sx(name, start, end, last_occur, daily_freq(start, 1));
}

static void
test_basic()
{
     GncSxInstanceModel *model;
     GDate *yesterday, *range_end_tomorrow;

     yesterday = g_date_new();
     g_date_clear(yesterday, 1);
     g_date_set_time_t(yesterday, time(NULL));
     g_date_subtract_days(yesterday, 1);

     add_daily_sx("foo", yesterday, NULL, NULL);

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
}

static void
test_once()
{
     ;
}

int
main(int argc, char **argv)
{
     g_type_init();
     qof_init();
     gnc_engine_init(0, NULL);

     test_empty();
     test_basic();
     test_once();

     print_test_results();
     exit(get_rv());
}
