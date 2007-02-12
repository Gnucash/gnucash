#include "config.h"
#include <glib.h>
#include "glib-compat.h"
#include "qof.h"
#include "gnc-engine.h"
#include "gnc-sx-instance-model.h"
#include "gnc-sx-instance-dense-cal-adapter.h"
#include "gnc-dense-cal.h"
#include "gnc-dense-cal-model.h"

#include "test-stuff.h"
#include "test-engine-stuff.h"

static void
_removing(GObject *obj, SchedXaction *removing, gpointer unused_user_data)
{
     gnc_sx_instance_model_remove_sx_instances(GNC_SX_INSTANCE_MODEL(obj), removing);
}

static void
setup_default_handlers(GncSxInstanceModel *model)
{
     g_signal_connect(model, "removing", (GCallback)_removing, NULL);
}

static void
test()
{
     GDate *start, *end;
     GncSxInstanceModel *model;
     GncSxInstanceDenseCalAdapter *dense_cal_model;
     GncDenseCal *cal;
     SchedXaction *foo, *bar;

     start = g_date_new();
     g_date_clear(start, 1);
     g_date_set_time_t(start, time(NULL));

     end = g_date_new();
     g_date_clear(end, 1);
     g_date_set_time_t(end, time(NULL));
     g_date_add_years(end, 1);
     
     foo = add_daily_sx("foo", start, NULL, NULL);

     model = gnc_sx_get_instances(end, TRUE);
     setup_default_handlers(model);

     do_test(g_list_length(model->sx_instance_list) == 1, "1 instances");

     dense_cal_model = gnc_sx_instance_dense_cal_adapter_new(model);
     cal = GNC_DENSE_CAL(gnc_dense_cal_new_with_model(GNC_DENSE_CAL_MODEL(dense_cal_model)));
     // gobject-2.10: g_object_ref_sink(cal);
     g_object_ref(G_OBJECT(cal));
     gtk_object_sink(GTK_OBJECT(cal));

     bar = add_daily_sx("bar", start, NULL, NULL);
     do_test(g_list_length(model->sx_instance_list) == 2, "2 instances");

     remove_sx(foo);

     do_test(g_list_length(model->sx_instance_list) == 1, "1 instance");

     g_object_unref(cal);
     success("freed calendar");
     g_object_unref(dense_cal_model);
     success("freed dense-cal model");
     g_object_unref(model);
     success("freed instances");
}

int
main(int argc, char **argv)
{
     g_type_init();
     gnc_engine_init(argc, argv);
     gtk_init(&argc, &argv);

     test();

     print_test_results();
     exit(get_rv());
}
