#include <ctype.h>
#include <glib.h>
#include <guile/gh.h>
#include <time.h>

#include "TransLog.h"
#include "date.h"
#include "gnc-module.h"
#include "test-stuff.h"
#include "test-engine-stuff.h"


static gboolean
check_time (Timespec ts, gboolean always_print)
{
  Timespec ts_2;
  char str[128];
  gboolean ok;

  ts.tv_nsec = MIN (ts.tv_nsec, 999999999);
  ts.tv_nsec /= 1000;
  ts.tv_nsec *= 1000;

  gnc_timespec_to_iso8601_buff (ts, str);

  ts_2 = gnc_iso8601_to_timespec_local (str);

  ok = timespec_equal (&ts, &ts_2);

  if (!ok || always_print)
  {
    fprintf (stderr,
             "%lld:%lld -> %s -> %lld:%lld "
             "(diff of %lld secs %lld nsecs)\n",
             (long long int) ts.tv_sec,
             (long long int) ts.tv_nsec,
             str,
             (long long int) ts_2.tv_sec,
             (long long int) ts_2.tv_nsec,
             (long long int) (ts.tv_sec - ts_2.tv_sec),
             (long long int) (ts.tv_nsec - ts_2.tv_nsec));

    if (!ok)
    {
      failure ("timespecs don't match");
      return FALSE;
    }
  }

  success ("timespecs match");

  return TRUE;
}

static void
run_test (void)
{
  Timespec ts;
  int i;

  ts.tv_sec = 152098136;
  ts.tv_nsec = 0;
  check_time (ts, FALSE);

  ts.tv_sec = 1162088421;
  ts.tv_nsec = 12548000;
  check_time (ts, FALSE);

  ts.tv_sec = 325659000 - 6500;
  ts.tv_nsec = 0;
  check_time (ts, FALSE);

  ts.tv_sec = 1143943200;
  ts.tv_nsec = 0;
  check_time (ts, FALSE);

  ts.tv_sec = 1603591171;
  ts.tv_nsec = 595311000;
  check_time (ts, FALSE);

  ts.tv_sec = 1738909365;
  ts.tv_nsec = 204102000;
  check_time (ts, FALSE);

  ts.tv_sec = 1603591171;
  ts.tv_nsec = 595311000;
  check_time (ts, FALSE);

  ts.tv_sec = 1143943200 - 1;
  ts.tv_nsec = 0;
  check_time (ts, FALSE);

  ts.tv_sec = 1143943200;
  ts.tv_nsec = 0;
  check_time (ts, FALSE);

  ts.tv_sec = 1143943200 + (7 * 60 * 60);
  ts.tv_nsec = 0;
  check_time (ts, FALSE);

  ts.tv_sec = 1143943200 + (8 * 60 * 60);
  ts.tv_nsec = 0;
  check_time (ts, FALSE);

  ts = *get_random_timespec ();

  for (i = 0; i < 10000; i++)
  {
    ts.tv_sec += 10800;
    if (!check_time (ts, FALSE))
      return;
  }

  for (i = 0; i < 5000; i++)
  {
    ts = *get_random_timespec ();

    ts.tv_nsec = MIN (ts.tv_nsec, 999999999);
    ts.tv_nsec /= 1000;
    ts.tv_nsec *= 1000;

    if (!check_time (ts, FALSE))
      return;
  }
}

int
main (int argc, char **argv)
{
  run_test ();

  success ("dates seem to work");

  print_test_results();
  exit(get_rv());
}
