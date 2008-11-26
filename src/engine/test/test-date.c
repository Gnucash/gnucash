/*
 * -- fix borken timezone test -- linas May 2004
 */

#include "config.h"
#include <ctype.h>
#include <glib.h>
#include <time.h>

#include "gnc-date.h"
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

  /* We just can't handle dates whose time_t doesn't fit in int - skip those
   * cases. */
  if (ts.tv_sec > (0x7fffffff - 3600*25))
      return TRUE;

  /* If we are east of UTC, we also can't handle dates whose tv_sec member
   * falls in the range [0, -gnc_timezone(tm)) - make sure were are at least 12
   * hours past the epoch to skip those cases too */
  if (ts.tv_sec < 3600*12)
      return TRUE;

  gnc_timespec_to_iso8601_buff (ts, str);

  /* The time, in seconds, everywhere on the planet, is always
   * the same, and is independent of location.  In particular,
   * the time, in seconds, is identical to the local time in 
   * Greenwich (GMT).
   */
  ts_2 = gnc_iso8601_to_timespec_gmt (str);

  ok = timespec_equal (&ts, &ts_2);

  if (!ok || always_print)
  {
    fprintf (stderr,
             "\n%" G_GINT64_FORMAT ":%ld -> %s ->\n"
             "\t%" G_GINT64_FORMAT ":%ld"
             " (diff of %" G_GINT64_FORMAT " secs %ld nsecs)\n",
             ts.tv_sec, ts.tv_nsec, str,
             ts_2.tv_sec, ts_2.tv_nsec,
             ts.tv_sec - ts_2.tv_sec, ts.tv_nsec - ts_2.tv_nsec);

    if (!ok)
    {
      failure ("timespec to iso8601 string conversion failed");
      return FALSE;
    }
  }

  success ("timespec to iso8601 string conversion passes");

  return TRUE;
}

static gboolean
check_conversion (const char * str, Timespec expected_ts)
{
  Timespec ts;

  ts = gnc_iso8601_to_timespec_gmt (str);

  if ((expected_ts.tv_sec != ts.tv_sec) || (expected_ts.tv_nsec != ts.tv_nsec)) 
  {
    fprintf (stderr, 
             "\nmis-converted \"%s\" to %" G_GUINT64_FORMAT ".%09ld seconds\n"
             "\twas expecting %" G_GUINT64_FORMAT ".%09ld seconds\n", 
             str, ts.tv_sec, ts.tv_nsec, 
             expected_ts.tv_sec, expected_ts.tv_nsec); 
    failure ("misconverted timespec");
    return FALSE;
  }
  success ("good conversion");
  return TRUE;
}

static void
run_test (void)
{
  Timespec ts;
  int i;
  gboolean do_print = FALSE;

  /* Now leaving the 60's:
   *
   * Black Panthers
   * Weather Underground
   * Kent State
   * Evacuation of Vietnam
   * Impeachment 
   * Gas Crisis
   * New York Garbage Crisis
   * Stagflation
   * Delapidated Bicentennial
   * Sex Pistols
   * Punk Rock
   *
   * Of course, anything had to be better than the miserable 70's, 
   * which explains why Reagan was elected.  Food for thought.
   */
  ts.tv_sec = 10*365*24*3600 + 2*24*3600;
  ts.tv_nsec = 0;
  check_conversion ("1979-12-31 15:00:00.000000 -0900", ts);
  check_conversion ("1979-12-31 16:00:00.000000 -0800", ts);
  check_conversion ("1979-12-31 17:00:00.000000 -0700", ts);
  check_conversion ("1979-12-31 18:00:00.000000 -0600", ts);
  check_conversion ("1979-12-31 19:00:00.000000 -0500", ts);
  check_conversion ("1979-12-31 20:00:00.000000 -0400", ts);
  check_conversion ("1979-12-31 21:00:00.000000 -0300", ts);
  check_conversion ("1979-12-31 22:00:00.000000 -0200", ts);
  check_conversion ("1979-12-31 23:00:00.000000 -0100", ts);

  check_conversion ("1980-01-01 00:00:00.000000 -0000", ts);
  check_conversion ("1980-01-01 00:00:00.000000 +0000", ts);

  check_conversion ("1980-01-01 01:00:00.000000 +0100", ts);
  check_conversion ("1980-01-01 02:00:00.000000 +0200", ts);
  check_conversion ("1980-01-01 03:00:00.000000 +0300", ts);
  check_conversion ("1980-01-01 04:00:00.000000 +0400", ts);
  check_conversion ("1980-01-01 05:00:00.000000 +0500", ts);
  check_conversion ("1980-01-01 06:00:00.000000 +0600", ts);
  check_conversion ("1980-01-01 07:00:00.000000 +0700", ts);
  check_conversion ("1980-01-01 08:00:00.000000 +0800", ts);

  /* check minute-offsets as well */
  check_conversion ("1980-01-01 08:01:00.000000 +0801", ts);
  check_conversion ("1980-01-01 08:02:00.000000 +0802", ts);
  check_conversion ("1980-01-01 08:03:00.000000 +0803", ts);
  check_conversion ("1980-01-01 08:23:00.000000 +0823", ts);
  check_conversion ("1980-01-01 08:35:00.000000 +0835", ts);
  check_conversion ("1980-01-01 08:47:00.000000 +0847", ts);
  check_conversion ("1980-01-01 08:59:00.000000 +0859", ts);

  check_conversion ("1979-12-31 15:01:00.000000 -0859", ts);
  check_conversion ("1979-12-31 15:02:00.000000 -0858", ts);
  check_conversion ("1979-12-31 15:03:00.000000 -0857", ts);
  check_conversion ("1979-12-31 15:23:00.000000 -0837", ts);
  check_conversion ("1979-12-31 15:45:00.000000 -0815", ts);


  /* The 90's */
  ts.tv_sec = 20*365*24*3600 + 5*24*3600;
  ts.tv_nsec = 0;
  check_conversion ("1989-12-31 15:00:00.000000 -0900", ts);
  check_conversion ("1989-12-31 16:00:00.000000 -0800", ts);
  check_conversion ("1989-12-31 17:00:00.000000 -0700", ts);
  check_conversion ("1989-12-31 18:00:00.000000 -0600", ts);
  check_conversion ("1989-12-31 19:00:00.000000 -0500", ts);
  check_conversion ("1989-12-31 20:00:00.000000 -0400", ts);
  check_conversion ("1989-12-31 21:00:00.000000 -0300", ts);
  check_conversion ("1989-12-31 22:00:00.000000 -0200", ts);
  check_conversion ("1989-12-31 23:00:00.000000 -0100", ts);

  check_conversion ("1990-01-01 00:00:00.000000 -0000", ts);
  check_conversion ("1990-01-01 00:00:00.000000 +0000", ts);

  check_conversion ("1990-01-01 01:00:00.000000 +0100", ts);
  check_conversion ("1990-01-01 02:00:00.000000 +0200", ts);
  check_conversion ("1990-01-01 03:00:00.000000 +0300", ts);
  check_conversion ("1990-01-01 04:00:00.000000 +0400", ts);
  check_conversion ("1990-01-01 05:00:00.000000 +0500", ts);
  check_conversion ("1990-01-01 06:00:00.000000 +0600", ts);
  check_conversion ("1990-01-01 07:00:00.000000 +0700", ts);
  check_conversion ("1990-01-01 08:00:00.000000 +0800", ts);

  /* check minute-offsets as well */
  check_conversion ("1990-01-01 08:01:00.000000 +0801", ts);
  check_conversion ("1990-01-01 08:02:00.000000 +0802", ts);
  check_conversion ("1990-01-01 08:03:00.000000 +0803", ts);
  check_conversion ("1990-01-01 08:23:00.000000 +0823", ts);
  check_conversion ("1990-01-01 08:35:00.000000 +0835", ts);
  check_conversion ("1990-01-01 08:47:00.000000 +0847", ts);
  check_conversion ("1990-01-01 08:59:00.000000 +0859", ts);

  check_conversion ("1989-12-31 15:01:00.000000 -0859", ts);
  check_conversion ("1989-12-31 15:02:00.000000 -0858", ts);
  check_conversion ("1989-12-31 15:03:00.000000 -0857", ts);
  check_conversion ("1989-12-31 15:23:00.000000 -0837", ts);
  check_conversion ("1989-12-31 15:45:00.000000 -0815", ts);


  /* The naughties */
  ts.tv_sec = 30*365*24*3600 + 7*24*3600;
  ts.tv_nsec = 0;
  check_conversion ("1999-12-31 15:00:00.000000 -0900", ts);
  check_conversion ("1999-12-31 16:00:00.000000 -0800", ts);
  check_conversion ("1999-12-31 17:00:00.000000 -0700", ts);
  check_conversion ("1999-12-31 18:00:00.000000 -0600", ts);
  check_conversion ("1999-12-31 19:00:00.000000 -0500", ts);
  check_conversion ("1999-12-31 20:00:00.000000 -0400", ts);
  check_conversion ("1999-12-31 21:00:00.000000 -0300", ts);
  check_conversion ("1999-12-31 22:00:00.000000 -0200", ts);
  check_conversion ("1999-12-31 23:00:00.000000 -0100", ts);

  check_conversion ("2000-01-01 00:00:00.000000 -0000", ts);
  check_conversion ("2000-01-01 00:00:00.000000 +0000", ts);

  check_conversion ("2000-01-01 01:00:00.000000 +0100", ts);
  check_conversion ("2000-01-01 02:00:00.000000 +0200", ts);
  check_conversion ("2000-01-01 03:00:00.000000 +0300", ts);
  check_conversion ("2000-01-01 04:00:00.000000 +0400", ts);
  check_conversion ("2000-01-01 05:00:00.000000 +0500", ts);
  check_conversion ("2000-01-01 06:00:00.000000 +0600", ts);
  check_conversion ("2000-01-01 07:00:00.000000 +0700", ts);
  check_conversion ("2000-01-01 08:00:00.000000 +0800", ts);

  /* check minute-offsets as well */
  check_conversion ("2000-01-01 08:01:00.000000 +0801", ts);
  check_conversion ("2000-01-01 08:02:00.000000 +0802", ts);
  check_conversion ("2000-01-01 08:03:00.000000 +0803", ts);
  check_conversion ("2000-01-01 08:23:00.000000 +0823", ts);
  check_conversion ("2000-01-01 08:35:00.000000 +0835", ts);
  check_conversion ("2000-01-01 08:47:00.000000 +0847", ts);
  check_conversion ("2000-01-01 08:59:00.000000 +0859", ts);

  check_conversion ("1999-12-31 15:01:00.000000 -0859", ts);
  check_conversion ("1999-12-31 15:02:00.000000 -0858", ts);
  check_conversion ("1999-12-31 15:03:00.000000 -0857", ts);
  check_conversion ("1999-12-31 15:23:00.000000 -0837", ts);
  check_conversion ("1999-12-31 15:45:00.000000 -0815", ts);


  /* The nows */
  ts.tv_sec = 35*365*24*3600 + 9*24*3600;
  ts.tv_nsec = 0;
  check_conversion ("2004-12-31 15:00:00.000000 -0900", ts);
  check_conversion ("2004-12-31 16:00:00.000000 -0800", ts);
  check_conversion ("2004-12-31 17:00:00.000000 -0700", ts);
  check_conversion ("2004-12-31 18:00:00.000000 -0600", ts);
  check_conversion ("2004-12-31 19:00:00.000000 -0500", ts);
  check_conversion ("2004-12-31 20:00:00.000000 -0400", ts);
  check_conversion ("2004-12-31 21:00:00.000000 -0300", ts);
  check_conversion ("2004-12-31 22:00:00.000000 -0200", ts);
  check_conversion ("2004-12-31 23:00:00.000000 -0100", ts);

  check_conversion ("2005-01-01 00:00:00.000000 -0000", ts);
  check_conversion ("2005-01-01 00:00:00.000000 +0000", ts);

  check_conversion ("2005-01-01 01:00:00.000000 +0100", ts);
  check_conversion ("2005-01-01 02:00:00.000000 +0200", ts);
  check_conversion ("2005-01-01 03:00:00.000000 +0300", ts);
  check_conversion ("2005-01-01 04:00:00.000000 +0400", ts);
  check_conversion ("2005-01-01 05:00:00.000000 +0500", ts);
  check_conversion ("2005-01-01 06:00:00.000000 +0600", ts);
  check_conversion ("2005-01-01 07:00:00.000000 +0700", ts);
  check_conversion ("2005-01-01 08:00:00.000000 +0800", ts);

  /* check minute-offsets as well */
  check_conversion ("2005-01-01 08:01:00.000000 +0801", ts);
  check_conversion ("2005-01-01 08:02:00.000000 +0802", ts);
  check_conversion ("2005-01-01 08:03:00.000000 +0803", ts);
  check_conversion ("2005-01-01 08:23:00.000000 +0823", ts);
  check_conversion ("2005-01-01 08:35:00.000000 +0835", ts);
  check_conversion ("2005-01-01 08:47:00.000000 +0847", ts);
  check_conversion ("2005-01-01 08:59:00.000000 +0859", ts);

  check_conversion ("2004-12-31 15:01:00.000000 -0859", ts);
  check_conversion ("2004-12-31 15:02:00.000000 -0858", ts);
  check_conversion ("2004-12-31 15:03:00.000000 -0857", ts);
  check_conversion ("2004-12-31 15:23:00.000000 -0837", ts);
  check_conversion ("2004-12-31 15:45:00.000000 -0815", ts);


  /* Various leap-year days and near-leap times. */
  ts = gnc_iso8601_to_timespec_gmt ("1980-02-29 00:00:00.000000 -0000");
  check_time (ts, do_print);

  ts = gnc_iso8601_to_timespec_gmt ("1979-02-28 00:00:00.000000 -0000");
  check_time (ts, do_print);

  ts = gnc_iso8601_to_timespec_gmt ("1990-02-28 00:00:00.000000 -0000");
  check_time (ts, do_print);

  ts = gnc_iso8601_to_timespec_gmt ("2000-02-29 00:00:00.000000 -0000");
  check_time (ts, do_print);

  ts = gnc_iso8601_to_timespec_gmt ("2004-02-29 00:00:00.000000 -0000");
  check_time (ts, do_print);

  ts = gnc_iso8601_to_timespec_gmt ("2008-02-29 00:00:00.000000 -0000");
  check_time (ts, do_print);

  ts = gnc_iso8601_to_timespec_gmt ("2008-02-29 00:01:00.000000 -0000");
  check_time (ts, do_print);

  ts = gnc_iso8601_to_timespec_gmt ("2008-02-29 02:02:00.000000 -0000");
  check_time (ts, do_print);

  ts = gnc_iso8601_to_timespec_gmt ("2008-02-28 23:23:23.000000 -0000");
  check_time (ts, do_print);

  /* Here's a date ten days after the 2038 rollover that should work
     if/when we support it. */
  ts.tv_nsec = 0;
  ts.tv_sec = (long long int) 0x7fffffff + 3600*24*10;
  //check_time(ts, do_print);

  /* Various 'special' times. What makes these so special? */
  ts.tv_sec = 152098136;
  ts.tv_nsec = 0;
  check_time (ts, do_print);

  ts.tv_sec = 1162088421;
  ts.tv_nsec = 12548000;
  check_time (ts, do_print);

  ts.tv_sec = 325659000 - 6500;
  ts.tv_nsec = 0;
  check_time (ts, do_print);

  ts.tv_sec = 1143943200;
  ts.tv_nsec = 0;
  check_time (ts, do_print);

  ts.tv_sec = 1603591171;
  ts.tv_nsec = 595311000;
  check_time (ts, do_print);

  ts.tv_sec = 1738909365;
  ts.tv_nsec = 204102000;
  check_time (ts, do_print);

  ts.tv_sec = 1603591171;
  ts.tv_nsec = 595311000;
  check_time (ts, do_print);

  ts.tv_sec = 1143943200 - 1;
  ts.tv_nsec = 0;
  check_time (ts, do_print);

  ts.tv_sec = 1143943200;
  ts.tv_nsec = 0;
  check_time (ts, do_print);

  ts.tv_sec = 1143943200 + (7 * 60 * 60);
  ts.tv_nsec = 0;
  check_time (ts, do_print);

  ts.tv_sec = 1143943200 + (8 * 60 * 60);
  ts.tv_nsec = 0;
  check_time (ts, do_print);

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
