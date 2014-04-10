#include <glib.h>
#include <stdlib.h>

#include "gnc-ui-util.h"
#include "gnc-numeric.h"
#include "test-engine-stuff.h"
#include "test-stuff.h"


static void
test_num_print_info (gnc_numeric n, GNCPrintAmountInfo print_info)
{
  gnc_numeric n_parsed;
  const char *s;
  gboolean ok;

  s = xaccPrintAmount (n, print_info);

  ok = xaccParseAmount (s, print_info.monetary, &n_parsed, NULL);

  do_test_args (ok, "parsing failure", __FILE__, __LINE__,
                "num: %s, string %s", gnc_numeric_to_string (n), s);

  ok = gnc_numeric_equal (n, n_parsed);

  do_test_args (ok, "not equal", __FILE__, __LINE__,
                "start: %s, string %s, finish: %s",
                gnc_numeric_to_string (n), s,
                gnc_numeric_to_string (n_parsed));
}

static void
test_num (gnc_numeric n)
{
  GNCPrintAmountInfo print_info;
  int fraction;
  int i;

  print_info.commodity = NULL;
  print_info.min_decimal_places = 0;
  print_info.use_locale = 1;
  print_info.use_symbol = 0;

  for (i = 1, fraction = 10; i < 9; i++, fraction *= 10)
  {
    gnc_numeric n1;

    print_info.use_separators = 1;
    print_info.monetary = 1;
    print_info.max_decimal_places = i;

    n1 = gnc_numeric_convert (n, fraction, GNC_RND_ROUND);

    test_num_print_info (n1, print_info);

    print_info.monetary = 0;
    test_num_print_info (n1, print_info);

    print_info.use_separators = 0;
    test_num_print_info (n1, print_info);
  }
}

static void
run_tests (void)
{
  int i;

  for (i = 0; i < 50; i++)
  {
    gnc_numeric n;

    n = get_random_gnc_numeric ();
    test_num (n);

    n = gnc_numeric_mul (n, n, n.denom, GNC_RND_ROUND);
    test_num (n);

    n = gnc_numeric_mul (n, n, n.denom, GNC_RND_ROUND);
    test_num (n);
  }
}

int
main (int argc, char **argv)
{
  run_tests ();
  print_test_results ();
  exit (get_rv ());
}
