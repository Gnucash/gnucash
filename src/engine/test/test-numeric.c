
/* Test file created by Linas Vepstas <linas@linas.org>
 * Review operation of the gnc-numeric tools by verifying results
 * of vairous operations.
 *
 * June 2004
 * License: GPL
 */

#include <ctype.h>
#include <glib.h>

#include "gnc-module.h"
#include "test-stuff.h"
#include "test-engine-stuff.h"
#include "gnc-numeric.h"



static char *
gnc_numeric_print(gnc_numeric in) {
  char * retval;
  if(gnc_numeric_check(in)) {
    retval = g_strdup_printf("<ERROR> [%lld / %lld]",
                             (long long int) in.num,
                             (long long int) in.denom); 
  }
  else {
    retval = g_strdup_printf("[%lld / %lld]",
                             (long long int) in.num,
                             (long long int) in.denom); 
  }
  return retval;
}

static void
check_binary_op (gnc_numeric expected, 
                 gnc_numeric actual, 
					  gnc_numeric input_a, 
					  gnc_numeric input_b, 
					  const char * errmsg)
{
	char *e = gnc_numeric_print (expected);
	char *r = gnc_numeric_print (actual);
	char *a = gnc_numeric_print (input_a);
	char *b = gnc_numeric_print (input_b);
	char * str = g_strdup_printf (errmsg, e,r,a,b);
	
	do_test (gnc_numeric_eq(expected, actual), str);
	
	g_free (a);
	g_free (b);
	g_free (r);
	g_free (e);
	g_free (str);
}

#ifdef XXX_GNC_NUMERIC_TEST
int
main(int argc, char ** argv) {
  gnc_numeric a = gnc_numeric_create(1, 3);
  gnc_numeric b = gnc_numeric_create(1, 4);
  gnc_numeric c;
  gnc_numeric d = gnc_numeric_create(1, 2);
  
  gnc_numeric err;
  int i;
  gint64 v;

  printf("add 100ths (banker's): %s + %s = %s\n",
         gnc_numeric_print(a), gnc_numeric_print(b),
         gnc_numeric_print(gnc_numeric_add(a, b, 100,
                                           GNC_RND_ROUND)));
  
  c = gnc_numeric_add_with_error(a, b, 100, GNC_RND_ROUND, &err);
  printf("add 100ths/error : %s + %s = %s + (error) %s\n\n",
         gnc_numeric_print(a), gnc_numeric_print(b),
         gnc_numeric_print(c),
         gnc_numeric_print(err));
  
  printf("sub exact : %s - %s = %s\n",
         gnc_numeric_print(a), gnc_numeric_print(b),
         gnc_numeric_print(gnc_numeric_sub(a, b, GNC_DENOM_AUTO, 
                                           GNC_DENOM_EXACT)));
  
  printf("sub least : %s - %s = %s\n",
         gnc_numeric_print(a), gnc_numeric_print(b),
         gnc_numeric_print(gnc_numeric_sub(a, b, 
                                           GNC_DENOM_AUTO, 
                                           GNC_DENOM_REDUCE)));
  
  printf("sub 100ths : %s - %s = %s\n",
         gnc_numeric_print(a), gnc_numeric_print(b),
         gnc_numeric_print(gnc_numeric_sub(a, b, 100,
                                           GNC_RND_ROUND)));
  
  c = gnc_numeric_sub_with_error(a, b, 100, GNC_RND_FLOOR, &err);
  printf("sub 100ths/error : %s - %s = %s + (error) %s\n\n",
         gnc_numeric_print(a), gnc_numeric_print(b),
         gnc_numeric_print(c),
         gnc_numeric_print(err));
  
  printf("mul exact : %s * %s = %s\n",
         gnc_numeric_print(a), gnc_numeric_print(b),
         gnc_numeric_print(gnc_numeric_mul(a, b, GNC_DENOM_AUTO, 
                                           GNC_DENOM_EXACT)));

  printf("mul least : %s * %s = %s\n",
         gnc_numeric_print(a), gnc_numeric_print(b),
         gnc_numeric_print(gnc_numeric_mul(a, b, GNC_DENOM_AUTO, 
                                           GNC_DENOM_REDUCE)));
  
  printf("mul 100ths : %s * %s = %s\n",
         gnc_numeric_print(a), gnc_numeric_print(b),
         gnc_numeric_print(gnc_numeric_mul(a, b, 100,
                                           GNC_RND_ROUND)));

  c = gnc_numeric_mul_with_error(a, b, 100, GNC_RND_ROUND, &err);
  printf("mul 100ths/error : %s * %s = %s + (error) %s\n\n",
         gnc_numeric_print(a), gnc_numeric_print(b),
         gnc_numeric_print(c),
         gnc_numeric_print(err));
  
  printf("div exact : %s / %s = %s\n",
         gnc_numeric_print(a), gnc_numeric_print(b),
         gnc_numeric_print(gnc_numeric_div(a, b, GNC_DENOM_AUTO, 
                                           GNC_DENOM_EXACT)));
  
  printf("div least : %s / %s = %s\n",
         gnc_numeric_print(a), gnc_numeric_print(b),
         gnc_numeric_print(gnc_numeric_div(a, b, GNC_DENOM_AUTO, 
                                           GNC_DENOM_REDUCE)));
  
  printf("div 100ths : %s / %s = %s\n",
         gnc_numeric_print(a), gnc_numeric_print(b),
         gnc_numeric_print(gnc_numeric_div(a, b, 100,
                                           GNC_RND_ROUND)));  
  
  c = gnc_numeric_div_with_error(a, b, 100, GNC_RND_ROUND, &err);
  printf("div 100ths/error : %s / %s = %s + (error) %s\n\n",
         gnc_numeric_print(a), gnc_numeric_print(b),
         gnc_numeric_print(c),
         gnc_numeric_print(err));
  
  printf("7/16 as float: %e\n",
         gnc_numeric_to_double(gnc_numeric_create(7, 16)));
  
  printf("7/16 as 100ths (floor): %s\n",
         gnc_numeric_print(gnc_numeric_convert(gnc_numeric_create(7, 16),
                                               100, GNC_RND_FLOOR)));
  printf("7/16 as 100ths (ceil): %s\n",
         gnc_numeric_print(gnc_numeric_convert(gnc_numeric_create(7, 16),
                                               100, GNC_RND_CEIL)));
  printf("7/16 as 100ths (trunc): %s\n",
         gnc_numeric_print(gnc_numeric_convert(gnc_numeric_create(7, 16),
                                               100, GNC_RND_TRUNC)));
  printf("7/16 as 100ths (round): %s\n",
         gnc_numeric_print(gnc_numeric_convert(gnc_numeric_create(7, 16),
                                               100, GNC_RND_ROUND)));

  printf("1511/1000 as 1/100 (round): %s\n",
         gnc_numeric_print(gnc_numeric_convert(gnc_numeric_create(1511, 1000),
                                               100, GNC_RND_ROUND)));
  printf("1516/1000 as 1/100 (round): %s\n",
         gnc_numeric_print(gnc_numeric_convert(gnc_numeric_create(1516, 1000),
                                               100, GNC_RND_ROUND)));
  printf("1515/1000 as 1/100 (round): %s\n",
         gnc_numeric_print(gnc_numeric_convert(gnc_numeric_create(1515, 1000),
                                               100, GNC_RND_ROUND)));
  printf("1525/1000 as 1/100 (round): %s\n",
         gnc_numeric_print(gnc_numeric_convert(gnc_numeric_create(1525, 1000),
                                               100, GNC_RND_ROUND)));

  printf("100023234 / 334216654 reduced: %s\n",
         gnc_numeric_print(gnc_numeric_reduce(gnc_numeric_create(10023234LL,
                                                                 334216654LL))));
  printf("2^10*3^10*17^2 / 2^8*3^12 reduced: %s\n",
         gnc_numeric_print
         (gnc_numeric_reduce(gnc_numeric_create(17474724864LL,
                                                136048896LL))));
  printf("1024 / 1024^4 reduced: %s\n",
         gnc_numeric_print
         (gnc_numeric_reduce(gnc_numeric_create(1024LL,
                                                1099511627776LL))));
  printf("reducing 100,000 times:\n\n");
  for(i = 0; i < 100000; i++) {
    gnc_numeric_reduce(gnc_numeric_create(17474724864LL,
                                          136048896LL));
  }
  
  printf("add LCM: %s + %s = %s\n",
         gnc_numeric_print(b), gnc_numeric_print(d),
         gnc_numeric_print(gnc_numeric_add(b, d, GNC_DENOM_AUTO,
                                           GNC_DENOM_LCD)));
 
  printf("float to 6 sigfigs: %s\n",
         gnc_numeric_print(double_to_gnc_numeric(1.1234567890123, 
                                                 GNC_DENOM_AUTO, 
                                                 GNC_DENOM_SIGFIGS(6) |
                                                 GNC_RND_ROUND)));
  printf("float to 6 sigfigs: %s\n",
         gnc_numeric_print(double_to_gnc_numeric(.011234567890123, 
                                                 GNC_DENOM_AUTO, 
                                                 GNC_DENOM_SIGFIGS(6) |
                                                 GNC_RND_ROUND)));
  printf("float to 6 sigfigs: %s\n",
         gnc_numeric_print(double_to_gnc_numeric(1123.4567890123, 
                                                 GNC_DENOM_AUTO, 
                                                 GNC_DENOM_SIGFIGS(6) |
                                                 GNC_RND_ROUND)));
  printf("float to 6 sigfigs: %s\n",
         gnc_numeric_print(double_to_gnc_numeric(1.1234567890123e-5, 
                                                 GNC_DENOM_AUTO, 
                                                 GNC_DENOM_SIGFIGS(6) |
                                                 GNC_RND_ROUND)));
  printf("add to 4 sigfigs: %s + %s = %s\n",
         gnc_numeric_print(a), gnc_numeric_print(b),
         gnc_numeric_print(gnc_numeric_add(a, b, 
                                           GNC_DENOM_AUTO, 
                                           GNC_DENOM_SIGFIGS(4) |
                                           GNC_RND_ROUND)));
  
   
  v = 1000000;
  a = gnc_numeric_create(1*v, v);
  b = gnc_numeric_create(10000000*v, v);
  printf("multiply (LCD): %s * %s = %s\n",
	 gnc_numeric_print(a), gnc_numeric_print(b),
	 gnc_numeric_print(gnc_numeric_mul(a, b, GNC_DENOM_AUTO, GNC_DENOM_LCD)));


  return 0;
}
#endif

static void
run_test (void)
{
  gnc_numeric a = gnc_numeric_create(1, 3);
  gnc_numeric b = gnc_numeric_create(1, 4);

  check_binary_op (gnc_numeric_create(9,12), 
                   gnc_numeric_add(a, b, GNC_DENOM_AUTO, GNC_DENOM_EXACT),
						 a, b, "expected %s got %s = %s + %s for add exact");
  
}

static void
main_helper (void *closure, int argc, char **argv)
{
  g_log_set_always_fatal( G_LOG_LEVEL_CRITICAL | G_LOG_LEVEL_WARNING );
  do_test((NULL!=gnc_module_load("gnucash/engine", 0)), "couldn't load engine");

  run_test ();

  print_test_results();
  exit(get_rv());
}

int
main (int argc, char **argv)
{
  scm_boot_guile(argc, argv, main_helper, NULL);
  return 0;
}
