
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

#define NREPS 3000


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
check_unary_op (gboolean (*eqtest) (gnc_numeric, gnc_numeric), 
                gnc_numeric expected, 
                gnc_numeric actual, 
                gnc_numeric input, 
                const char * errmsg)
{
	char *e = gnc_numeric_print (expected);
	char *r = gnc_numeric_print (actual);
	char *a = gnc_numeric_print (input);
	char *str = g_strdup_printf (errmsg, e,r, a);
	
	do_test (eqtest(expected, actual), str);
	
	g_free (a);
	g_free (r);
	g_free (e);
	g_free (str);
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
	char *str = g_strdup_printf (errmsg, e,r,a,b);
	
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
  gnc_numeric d = gnc_numeric_create(1, 2);
  
  gnc_numeric err;
  int i;
  gint64 v;

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

static gboolean
gnc_numeric_unequal (gnc_numeric a, gnc_numeric b)
{
	return (0 == gnc_numeric_equal (a,b));
}

/* Make sure that the equivalence operator we use for 
 * later tests actually works */
static void
check_equality_operator (void)
{
	gnc_numeric a = gnc_numeric_create (42, 58);
	gnc_numeric b = gnc_numeric_create (42, 58);
	gnc_numeric c = gnc_numeric_create (40, 58);
	
	/* Check strict equivalence and non-equivalence */
	do_test (gnc_numeric_eq(a, a), "expected self-equivalence");
	do_test (gnc_numeric_eq(a, b), "expected equivalence");
	do_test (0 == gnc_numeric_eq(a, c), "expected inequivalence");


	/* Check common factor elimination (needed for equality checks) */
	gnc_numeric one = gnc_numeric_create (1,1);
	gnc_numeric rone = gnc_numeric_create (1000000,1000000);
	rone = gnc_numeric_reduce (rone);
	do_test (gnc_numeric_eq(one, rone), "reduce to one");

	gnc_numeric four = gnc_numeric_create (4,1);
	gnc_numeric rfour = gnc_numeric_create (480,120);
	rfour = gnc_numeric_reduce (rfour);
	do_test (gnc_numeric_eq(four, rfour), "reduce to four");

	/* Check equality operator for some large numer/denom values */
	gint64 numer = 1<<30;
	numer <<= 30;   /* we don't trust cpp to compute 1<<60 correctly */
	gint64 deno = 1<<30;
	deno <<= 20;
	gnc_numeric rbig = gnc_numeric_create (numer, deno);
	
	gnc_numeric big = gnc_numeric_create (1<<10,1);
	do_test (gnc_numeric_equal(big, rbig), "equal to billion");
	
	big = gnc_numeric_create (1<<20,1<<10);
	do_test (gnc_numeric_equal(big, rbig), "equal to 1<<20/1<<10");

	big = gnc_numeric_create (1<<30,1<<20);
	do_test (gnc_numeric_equal(big, rbig), "equal to 1<<30/1<<20");

	numer = 1<<30;
	numer <<= 30;   /* we don't trust cpp to compute 1<<60 correctly */
	deno = 1<<30;
	rbig = gnc_numeric_create (numer, deno);
	
	big = gnc_numeric_create (1<<30,1);
	do_test (gnc_numeric_equal(big, rbig), "equal to 1<<30");

	numer = 1<<30;
	numer <<= 10;
	big = gnc_numeric_create (numer, 1<<10);
	do_test (gnc_numeric_equal(big, rbig), "equal to 1<<40/1<<10");
	
	numer <<= 10;
	big = gnc_numeric_create (numer, 1<<20);
	do_test (gnc_numeric_equal(big, rbig), "equal to 1<<50/1<<20");

	int i;
	/* We assume RAND_MAX is less that 1<<32 */
	for (i=0; i<NREPS; i++) 
	{
		gint64 deno = rand() / 2;
		gint64 mult = rand() / 2;
		gint64 numer = rand() / 2;

		gnc_numeric val = gnc_numeric_create (numer, deno);
		gnc_numeric mval = gnc_numeric_create (numer*mult, deno*mult);
		
		/* The reduced version should be equivalent */
		gnc_numeric bval = gnc_numeric_reduce (val);
		gnc_numeric rval = gnc_numeric_reduce (mval);
		check_unary_op (gnc_numeric_eq, 
                      bval, rval, mval, "expected %s = %s = reduce(%s)");
		
		/* The unreduced versions should be equal */
		check_unary_op (gnc_numeric_equal, 
                      val, mval, mval, "expected %s = %s");
		
      /* Certain modulo's should be very cleary un-equal; this
		 * helps stop funky modulo-64 aliasing in compares that 
		 * might creep in. */
		int m=0;
		gint64 f = mval.denom;
		while (f%2 == 0)
		{
			f >>= 1;
			m++;
		}
		if (m)
		{
			gint64 nn = 1 << (32-m);
			nn <<= 32;
			nn += mval.num;
			val = gnc_numeric_create (2*nn, 2*mval.denom);
			check_unary_op (gnc_numeric_unequal, 
                      val, mval, mval, "expected unequality %s != %s");
		
		}
	}
}
	
static void
check_add_subtract (void)
{
  gnc_numeric a = gnc_numeric_create(1, 3);
  gnc_numeric b = gnc_numeric_create(1, 4);

  check_binary_op (gnc_numeric_create(7,12), 
                   gnc_numeric_add(a, b, GNC_DENOM_AUTO, GNC_DENOM_EXACT),
						 a, b, "expected %s got %s = %s + %s for add exact");
  
  check_binary_op (gnc_numeric_create(58,100), 
                   gnc_numeric_add(a, b, 100, GNC_RND_ROUND),
						 a, b, "expected %s got %s = %s + %s for add 100ths (banker's)");
  
  check_binary_op (gnc_numeric_create(1,12), 
                   gnc_numeric_sub(a, b, GNC_DENOM_AUTO, GNC_DENOM_EXACT),
						 a, b, "expected %s got %s = %s - %s for sub exact");
  
  check_binary_op (gnc_numeric_create(1,12), 
                   gnc_numeric_sub(a, b, GNC_DENOM_AUTO, GNC_DENOM_REDUCE),
						 a, b, "expected %s got %s = %s - %s for sub least");
  
  check_binary_op (gnc_numeric_create(8,100), 
                   gnc_numeric_sub(a, b, 100, GNC_RND_ROUND),
						 a, b, "expected %s got %s = %s - %s for add 100ths (banker's)");
  
#if CHECK_ERRORS_TOO
  gnc_numeric c;
  c = gnc_numeric_add_with_error(a, b, 100, GNC_RND_ROUND, &err);
  printf("add 100ths/error : %s + %s = %s + (error) %s\n\n",
         gnc_numeric_print(a), gnc_numeric_print(b),
         gnc_numeric_print(c),
         gnc_numeric_print(err));
  
  c = gnc_numeric_sub_with_error(a, b, 100, GNC_RND_FLOOR, &err);
  printf("sub 100ths/error : %s - %s = %s + (error) %s\n\n",
         gnc_numeric_print(a), gnc_numeric_print(b),
         gnc_numeric_print(c),
         gnc_numeric_print(err));
  
#endif

	/* Add and subtract some random numbers */
	int i;
	for (i=0; i<NREPS; i++)
	{
		gnc_numeric e;
		gint64 deno = rand() +1;
		gint64 na = get_random_gint64();
		gint64 nb = get_random_gint64();
		gint64 ne;

		/* avoid overflow; */
		na /=2;
		nb /=2;
		
		a = gnc_numeric_create(na, deno);
		b = gnc_numeric_create(nb, deno);

		/* Add */
		ne = na+nb;
		e = gnc_numeric_create(ne, deno);
		check_binary_op (e,
                   gnc_numeric_add(a, b, GNC_DENOM_AUTO, GNC_DENOM_EXACT),
						 a, b, "expected %s got %s = %s + %s for exact addition");

		/* Subtract */
		ne = na-nb;
		e = gnc_numeric_create(ne, deno);
		check_binary_op (e,
                   gnc_numeric_sub(a, b, GNC_DENOM_AUTO, GNC_DENOM_EXACT),
						 a, b, "expected %s got %s = %s - %s for exact subtraction");
	}
}

static void
check_mult_div (void)
{
  gnc_numeric a = gnc_numeric_create(1, 3);
  gnc_numeric b = gnc_numeric_create(1, 4);

  check_binary_op (gnc_numeric_create(1,12), 
                   gnc_numeric_mul(a, b, GNC_DENOM_AUTO, GNC_DENOM_EXACT),
						 a, b, "expected %s got %s = %s * %s for mult exact");

  check_binary_op (gnc_numeric_create(1,12), 
                   gnc_numeric_mul(a, b, GNC_DENOM_AUTO, GNC_DENOM_REDUCE),
						 a, b, "expected %s got %s = %s * %s for mult reduce");

}
  

static void
run_test (void)
{
	check_equality_operator ();
	check_add_subtract();
	check_mult_div ();
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
