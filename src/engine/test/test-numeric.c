
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

#define NREPS 2000

static char *
gnc_numeric_print(gnc_numeric in) 
{
  char * retval;
  if(gnc_numeric_check(in)) 
  {
    retval = g_strdup_printf("<ERROR> [%lld / %lld]",
                             (long long int) in.num,
                             (long long int) in.denom); 
  }
  else 
  {
    retval = g_strdup_printf("[%lld / %lld]",
                             (long long int) in.num,
                             (long long int) in.denom); 
  }
  return retval;
}

/* ======================================================= */

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

/* ======================================================= */

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

/* ======================================================= */

static gboolean
gnc_numeric_unequal (gnc_numeric a, gnc_numeric b)
{
	return (0 == gnc_numeric_equal (a,b));
}

/* ======================================================= */

/* Make sure that the equivalence operator we use for 
 * later tests actually works */
static void
check_eq_operator (void)
{
	gnc_numeric a = gnc_numeric_create (42, 58);
	gnc_numeric b = gnc_numeric_create (42, 58);
	gnc_numeric c = gnc_numeric_create (40, 58);
	
	/* Check strict equivalence and non-equivalence */
	do_test (gnc_numeric_eq(a, a), "expected self-equivalence");
	do_test (gnc_numeric_eq(a, b), "expected equivalence");
	do_test (0 == gnc_numeric_eq(a, c), "expected inequivalence");
}

/* ======================================================= */

static void
check_reduce (void)
{
	/* Check common factor elimination (needed for equality checks) */
	gnc_numeric one = gnc_numeric_create (1,1);
	gnc_numeric rone = gnc_numeric_create (1000000,1000000);
	rone = gnc_numeric_reduce (rone);
	do_test (gnc_numeric_eq(one, rone), "reduce to one");

	gnc_numeric four = gnc_numeric_create (4,1);
	gnc_numeric rfour = gnc_numeric_create (480,120);
	rfour = gnc_numeric_reduce (rfour);
	do_test (gnc_numeric_eq(four, rfour), "reduce to four");

	gnc_numeric val = gnc_numeric_create(10023234LL, 334216654LL);
	gnc_numeric rval = gnc_numeric_reduce (val);
	check_unary_op (gnc_numeric_eq,
	                gnc_numeric_create (5011617,167108327),
	                rval,
                   val, "expected %s = %s = reduce(%s)");

	val = gnc_numeric_create(17474724864LL,136048896LL);
	rval = gnc_numeric_reduce (val);
	check_unary_op (gnc_numeric_eq,
	                gnc_numeric_create (4*17*17,9),
	                rval,
                   val, "expected %s = %s = reduce(%s)");

	val = gnc_numeric_create(1024LL,1099511627776LL);
	rval = gnc_numeric_reduce (val);
	check_unary_op (gnc_numeric_eq,
	                gnc_numeric_create (1,1024*1024*1024),
	                rval,
                   val, "expected %s = %s = reduce(%s)");
}

/* ======================================================= */

static void
check_equality_operator (void)
{
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
		mval.denom >>= 1;
		mval.num >>= 1;
		int m=0;
		gint64 f = mval.denom;
		while (f%2 == 0)
		{
			f >>= 1;
			m++;
		}
		if (1 < m)
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
	
/* ======================================================= */

static void 
check_rounding (void)
{
	gnc_numeric val;

	val = gnc_numeric_create(7, 16);
	check_unary_op (gnc_numeric_eq,
	                gnc_numeric_create (43,100),
	                gnc_numeric_convert (val, 100, GNC_HOW_RND_FLOOR),
                   val, "expected %s = %s = (%s as 100th's floor)");
	check_unary_op (gnc_numeric_eq,
	                gnc_numeric_create (44,100),
	                gnc_numeric_convert (val, 100, GNC_HOW_RND_CEIL),
                   val, "expected %s = %s = (%s as 100th's ceiling)");
	check_unary_op (gnc_numeric_eq,
	                gnc_numeric_create (43,100),
	                gnc_numeric_convert (val, 100, GNC_HOW_RND_TRUNC),
                   val, "expected %s = %s = (%s as 100th's trunc)");
	check_unary_op (gnc_numeric_eq,
	                gnc_numeric_create (44,100),
	                gnc_numeric_convert (val, 100, GNC_HOW_RND_ROUND),
                   val, "expected %s = %s = (%s as 100th's round)");

	val = gnc_numeric_create(1511, 1000);
	check_unary_op (gnc_numeric_eq,
	                gnc_numeric_create (151,100),
	                gnc_numeric_convert (val, 100, GNC_HOW_RND_ROUND),
                   val, "expected %s = %s = (%s as 100th's round)");

	val = gnc_numeric_create(1516, 1000);
	check_unary_op (gnc_numeric_eq,
	                gnc_numeric_create (152,100),
	                gnc_numeric_convert (val, 100, GNC_HOW_RND_ROUND),
                   val, "expected %s = %s = (%s as 100th's round)");

	/* Half-values always get rounded to nearest even number */
	val = gnc_numeric_create(1515, 1000);
	check_unary_op (gnc_numeric_eq,
	                gnc_numeric_create (152,100),
	                gnc_numeric_convert (val, 100, GNC_HOW_RND_ROUND),
                   val, "expected %s = %s = (%s as 100th's round)");

	val = gnc_numeric_create(1525, 1000);
	check_unary_op (gnc_numeric_eq,
	                gnc_numeric_create (152,100),
	                gnc_numeric_convert (val, 100, GNC_HOW_RND_ROUND),
                   val, "expected %s = %s = (%s as 100th's round)");

	val = gnc_numeric_create(1535, 1000);
	check_unary_op (gnc_numeric_eq,
	                gnc_numeric_create (154,100),
	                gnc_numeric_convert (val, 100, GNC_HOW_RND_ROUND),
                   val, "expected %s = %s = (%s as 100th's round)");

	val = gnc_numeric_create(1545, 1000);
	check_unary_op (gnc_numeric_eq,
	                gnc_numeric_create (154,100),
	                gnc_numeric_convert (val, 100, GNC_HOW_RND_ROUND),
                   val, "expected %s = %s = (%s as 100th's round)");
}

/* ======================================================= */

static void
check_double (void)
{
	gnc_numeric val = gnc_numeric_create (0,1);

	check_unary_op (gnc_numeric_eq,
	                gnc_numeric_create (112346,100000),
                   double_to_gnc_numeric(1.1234567890123, 
                                         GNC_DENOM_AUTO, 
                                         GNC_HOW_DENOM_SIGFIGS(6) |
                                         GNC_HOW_RND_ROUND),
                   val, "expected %s = %s double 6 figs");

	check_unary_op (gnc_numeric_eq,
	                gnc_numeric_create (112346,10000000),
                   double_to_gnc_numeric(0.011234567890123, 
                                         GNC_DENOM_AUTO, 
                                         GNC_HOW_DENOM_SIGFIGS(6) |
                                         GNC_HOW_RND_ROUND),
                   val, "expected %s = %s double 6 figs");

	check_unary_op (gnc_numeric_eq,
	                gnc_numeric_create (112346,100),
                   double_to_gnc_numeric(1123.4567890123, 
                                         GNC_DENOM_AUTO, 
                                         GNC_HOW_DENOM_SIGFIGS(6) |
                                         GNC_HOW_RND_ROUND),
                   val, "expected %s = %s double 6 figs");
	check_unary_op (gnc_numeric_eq,
	                gnc_numeric_create (112346,10000000000LL),
                   double_to_gnc_numeric(1.1234567890123e-5, 
                                         GNC_DENOM_AUTO, 
                                         GNC_HOW_DENOM_SIGFIGS(6) |
                                         GNC_HOW_RND_ROUND),
                   val, "expected %s = %s double 6 figs");

	double flo = gnc_numeric_to_double(gnc_numeric_create(7, 16));
	do_test ((0.4375 == flo), "float pt conversion");
}

/* ======================================================= */

static void
check_neg (void)
{
	gnc_numeric a = gnc_numeric_create(2, 6);
	gnc_numeric b = gnc_numeric_create(1, 4);
	gnc_numeric c = gnc_numeric_neg (a);
	gnc_numeric d = gnc_numeric_neg (b);

	check_unary_op (gnc_numeric_eq,
	                gnc_numeric_create (-2,6), c, 
                   a, "expected %s = %s = -(%s)");

	check_unary_op (gnc_numeric_eq,
	                gnc_numeric_create (-1,4), d, 
                   b, "expected %s = %s = -(%s)");

}

/* ======================================================= */

static void
check_add_subtract (void)
{
  gnc_numeric a = gnc_numeric_create(2, 6);
  gnc_numeric b = gnc_numeric_create(1, 4);

  /* Well, actually 14/24 would be acceptable/better in this case */
  check_binary_op (gnc_numeric_create(7,12), 
                   gnc_numeric_add(a, b, GNC_DENOM_AUTO, GNC_HOW_DENOM_EXACT),
						 a, b, "expected %s got %s = %s + %s for add exact");
  
  check_binary_op (gnc_numeric_create(58,100), 
                   gnc_numeric_add(a, b, 100, GNC_HOW_RND_ROUND),
						 a, b, "expected %s got %s = %s + %s for add 100ths (banker's)");
  
  check_binary_op (gnc_numeric_create(5833,10000), 
                   gnc_numeric_add(a, b, GNC_DENOM_AUTO, 
                                         GNC_HOW_DENOM_SIGFIGS(4) |
                                         GNC_HOW_RND_ROUND),
						 a, b, "expected %s got %s = %s + %s for add 4 sig figs");
  
  check_binary_op (gnc_numeric_create(583333,1000000), 
                   gnc_numeric_add(a, b, GNC_DENOM_AUTO, 
                                         GNC_HOW_DENOM_SIGFIGS(6) |
                                         GNC_HOW_RND_ROUND),
						 a, b, "expected %s got %s = %s + %s for add 6 sig figs");
  
  check_binary_op (gnc_numeric_create(1,12), 
                   gnc_numeric_sub(a, b, GNC_DENOM_AUTO, GNC_HOW_DENOM_EXACT),
						 a, b, "expected %s got %s = %s - %s for sub exact");
  
  /* We should try something trickier for reduce & lcd */
  check_binary_op (gnc_numeric_create(1,12), 
                   gnc_numeric_sub(a, b, GNC_DENOM_AUTO, GNC_HOW_DENOM_REDUCE),
						 a, b, "expected %s got %s = %s - %s for sub reduce");
  
  check_binary_op (gnc_numeric_create(1,12), 
                   gnc_numeric_sub(a, b, GNC_DENOM_AUTO, GNC_HOW_DENOM_LCD),
						 a, b, "expected %s got %s = %s - %s for sub reduce");
  
  check_binary_op (gnc_numeric_create(8,100), 
                   gnc_numeric_sub(a, b, 100, GNC_HOW_RND_ROUND),
						 a, b, "expected %s got %s = %s - %s for sub 100ths (banker's)");
  
  /* This test has failed before */
  gnc_numeric c = gnc_numeric_neg (b);
  gnc_numeric z = gnc_numeric_zero();
  check_binary_op (c, gnc_numeric_add_fixed(z,c),
						 z, c, "expected %s got %s = %s + %s for add fixed");
  
#if CHECK_ERRORS_TOO
  gnc_numeric c;
  c = gnc_numeric_add_with_error(a, b, 100, GNC_HOW_RND_ROUND, &err);
  printf("add 100ths/error : %s + %s = %s + (error) %s\n\n",
         gnc_numeric_print(a), gnc_numeric_print(b),
         gnc_numeric_print(c),
         gnc_numeric_print(err));
  
  c = gnc_numeric_sub_with_error(a, b, 100, GNC_HOW_RND_FLOOR, &err);
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
                   gnc_numeric_add(a, b, GNC_DENOM_AUTO, GNC_HOW_DENOM_EXACT),
						 a, b, "expected %s got %s = %s + %s for exact addition");

		/* Subtract */
		ne = na-nb;
		e = gnc_numeric_create(ne, deno);
		check_binary_op (e,
                   gnc_numeric_sub(a, b, GNC_DENOM_AUTO, GNC_HOW_DENOM_EXACT),
						 a, b, "expected %s got %s = %s - %s for exact subtraction");
	}
}

/* ======================================================= */

static void
check_mult_div (void)
{
  gnc_numeric c, d;
  gnc_numeric a = gnc_numeric_create(2, 6);
  gnc_numeric b = gnc_numeric_create(1, 4);

  check_binary_op (gnc_numeric_create(2,24), 
                   gnc_numeric_mul(a, b, GNC_DENOM_AUTO, GNC_HOW_DENOM_EXACT),
						 a, b, "expected %s got %s = %s * %s for mult exact");

  check_binary_op (gnc_numeric_create(1,12), 
                   gnc_numeric_mul(a, b, GNC_DENOM_AUTO, GNC_HOW_DENOM_REDUCE),
						 a, b, "expected %s got %s = %s * %s for mult reduce");

  check_binary_op (gnc_numeric_create(8,100), 
                   gnc_numeric_mul(a, b, 100, GNC_HOW_RND_ROUND),
						 a, b, "expected %s got %s = %s * %s for mult 100th's");

  check_binary_op (gnc_numeric_create(8,6), 
                   gnc_numeric_div(a, b, GNC_DENOM_AUTO, GNC_HOW_DENOM_EXACT),
						 a, b, "expected %s got %s = %s / %s for div exact");

  check_binary_op (gnc_numeric_create(4,3), 
                   gnc_numeric_div(a, b, GNC_DENOM_AUTO, GNC_HOW_DENOM_REDUCE),
						 a, b, "expected %s got %s = %s / %s for div reduce");

  check_binary_op (gnc_numeric_create(133,100), 
                   gnc_numeric_div(a, b, 100, GNC_HOW_RND_ROUND),
						 a, b, "expected %s got %s = %s * %s for div 100th's");

#if CHECK_ERRORS_TOO
  gnc_numeric c;
  c = gnc_numeric_mul_with_error(a, b, 100, GNC_HOW_RND_ROUND, &err);
  printf("mul 100ths/error : %s * %s = %s + (error) %s\n\n",
         gnc_numeric_print(a), gnc_numeric_print(b),
         gnc_numeric_print(c),
         gnc_numeric_print(err));

  c = gnc_numeric_div_with_error(a, b, 100, GNC_HOW_RND_ROUND, &err);
  printf("div 100ths/error : %s / %s = %s + (error) %s\n\n",
         gnc_numeric_print(a), gnc_numeric_print(b),
         gnc_numeric_print(c),
         gnc_numeric_print(err));
  
#endif
  
  /* Check for math with 2^63 < num*num < 2^64 which previously failed 
   * see http://bugzilla.gnome.org/show_bug.cgi?id=144980 
   */
  gint64 v = 1000000;
  a = gnc_numeric_create(1*v, v);
  b = gnc_numeric_create(10000000*v, v);

  check_binary_op (b,
	                gnc_numeric_mul(a, b, GNC_DENOM_AUTO, GNC_HOW_DENOM_LCD),
						 a, b, "expected %s got %s = %s * %s for multiply");

	/* Multiply some random numbers.  This test presumes that
	 * RAND_MAX is approx 2^32 
	 */
	int i;
	for (i=0; i<NREPS; i++)
	{
		gint64 deno = 1;
		gint64 na = rand();
		gint64 nb = rand();
		gint64 ne;

		/* avoid overflow; */
		na /= 2;
		nb /= 2;
		ne = na*nb;
		
		a = gnc_numeric_create(na, deno);
		b = gnc_numeric_create(nb, deno);

		check_binary_op (gnc_numeric_create(ne,1), 
			          gnc_numeric_mul(a, b, GNC_DENOM_AUTO, GNC_HOW_DENOM_EXACT),
						 a, b, "expected %s got %s = %s * %s for mult exact");

		/* Force 128-bit math to come into play */
		int j;
		for (j=1; j<31; j++)
		{
			a = gnc_numeric_create(na << j, 1<<j);
			b = gnc_numeric_create(nb << j, 1<<j);
			check_binary_op (gnc_numeric_create(ne, 1), 
			          gnc_numeric_mul(a, b, GNC_DENOM_AUTO, GNC_HOW_DENOM_REDUCE),
						 a, b, "expected %s got %s = %s * %s for mult reduce");
		}

		/* Do some hokey random 128-bit division too */
		b = gnc_numeric_create(deno, nb);

		check_binary_op (gnc_numeric_create(ne,1), 
			          gnc_numeric_div(a, b, GNC_DENOM_AUTO, GNC_HOW_DENOM_EXACT),
						 a, b, "expected %s got %s = %s / %s for div exact");

		/* avoid overflow; */
		na /= 2;
		nb /= 2;
		ne = na*nb;
		for (j=1; j<16; j++)
		{
			a = gnc_numeric_create(na << j, 1<<j);
			b = gnc_numeric_create(1<<j, nb << j);
			check_binary_op (gnc_numeric_create(ne, 1), 
			          gnc_numeric_div(a, b, GNC_DENOM_AUTO, GNC_HOW_DENOM_REDUCE),
						 a, b, "expected %s got %s = %s / %s for div reduce");
		}
	}

	a = gnc_numeric_create(782592055622866ULL,89025);
	b = gnc_numeric_create(2222554708930978ULL,85568);
	/* Dividing the above pair overflows, in that after
	 * the division the denominator won't fit into a 
	 * 64-bit quantity.  This can be seen from
	 * the factorization int primes:
	 * 782592055622866 = 2 * 2283317 * 171371749
	 * (yes, thats a seven and a nine digit prime)
	 * 2222554708930978 = 2 * 1111277354465489
	 * (yes, that's a sixteen-digit prime number)
	 * 89025 = 3*5*5*1187 
	 * 85568= 64*7*191   
	 * If the rounding method is exact/no-round, then 
	 * an overflow error should be signalled; else the 
	 * divide routine should shift down the results till
	 * the overflow is eliminated.
	 * 
	 */
	check_binary_op (gnc_numeric_error (GNC_ERROR_OVERFLOW),
			 gnc_numeric_div(a, b, GNC_DENOM_AUTO,
					 GNC_HOW_DENOM_EXACT),
			 a, b, "expected %s got %s = %s / %s for div exact");

	check_binary_op (gnc_numeric_create(338441, 1000000),
			 gnc_numeric_div(a, b, GNC_DENOM_AUTO,
					 GNC_HOW_DENOM_SIGFIGS(6) | GNC_HOW_RND_ROUND),
			 a, b, "expected %s got %s = %s / %s for div round");

	/* The below is a 'typical' value calculation: 
	 * value_frac = value_tot * amt_frace / amt_tot
	 * and has some typical potential-overflow values. 
	 * 82718 = 2 * 59 * 701
	 * 47497125586 = 2 * 1489 * 15949337
	 * 69100955 = 5 * 7 * 11 * 179483
	 * 32005637020 = 4 * 5 * 7 * 43 * 71 * 103 * 727
	 */
	a = gnc_numeric_create (-47497125586LL, 82718);
	b = gnc_numeric_create (-69100955LL, 55739);
	c = gnc_numeric_mul (a,b, GNC_DENOM_AUTO, GNC_HOW_DENOM_EXACT);
	d = gnc_numeric_create (-32005637020LL, 55739);

	check_binary_op (gnc_numeric_create(-102547458LL, 82718),
			 gnc_numeric_div(c, d, 82718,
					 GNC_HOW_DENOM_EXACT),
			 c, d, "expected %s got %s = %s / %s for div round");

	/* If we specify GNC_HOW_RND_NEVER, then we shoukld get an error,
	 * since the exact result won't fit into a 64-bit quantity. */
	check_binary_op (gnc_numeric_error (GNC_ERROR_REMAINDER),
			 gnc_numeric_div(c, d, 82718,
					 GNC_HOW_DENOM_EXACT|GNC_HOW_RND_NEVER),
			 c, d, "expected %s got %s = %s / %s for div round");
}
  
/* ======================================================= */

static void
run_test (void)
{
	check_eq_operator ();
	check_reduce ();
	check_equality_operator ();
	check_rounding();
	check_double();
	check_neg();
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

/* ======================== END OF FILE ====================== */
