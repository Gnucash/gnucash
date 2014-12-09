
/* Test file created by Linas Vepstas <linas@linas.org>
 * Review operation of the gnc-numeric tools by verifying results
 * of vairous operations.
 *
 * June 2004
 */
/*
 *  This program is free software; you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License as published by
 *  the Free Software Foundation; either version 2 of the License, or
 *  (at your option) any later version.
 *
 *  This program is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU General Public License for more details.
 *
 *  You should have received a copy of the GNU General Public License
 *  along with this program; if not, write to the Free Software
 *  Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
 *  02110-1301, USA.
 */


#include "config.h"
#include <ctype.h>
#include <glib.h>
#include "cashobjects.h"
#include "test-stuff.h"
#include "test-engine-stuff.h"
#include "gnc-numeric.h"

#define NREPS 2000

static char *
gnc_numeric_print(gnc_numeric in)
{
    char * retval;
    if (gnc_numeric_check(in))
    {
        retval = g_strdup_printf("<ERROR> [%" G_GINT64_FORMAT " / %" G_GINT64_FORMAT "]",
                                 in.num,
                                 in.denom);
    }
    else
    {
        retval = g_strdup_printf("[%" G_GINT64_FORMAT " / %" G_GINT64_FORMAT "]",
                                 in.num,
                                 in.denom);
    }
    return retval;
}

/* ======================================================= */

#define check_unary_op(eq,ex,a,i,e) check_unary_op_r(eq,ex,a,i,e,__LINE__)
static void
check_unary_op_r (gboolean (*eqtest) (gnc_numeric, gnc_numeric),
                  gnc_numeric expected,
                  gnc_numeric actual,
                  gnc_numeric input,
                  const char * errmsg,
                  int line)
{
    char *e = gnc_numeric_print (expected);
    char *r = gnc_numeric_print (actual);
    char *a = gnc_numeric_print (input);
    char *str = g_strdup_printf (errmsg, e, r, a);

    do_test_call (eqtest(expected, actual), str, __FILE__, line);

    g_free (a);
    g_free (r);
    g_free (e);
    g_free (str);
}

/* ======================================================= */

#define check_binary_op(ex,a,ia,ib,e) check_binary_op_r(ex,a,ia,ib,e,__LINE__,gnc_numeric_eq)
#define check_binary_op_equal(ex,a,ia,ib,e) check_binary_op_r(ex,a,ia,ib,e,__LINE__,gnc_numeric_equal)
static void
check_binary_op_r (gnc_numeric expected,
                   gnc_numeric actual,
                   gnc_numeric input_a,
                   gnc_numeric input_b,
                   const char * errmsg,
                   int line,
                   gboolean (*eq)(gnc_numeric, gnc_numeric))
{
    char *e = gnc_numeric_print (expected);
    char *r = gnc_numeric_print (actual);
    char *a = gnc_numeric_print (input_a);
    char *b = gnc_numeric_print (input_b);
    char *str = g_strdup_printf (errmsg, e, r, a, b);

    do_test_call ((eq)(expected, actual), str, __FILE__, line);

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
    return (0 == gnc_numeric_equal (a, b));
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
    gnc_numeric one, rone;
    gnc_numeric four, rfour;
    gnc_numeric val, rval;
    /* Check common factor elimination (needed for equality checks) */
    one = gnc_numeric_create (1, 1);
    rone = gnc_numeric_create (1000000, 1000000);
    rone = gnc_numeric_reduce (rone);
    do_test (gnc_numeric_eq(one, rone), "reduce to one");

    four = gnc_numeric_create (4, 1);
    rfour = gnc_numeric_create (480, 120);
    rfour = gnc_numeric_reduce (rfour);
    do_test (gnc_numeric_eq(four, rfour), "reduce to four");

    val = gnc_numeric_create(10023234LL, 334216654LL);
    rval = gnc_numeric_reduce (val);
    check_unary_op (gnc_numeric_eq,
                    gnc_numeric_create (5011617, 167108327),
                    rval,
                    val, "check_reduce(1) expected %s got %s = reduce(%s)");

    val = gnc_numeric_create(17474724864LL, 136048896LL);
    rval = gnc_numeric_reduce (val);
    check_unary_op (gnc_numeric_eq,
                    gnc_numeric_create (4 * 17 * 17, 9),
                    rval,
                    val, "check_reduce(2) expected %s got %s = reduce(%s)");

    val = gnc_numeric_create(1024LL, 1099511627776LL);
    rval = gnc_numeric_reduce (val);
    check_unary_op (gnc_numeric_eq,
                    gnc_numeric_create (1, 1024 * 1024 * 1024),
                    rval,
                    val, "check_reduce(3): expected %s got %s = reduce(%s)");
}

/* ======================================================= */

static void
check_equality_operator (void)
{
    int i, m;
    gint mult;
    gint64 f, deno, numer;
    gnc_numeric big, rbig;
    gnc_numeric val, mval;
    gnc_numeric bval, rval;
    /* Check equality operator for some large numer/denom values */
    numer = 1 << 30;
    numer <<= 30;   /* we don't trust cpp to compute 1<<60 correctly */
    deno = 1 << 30;
    deno <<= 20;
    rbig = gnc_numeric_create (numer, deno);

    big = gnc_numeric_create (1 << 10, 1);
    do_test (gnc_numeric_equal(big, rbig), "equal to billion");

    big = gnc_numeric_create (1 << 20, 1 << 10);
    do_test (gnc_numeric_equal(big, rbig), "equal to 1<<20/1<<10");

    big = gnc_numeric_create (1 << 30, 1 << 20);
    do_test (gnc_numeric_equal(big, rbig), "equal to 1<<30/1<<20");

    numer = 1 << 30;
    numer <<= 30;   /* we don't trust cpp to compute 1<<60 correctly */
    deno = 1 << 30;
    rbig = gnc_numeric_create (numer, deno);

    big = gnc_numeric_create (1 << 30, 1);
    do_test (gnc_numeric_equal(big, rbig), "equal to 1<<30");

    numer = 1 << 30;
    numer <<= 10;
    big = gnc_numeric_create (numer, 1 << 10);
    do_test (gnc_numeric_equal(big, rbig), "equal to 1<<40/1<<10");

    numer <<= 10;
    big = gnc_numeric_create (numer, 1 << 20);
    do_test (gnc_numeric_equal(big, rbig), "equal to 1<<50/1<<20");

    /* We assume RAND_MAX is less that 1<<32 */
    for (i = 0; i < NREPS; i++)
    {
        deno = rand() / 2;
        mult = rand() / 2;
        numer = rand() / 2;

        /* avoid 0 */
        if (deno == 0 || mult == 0)
        {
            i--;
            continue;
        }

        val = gnc_numeric_create (numer, deno);
        mval = gnc_numeric_create (numer * mult, deno * mult);

        /* The reduced version should be equivalent */
        bval = gnc_numeric_reduce (val);
        rval = gnc_numeric_reduce (mval);
        check_unary_op (gnc_numeric_eq,
                        bval, rval, mval, "expected %s got %s = reduce(%s)");

        /* The unreduced versions should be equal */
        check_unary_op (gnc_numeric_equal,
                        val, mval, mval, "expected %s = %s");

        /* Certain modulo's should be very cleary un-equal; this
         * helps stop funky modulo-64 aliasing in compares that
         * might creep in. */
        mval.denom >>= 1;
        mval.num >>= 1;
        m = 0;
        f = mval.denom;
        while (f % 2 == 0)
        {
            f >>= 1;
            m++;
        }
        if (1 < m)
        {
            gint64 nn = 1 << (32 - m);
            nn <<= 32;
            nn += mval.num;
            val = gnc_numeric_create (2 * nn, 2 * mval.denom);
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
                    gnc_numeric_create (43, 100),
                    gnc_numeric_convert (val, 100, GNC_HOW_RND_FLOOR),
                    val, "expected %s got %s = (%s as 100th's floor)");
    check_unary_op (gnc_numeric_eq,
                    gnc_numeric_create (44, 100),
                    gnc_numeric_convert (val, 100, GNC_HOW_RND_CEIL),
                    val, "expected %s got %s = (%s as 100th's ceiling)");
    check_unary_op (gnc_numeric_eq,
                    gnc_numeric_create (43, 100),
                    gnc_numeric_convert (val, 100, GNC_HOW_RND_TRUNC),
                    val, "expected %s got %s = (%s as 100th's trunc)");
    check_unary_op (gnc_numeric_eq,
                    gnc_numeric_create (44, 100),
                    gnc_numeric_convert (val, 100, GNC_HOW_RND_ROUND),
                    val, "expected %s got %s = (%s as 100th's round)");

    val = gnc_numeric_create(1511, 1000);
    check_unary_op (gnc_numeric_eq,
                    gnc_numeric_create (151, 100),
                    gnc_numeric_convert (val, 100, GNC_HOW_RND_ROUND),
                    val, "expected %s got %s = (%s as 100th's round)");

    val = gnc_numeric_create(1516, 1000);
    check_unary_op (gnc_numeric_eq,
                    gnc_numeric_create (152, 100),
                    gnc_numeric_convert (val, 100, GNC_HOW_RND_ROUND),
                    val, "expected %s got %s = (%s as 100th's round)");

    /* Half-values always get rounded to nearest even number */
    val = gnc_numeric_create(1515, 1000);
    check_unary_op (gnc_numeric_eq,
                    gnc_numeric_create (152, 100),
                    gnc_numeric_convert (val, 100, GNC_HOW_RND_ROUND),
                    val, "expected %s got %s = (%s as 100th's round)");

    val = gnc_numeric_create(1525, 1000);
    check_unary_op (gnc_numeric_eq,
                    gnc_numeric_create (152, 100),
                    gnc_numeric_convert (val, 100, GNC_HOW_RND_ROUND),
                    val, "expected %s got %s = (%s as 100th's round)");

    val = gnc_numeric_create(1535, 1000);
    check_unary_op (gnc_numeric_eq,
                    gnc_numeric_create (154, 100),
                    gnc_numeric_convert (val, 100, GNC_HOW_RND_ROUND),
                    val, "expected %s got %s = (%s as 100th's round)");

    val = gnc_numeric_create(1545, 1000);
    check_unary_op (gnc_numeric_eq,
                    gnc_numeric_create (154, 100),
                    gnc_numeric_convert (val, 100, GNC_HOW_RND_ROUND),
                    val, "expected %s got %s = (%s as 100th's round)");
}

/* ======================================================= */

static void
check_double (void)
{
    double flo;
    gnc_numeric val = gnc_numeric_create (0, 1);

    check_unary_op (gnc_numeric_eq,
                    gnc_numeric_create (112346, 100000),
                    double_to_gnc_numeric(1.1234567890123,
                                          GNC_DENOM_AUTO,
                                          GNC_HOW_DENOM_SIGFIGS(6) |
                                          GNC_HOW_RND_ROUND),
                    val, "expected %s = %s double 6 figs");

    check_unary_op (gnc_numeric_eq,
                    gnc_numeric_create (112346, 10000000),
                    double_to_gnc_numeric(0.011234567890123,
                                          GNC_DENOM_AUTO,
                                          GNC_HOW_DENOM_SIGFIGS(6) |
                                          GNC_HOW_RND_ROUND),
                    val, "expected %s = %s double 6 figs");

    check_unary_op (gnc_numeric_eq,
                    gnc_numeric_create (112346, 100),
                    double_to_gnc_numeric(1123.4567890123,
                                          GNC_DENOM_AUTO,
                                          GNC_HOW_DENOM_SIGFIGS(6) |
                                          GNC_HOW_RND_ROUND),
                    val, "expected %s = %s double 6 figs");
    check_unary_op (gnc_numeric_eq,
                    gnc_numeric_create (112346, 10000000000LL),
                    double_to_gnc_numeric(1.1234567890123e-5,
                                          GNC_DENOM_AUTO,
                                          GNC_HOW_DENOM_SIGFIGS(6) |
                                          GNC_HOW_RND_ROUND),
                    val, "expected %s = %s double 6 figs");

    flo = gnc_numeric_to_double(gnc_numeric_create(7, 16));
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
                    gnc_numeric_create (-2, 6), c,
                    a, "expected %s got %s = -(%s)");

    check_unary_op (gnc_numeric_eq,
                    gnc_numeric_create (-1, 4), d,
                    b, "expected %s got %s = -(%s)");

}

/* ======================================================= */

static void
check_add_subtract (void)
{
    int i;
    gnc_numeric a, b, c, d, z;
#if CHECK_ERRORS_TOO
    gnc_numeric c;
#endif

    a = gnc_numeric_create(2, 6);
    b = gnc_numeric_create(1, 4);

    /* Well, actually 14/24 would be acceptable/better in this case */
    check_binary_op (gnc_numeric_create(7, 12),
                     gnc_numeric_add(a, b, GNC_DENOM_AUTO, GNC_HOW_DENOM_EXACT),
                     a, b, "expected %s got %s = %s + %s for add exact");

    check_binary_op (gnc_numeric_create(58, 100),
                     gnc_numeric_add(a, b, 100, GNC_HOW_RND_ROUND),
                     a, b, "expected %s got %s = %s + %s for add 100ths (banker's)");

    check_binary_op (gnc_numeric_create(5833, 10000),
                     gnc_numeric_add(a, b, GNC_DENOM_AUTO,
                                     GNC_HOW_DENOM_SIGFIGS(4) |
                                     GNC_HOW_RND_ROUND),
                     a, b, "expected %s got %s = %s + %s for add 4 sig figs");

    check_binary_op (gnc_numeric_create(583333, 1000000),
                     gnc_numeric_add(a, b, GNC_DENOM_AUTO,
                                     GNC_HOW_DENOM_SIGFIGS(6) |
                                     GNC_HOW_RND_ROUND),
                     a, b, "expected %s got %s = %s + %s for add 6 sig figs");

    check_binary_op (gnc_numeric_create(1, 12),
                     gnc_numeric_sub(a, b, GNC_DENOM_AUTO, GNC_HOW_DENOM_EXACT),
                     a, b, "expected %s got %s = %s - %s for sub exact");

    /* We should try something trickier for reduce & lcd */
    check_binary_op (gnc_numeric_create(1, 12),
                     gnc_numeric_sub(a, b, GNC_DENOM_AUTO, GNC_HOW_DENOM_REDUCE),
                     a, b, "expected %s got %s = %s - %s for sub reduce");

    check_binary_op (gnc_numeric_create(1, 12),
                     gnc_numeric_sub(a, b, GNC_DENOM_AUTO, GNC_HOW_DENOM_LCD),
                     a, b, "expected %s got %s = %s - %s for sub reduce");

    check_binary_op (gnc_numeric_create(8, 100),
                     gnc_numeric_sub(a, b, 100, GNC_HOW_RND_ROUND),
                     a, b, "expected %s got %s = %s - %s for sub 100ths (banker's)");

    /* ------------------------------------------------------------ */
    /* This test has failed before */
    c = gnc_numeric_neg (a);
    d = gnc_numeric_neg (b);
    z = gnc_numeric_zero();
    check_binary_op (c, gnc_numeric_add_fixed(z, c),
                     z, c, "expected %s got %s = %s + %s for add fixed");

    check_binary_op (d, gnc_numeric_add_fixed(z, d),
                     z, d, "expected %s got %s = %s + %s for add fixed");

    /* ------------------------------------------------------------ */
    /* Same as above, but with signs reviersed */
    a = c;
    b = d;
    /* Well, actually 14/24 would be acceptable/better in this case */
    check_binary_op (gnc_numeric_create(-7, 12),
                     gnc_numeric_add(a, b, GNC_DENOM_AUTO, GNC_HOW_DENOM_EXACT),
                     a, b, "expected %s got %s = %s + %s for add exact");

    check_binary_op (gnc_numeric_create(-58, 100),
                     gnc_numeric_add(a, b, 100, GNC_HOW_RND_ROUND),
                     a, b, "expected %s got %s = %s + %s for add 100ths (banker's)");

    check_binary_op (gnc_numeric_create(-5833, 10000),
                     gnc_numeric_add(a, b, GNC_DENOM_AUTO,
                                     GNC_HOW_DENOM_SIGFIGS(4) |
                                     GNC_HOW_RND_ROUND),
                     a, b, "expected %s got %s = %s + %s for add 4 sig figs");

    check_binary_op (gnc_numeric_create(-583333, 1000000),
                     gnc_numeric_add(a, b, GNC_DENOM_AUTO,
                                     GNC_HOW_DENOM_SIGFIGS(6) |
                                     GNC_HOW_RND_ROUND),
                     a, b, "expected %s got %s = %s + %s for add 6 sig figs");

    check_binary_op (gnc_numeric_create(-1, 12),
                     gnc_numeric_sub(a, b, GNC_DENOM_AUTO, GNC_HOW_DENOM_EXACT),
                     a, b, "expected %s got %s = %s - %s for sub exact");

    /* We should try something trickier for reduce & lcd */
    check_binary_op (gnc_numeric_create(-1, 12),
                     gnc_numeric_sub(a, b, GNC_DENOM_AUTO, GNC_HOW_DENOM_REDUCE),
                     a, b, "expected %s got %s = %s - %s for sub reduce");

    check_binary_op (gnc_numeric_create(-1, 12),
                     gnc_numeric_sub(a, b, GNC_DENOM_AUTO, GNC_HOW_DENOM_LCD),
                     a, b, "expected %s got %s = %s - %s for sub reduce");

    check_binary_op (gnc_numeric_create(-8, 100),
                     gnc_numeric_sub(a, b, 100, GNC_HOW_RND_ROUND),
                     a, b, "expected %s got %s = %s - %s for sub 100ths (banker's)");

    /* ------------------------------------------------------------ */
#if CHECK_ERRORS_TOO
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

    /* ------------------------------------------------------------ */
    /* Add and subtract some random numbers */
    for (i = 0; i < NREPS; i++)
    {
        gnc_numeric e;
        gint64 deno = rand() + 1;
        gint64 na = get_random_gint64();
        gint64 nb = get_random_gint64();
        gint64 ne;

        /* avoid overflow; */
        na /= 2;
        nb /= 2;

        a = gnc_numeric_create(na, deno);
        b = gnc_numeric_create(nb, deno);

        /* Add */
        ne = na + nb;
        e = gnc_numeric_create(ne, deno);
        check_binary_op (e,
                         gnc_numeric_add(a, b, GNC_DENOM_AUTO, GNC_HOW_DENOM_EXACT),
                         a, b, "expected %s got %s = %s + %s for exact addition");

        /* Subtract */
        ne = na - nb;
        e = gnc_numeric_create(ne, deno);
        check_binary_op (e,
                         gnc_numeric_sub(a, b, GNC_DENOM_AUTO, GNC_HOW_DENOM_EXACT),
                         a, b, "expected %s got %s = %s - %s for exact subtraction");
    }
}

static const gint64 pten[] = { 1, 10, 100, 1000, 10000, 100000, 1000000,
			       10000000, 100000000, 1000000000, 10000000000,
			       100000000000, 1000000000000, 10000000000000,
			       100000000000000, 10000000000000000,
			       100000000000000000, 1000000000000000000};
#define POWTEN_OVERFLOW -5

static inline gint64
powten (int exp)
{
    if (exp > 18 || exp < -18)
	return POWTEN_OVERFLOW;
    return exp < 0 ? -pten[-exp] : pten[exp];
}

static void
check_add_subtract_overflow (void)
{
    int i;

    for (i = 0; i < NREPS; i++)
    {
	/* Div to avoid addition overflows; we're looking for lcd conversion overflows here. */

	int exp_a = rand () % 1000;
	int exp_b = rand () % 1000;
        gint64 bin_deno_a = (exp_a == 0 ? 1 : exp_a);
	gint64 bin_deno_b = (exp_b == 0 ? 1 : exp_b);
/*
	int exp_a = rand () % 11;
	int exp_b = rand () % 11;
	gint64 bin_deno_a = (1 << exp_a);
	gint64 bin_deno_b = (1 << exp_a);
*/
	gint64 dec_deno_a = powten (exp_a % 7);
	gint64 dec_deno_b = powten (exp_b % 7);
        gint64 na = get_random_gint64 () % (1000000 * dec_deno_a);
        gint64 nb = get_random_gint64 () % (1000000 * dec_deno_b);
	gnc_numeric result;
	GNCNumericErrorCode err;
	gchar *errmsg;

        gnc_numeric ba = gnc_numeric_create(na, bin_deno_a);
        gnc_numeric bb = gnc_numeric_create(nb, bin_deno_b);
        gnc_numeric da = gnc_numeric_create(na, dec_deno_a);
        gnc_numeric db = gnc_numeric_create(nb, dec_deno_b);
	gchar *ba_str = gnc_numeric_to_string (ba);
	gchar *bb_str = gnc_numeric_to_string (bb);
	gchar *da_str = gnc_numeric_to_string (da);
	gchar *db_str = gnc_numeric_to_string (db);


        /* Add */

	result = gnc_numeric_add(ba, bb, GNC_DENOM_AUTO, GNC_HOW_DENOM_EXACT);
	err = gnc_numeric_check (result);
	errmsg = g_strdup_printf ("%s + %s raised %s", ba_str, bb_str,
				  gnc_numeric_errorCode_to_string (err));
	do_test (err == 0, errmsg);
	g_free (errmsg);

	result = gnc_numeric_add(da, bb, GNC_DENOM_AUTO, GNC_HOW_DENOM_EXACT);
	err = gnc_numeric_check (result);
	errmsg = g_strdup_printf ("%s + %s raised %s", da_str, bb_str,
				  gnc_numeric_errorCode_to_string (err));
	do_test (err == 0, errmsg);
	g_free (errmsg);
	result = gnc_numeric_add(ba, db, GNC_DENOM_AUTO, GNC_HOW_DENOM_EXACT);
	err = gnc_numeric_check (result);
	errmsg = g_strdup_printf ("%s + %s raised %s", ba_str, db_str,
				  gnc_numeric_errorCode_to_string (err));
	do_test (err == 0, errmsg);
	g_free (errmsg);

	result = gnc_numeric_add(da, db, GNC_DENOM_AUTO, GNC_HOW_DENOM_EXACT);
	err = gnc_numeric_check (result);
	errmsg = g_strdup_printf ("%s + %s raised %s", da_str, db_str,
				  gnc_numeric_errorCode_to_string (err));
	do_test (err == 0, errmsg);
	g_free (errmsg);
        /* Subtract */

	result = gnc_numeric_sub(ba, bb, GNC_DENOM_AUTO, GNC_HOW_DENOM_EXACT);
	err = gnc_numeric_check (result);
	errmsg = g_strdup_printf ("%s + %s raised %s", ba_str, bb_str,
				  gnc_numeric_errorCode_to_string (err));
	do_test (err == 0, errmsg);
	g_free (errmsg);

	result = gnc_numeric_sub(da, bb, GNC_DENOM_AUTO, GNC_HOW_DENOM_EXACT);
	err = gnc_numeric_check (result);
	errmsg = g_strdup_printf ("%s + %s raised %s", da_str, bb_str,
				  gnc_numeric_errorCode_to_string (err));
	do_test (err == 0, errmsg);
	g_free (errmsg);
	result = gnc_numeric_sub(ba, db, GNC_DENOM_AUTO, GNC_HOW_DENOM_EXACT);
	err = gnc_numeric_check (result);
	errmsg = g_strdup_printf ("%s + %s raised %s", ba_str, db_str,
				  gnc_numeric_errorCode_to_string (err));
	do_test (err == 0, errmsg);
	g_free (errmsg);

	result = gnc_numeric_sub(da, db, GNC_DENOM_AUTO, GNC_HOW_DENOM_EXACT);
	err = gnc_numeric_check (result);
	errmsg = g_strdup_printf ("%s + %s raised %s", da_str, db_str,
				  gnc_numeric_errorCode_to_string (err));
	do_test (err == 0, errmsg);
	g_free (errmsg);

	g_free (ba_str);
	g_free (bb_str);
	g_free (da_str);
	g_free (db_str);
    }

}

/* ======================================================= */


static void
check_mult_div (void)
{
    int i, j;
    gint64 v;
    gnc_numeric c, d;
    gnc_numeric amt_a, amt_tot, frac, val_tot, val_a;
    gnc_numeric a, b;

    a = gnc_numeric_create(-100, 100);
    b = gnc_numeric_create(1, 1);
    check_binary_op (gnc_numeric_create(-100, 100),
                     gnc_numeric_div(a, b, GNC_DENOM_AUTO, GNC_HOW_DENOM_EXACT),
                     a, b, "expected %s got %s = %s / %s div exact");

    a = gnc_numeric_create(-100, 100);
    b = gnc_numeric_create(-1, 1);
    check_binary_op (gnc_numeric_create(100, 100),
                     gnc_numeric_div(a, b, GNC_DENOM_AUTO, GNC_HOW_DENOM_EXACT),
                     a, b, "expected %s got %s = %s / %s div exact");

    a = gnc_numeric_create(-100, 100);
    b = gnc_numeric_create(-1, 1);
    check_binary_op (gnc_numeric_create(100, 100),
                     gnc_numeric_mul(a, b, GNC_DENOM_AUTO, GNC_HOW_DENOM_EXACT),
                     a, b, "expected %s got %s = %s * %s mult exact");

    a = gnc_numeric_create(2, 6);
    b = gnc_numeric_create(1, 4);

    check_binary_op (gnc_numeric_create(2, 24),
                     gnc_numeric_mul(a, b, GNC_DENOM_AUTO, GNC_HOW_DENOM_EXACT),
                     a, b, "expected %s got %s = %s * %s for mult exact");

    check_binary_op (gnc_numeric_create(1, 12),
                     gnc_numeric_mul(a, b, GNC_DENOM_AUTO, GNC_HOW_DENOM_REDUCE),
                     a, b, "expected %s got %s = %s * %s for mult reduce");

    check_binary_op (gnc_numeric_create(8, 100),
                     gnc_numeric_mul(a, b, 100, GNC_HOW_RND_ROUND),
                     a, b, "expected %s got %s = %s * %s for mult 100th's");

    check_binary_op (gnc_numeric_create(8, 6),
                     gnc_numeric_div(a, b, GNC_DENOM_AUTO, GNC_HOW_DENOM_EXACT),
                     a, b, "expected %s got %s = %s / %s for div exact");

    check_binary_op (gnc_numeric_create(4, 3),
                     gnc_numeric_div(a, b, GNC_DENOM_AUTO, GNC_HOW_DENOM_REDUCE),
                     a, b, "expected %s got %s = %s / %s for div reduce");

    check_binary_op (gnc_numeric_create(133, 100),
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
    v = 1000000;
    a = gnc_numeric_create(1 * v, v);
    b = gnc_numeric_create(10000000 * v, v);

    check_binary_op (b,
                     gnc_numeric_mul(a, b, GNC_DENOM_AUTO, GNC_HOW_DENOM_LCD),
                     a, b, "expected %s got %s = %s * %s for multiply");

    /* Multiply some random numbers.  This test presumes that
     * RAND_MAX is approx 2^32
     */
    for (i = 0; i < NREPS; i++)
    {
        gint64 deno = 1;
        gint64 na = rand();
        gint64 nb = rand();
        gint64 ne;

        /* avoid 0 */
        if (nb / 4 == 0)
        {
            i--;
            continue;
        }

        /* avoid overflow; */
        na /= 2;
        nb /= 2;
        ne = na * nb;

        a = gnc_numeric_create(na, deno);
        b = gnc_numeric_create(nb, deno);

        check_binary_op_equal (gnc_numeric_create(ne, 1),
                               gnc_numeric_mul(a, b, GNC_DENOM_AUTO, GNC_HOW_DENOM_EXACT),
                               a, b, "expected %s got %s = %s * %s for mult exact");

        /* Force 128-bit math to come into play */
        for (j = 1; j < 31; j++)
        {
            a = gnc_numeric_create(na << j, 1 << j);
            b = gnc_numeric_create(nb << j, 1 << j);
            check_binary_op (gnc_numeric_create(ne, 1),
                             gnc_numeric_mul(a, b, GNC_DENOM_AUTO, GNC_HOW_DENOM_REDUCE),
                             a, b, "expected %s got %s = %s * %s for mult reduce");
        }

        /* Do some hokey random 128-bit division too */
        b = gnc_numeric_create(deno, nb);

        check_binary_op_equal (gnc_numeric_create(ne, 1),
                               gnc_numeric_div(a, b, GNC_DENOM_AUTO, GNC_HOW_DENOM_EXACT),
                               a, b, "expected %s got %s = %s / %s for div exact");

        /* avoid overflow; */
        na /= 2;
        nb /= 2;
        ne = na * nb;
        for (j = 1; j < 16; j++)
        {
            a = gnc_numeric_create(na << j, 1 << j);
            b = gnc_numeric_create(1 << j, nb << j);
            check_binary_op (gnc_numeric_create(ne, 1),
                             gnc_numeric_div(a, b, GNC_DENOM_AUTO, GNC_HOW_DENOM_REDUCE),
                             a, b, "expected %s got %s = %s / %s for div reduce");
        }
    }

    a = gnc_numeric_create(782592055622866ULL, 89025);
    b = gnc_numeric_create(2222554708930978ULL, 85568);
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
     */
/* Doesn't overflow any more! */
    check_binary_op (gnc_numeric_error (GNC_ERROR_REMAINDER),
                     gnc_numeric_div(a, b, GNC_DENOM_AUTO,
                                     GNC_HOW_RND_NEVER | GNC_HOW_DENOM_EXACT),
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
    c = gnc_numeric_mul (a, b, GNC_DENOM_AUTO, GNC_HOW_DENOM_EXACT);
    d = gnc_numeric_create (-32005637020LL, 55739);

    check_binary_op (gnc_numeric_create(-102547458LL, 82718),
                     gnc_numeric_div(c, d, 82718,
                                     GNC_HOW_DENOM_EXACT),
                     c, d, "expected %s got %s = %s / %s for div round");

    /* If we specify GNC_HOW_RND_NEVER, then we shoukld get an error,
     * since the exact result won't fit into a 64-bit quantity. */
    check_binary_op (gnc_numeric_error (GNC_ERROR_REMAINDER),
                     gnc_numeric_div(c, d, 82718,
                                     GNC_HOW_DENOM_EXACT | GNC_HOW_RND_NEVER),
                     c, d, "expected %s got %s = %s / %s for div round");

    /* A simple irreducible ratio, involving negative numbers */
    amt_a = gnc_numeric_create (-6005287905LL, 40595);
    amt_tot = gnc_numeric_create (-8744187958LL, 40595);
    frac = gnc_numeric_div (amt_a, amt_tot,
                            GNC_DENOM_AUTO, GNC_HOW_DENOM_REDUCE);

    check_binary_op (gnc_numeric_create(6005287905LL, 8744187958LL),
                     frac, amt_a, amt_tot,
                     "expected %s got %s = %s / %s for div reduce");

    /* Another overflow-prone condition */
    val_tot = gnc_numeric_create (-4280656418LL, 19873);
    val_a = gnc_numeric_mul (frac, val_tot,
                             gnc_numeric_denom(val_tot),
                             GNC_HOW_RND_ROUND | GNC_HOW_DENOM_EXACT);
    check_binary_op (gnc_numeric_create(-2939846940LL, 19873),
                     val_a, val_tot, frac,
                     "expected %s got %s = %s * %s for mult round");

    frac = gnc_numeric_create (396226789777979LL, 328758834367851752LL);
    val_tot = gnc_numeric_create (467013515494988LL, 100);
    val_a = gnc_numeric_mul (frac, val_tot,
                             gnc_numeric_denom(val_tot),
                             GNC_HOW_RND_ROUND | GNC_HOW_DENOM_EXACT);
    check_binary_op (gnc_numeric_create(562854124919LL, 100),
                     val_a, val_tot, frac,
                     "expected %s got %s = %s * %s for mult round");

    /* Yet another bug from bugzilla ... */
    a = gnc_numeric_create (40066447153986554LL, 4518);
    b = gnc_numeric_create (26703286457229LL, 3192);
    frac = gnc_numeric_div(a, b,
                           GNC_DENOM_AUTO,
                           GNC_HOW_DENOM_SIGFIGS(6) |
                           GNC_HOW_RND_ROUND);

    check_binary_op (gnc_numeric_create(106007, 100),
                     frac, a, b,
                     "expected %s got %s = %s / %s for mult sigfigs");

}

static void
check_reciprocal(void)
{
    gnc_numeric a, b, ans, val;
    double flo;

    val = gnc_numeric_create(-60, 20);
    check_unary_op (gnc_numeric_eq, gnc_numeric_create (-3, -1),
                    gnc_numeric_convert(val, GNC_DENOM_RECIPROCAL(1),
                                        GNC_HOW_RND_NEVER),
                    val, "expected %s got %s = (%s as RECIP(1))");

    a = gnc_numeric_create(200, 100);
    b = gnc_numeric_create(300, 100);

    /* 2 + 3 = 5 */
    ans = gnc_numeric_add(a, b, GNC_DENOM_RECIPROCAL(1), GNC_HOW_RND_NEVER);
    check_binary_op (gnc_numeric_create(5, -1),
                     ans, a, b, "expected %s got %s = %s + %s for reciprocal");

    /* 2 + 3 = 5 */
    a = gnc_numeric_create(2, -1);
    b = gnc_numeric_create(300, 100);
    ans = gnc_numeric_add(a, b, GNC_DENOM_RECIPROCAL(1), GNC_HOW_RND_NEVER);
    check_binary_op (gnc_numeric_create(5, -1),
                     ans, a, b, "expected %s got %s = %s + %s for reciprocal");

    /* check gnc_numeric_to_double */
    flo = gnc_numeric_to_double(gnc_numeric_create(5, -1));
    do_test ((5.0 == flo), "reciprocal conversion");

    /* check gnc_numeric_compare */
    a = gnc_numeric_create(2, 1);
    b = gnc_numeric_create(2, -1);
    do_test((0 == gnc_numeric_compare(a, b)), " 2 == 2 ");
    a = gnc_numeric_create(2, 1);
    b = gnc_numeric_create(3, -1);
    do_test((-1 == gnc_numeric_compare(a, b)), " 2 < 3 ");
    a = gnc_numeric_create(-2, 1);
    b = gnc_numeric_create(2, -1);
    do_test((-1 == gnc_numeric_compare(a, b)), " -2 < 2 ");
    a = gnc_numeric_create(2, -1);
    b = gnc_numeric_create(3, -1);
    do_test((-1 == gnc_numeric_compare(a, b)), " 2 < 3 ");

    /* check for equality */
    a = gnc_numeric_create(2, 1);
    b = gnc_numeric_create(2, -1);
    do_test(gnc_numeric_equal(a, b), " 2 == 2 ");

    /* check gnc_numeric_mul */
    a = gnc_numeric_create(2, 1);
    b = gnc_numeric_create(3, -1);
    ans = gnc_numeric_mul(a, b, GNC_DENOM_RECIPROCAL(1), GNC_HOW_RND_NEVER);
    check_binary_op (gnc_numeric_create(6, -1),
                     ans, a, b, "expected %s got %s = %s * %s for reciprocal");

    /* check gnc_numeric_div */
    /* -60 / 20 = -3 */
    a = gnc_numeric_create(-60, 1);
    b = gnc_numeric_create(2, -10);
    ans = gnc_numeric_div(a, b, GNC_DENOM_RECIPROCAL(1), GNC_HOW_RND_NEVER);
    check_binary_op (gnc_numeric_create(-3, -1),
                     ans, a, b, "expected %s got %s = %s / %s for reciprocal");

    /* 60 / 20 = 3 */
    a = gnc_numeric_create(60, 1);
    b = gnc_numeric_create(2, -10);
    ans = gnc_numeric_div(a, b, GNC_DENOM_RECIPROCAL(1), GNC_HOW_RND_NEVER);
    check_binary_op (gnc_numeric_create(3, -1),
                     ans, a, b, "expected %s got %s = %s / %s for reciprocal");


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
    check_add_subtract_overflow ();
    check_mult_div ();
    check_reciprocal();
}

int
main (int argc, char **argv)
{
    qof_init();
    if (cashobjects_register())
    {
        run_test ();
        print_test_results();
    }
    qof_close();
    return get_rv();
}

/* ======================== END OF FILE ====================== */
