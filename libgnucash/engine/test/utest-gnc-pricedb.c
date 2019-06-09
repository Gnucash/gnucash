/********************************************************************
 * utest-gnc-pricedb.c: GLib g_test test suite for gnc-pricedb.c.   *
 * Copyright 2015 John Ralls <jralls@ceridwen.us>		    *
 *                                                                  *
 * This program is free software; you can redistribute it and/or    *
 * modify it under the terms of the GNU General Public License as   *
 * published by the Free Software Foundation; either version 2 of   *
 * the License, or (at your option) any later version.              *
 *                                                                  *
 * This program is distributed in the hope that it will be useful,  *
 * but WITHOUT ANY WARRANTY; without even the implied warranty of   *
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the    *
 * GNU General Public License for more details.                     *
 *                                                                  *
 * You should have received a copy of the GNU General Public License*
 * along with this program; if not, you can retrieve it from        *
 * https://www.gnu.org/licenses/old-licenses/gpl-2.0.html            *
 * or contact:                                                      *
 *                                                                  *
 * Free Software Foundation           Voice:  +1-617-542-5942       *
 * 51 Franklin Street, Fifth Floor    Fax:    +1-617-542-2652       *
 * Boston, MA  02110-1301,  USA       gnu@gnu.org                   *
 ********************************************************************/
#include <config.h>
#include <string.h>
#include <glib.h>
#include <unittest-support.h>
/* Add specific headers for this class */
#include <gnc-pricedb.h>
#include <gnc-pricedb-p.h>

static const gchar *suitename = "/engine/gnc-pricedb";
void test_suite_gnc_pricedb ( void );

typedef struct
{
    gnc_commodity *aud;
    gnc_commodity *bgn;
    gnc_commodity *dkk;
    gnc_commodity *eur;
    gnc_commodity *gbp;
    gnc_commodity *usd;
    gnc_commodity *amzn;
} Commodities;

static Commodities *
setup_commodities (QofBook *book)
{
    Commodities *com = g_new0(Commodities, 1);
    com->aud = gnc_commodity_new(book, "Australian Dollar",
                                 "ISO4217", "AUD", "036", 100);
    com->bgn = gnc_commodity_new(book, "Bulgarian Lev",
                                 "ISO4217", "BGN", "975", 100);
    com->dkk = gnc_commodity_new(book, "Danish Krone",
                                 "ISO4217", "DKK", "208", 100);
    com->eur = gnc_commodity_new(book, "Euro",
                                 "ISO4217", "EUR", "978", 100);
    com->gbp = gnc_commodity_new(book, "Pound Sterling",
                                 "ISO4217", "GBP", "826", 100);
    com->usd = gnc_commodity_new(book, "US Dollar",
                                 "ISO4217", "USD", "840", 100);
    com->amzn = gnc_commodity_new(book, "Amazon.com",
                                  "NASDAQ", "AMZN", "", 1);
    return com;
}

/* Create a new GNCPrice and populate it. Could do this with g_object_new, but
 * this is faster. It's not reffed, so if you don't put it into a GNCPriceDB or
 * a PriceList you'll have to call gnc_price_destroy on it.
 */
static GNCPrice *
construct_price(QofBook *book, gnc_commodity *com, gnc_commodity *cur,
                time64 t, PriceSource source, gnc_numeric price)
{
    GNCPrice *p = gnc_price_create(book);
    gnc_price_set_commodity(p, com);
    gnc_price_set_currency(p, cur);
    gnc_price_set_time64(p, t);
    gnc_price_set_source(p, source);
    gnc_price_set_value(p, price);
    return p;
}

/* gnc_price_init
static void
gnc_price_init(GNCPrice* price)*/
/* static void
test_gnc_price_init (Fixture *fixture, gconstpointer pData)
{
}*/
/* gnc_price_dispose
static void
gnc_price_dispose(GObject *pricep)*/
/* static void
test_gnc_price_dispose (Fixture *fixture, gconstpointer pData)
{
}*/
/* gnc_price_finalize
static void
gnc_price_finalize(GObject* pricep)*/
/* static void
test_gnc_price_finalize (Fixture *fixture, gconstpointer pData)
{
}*/
/* gnc_price_get_property
static void
gnc_price_get_property(GObject* object, guint prop_id, GValue* value, GParamSpec* pspec)*/
/* static void
test_gnc_price_get_property (Fixture *fixture, gconstpointer pData)
{
}*/
/* gnc_price_set_property
static void
gnc_price_set_property(GObject* object, guint prop_id, const GValue* value, GParamSpec* pspec)*/
/* static void
test_gnc_price_set_property (Fixture *fixture, gconstpointer pData)
{
}*/
/* gnc_price_class_init
static void
gnc_price_class_init(GNCPriceClass *klass)*/
/* static void
test_gnc_price_class_init (Fixture *fixture, gconstpointer pData)
{
}*/
/* gnc_price_create
GNCPrice *
gnc_price_create (QofBook *book)// C: 9 in 8 SCM: 1  Local: 2:0:0
*/
/* static void
test_gnc_price_create (Fixture *fixture, gconstpointer pData)
{
}*/
/* gnc_price_destroy
static void
gnc_price_destroy (GNCPrice *p)// Local: 1:0:0
*/
/* static void
test_gnc_price_destroy (Fixture *fixture, gconstpointer pData)
{
}*/
/* gnc_price_ref
void
gnc_price_ref(GNCPrice *p)// C: 1 SCM: 3 in 2 Local: 17:0:0
*/
/* static void
test_gnc_price_ref (Fixture *fixture, gconstpointer pData)
{
}*/
/* gnc_price_unref
void
gnc_price_unref(GNCPrice *p)// C: 23 in 10 SCM: 7 in 4 Local: 9:0:0
*/
/* static void
test_gnc_price_unref (Fixture *fixture, gconstpointer pData)
{
}*/
/* gnc_price_clone
GNCPrice *
gnc_price_clone (GNCPrice* p, QofBook *book)// C: 4 in 3  Local: 0:0:0
*/
/* static void
test_gnc_price_clone (Fixture *fixture, gconstpointer pData)
{
}*/
/* gnc_price_begin_edit
void
gnc_price_begin_edit (GNCPrice *p)// C: 10 in 8 SCM: 2 in 1 Local: 8:0:0
*/
/* static void
test_gnc_price_begin_edit (Fixture *fixture, gconstpointer pData)
{
}*/
/* commit_err
static void commit_err (QofInstance *inst, QofBackendError errcode)// Local: 0:2:0
*/
/* static void
test_commit_err (Fixture *fixture, gconstpointer pData)
{
}*/
/* gnc_price_commit_edit
static void noop (QofInstance *inst) {}
void
gnc_price_commit_edit (GNCPrice *p)// Local: 8:0:0
*/
/* static void
test_gnc_price_commit_edit (Fixture *fixture, gconstpointer pData)
{
}*/
// Make Static
/* gnc_pricedb_begin_edit
void
gnc_pricedb_begin_edit (GNCPriceDB *pdb)// Local: 2:0:0
*/
/* static void
test_gnc_pricedb_begin_edit (Fixture *fixture, gconstpointer pData)
{
}*/
// Make Static
/* gnc_pricedb_commit_edit
void
gnc_pricedb_commit_edit (GNCPriceDB *pdb)// Local: 2:0:0
*/
/* static void
test_gnc_pricedb_commit_edit (Fixture *fixture, gconstpointer pData)
{
}*/
/* gnc_price_set_dirty
static void
gnc_price_set_dirty (GNCPrice *p)// Local: 6:0:0
*/
/* static void
test_gnc_price_set_dirty (Fixture *fixture, gconstpointer pData)
{
}*/
/* gnc_price_set_commodity
void
gnc_price_set_commodity(GNCPrice *p, gnc_commodity *c)// C: 7 in 7 SCM: 1  Local: 3:0:0
*/
/* static void
test_gnc_price_set_commodity (Fixture *fixture, gconstpointer pData)
{
}*/
/* gnc_price_set_currency
void
gnc_price_set_currency(GNCPrice *p, gnc_commodity *c)// C: 7 in 7 SCM: 1  Local: 3:0:0
*/
/* static void
test_gnc_price_set_currency (Fixture *fixture, gconstpointer pData)
{
}*/
/* gnc_price_set_time64
void
gnc_price_set_time64(GNCPrice *p, time64 t)// C: 9 in 7 SCM: 2 in 1 Local: 2:0:0
*/
/* static void
test_gnc_price_set_time64 (Fixture *fixture, gconstpointer pData)
{
}*/
/* gnc_price_set_source
void
gnc_price_set_source(GNCPrice *p, PriceSource s)// C: 10 in 7 SCM: 2 in 1 Local: 2:0:0
*/
/* static void
test_gnc_price_set_source (Fixture *fixture, gconstpointer pData)
{
}*/
/* gnc_price_set_source_string
void
gnc_price_set_source_string(GNCPrice *p, const char* str)// C: 3 in 3  Local: 1:0:0
*/
/* static void
test_gnc_price_set_source_string (Fixture *fixture, gconstpointer pData)
{
}*/
/* gnc_price_set_typestr
void
gnc_price_set_typestr(GNCPrice *p, const char* type)// C: 9 in 7 SCM: 2 in 1 Local: 2:0:0
*/
/* static void
test_gnc_price_set_typestr (Fixture *fixture, gconstpointer pData)
{
}*/
/* gnc_price_set_value
void
gnc_price_set_value(GNCPrice *p, gnc_numeric value)// C: 9 in 7 SCM: 2 in 1 Local: 2:0:0
*/
/* static void
test_gnc_price_set_value (Fixture *fixture, gconstpointer pData)
{
}*/
/* gnc_price_lookup
GNCPrice *
gnc_price_lookup (const GncGUID *guid, QofBook *book)// C: 2 in 2  Local: 0:0:0
*/
/* static void
test_gnc_price_lookup (Fixture *fixture, gconstpointer pData)
{
}*/
/* gnc_price_get_commodity
gnc_commodity *
gnc_price_get_commodity(const GNCPrice *p)// C: 12 in 5  Local: 20:1:0
*/
/* static void
test_gnc_price_get_commodity (Fixture *fixture, gconstpointer pData)
{
}*/
/* gnc_price_get_time64
time64
gnc_price_get_time64(const GNCPrice *p)// C: 7 in 4 SCM: 1  Local: 18:1:0
*/
/* static void
test_gnc_price_get_time64 (Fixture *fixture, gconstpointer pData)
{
}*/
/* gnc_price_get_source
PriceSource
gnc_price_get_source(const GNCPrice *p)// C: 7 in 7 SCM: 1  Local: 4:1:0
*/
/* static void
test_gnc_price_get_source (Fixture *fixture, gconstpointer pData)
{
}*/
/* gnc_price_get_source_string
const char*
gnc_price_get_source_string(const GNCPrice *p)// C: 3 in 3  Local: 1:0:0
*/
/* static void
test_gnc_price_get_source_string (Fixture *fixture, gconstpointer pData)
{
}*/
/* gnc_price_get_typestr
const char *
gnc_price_get_typestr(const GNCPrice *p)// C: 5 in 4  Local: 4:1:0
*/
/* static void
test_gnc_price_get_typestr (Fixture *fixture, gconstpointer pData)
{
}*/
/* gnc_price_get_value
gnc_numeric
gnc_price_get_value(const GNCPrice *p)// C: 22 in 12 SCM: 9 in 5 Local: 9:1:0
*/
/* static void
test_gnc_price_get_value (Fixture *fixture, gconstpointer pData)
{
}*/
/* gnc_price_get_currency
gnc_commodity *
gnc_price_get_currency(const GNCPrice *p)// C: 14 in 8 SCM: 6 in 2 Local: 16:1:0
*/
/* static void
test_gnc_price_get_currency (Fixture *fixture, gconstpointer pData)
{
}*/
/* gnc_price_equal
gboolean
gnc_price_equal (const GNCPrice *p1, const GNCPrice *p2)// Local: 1:0:0
*/
/* static void
test_gnc_price_equal (Fixture *fixture, gconstpointer pData)
{
}*/
/* compare_prices_by_date
static gint
compare_prices_by_date(gconstpointer a, gconstpointer b)// Local: 0:2:0
*/
/* static void
test_compare_prices_by_date (Fixture *fixture, gconstpointer pData)
{
}*/
/* price_list_is_duplicate
static void
price_list_is_duplicate( gpointer data, gpointer user_data )// Local: 0:1:0
*/
/* static void
test_price_list_is_duplicate (Fixture *fixture, gconstpointer pData)
{
}*/
// Make Static
/* gnc_price_list_insert
gboolean
gnc_price_list_insert(PriceList **prices, GNCPrice *p, gboolean check_dupl)// Local: 3:0:0
*/
/* static void
test_gnc_price_list_insert (Fixture *fixture, gconstpointer pData)
{
}*/
/* gnc_price_list_remove
gboolean
gnc_price_list_remove(PriceList **prices, GNCPrice *p)// Local: 1:0:0
*/
/* static void
test_gnc_price_list_remove (Fixture *fixture, gconstpointer pData)
{
}*/
/* price_list_destroy_helper
static void
price_list_destroy_helper(gpointer data, gpointer user_data)// Local: 0:1:0
*/
/* static void
test_price_list_destroy_helper (Fixture *fixture, gconstpointer pData)
{
}*/
/* gnc_price_list_destroy
void
gnc_price_list_destroy(PriceList *prices)// C: 12 in 6 SCM: 3 in 2 Local: 7:0:0
*/
/* static void
test_gnc_price_list_destroy (Fixture *fixture, gconstpointer pData)
{
}*/
/* gnc_price_list_equal
gboolean
gnc_price_list_equal(PriceList *prices1, PriceList *prices2)// Local: 1:0:0
*/
/* static void
test_gnc_price_list_equal (Fixture *fixture, gconstpointer pData)
{
}*/
typedef struct
{
    GNCPriceDB *pricedb;
    Commodities *com;
} PriceDBFixture;

static void
create_some_prices (PriceDBFixture *fixture)
{
    GNCPriceDB *db = fixture->pricedb;
    QofBook *book = qof_instance_get_book(QOF_INSTANCE(db));
    Commodities *c = fixture->com;
    gnc_pricedb_set_bulk_update(db, TRUE);
    gnc_pricedb_add_price(db, construct_price(book, c->usd, c->aud,
                                              gnc_dmy2time64(11, 4, 2009),
                                              PRICE_SOURCE_FQ,
                                            gnc_numeric_create(131190, 10000)));
    gnc_pricedb_add_price(db, construct_price(book, c->usd, c->aud,
                                              gnc_dmy2time64(12, 4, 2009),
                                              PRICE_SOURCE_USER_PRICE,
                                            gnc_numeric_create(131190, 10000)));
    gnc_pricedb_add_price(db, construct_price(book, c->usd, c->aud,
                                              gnc_dmy2time64(21, 8, 2010),
                                              PRICE_SOURCE_FQ,
                                            gnc_numeric_create(111794, 10000)));
    gnc_pricedb_add_price(db, construct_price(book, c->usd, c->aud,
                                              gnc_dmy2time64(1, 8, 2013),
                                              PRICE_SOURCE_FQ,
                                           gnc_numeric_create(111878, 100000)));
    gnc_pricedb_add_price(db, construct_price(book, c->usd, c->aud,
                                              gnc_dmy2time64(12, 11, 2014),
                                              PRICE_SOURCE_FQ,
                                           gnc_numeric_create(114784, 100000)));
    gnc_pricedb_add_price(db, construct_price(book, c->gbp, c->usd,
                                              gnc_dmy2time64(11, 4, 2009),
                                              PRICE_SOURCE_FQ,
                                           gnc_numeric_create(166651, 100000)));
    gnc_pricedb_add_price(db, construct_price(book, c->aud, c->usd,
                                              gnc_dmy2time64(20, 7, 2011),
                                              PRICE_SOURCE_FQ,
                                           gnc_numeric_create(106480, 100000)));
    gnc_pricedb_add_price(db, construct_price(book, c->aud, c->usd,
                                              gnc_dmy2time64(17, 11, 2012),
                                              PRICE_SOURCE_FQ,
                                           gnc_numeric_create(103415, 100000)));
    gnc_pricedb_add_price(db, construct_price(book, c->gbp, c->usd,
                                              gnc_dmy2time64(21, 8, 2010),
                                              PRICE_SOURCE_FQ,
                                           gnc_numeric_create(159037, 100000)));
    gnc_pricedb_add_price(db, construct_price(book, c->gbp, c->usd,
                                              gnc_dmy2time64(20, 7, 2011),
                                              PRICE_SOURCE_FQ,
                                           gnc_numeric_create(161643, 100000)));
    gnc_pricedb_add_price(db, construct_price(book, c->gbp, c->usd,
                                              gnc_dmy2time64(17, 11, 2012),
                                              PRICE_SOURCE_FQ,
                                           gnc_numeric_create(158855, 100000)));
    gnc_pricedb_add_price(db, construct_price(book, c->gbp, c->usd,
                                              gnc_dmy2time64(13, 10, 2012),
                                              PRICE_SOURCE_FQ,
                                           gnc_numeric_create(160705, 100000)));
    gnc_pricedb_add_price(db, construct_price(book, c->gbp, c->usd,
                                              gnc_dmy2time64(1, 8, 2013),
                                              PRICE_SOURCE_FQ,
                                           gnc_numeric_create(151173, 100000)));
    gnc_pricedb_add_price(db, construct_price(book, c->gbp, c->usd,
                                              gnc_dmy2time64(12, 11, 2014),
                                              PRICE_SOURCE_FQ,
                                           gnc_numeric_create(157658, 100000)));
    gnc_pricedb_add_price(db, construct_price(book, c->gbp, c->eur,
                                              gnc_dmy2time64(11, 4, 2009),
                                              PRICE_SOURCE_FQ,
                                           gnc_numeric_create(111257, 100000)));
    gnc_pricedb_add_price(db, construct_price(book, c->gbp, c->eur,
                                              gnc_dmy2time64(21, 8, 2010),
                                              PRICE_SOURCE_FQ,
                                           gnc_numeric_create(122195, 100000)));
    gnc_pricedb_add_price(db, construct_price(book, c->gbp, c->eur,
                                              gnc_dmy2time64(20, 7, 2011),
                                              PRICE_SOURCE_FQ,
                                           gnc_numeric_create(113289, 100000)));
    gnc_pricedb_add_price(db, construct_price(book, c->gbp, c->eur,
                                              gnc_dmy2time64(17, 11, 2012),
                                              PRICE_SOURCE_FQ,
                                           gnc_numeric_create(124646, 100000)));
    gnc_pricedb_add_price(db, construct_price(book, c->gbp, c->eur,
                                              gnc_dmy2time64(13, 10, 2012),
                                              PRICE_SOURCE_FQ,
                                           gnc_numeric_create(124072, 100000)));
    gnc_pricedb_add_price(db, construct_price(book, c->gbp, c->eur,
                                              gnc_dmy2time64(1, 8, 2013),
                                              PRICE_SOURCE_FQ,
                                           gnc_numeric_create(114420, 100000)));
    gnc_pricedb_add_price(db, construct_price(book, c->gbp, c->eur,
                                              gnc_dmy2time64(12, 11, 2014),
                                              PRICE_SOURCE_FQ,
                                           gnc_numeric_create(126836, 100000)));
    gnc_pricedb_add_price(db, construct_price(book, c->usd, c->dkk,
                                              gnc_dmy2time64(11, 4, 2009),
                                              PRICE_SOURCE_FQ,
                                           gnc_numeric_create(567859, 100000)));
    gnc_pricedb_add_price(db, construct_price(book, c->usd, c->dkk,
                                              gnc_dmy2time64(21, 8, 2010),
                                              PRICE_SOURCE_FQ,
                                           gnc_numeric_create(585810, 100000)));
    gnc_pricedb_add_price(db, construct_price(book, c->usd, c->dkk,
                                              gnc_dmy2time64(20, 7, 2011),
                                              PRICE_SOURCE_FQ,
                                           gnc_numeric_create(522449, 100000)));
    gnc_pricedb_add_price(db, construct_price(book, c->usd, c->dkk,
                                              gnc_dmy2time64(17, 11, 2012),
                                              PRICE_SOURCE_FQ,
                                           gnc_numeric_create(585380, 100000)));
    gnc_pricedb_add_price(db, construct_price(book, c->usd, c->dkk,
                                              gnc_dmy2time64(1, 8, 2013),
                                              PRICE_SOURCE_FQ,
                                           gnc_numeric_create(564281, 100000)));
    gnc_pricedb_add_price(db, construct_price(book, c->usd, c->dkk,
                                              gnc_dmy2time64(12, 11, 2014),
                                              PRICE_SOURCE_FQ,
                                           gnc_numeric_create(598693, 100000)));
    gnc_pricedb_add_price(db, construct_price(book, c->amzn, c->usd,
                                              gnc_dmy2time64(13, 4, 2009),
                                              PRICE_SOURCE_FQ,
                                              gnc_numeric_create(7805, 100)));
    gnc_pricedb_add_price(db, construct_price(book, c->amzn, c->usd,
                                              gnc_dmy2time64(23, 8, 2010),
                                              PRICE_SOURCE_FQ,
                                              gnc_numeric_create(12664, 100)));
    gnc_pricedb_add_price(db, construct_price(book, c->amzn, c->usd,
                                              gnc_dmy2time64(25, 7, 2011),
                                              PRICE_SOURCE_FQ,
                                              gnc_numeric_create(22252, 100)));
    gnc_pricedb_add_price(db, construct_price(book, c->amzn, c->usd,
                                              gnc_dmy2time64(19, 11, 2012),
                                              PRICE_SOURCE_FQ,
                                              gnc_numeric_create(23988, 100)));
    gnc_pricedb_add_price(db, construct_price(book, c->amzn, c->usd,
                                              gnc_dmy2time64(5, 8, 2013),
                                              PRICE_SOURCE_FQ,
                                              gnc_numeric_create(29726, 100)));
    gnc_pricedb_add_price(db, construct_price(book, c->amzn, c->usd,
                                              gnc_dmy2time64(12, 11, 2014),
                                              PRICE_SOURCE_FQ,
                                              gnc_numeric_create(31151, 100)));
    gnc_pricedb_add_price(db, construct_price(book, c->gbp, c->eur,
                                              gnc_dmy2time64(12, 05, 2007),
                                              PRICE_SOURCE_USER_PRICE,
                                              gnc_numeric_create(126836, 100000)));
    gnc_pricedb_add_price(db, construct_price(book, c->gbp, c->eur,
                                              gnc_dmy2time64(12, 05, 2008),
                                              PRICE_SOURCE_FQ,
                                              gnc_numeric_create(126836, 100000)));
    gnc_pricedb_add_price(db, construct_price(book, c->gbp, c->eur,
                                              gnc_dmy2time64(13, 05, 2008),
                                              PRICE_SOURCE_USER_PRICE,
                                              gnc_numeric_create(126836, 100000)));
    gnc_pricedb_add_price(db, construct_price(book, c->gbp, c->eur,
                                              gnc_dmy2time64(14, 05, 2008),
                                              PRICE_SOURCE_USER_PRICE,
                                              gnc_numeric_create(126836, 100000)));
    gnc_pricedb_add_price(db, construct_price(book, c->gbp, c->eur,
                                              gnc_dmy2time64(19, 05, 2008),
                                              PRICE_SOURCE_FQ,
                                              gnc_numeric_create(126836, 100000)));
    gnc_pricedb_add_price(db, construct_price(book, c->gbp, c->eur,
                                              gnc_dmy2time64(22, 05, 2008),
                                              PRICE_SOURCE_FQ,
                                              gnc_numeric_create(126836, 100000)));
    gnc_pricedb_add_price(db, construct_price(book, c->gbp, c->eur,
                                              gnc_dmy2time64(12, 06, 2008),
                                              PRICE_SOURCE_FQ,
                                              gnc_numeric_create(126836, 100000)));
    gnc_pricedb_add_price(db, construct_price(book, c->gbp, c->eur,
                                              gnc_dmy2time64(12, 07, 2008),
                                              PRICE_SOURCE_FQ,
                                              gnc_numeric_create(126836, 100000)));
    gnc_pricedb_add_price(db, construct_price(book, c->gbp, c->eur,
                                              gnc_dmy2time64(12, 11, 2008),
                                              PRICE_SOURCE_FQ,
                                              gnc_numeric_create(126836, 100000)));
    gnc_pricedb_set_bulk_update(db, FALSE);
}

static void
setup(PriceDBFixture *fixture, gconstpointer data)
{
    QofBook *book = NULL;
    GNCPrice *price = NULL;
    gnc_pricedb_register();
    book = qof_book_new();
    fixture->com = setup_commodities(book);
    fixture->pricedb = gnc_pricedb_get_db(book);
    create_some_prices(fixture);
}

static void
teardown(PriceDBFixture *fixture, gconstpointer data)
{
    QofBook *book = qof_instance_get_book(fixture->pricedb);
    qof_book_destroy(book);
    g_free(fixture->com);
}
/* gnc_pricedb_init
static void
gnc_pricedb_init(GNCPriceDB* pdb)*/
/* static void
test_gnc_pricedb_init (Fixture *fixture, gconstpointer pData)
{
}*/
// Not Used
/* gnc_pricedb_dispose_real
static void
gnc_pricedb_dispose_real (GObject *pdbp)// Local: 0:0:0
*/
// Not Used
/* gnc_pricedb_finalize_real
static void
gnc_pricedb_finalize_real(GObject* pdbp)// Local: 0:0:0
*/
/* gnc_pricedb_create
static GNCPriceDB *
gnc_pricedb_create(QofBook * book)// Local: 1:0:0
*/
/* static void
test_gnc_pricedb_create (Fixture *fixture, gconstpointer pData)
{
}*/
/* destroy_pricedb_currency_hash_data
static void
destroy_pricedb_currency_hash_data(gpointer key,// Local: 0:1:0
*/
/* static void
test_destroy_pricedb_currency_hash_data (Fixture *fixture, gconstpointer pData)
{
}*/
/* destroy_pricedb_commodity_hash_data
static void
destroy_pricedb_commodity_hash_data(gpointer key,// Local: 0:1:0
*/
/* static void
test_destroy_pricedb_commodity_hash_data (Fixture *fixture, gconstpointer pData)
{
}*/
/* gnc_pricedb_destroy
void
gnc_pricedb_destroy(GNCPriceDB *db)// C: 2 in 2  Local: 1:0:0
*/
/* static void
test_gnc_pricedb_destroy (Fixture *fixture, gconstpointer pData)
{
}*/
/* gnc_pricedb_set_bulk_update
void
gnc_pricedb_set_bulk_update(GNCPriceDB *db, gboolean bulk_update)// C: 4 in 2  Local: 0:0:0
*/
/* static void
test_gnc_pricedb_set_bulk_update (Fixture *fixture, gconstpointer pData)
{
}*/
/* gnc_collection_get_pricedb
GNCPriceDB *
gnc_collection_get_pricedb(QofCollection *col)// Local: 1:0:0
*/
/* static void
test_gnc_collection_get_pricedb (Fixture *fixture, gconstpointer pData)
{
}*/
/* gnc_pricedb_get_db
GNCPriceDB *
gnc_pricedb_get_db(QofBook *book)// C: 26 in 18 SCM: 8 in 6 Local: 0:0:0
*/
/* static void
test_gnc_pricedb_get_db (Fixture *fixture, gconstpointer pData)
{
}*/
/* num_prices_helper
static gboolean
num_prices_helper (GNCPrice *p, gpointer user_data)// Local: 0:1:0
*/
/* static void
test_num_prices_helper (Fixture *fixture, gconstpointer pData)
{
}*/
/* gnc_pricedb_get_num_prices
guint
gnc_pricedb_get_num_prices(GNCPriceDB *db)// C: 2 in 1  Local: 0:0:0
*/
static void
test_gnc_pricedb_get_num_prices (PriceDBFixture *fixture, gconstpointer pData)
{
    int num = gnc_pricedb_get_num_prices(fixture->pricedb);
    g_assert_cmpint(num, ==, 42);
}
/* pricedb_equal_foreach_pricelist
static void
pricedb_equal_foreach_pricelist(gpointer key, gpointer val, gpointer user_data)// Local: 0:1:0
*/
/* static void
test_pricedb_equal_foreach_pricelist (Fixture *fixture, gconstpointer pData)
{
}*/
/* pricedb_equal_foreach_currencies_hash
static void
pricedb_equal_foreach_currencies_hash (gpointer key, gpointer val,// Local: 0:1:0
*/
/* static void
test_pricedb_equal_foreach_currencies_hash (Fixture *fixture, gconstpointer pData)
{
}*/
// Not Used
/* gnc_pricedb_equal
gboolean
gnc_pricedb_equal (GNCPriceDB *db1, GNCPriceDB *db2)// Local: 0:0:0
*/
/* insert_or_replace_price
static gboolean
insert_or_replace_price(GNCPriceDB *db, GNCPrice *p)// Local: 1:0:0
*/
/* static void
test_insert_or_replace_price (Fixture *fixture, gconstpointer pData)
{
}*/
/* add_price
static gboolean
add_price(GNCPriceDB *db, GNCPrice *p)// Local: 4:0:0
*/
/* static void
test_add_price (Fixture *fixture, gconstpointer pData)
{
}*/
/* gnc_pricedb_add_price
gboolean
gnc_pricedb_add_price(GNCPriceDB *db, GNCPrice *p)// C: 7 in 7 SCM: 1  Local: 0:0:0
*/
/* static void
test_gnc_pricedb_add_price (Fixture *fixture, gconstpointer pData)
{
}*/
/* remove_price
static gboolean
remove_price(GNCPriceDB *db, GNCPrice *p, gboolean cleanup)// Local: 4:0:0
*/
/* static void
test_remove_price (Fixture *fixture, gconstpointer pData)
{
}*/
/* gnc_pricedb_remove_price
gboolean
gnc_pricedb_remove_price(GNCPriceDB *db, GNCPrice *p)// C: 2 in 2  Local: 1:0:0
*/
/* static void
test_gnc_pricedb_remove_price (Fixture *fixture, gconstpointer pData)
{
}*/
/* check_one_price_date
static gboolean
check_one_price_date (GNCPrice *price, gpointer user_data)// Local: 0:1:0
*/
/* static void
test_check_one_price_date (Fixture *fixture, gconstpointer pData)
{
}*/
/* pricedb_remove_foreach_pricelist
static void
pricedb_remove_foreach_pricelist (gpointer key,// Local: 0:1:0
*/
/* static void
test_pricedb_remove_foreach_pricelist (Fixture *fixture, gconstpointer pData)
{
}*/
/* pricedb_remove_foreach_currencies_hash
static void
pricedb_remove_foreach_currencies_hash (gpointer key,// Local: 0:1:0
*/
/* static void
test_pricedb_remove_foreach_currencies_hash (Fixture *fixture, gconstpointer pData)
{
}*/
/* gnc_pricedb_remove_old_prices
gboolean
gnc_pricedb_remove_old_prices(GNCPriceDB *db,// C: 1  Local: 0:0:0
*/
static void test_gnc_pricedb_remove_old_prices (PriceDBFixture *fixture, gconstpointer pData)
{
    GList *comm_list = NULL;
    Commodities *c = fixture->com;
    PriceRemoveSourceFlags source_all = PRICE_REMOVE_SOURCE_FQ |
                                        PRICE_REMOVE_SOURCE_USER |
                                        PRICE_REMOVE_SOURCE_APP;

    time64 t_cut = gnc_dmy2time64(1, 1, 2008);
    time64 t_cut1 = gnc_dmy2time64(1, 1, 2009);
    time64 t_cut2 = gnc_dmy2time64(1, 1, 2010);

    GDate *fiscal_end_date = g_date_new ();
    g_date_set_dmy (fiscal_end_date, 31, 12, 2017);

    comm_list = g_list_append (comm_list, c->usd);
    comm_list = g_list_append (comm_list, c->gbp);

    g_assert_cmpint (gnc_pricedb_num_prices(fixture->pricedb, c->gbp), ==, 23);
    g_assert_cmpint (gnc_pricedb_num_prices(fixture->pricedb, c->usd), ==, 11);

    g_assert_cmpint (gnc_pricedb_get_num_prices(fixture->pricedb), ==, 42);

    g_assert (gnc_pricedb_remove_old_prices(fixture->pricedb, comm_list,
                                           fiscal_end_date, t_cut1,
                                           PRICE_REMOVE_SOURCE_USER, // source is USER
                                           PRICE_REMOVE_KEEP_NONE)); // keep none

    g_assert_cmpint (gnc_pricedb_get_num_prices(fixture->pricedb), ==, 39);

    // there should be no prices before cutoff, returns false
    g_assert (!gnc_pricedb_remove_old_prices(fixture->pricedb, comm_list,
                                           NULL, t_cut,
                                           PRICE_REMOVE_SOURCE_FQ,   // source is FQ
                                           PRICE_REMOVE_KEEP_NONE)); // keep none

    g_assert_cmpint (gnc_pricedb_get_num_prices(fixture->pricedb), ==, 39);

    g_assert (gnc_pricedb_remove_old_prices(fixture->pricedb, comm_list,
                                           fiscal_end_date, t_cut1,
                                           source_all,                      // source is ALL
                                           PRICE_REMOVE_KEEP_LAST_WEEKLY)); // keep last of week

    g_assert_cmpint (gnc_pricedb_get_num_prices(fixture->pricedb), ==, 38);

    g_assert (gnc_pricedb_remove_old_prices(fixture->pricedb, comm_list,
                                           fiscal_end_date, t_cut2,
                                           PRICE_REMOVE_SOURCE_FQ,           // source is FQ
                                           PRICE_REMOVE_KEEP_LAST_MONTHLY)); // keep last of month

    g_assert_cmpint (gnc_pricedb_get_num_prices(fixture->pricedb), ==, 37);

    g_assert (gnc_pricedb_remove_old_prices(fixture->pricedb, comm_list,
                                           fiscal_end_date, t_cut2,
                                           source_all,                         // source is all
                                           PRICE_REMOVE_KEEP_LAST_QUARTERLY)); // keep last of quarter

    g_assert_cmpint (gnc_pricedb_get_num_prices(fixture->pricedb), ==, 35);

    g_assert (gnc_pricedb_remove_old_prices(fixture->pricedb, comm_list,
                                           fiscal_end_date, t_cut2,
                                           source_all,                      // source is all
                                           PRICE_REMOVE_KEEP_LAST_PERIOD)); // keep last of period

    g_assert_cmpint (gnc_pricedb_get_num_prices(fixture->pricedb), ==, 33);

    g_list_free (comm_list);
    g_date_free (fiscal_end_date);
}
/* price_list_from_hashtable
static PriceList *
price_list_from_hashtable (GHashTable *hash, const gnc_commodity *currency)// Local: 2:0:0
*/
/* static void
test_price_list_from_hashtable (Fixture *fixture, gconstpointer pData)
{
}*/
/* pricedb_get_prices_internal
static PriceList*
pricedb_get_prices_internal(GNCPriceDB *db, const gnc_commodity *commodity,// Local: 5:0:0
*/
/* static void
test_pricedb_get_prices_internal (Fixture *fixture, gconstpointer pData)
{
}*/
/* gnc_pricedb_lookup_latest
GNCPrice *
gnc_pricedb_lookup_latest(GNCPriceDB *db,// C: 12 in 7  Local: 1:0:0
*/
static void
test_gnc_pricedb_lookup_latest (PriceDBFixture *fixture, gconstpointer pData)
{
    GNCPrice *price2, *price = gnc_pricedb_lookup_latest(fixture->pricedb,
                                                         fixture->com->gbp,
                                                         fixture->com->eur);
    time64 t = gnc_dmy2time64(12, 11, 2014);
    time64 price_time = gnc_price_get_time64(price);
    g_assert(gnc_price_get_commodity(price) == fixture->com->gbp);
    g_assert(gnc_price_get_currency(price) == fixture->com->eur);
    g_assert_cmpint(price_time, ==, t);
    price2 = gnc_pricedb_lookup_latest(fixture->pricedb, fixture->com->eur,
                                      fixture->com->gbp);
    g_assert(price2 == price);
    gnc_price_unref(price);
    gnc_price_unref(price2);
}
// Not Used
/* lookup_latest
static void
lookup_latest(gpointer key, gpointer val, gpointer user_data)// Local: 0:0:0
*/
/* price_uses_commodity
static gboolean
price_uses_commodity(GNCPrice *price, gpointer data)// Local: 0:3:0
*/
/* static void
test_price_uses_commodity (Fixture *fixture, gconstpointer pData)
{
}*/
/* is_in_list
static gboolean
is_in_list (GList *list, const gnc_commodity *c)// Local: 2:0:0
*/
/* static void
test_is_in_list (Fixture *fixture, gconstpointer pData)
{
}*/
/* latest_before
static PriceList*
latest_before (PriceList *prices, const gnc_commodity* target, time64 t)// Local: 2:0:0
*/
/* static void
test_latest_before (Fixture *fixture, gconstpointer pData)
{
}*/
/* find_comtime
static GNCPrice**
find_comtime(GArray* array, gnc_commodity *com)// Local: 1:0:0
*/
/* static void
test_find_comtime (Fixture *fixture, gconstpointer pData)
{
}*/
/* add_nearest_price
static GList*
add_nearest_price(GList *target_list, GArray *price_array, GNCPrice *price,// Local: 1:0:0
*/
/* static void
test_add_nearest_price (Fixture *fixture, gconstpointer pData)
{
}*/
/* nearest_to
static PriceList *
nearest_to (PriceList *prices, const gnc_commodity* target, time64 t)// Local: 1:0:0
*/
/* static void
test_nearest_to (PriceDBFixture *fixture, gconstpointer pData)
{
}*/
#define GET_COM_NAME(x) \
    gnc_commodity_get_mnemonic(gnc_price_get_commodity(x))
#define GET_CUR_NAME(x)                                         \
    gnc_commodity_get_mnemonic(gnc_price_get_currency(x))
static int
compare_price_commodities(gconstpointer a, gconstpointer b)
{
    GNCPrice *p1 = GNC_PRICE(a);
    GNCPrice *p2 = GNC_PRICE(b);
    const gchar *p1other = g_strcmp0(GET_COM_NAME(p1), "USD") == 0 ?
        GET_CUR_NAME(p1) : GET_COM_NAME(p1);
    const gchar *p2other = g_strcmp0(GET_COM_NAME(p2), "USD") == 0 ?
        GET_CUR_NAME(p2) : GET_COM_NAME(p2);
    return g_strcmp0(p1other, p2other);
}
/* gnc_pricedb_lookup_latest_any_currency
PriceList *
gnc_pricedb_lookup_latest_any_currency(GNCPriceDB *db,// C: 4 in 4 SCM: 1  Local: 2:0:0
*/
static void
test_gnc_pricedb_lookup_latest_any_currency (PriceDBFixture *fixture, gconstpointer pData)
{
    PriceList *prices =
        gnc_pricedb_lookup_latest_any_currency(fixture->pricedb,
                                               fixture->com->usd);
    g_assert_cmpint(g_list_length(prices), ==, 4);
    prices = g_list_sort(prices, compare_price_commodities);
    g_assert_cmpstr(GET_COM_NAME(prices->data), ==, "AMZN");
    g_assert_cmpstr(GET_CUR_NAME(prices->next->data), ==, "AUD");
    g_assert_cmpstr(GET_CUR_NAME(prices->next->next->data), ==, "DKK");
    g_assert_cmpstr(GET_COM_NAME(prices->next->next->next->data), ==, "GBP");
    gnc_price_list_destroy(prices);
}
// Make Static
/* gnc_pricedb_lookup_nearest_in_time_any_currency_t64
PriceList *
gnc_pricedb_lookup_nearest_in_time_any_currency_t64(GNCPriceDB *db,// Local: 2:0:0
*/
static void
test_gnc_pricedb_lookup_nearest_in_time_any_currency_t64 (PriceDBFixture *fixture, gconstpointer pData)
{
    time64 t1 = gnc_dmy2time64(25, 3, 2013);
    time64 t2 = gnc_dmy2time64(26, 3, 2013);
    PriceList *prices =
        gnc_pricedb_lookup_nearest_in_time_any_currency_t64(fixture->pricedb,
                                                            fixture->com->usd,
                                                            t1);
    g_assert_cmpint(g_list_length(prices), ==, 4);
    prices = g_list_sort(prices, compare_price_commodities);
    g_assert_cmpstr(GET_COM_NAME(prices->next->data), ==, "AUD");
    g_assert_cmpstr(GET_CUR_NAME(prices->next->data), ==, "USD");
    gnc_price_list_destroy(prices);
    prices =
        gnc_pricedb_lookup_nearest_in_time_any_currency_t64(fixture->pricedb,
                                                            fixture->com->usd,
                                                            t2);
    g_assert_cmpint(g_list_length(prices), ==, 4);
    prices = g_list_sort(prices, compare_price_commodities);
    g_assert_cmpstr(GET_CUR_NAME(prices->next->data), ==, "AUD");
    g_assert_cmpstr(GET_COM_NAME(prices->next->data), ==, "USD");
    gnc_price_list_destroy(prices);
}

// Not Used
/* gnc_pricedb_lookup_latest_before_any_currency_t64
PriceList *
gnc_pricedb_lookup_latest_before_any_currency_t64(GNCPriceDB *db,// Local: 0:0:0
*/
static void
test_gnc_pricedb_lookup_latest_before_any_currency_t64 (PriceDBFixture *fixture,
                                                    gconstpointer pData)
{
    time64 t1 = gnc_dmy2time64(31, 7, 2013);
    time64 t2 = gnc_dmy2time64(5, 8, 2013);
    PriceList *prices =
        gnc_pricedb_lookup_latest_before_any_currency_t64(fixture->pricedb,
                                                        fixture->com->usd, t1);
    g_assert_cmpint(g_list_length(prices), ==, 4);
    prices = g_list_sort(prices, compare_price_commodities);
    g_assert_cmpstr(GET_COM_NAME(prices->next->data), ==, "AUD");
    g_assert_cmpstr(GET_CUR_NAME(prices->next->data), ==, "USD");
    gnc_price_list_destroy(prices);
    prices =
        gnc_pricedb_lookup_latest_before_any_currency_t64(fixture->pricedb,
                                                        fixture->com->usd, t2);
    g_assert_cmpint(g_list_length(prices), ==, 4);
    prices = g_list_sort(prices, compare_price_commodities);
    g_assert_cmpstr(GET_CUR_NAME(prices->next->data), ==, "AUD");
    g_assert_cmpstr(GET_COM_NAME(prices->next->data), ==, "USD");
    gnc_price_list_destroy(prices);
}
/* hash_values_helper
static void
hash_values_helper(gpointer key, gpointer value, gpointer data)// Local: 0:1:0
*/
/* static void
test_hash_values_helper (Fixture *fixture, gconstpointer pData)
{
}*/
/* gnc_pricedb_has_prices
gboolean
gnc_pricedb_has_prices(GNCPriceDB *db,// C: 4 in 3 SCM: 1  Local: 0:0:0
*/
static void
test_gnc_pricedb_has_prices (PriceDBFixture *fixture, gconstpointer pData)
{
    g_assert(gnc_pricedb_has_prices(fixture->pricedb, fixture->com->usd,
                                    fixture->com->dkk));
    g_assert(!gnc_pricedb_has_prices(fixture->pricedb, fixture->com->usd,
                                     fixture->com->gbp));
}
/* gnc_pricedb_get_prices
PriceList *
gnc_pricedb_get_prices(GNCPriceDB *db,// C: 6 in 1  Local: 1:0:0
*/
static void
test_gnc_pricedb_get_prices (PriceDBFixture *fixture, gconstpointer pData)
{
    PriceList *prices = gnc_pricedb_get_prices(fixture->pricedb,
                                               fixture->com->usd,
                                               fixture->com->aud);
    g_assert_cmpint(g_list_length(prices), ==, 5);
    gnc_price_list_destroy(prices);
}
/* gnc_pricedb_lookup_day_t64
GNCPrice *
gnc_pricedb_lookup_day_t64(GNCPriceDB *db,// C: 4 in 2 SCM: 2 in 1 Local: 1:0:0
*/
static void
test_gnc_pricedb_lookup_day_t64 (PriceDBFixture *fixture, gconstpointer pData)
{
    gchar *msg1 = "[gnc_dmy2time64_internal()] Date computation error from Y-M-D 12-11-18: Year is out of valid range: 1400..9999";
    gint loglevel = G_LOG_LEVEL_WARNING | G_LOG_FLAG_FATAL;
    gchar *logdomain = "qof.engine";
    TestErrorStruct check = {loglevel, logdomain, msg1, 0};
    GLogFunc hdlr = g_log_set_default_handler ((GLogFunc)test_null_handler, &check);
    time64 t = gnc_dmy2time64(17, 11, 2012);
    GNCPrice *price = gnc_pricedb_lookup_day_t64(fixture->pricedb,
                                             fixture->com->usd,
                                             fixture->com->gbp, t);
    g_assert_cmpstr(GET_COM_NAME(price), ==, "GBP");
    t += 20247; /* A random number of seconds later, still the same day.*/
    price = gnc_pricedb_lookup_day_t64(fixture->pricedb,
                                   fixture->com->usd,
                                   fixture->com->gbp, t);
    g_assert_cmpstr(GET_COM_NAME(price), ==, "GBP");
    g_test_log_set_fatal_handler ((GTestLogFatalFunc)test_checked_handler, &check);
    t = gnc_dmy2time64(18, 11, 12);
    price = gnc_pricedb_lookup_day_t64(fixture->pricedb,
                                   fixture->com->usd,
                                   fixture->com->gbp, t);
    g_assert(price == NULL);
    g_log_set_default_handler (hdlr, 0);
}

// Not Used
/* gnc_pricedb_lookup_at_time64
GNCPrice *
gnc_pricedb_lookup_at_time64(GNCPriceDB *db,// Local: 0:0:0
*/
/* lookup_nearest_in_time
static GNCPrice *
lookup_nearest_in_time(GNCPriceDB *db,// Local: 2:0:0
*/
/* test_lookup_nearest_in_time (Fixture *fixture, gconstpointer pData)
{
}*/
/* gnc_pricedb_lookup_nearest_in_time64
GNCPrice *
gnc_pricedb_lookup_nearest_in_time64(GNCPriceDB *db,// C: 2 in 1  Local: 1:0:0
*/
static void
test_gnc_pricedb_lookup_nearest_in_time64 (PriceDBFixture *fixture, gconstpointer pData)
{
    time64 t1 = gnc_dmy2time64(25, 3, 2013);
    time64 t2 = gnc_dmy2time64(26, 3, 2013);
    GNCPrice *price =
        gnc_pricedb_lookup_nearest_in_time64(fixture->pricedb,
                                             fixture->com->usd,
                                             fixture->com->aud, t1);
    g_assert_cmpstr(GET_COM_NAME(price), ==, "AUD");
    g_assert_cmpstr(GET_CUR_NAME(price), ==, "USD");
    price =
        gnc_pricedb_lookup_nearest_in_time64(fixture->pricedb,
                                             fixture->com->usd,
                                             fixture->com->aud, t2);
    g_assert_cmpstr(GET_CUR_NAME(price), ==, "AUD");
    g_assert_cmpstr(GET_COM_NAME(price), ==, "USD");
}
// Not Used
/* gnc_pricedb_lookup_latest_before_t64
GNCPrice *
gnc_pricedb_lookup_latest_before_t64 (GNCPriceDB *db,// Local: 0:0:0
*/
/* direct_balance_conversion
static gnc_numeric
direct_balance_conversion (GNCPriceDB *db, gnc_numeric bal,// Local: 2:0:0
*/
/* static void
test_direct_balance_conversion (Fixture *fixture, gconstpointer pData)
{
}*/
/* extract_common_prices
static PriceTuple
extract_common_prices (PriceList *from_prices, PriceList *to_prices)// Local: 1:0:0
*/
/* static void
test_extract_common_prices (Fixture *fixture, gconstpointer pData)
{
}*/
/* convert_balance
static gnc_numeric
convert_balance(gnc_numeric bal, const gnc_commodity *from,// Local: 1:0:0
*/
/* static void
test_convert_balance (Fixture *fixture, gconstpointer pData)
{
}*/
/* indirect_balance_conversion
static gnc_numeric
indirect_balance_conversion (GNCPriceDB *db, gnc_numeric bal,// Local: 2:0:0
*/
/* static void
test_indirect_balance_conversion (Fixture *fixture, gconstpointer pData)
{
}*/
/* gnc_pricedb_convert_balance_latest_price
gnc_numeric
gnc_pricedb_convert_balance_latest_price(GNCPriceDB *pdb,// C: 2 in 2  Local: 0:0:0
*/
static void
test_gnc_pricedb_convert_balance_latest_price (PriceDBFixture *fixture, gconstpointer pData)
{
    gnc_numeric from = gnc_numeric_create(10000, 100);
    gnc_numeric result =
        gnc_pricedb_convert_balance_latest_price(fixture->pricedb, from,
                                                 fixture->com->usd,
                                                 fixture->com->aud);
    g_assert_cmpint(result.num, ==, 11478);
    g_assert_cmpint(result.denom, ==, 100);
    result = gnc_pricedb_convert_balance_latest_price(fixture->pricedb, from,
                                                      fixture->com->usd,
                                                      fixture->com->gbp);
    g_assert_cmpint(result.num, ==, 6343);
    g_assert_cmpint(result.denom, ==, 100);
    result = gnc_pricedb_convert_balance_latest_price(fixture->pricedb, from,
                                                      fixture->com->usd,
                                                      fixture->com->eur);
    g_assert_cmpint(result.num, ==, 8045);
    g_assert_cmpint(result.denom, ==, 100);
    result = gnc_pricedb_convert_balance_latest_price(fixture->pricedb, from,
                                                      fixture->com->gbp,
                                                      fixture->com->dkk);
    g_assert_cmpint(result.num, ==, 94389);
    g_assert_cmpint(result.denom, ==, 100);
    result = gnc_pricedb_convert_balance_latest_price(fixture->pricedb, from,
                                                      fixture->com->amzn,
                                                      fixture->com->aud);
    g_assert_cmpint(result.num, ==, 3575636);
    g_assert_cmpint(result.denom, ==, 100);


}
/* gnc_pricedb_convert_balance_nearest_price_t64
gnc_numeric
gnc_pricedb_convert_balance_nearest_price_t64(GNCPriceDB *pdb,// C: 1  Local: 0:0:0
*/
static void
test_gnc_pricedb_convert_balance_nearest_price_t64 (PriceDBFixture *fixture, gconstpointer pData)
{
    time64 t = gnc_dmy2time64(15, 8, 2011);
    gnc_numeric from = gnc_numeric_create(10000, 100);
    gnc_numeric result =
        gnc_pricedb_convert_balance_nearest_price_t64(fixture->pricedb, from,
                                                      fixture->com->usd,
                                                      fixture->com->aud, t);
    g_assert_cmpint(result.num, ==, 9391);
    g_assert_cmpint(result.denom, ==, 100);
    result = gnc_pricedb_convert_balance_nearest_price_t64(fixture->pricedb,
                                                           from,
                                                           fixture->com->usd,
                                                           fixture->com->gbp,
                                                           t);
    g_assert_cmpint(result.num, ==, 6186);
    g_assert_cmpint(result.denom, ==, 100);
    result = gnc_pricedb_convert_balance_nearest_price_t64(fixture->pricedb,
                                                           from,
                                                           fixture->com->usd,
                                                           fixture->com->eur,
                                                           t);
    g_assert_cmpint(result.num, ==, 7009);
    g_assert_cmpint(result.denom, ==, 100);
    result = gnc_pricedb_convert_balance_nearest_price_t64(fixture->pricedb,
                                                           from,
                                                           fixture->com->gbp,
                                                           fixture->com->dkk,
                                                           t);
    g_assert_cmpint(result.num, ==, 84450);
    g_assert_cmpint(result.denom, ==, 100);
    result = gnc_pricedb_convert_balance_nearest_price_t64(fixture->pricedb,
                                                           from,
                                                           fixture->com->amzn,
                                                           fixture->com->aud,
                                                           t);
    g_assert_cmpint(result.num, ==, 2089782);
    g_assert_cmpint(result.denom, ==, 100);

}
/* pricedb_foreach_pricelist
static void
pricedb_foreach_pricelist(gpointer key, gpointer val, gpointer user_data)// Local: 0:1:0
*/
/* static void
test_pricedb_foreach_pricelist (Fixture *fixture, gconstpointer pData)
{
}*/
/* pricedb_foreach_currencies_hash
static void
pricedb_foreach_currencies_hash(gpointer key, gpointer val, gpointer user_data)// Local: 0:1:0
*/
/* static void
test_pricedb_foreach_currencies_hash (Fixture *fixture, gconstpointer pData)
{
}*/
/* unstable_price_traversal
static gboolean
unstable_price_traversal(GNCPriceDB *db,// Local: 1:0:0
*/
/* static void
test_unstable_price_traversal (Fixture *fixture, gconstpointer pData)
{
}*/
/* compare_kvpairs_by_commodity_key
static gint
compare_kvpairs_by_commodity_key(gconstpointer a, gconstpointer b)// Local: 0:2:0
*/
/* static void
test_compare_kvpairs_by_commodity_key (Fixture *fixture, gconstpointer pData)
{
}*/
/* stable_price_traversal
static gboolean
stable_price_traversal(GNCPriceDB *db,// Local: 1:0:0
*/
/* static void
test_stable_price_traversal (Fixture *fixture, gconstpointer pData)
{
}*/
/* gnc_pricedb_foreach_price
gboolean
gnc_pricedb_foreach_price(GNCPriceDB *db,// C: 2 in 2  Local: 6:0:0
*/
/* static void
test_gnc_pricedb_foreach_price (Fixture *fixture, gconstpointer pData)
{
}*/
/* add_price_to_list
static gboolean
add_price_to_list (GNCPrice *p, gpointer data)// Local: 0:1:0
*/
/* static void
test_add_price_to_list (Fixture *fixture, gconstpointer pData)
{
}*/
/* gnc_price_fixup_legacy_commods
static void
gnc_price_fixup_legacy_commods(gpointer data, gpointer user_data)// Local: 0:1:0
*/
/* static void
test_gnc_price_fixup_legacy_commods (Fixture *fixture, gconstpointer pData)
{
}*/
// Not Used
/* gnc_pricedb_substitute_commodity
void
gnc_pricedb_substitute_commodity(GNCPriceDB *db,// Local: 0:0:0
*/
/* gnc_price_print
void
gnc_price_print(GNCPrice *p, FILE *f, int indent)// Local: 1:0:0
*/
/* static void
test_gnc_price_print (Fixture *fixture, gconstpointer pData)
{
}*/
/* print_pricedb_adapter
static gboolean
print_pricedb_adapter(GNCPrice *p, gpointer user_data)// Local: 0:1:0
*/
/* static void
test_print_pricedb_adapter (Fixture *fixture, gconstpointer pData)
{
}*/
/* gnc_pricedb_print_contents
void
gnc_pricedb_print_contents(GNCPriceDB *db, FILE *f)// C: 1  Local: 0:0:0
*/
/* static void
test_gnc_pricedb_print_contents (Fixture *fixture, gconstpointer pData)
{
}*/
/* pricedb_book_begin
static void
pricedb_book_begin (QofBook *book)// Local: 0:1:0
*/
/* static void
test_pricedb_book_begin (Fixture *fixture, gconstpointer pData)
{
}*/
/* pricedb_book_end
static void
pricedb_book_end (QofBook *book)// Local: 0:1:0
*/
/* static void
test_pricedb_book_end (Fixture *fixture, gconstpointer pData)
{
}*/
/* price_create
static gpointer
price_create (QofBook *book)// Local: 0:1:0
*/
/* static void
test_price_create (Fixture *fixture, gconstpointer pData)
{
}*/
/* void_pricedb_foreach_pricelist
static void
void_pricedb_foreach_pricelist(gpointer key, gpointer val, gpointer user_data)// Local: 0:1:0
*/
/* static void
test_void_pricedb_foreach_pricelist (Fixture *fixture, gconstpointer pData)
{
}*/
/* void_pricedb_foreach_currencies_hash
static void
void_pricedb_foreach_currencies_hash(gpointer key, gpointer val, gpointer user_data)// Local: 0:1:0
*/
/* static void
test_void_pricedb_foreach_currencies_hash (Fixture *fixture, gconstpointer pData)
{
}*/
/* void_unstable_price_traversal
static void
void_unstable_price_traversal(GNCPriceDB *db,// Local: 1:0:0
*/
/* static void
test_void_unstable_price_traversal (Fixture *fixture, gconstpointer pData)
{
}*/
/* price_foreach
static void
price_foreach(const QofCollection *col, QofInstanceForeachCB cb, gpointer data)// Local: 0:1:0
*/
/* static void
test_price_foreach (Fixture *fixture, gconstpointer pData)
{
}*/
/* price_printable
static const char *
price_printable(gpointer obj)// Local: 0:1:0
*/
/* static void
test_price_printable (Fixture *fixture, gconstpointer pData)
{
}*/
/* gnc_pricedb_register
gboolean
gnc_pricedb_register (void)// C: 1  Local: 0:0:0
*/
/* static void
test_gnc_pricedb_register (Fixture *fixture, gconstpointer pData)
{
}*/


void
test_suite_gnc_pricedb (void)
{

// GNC_TEST_ADD (suitename, "gnc price init", Fixture, NULL, setup, test_gnc_price_init, teardown);
// GNC_TEST_ADD (suitename, "gnc price dispose", Fixture, NULL, setup, test_gnc_price_dispose, teardown);
// GNC_TEST_ADD (suitename, "gnc price finalize", Fixture, NULL, setup, test_gnc_price_finalize, teardown);
// GNC_TEST_ADD (suitename, "gnc price get property", Fixture, NULL, setup, test_gnc_price_get_property, teardown);
// GNC_TEST_ADD (suitename, "gnc price set property", Fixture, NULL, setup, test_gnc_price_set_property, teardown);
// GNC_TEST_ADD (suitename, "gnc price class init", Fixture, NULL, setup, test_gnc_price_class_init, teardown);
// GNC_TEST_ADD (suitename, "gnc price create", Fixture, NULL, setup, test_gnc_price_create, teardown);
// GNC_TEST_ADD (suitename, "gnc price destroy", Fixture, NULL, setup, test_gnc_price_destroy, teardown);
// GNC_TEST_ADD (suitename, "gnc price ref", Fixture, NULL, setup, test_gnc_price_ref, teardown);
// GNC_TEST_ADD (suitename, "gnc price unref", Fixture, NULL, setup, test_gnc_price_unref, teardown);
// GNC_TEST_ADD (suitename, "gnc price clone", Fixture, NULL, setup, test_gnc_price_clone, teardown);
// GNC_TEST_ADD (suitename, "gnc price begin edit", Fixture, NULL, setup, test_gnc_price_begin_edit, teardown);
// GNC_TEST_ADD (suitename, "commit err", Fixture, NULL, setup, test_commit_err, teardown);
// GNC_TEST_ADD (suitename, "gnc price commit edit", Fixture, NULL, setup, test_gnc_price_commit_edit, teardown);
// GNC_TEST_ADD (suitename, "gnc pricedb begin edit", Fixture, NULL, setup, test_gnc_pricedb_begin_edit, teardown);
// GNC_TEST_ADD (suitename, "gnc pricedb commit edit", Fixture, NULL, setup, test_gnc_pricedb_commit_edit, teardown);
// GNC_TEST_ADD (suitename, "gnc price set dirty", Fixture, NULL, setup, test_gnc_price_set_dirty, teardown);
// GNC_TEST_ADD (suitename, "gnc price set commodity", Fixture, NULL, setup, test_gnc_price_set_commodity, teardown);
// GNC_TEST_ADD (suitename, "gnc price set currency", Fixture, NULL, setup, test_gnc_price_set_currency, teardown);
// GNC_TEST_ADD (suitename, "gnc price set time", Fixture, NULL, setup, test_gnc_price_set_time64, teardown);
// GNC_TEST_ADD (suitename, "gnc price set source", Fixture, NULL, setup, test_gnc_price_set_source, teardown);
// GNC_TEST_ADD (suitename, "gnc price set source string", Fixture, NULL, setup, test_gnc_price_set_source_string, teardown);
// GNC_TEST_ADD (suitename, "gnc price set typestr", Fixture, NULL, setup, test_gnc_price_set_typestr, teardown);
// GNC_TEST_ADD (suitename, "gnc price set value", Fixture, NULL, setup, test_gnc_price_set_value, teardown);
// GNC_TEST_ADD (suitename, "gnc price lookup", Fixture, NULL, setup, test_gnc_price_lookup, teardown);
// GNC_TEST_ADD (suitename, "gnc price get commodity", Fixture, NULL, setup, test_gnc_price_get_commodity, teardown);
// GNC_TEST_ADD (suitename, "gnc price get time", Fixture, NULL, setup, test_gnc_price_get_time64, teardown);
// GNC_TEST_ADD (suitename, "gnc price get source", Fixture, NULL, setup, test_gnc_price_get_source, teardown);
// GNC_TEST_ADD (suitename, "gnc price get source string", Fixture, NULL, setup, test_gnc_price_get_source_string, teardown);
// GNC_TEST_ADD (suitename, "gnc price get typestr", Fixture, NULL, setup, test_gnc_price_get_typestr, teardown);
// GNC_TEST_ADD (suitename, "gnc price get value", Fixture, NULL, setup, test_gnc_price_get_value, teardown);
// GNC_TEST_ADD (suitename, "gnc price get currency", Fixture, NULL, setup, test_gnc_price_get_currency, teardown);
// GNC_TEST_ADD (suitename, "gnc price equal", Fixture, NULL, setup, test_gnc_price_equal, teardown);
// GNC_TEST_ADD (suitename, "compare prices by date", Fixture, NULL, setup, test_compare_prices_by_date, teardown);
// GNC_TEST_ADD (suitename, "price list is duplicate", Fixture, NULL, setup, test_price_list_is_duplicate, teardown);
// GNC_TEST_ADD (suitename, "gnc price list insert", Fixture, NULL, setup, test_gnc_price_list_insert, teardown);
// GNC_TEST_ADD (suitename, "gnc price list remove", Fixture, NULL, setup, test_gnc_price_list_remove, teardown);
// GNC_TEST_ADD (suitename, "price list destroy helper", Fixture, NULL, setup, test_price_list_destroy_helper, teardown);
// GNC_TEST_ADD (suitename, "gnc price list destroy", Fixture, NULL, setup, test_gnc_price_list_destroy, teardown);
// GNC_TEST_ADD (suitename, "gnc price list equal", Fixture, NULL, setup, test_gnc_price_list_equal, teardown);
// GNC_TEST_ADD (suitename, "gnc pricedb init", Fixture, NULL, setup, test_gnc_pricedb_init, teardown);
// GNC_TEST_ADD (suitename, "gnc pricedb create", Fixture, NULL, setup, test_gnc_pricedb_create, teardown);
// GNC_TEST_ADD (suitename, "destroy pricedb currency hash data", Fixture, NULL, setup, test_destroy_pricedb_currency_hash_data, teardown);
// GNC_TEST_ADD (suitename, "destroy pricedb commodity hash data", Fixture, NULL, setup, test_destroy_pricedb_commodity_hash_data, teardown);
// GNC_TEST_ADD (suitename, "gnc pricedb destroy", Fixture, NULL, setup, test_gnc_pricedb_destroy, teardown);
// GNC_TEST_ADD (suitename, "gnc pricedb set bulk update", Fixture, NULL, setup, test_gnc_pricedb_set_bulk_update, teardown);
// GNC_TEST_ADD (suitename, "gnc collection get pricedb", Fixture, NULL, setup, test_gnc_collection_get_pricedb, teardown);
// GNC_TEST_ADD (suitename, "gnc pricedb get db", Fixture, NULL, setup, test_gnc_pricedb_get_db, teardown);
// GNC_TEST_ADD (suitename, "num prices helper", Fixture, NULL, setup, test_num_prices_helper, teardown);
    GNC_TEST_ADD (suitename, "gnc pricedb get num prices", PriceDBFixture, NULL, setup, test_gnc_pricedb_get_num_prices, teardown);
// GNC_TEST_ADD (suitename, "pricedb equal foreach pricelist", Fixture, NULL, setup, test_pricedb_equal_foreach_pricelist, teardown);
// GNC_TEST_ADD (suitename, "pricedb equal foreach currencies hash", Fixture, NULL, setup, test_pricedb_equal_foreach_currencies_hash, teardown);
// GNC_TEST_ADD (suitename, "insert or replace price", Fixture, NULL, setup, test_insert_or_replace_price, teardown);
// GNC_TEST_ADD (suitename, "add price", Fixture, NULL, setup, test_add_price, teardown);
// GNC_TEST_ADD (suitename, "gnc pricedb add price", Fixture, NULL, setup, test_gnc_pricedb_add_price, teardown);
// GNC_TEST_ADD (suitename, "remove price", Fixture, NULL, setup, test_remove_price, teardown);
// GNC_TEST_ADD (suitename, "gnc pricedb remove price", Fixture, NULL, setup, test_gnc_pricedb_remove_price, teardown);
// GNC_TEST_ADD (suitename, "check one price date", Fixture, NULL, setup, test_check_one_price_date, teardown);
// GNC_TEST_ADD (suitename, "pricedb remove foreach pricelist", Fixture, NULL, setup, test_pricedb_remove_foreach_pricelist, teardown);
// GNC_TEST_ADD (suitename, "pricedb remove foreach currencies hash", Fixture, NULL, setup, test_pricedb_remove_foreach_currencies_hash, teardown);
    GNC_TEST_ADD (suitename, "gnc pricedb remove old prices", PriceDBFixture, NULL, setup, test_gnc_pricedb_remove_old_prices, teardown);
// GNC_TEST_ADD (suitename, "price list from hashtable", Fixture, NULL, setup, test_price_list_from_hashtable, teardown);
// GNC_TEST_ADD (suitename, "pricedb get prices internal", Fixture, NULL, setup, test_pricedb_get_prices_internal, teardown);
    GNC_TEST_ADD (suitename, "gnc pricedb lookup latest", PriceDBFixture, NULL, setup, test_gnc_pricedb_lookup_latest, teardown);
// GNC_TEST_ADD (suitename, "price uses commodity", Fixture, NULL, setup, test_price_uses_commodity, teardown);
// GNC_TEST_ADD (suitename, "is in list", Fixture, NULL, setup, test_is_in_list, teardown);
// GNC_TEST_ADD (suitename, "latest before", Fixture, NULL, setup, test_latest_before, teardown);
// GNC_TEST_ADD (suitename, "find comtime", Fixture, NULL, setup, test_find_comtime, teardown);
// GNC_TEST_ADD (suitename, "add nearest price", Fixture, NULL, setup, test_add_nearest_price, teardown);
// GNC_TEST_ADD (suitename, "nearest to", Fixture, NULL, setup, test_nearest_to, teardown);
    GNC_TEST_ADD (suitename, "gnc pricedb lookup latest any currency", PriceDBFixture, NULL, setup, test_gnc_pricedb_lookup_latest_any_currency, teardown);
    GNC_TEST_ADD (suitename, "gnc pricedb lookup nearest in time any currency", PriceDBFixture, NULL, setup, test_gnc_pricedb_lookup_nearest_in_time_any_currency_t64, teardown);
    GNC_TEST_ADD (suitename, "gnc pricedb lookup latest before any currency", PriceDBFixture, NULL, setup, test_gnc_pricedb_lookup_latest_before_any_currency_t64, teardown);
// GNC_TEST_ADD (suitename, "hash values helper", PriceDBFixture, NULL, setup, test_hash_values_helper, teardown);
    GNC_TEST_ADD (suitename, "gnc pricedb has prices", PriceDBFixture, NULL, setup, test_gnc_pricedb_has_prices, teardown);
    GNC_TEST_ADD (suitename, "gnc pricedb get prices", PriceDBFixture, NULL, setup, test_gnc_pricedb_get_prices, teardown);
    GNC_TEST_ADD (suitename, "gnc pricedb lookup day", PriceDBFixture, NULL, setup, test_gnc_pricedb_lookup_day_t64, teardown);
// GNC_TEST_ADD (suitename, "lookup nearest in time", Fixture, NULL, setup, test_lookup_nearest_in_time, teardown);
    GNC_TEST_ADD (suitename, "gnc pricedb lookup nearest in time", PriceDBFixture, NULL, setup, test_gnc_pricedb_lookup_nearest_in_time64, teardown);
// GNC_TEST_ADD (suitename, "direct balance conversion", Fixture, NULL, setup, test_direct_balance_conversion, teardown);
// GNC_TEST_ADD (suitename, "extract common prices", Fixture, NULL, setup, test_extract_common_prices, teardown);
// GNC_TEST_ADD (suitename, "convert balance", Fixture, NULL, setup, test_convert_balance, teardown);
// GNC_TEST_ADD (suitename, "indirect balance conversion", Fixture, NULL, setup, test_indirect_balance_conversion, teardown);
    GNC_TEST_ADD (suitename, "gnc pricedb convert balance latest price", PriceDBFixture, NULL, setup, test_gnc_pricedb_convert_balance_latest_price, teardown);
    GNC_TEST_ADD (suitename, "gnc pricedb convert balance nearest price", PriceDBFixture, NULL, setup, test_gnc_pricedb_convert_balance_nearest_price_t64, teardown);
// GNC_TEST_ADD (suitename, "pricedb foreach pricelist", Fixture, NULL, setup, test_pricedb_foreach_pricelist, teardown);
// GNC_TEST_ADD (suitename, "pricedb foreach currencies hash", Fixture, NULL, setup, test_pricedb_foreach_currencies_hash, teardown);
// GNC_TEST_ADD (suitename, "unstable price traversal", Fixture, NULL, setup, test_unstable_price_traversal, teardown);
// GNC_TEST_ADD (suitename, "compare kvpairs by commodity key", Fixture, NULL, setup, test_compare_kvpairs_by_commodity_key, teardown);
// GNC_TEST_ADD (suitename, "stable price traversal", Fixture, NULL, setup, test_stable_price_traversal, teardown);
// GNC_TEST_ADD (suitename, "gnc pricedb foreach price", Fixture, NULL, setup, test_gnc_pricedb_foreach_price, teardown);
// GNC_TEST_ADD (suitename, "add price to list", Fixture, NULL, setup, test_add_price_to_list, teardown);
// GNC_TEST_ADD (suitename, "gnc price fixup legacy commods", Fixture, NULL, setup, test_gnc_price_fixup_legacy_commods, teardown);
// GNC_TEST_ADD (suitename, "gnc price print", Fixture, NULL, setup, test_gnc_price_print, teardown);
// GNC_TEST_ADD (suitename, "print pricedb adapter", Fixture, NULL, setup, test_print_pricedb_adapter, teardown);
// GNC_TEST_ADD (suitename, "gnc pricedb print contents", Fixture, NULL, setup, test_gnc_pricedb_print_contents, teardown);
// GNC_TEST_ADD (suitename, "pricedb book begin", Fixture, NULL, setup, test_pricedb_book_begin, teardown);
// GNC_TEST_ADD (suitename, "pricedb book end", Fixture, NULL, setup, test_pricedb_book_end, teardown);
// GNC_TEST_ADD (suitename, "price create", Fixture, NULL, setup, test_price_create, teardown);
// GNC_TEST_ADD (suitename, "void pricedb foreach pricelist", Fixture, NULL, setup, test_void_pricedb_foreach_pricelist, teardown);
// GNC_TEST_ADD (suitename, "void pricedb foreach currencies hash", Fixture, NULL, setup, test_void_pricedb_foreach_currencies_hash, teardown);
// GNC_TEST_ADD (suitename, "void unstable price traversal", Fixture, NULL, setup, test_void_unstable_price_traversal, teardown);
// GNC_TEST_ADD (suitename, "price foreach", Fixture, NULL, setup, test_price_foreach, teardown);
// GNC_TEST_ADD (suitename, "price printable", Fixture, NULL, setup, test_price_printable, teardown);
// GNC_TEST_ADD (suitename, "gnc pricedb register", Fixture, NULL, setup, test_gnc_pricedb_register, teardown);

}
