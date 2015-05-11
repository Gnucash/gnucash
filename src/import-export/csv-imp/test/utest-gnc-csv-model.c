/********************************************************************
 * utest-gnc-csv-model.c: GLib g_test test suite for gnc-csv-model.c.		    *
 * Copyright 2015 Geert Janssens <geert.gnucash@kobaltwit.be>		    *
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
 * http://www.gnu.org/licenses/old-licenses/gpl-2.0.html            *
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
#include "import-export/csv-imp/gnc-csv-model.h"

typedef struct
{
} Fixture;

static void
setup( Fixture *fixture, gconstpointer pData )
{

}

static void
teardown( Fixture *fixture, gconstpointer pData )
{
}

static const gchar *suitename = "/import-export/csv-imp/gnc-csv-model";
void test_suite_gnc_csv_model ( void );

/* parse_date_with_year
time64 parse_date_with_year (const char* date_str, int format)// Local: 1:0:0
*/
/* static void
test_parse_date_with_year (Fixture *fixture, gconstpointer pData)
{
}*/
/* parse_date_without_year
static time64 parse_date_without_year (const char* date_str, int format)// Local: 1:0:0
*/
/* static void
test_parse_date_without_year (Fixture *fixture, gconstpointer pData)
{
}*/
/* parse_date
time64 parse_date (const char* date_str, int format)// C: 14 in 7 SCM: 9 in 2 Local: 1:0:0
*/
/* static void
test_parse_date (Fixture *fixture, gconstpointer pData)
{
}*/
/* gnc_csv_parse_data_free
void gnc_csv_parse_data_free (GncCsvParseData* parse_data)// C: 3 in 1  Local: 0:0:0
*/
/* static void
test_gnc_csv_parse_data_free (Fixture *fixture, gconstpointer pData)
{
}*/
/* gnc_csv_convert_encoding
int gnc_csv_convert_encoding (GncCsvParseData* parse_data, const char* encoding,// C: 1  Local: 1:0:0
*/
/* static void
test_gnc_csv_convert_encoding (Fixture *fixture, gconstpointer pData)
{
}*/
/* gnc_csv_load_file
int gnc_csv_load_file (GncCsvParseData* parse_data, const char* filename,// C: 1  Local: 0:0:0
*/
/* static void
test_gnc_csv_load_file (Fixture *fixture, gconstpointer pData)
{
}*/
/* gnc_csv_parse
int gnc_csv_parse (GncCsvParseData* parse_data, gboolean guessColTypes, GError** error)// C: 13 in 1  Local: 0:0:0
*/
/* static void
test_gnc_csv_parse (Fixture *fixture, gconstpointer pData)
{
}*/
/* trans_property_free
static void trans_property_free (TransProperty* prop)// Local: 2:0:0
*/
/* static void
test_trans_property_free (Fixture *fixture, gconstpointer pData)
{
}*/
/* trans_property_set
static gboolean trans_property_set (TransProperty* prop, char* str)// Local: 1:0:0
*/
/* static void
test_trans_property_set (Fixture *fixture, gconstpointer pData)
{
}*/
/* trans_property_list_free
static void trans_property_list_free (TransPropertyList* list)// Local: 1:0:0
*/
/* static void
test_trans_property_list_free (Fixture *fixture, gconstpointer pData)
{
}*/
/* trans_property_list_add
static void trans_property_list_add (TransProperty* property)// Local: 1:0:0
*/
/* static void
test_trans_property_list_add (Fixture *fixture, gconstpointer pData)
{
}*/
/* trans_add_split
static void trans_add_split (Transaction* trans, Account* account, QofBook* book,// Local: 2:0:0
*/
/* static void
test_trans_add_split (Fixture *fixture, gconstpointer pData)
{
}*/
/* trans_property_list_verify_essentials
static gboolean trans_property_list_verify_essentials (TransPropertyList* list, gchar** error)// Local: 1:0:0
*/
/* static void
test_trans_property_list_verify_essentials (Fixture *fixture, gconstpointer pData)
{
}*/
/* gnc_csv_parse_to_trans
int gnc_csv_parse_to_trans (GncCsvParseData* parse_data, Account* account,// C: 2 in 1  Local: 0:0:0
*/
/* static void
test_gnc_csv_parse_to_trans (Fixture *fixture, gconstpointer pData)
{
}*/


void
test_suite_gnc_csv_model (void)
{

// GNC_TEST_ADD (suitename, "parse date with year", Fixture, NULL, setup, test_parse_date_with_year, teardown);
// GNC_TEST_ADD (suitename, "parse date without year", Fixture, NULL, setup, test_parse_date_without_year, teardown);
// GNC_TEST_ADD (suitename, "parse date", Fixture, NULL, setup, test_parse_date, teardown);
// GNC_TEST_ADD (suitename, "gnc csv parse data free", Fixture, NULL, setup, test_gnc_csv_parse_data_free, teardown);
// GNC_TEST_ADD (suitename, "gnc csv convert encoding", Fixture, NULL, setup, test_gnc_csv_convert_encoding, teardown);
// GNC_TEST_ADD (suitename, "gnc csv load file", Fixture, NULL, setup, test_gnc_csv_load_file, teardown);
// GNC_TEST_ADD (suitename, "gnc csv parse", Fixture, NULL, setup, test_gnc_csv_parse, teardown);
// GNC_TEST_ADD (suitename, "trans property free", Fixture, NULL, setup, test_trans_property_free, teardown);
// GNC_TEST_ADD (suitename, "trans property set", Fixture, NULL, setup, test_trans_property_set, teardown);
// GNC_TEST_ADD (suitename, "trans property list free", Fixture, NULL, setup, test_trans_property_list_free, teardown);
// GNC_TEST_ADD (suitename, "trans property list add", Fixture, NULL, setup, test_trans_property_list_add, teardown);
// GNC_TEST_ADD (suitename, "trans add split", Fixture, NULL, setup, test_trans_add_split, teardown);
// GNC_TEST_ADD (suitename, "trans property list verify essentials", Fixture, NULL, setup, test_trans_property_list_verify_essentials, teardown);
// GNC_TEST_ADD (suitename, "gnc csv parse to trans", Fixture, NULL, setup, test_gnc_csv_parse_to_trans, teardown);

}
