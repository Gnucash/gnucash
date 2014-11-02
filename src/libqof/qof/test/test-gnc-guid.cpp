/********************************************************************
 * GLib test suite for guid.cpp                                     *
 * Copyright 2014 Aaron Laws <dartmetrash@gmail.com>                *
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

extern "C"
{
#include <config.h>
#include <string.h>
#include <glib.h>
#include <unittest-support.h>
void test_suite_gnc_guid (void);
}

/*Can be included as c++, because it's c++ tolerant*/
#include "../guid.h"

#include <random>
#include <sstream>
#include <iomanip>
#include <string>
#include <iostream>

using namespace std;

static const gchar * suitename {"/qof/gnc-guid"};

/*Create a GUID and free it.*/
static void test_create_gnc_guid (void){
    GncGUID * guid {guid_malloc ()};
    g_assert (guid != nullptr);
    guid_replace (guid);
    /*We apparently don't need to free guid_null (based on its being const)*/
    const GncGUID * guidnull {guid_null ()};
    g_assert (!guid_equal (guid, guidnull));
    guid_free (guid);
}

/*We create a GUID, create a copy, and compare them to ensure they're not different.*/
static void test_gnc_guid_copy (void) {
    GncGUID * guid {guid_malloc ()};
    g_assert (guid != nullptr);
    guid_replace (guid);
    GncGUID * cp {guid_copy (guid)};
    g_assert (guid_equal (guid, cp));
    guid_free (cp);
    guid_free (guid);
}

/* We create a GUID, then convert it to a string using the two methods
defined in the guid api. We then compare them.*/
static void test_gnc_guid_to_string (void) {
    GncGUID * guid {guid_malloc()};
    gchar guidstrp [GUID_ENCODING_LENGTH+1];
    gchar guidstrp2[GUID_ENCODING_LENGTH+1];
    g_assert (guid != nullptr);
    guid_replace (guid);
    string message {" using guid_to_string (deprecated): "};
    guid_to_string_buff (guid,guidstrp);
    string guidstr {guidstrp};
    g_assert (guidstr.size () == GUID_ENCODING_LENGTH);
    message += guidstr;
    g_test_message ("%s", message.c_str ());
    message = " using guid_to_string_buff: ";
    gchar * ret {guid_to_string_buff (guid, guidstrp2)};
    g_assert (ret == guidstrp2 + GUID_ENCODING_LENGTH);
    string guidstr2 {guidstrp2};
    g_assert (guidstr2.size () == GUID_ENCODING_LENGTH);
    message += guidstr2;
    g_test_message ("%s", message.c_str ());

    g_assert (guidstr2 == guidstr);
    guid_free (guid);
}

/*We fill a stringstream with random data, convert those to 
two GUIDs, then ensure that they are equal*/
static void test_gnc_guid_equals (void) {
    GncGUID * guid1 {guid_malloc ()};
    g_assert (guid1 != nullptr);
    GncGUID * guid2 {guid_malloc ()};
    g_assert (guid2 != nullptr);
    GncGUID * guidold {guid_malloc ()};
    g_assert (guidold != nullptr);

    mt19937 m;
    uniform_int_distribution<unsigned> dist;
    /*we use the same seed every time for reproducibility's sake, and
    for sanity's sake. It may be nerve-wracking for this test to fail
    only sometimes, or in different ways.*/
    m.seed (0);
    unsigned num_tests {200};
    for (unsigned test {0}; test < num_tests; ++test){
        ostringstream o;
        o << hex << setfill ('0') << right;
        /*Now we put a GUID into the stringstream*/
        for (unsigned spot {0}; spot < 4; ++spot)
            o << setw (8) << dist (m);
        string guids {o.str ()};
        g_assert (guids.size() == GUID_ENCODING_LENGTH);
        g_assert (string_to_guid (guids.c_str (), guid1));
        g_assert (string_to_guid (guids.c_str (), guid2));
        g_assert (guid_equal (guid1, guid2));
        /*Assuming that our distribution won't give the same
        GUID twice in a row.*/
        g_assert (!guid_equal (guid1, guidold));
        g_assert (string_to_guid (guids.c_str (), guidold));
    }
    guid_free (guidold);
    guid_free (guid2);
    guid_free (guid1);
}

/*We create a new guid, convert it to string, and convert that to
a guid, ensuring that we end up with an equivalent structure*/
static void test_gnc_guid_roundtrip (void) {
    GncGUID * guid1 {guid_malloc ()};
    g_assert (guid1 != nullptr);
    GncGUID * guid2 {guid_malloc ()};
    g_assert (guid2 != nullptr);
    guid_replace (guid1);

    gchar guidstrp [GUID_ENCODING_LENGTH+1];
    gchar * temp {guid_to_string_buff (guid1, guidstrp)};
    g_assert (temp == guidstrp + GUID_ENCODING_LENGTH);

    g_assert (string_to_guid (guidstrp, guid2));
    g_assert (guid_equal (guid1, guid2));
    guid_free (guid2);
    guid_free (guid1);
}

/**
 * guid_replace should put a newly generated guid into the parameter. In
 * this test, we ensure that the first "new" guid doesn't match a subsequent
 * "new" guid in the same memory location.
 */
static void test_gnc_guid_replace (void)
{
    GncGUID * guid1 {guid_malloc ()};

    guid_replace (guid1);
    GncGUID * guid2 {guid_copy (guid1)};
    guid_replace (guid1);
    g_assert (! guid_equal (guid1, guid2));

    guid_free (guid2);
    guid_free (guid1);
}

/**
 * We create a bogus guid and ensure that it doesn't get parsed successfully,
 * then we pass in a good GUID from string and ensure that the function returns true.
 */
static void test_gnc_guid_from_string (void) {
    GncGUID * guid {guid_malloc ()};
    const char * bogus {"01-23-45-6789a.cDeF0123z56789abcdef"};

    /* string_to_guid should return false if either parameter is null*/
    g_assert (!string_to_guid (nullptr, guid));
    g_assert (!string_to_guid (bogus, nullptr));

    g_assert (!string_to_guid (bogus, guid));

    const char * good {"0123456789abcdef1234567890abcdef"};
    g_assert (string_to_guid (good, guid));

    guid_free (guid);
}

void test_suite_gnc_guid (void)
{
    GNC_TEST_ADD_FUNC (suitename, "gnc create guid", test_create_gnc_guid);
    GNC_TEST_ADD_FUNC (suitename, "gnc copy guid", test_gnc_guid_copy);
    GNC_TEST_ADD_FUNC (suitename, "gnc guid to string", test_gnc_guid_to_string);
    GNC_TEST_ADD_FUNC (suitename, "gnc guid equal", test_gnc_guid_equals);
    GNC_TEST_ADD_FUNC (suitename, "gnc guid string roundtrip", test_gnc_guid_roundtrip);
    GNC_TEST_ADD_FUNC (suitename, "gnc guid from string", test_gnc_guid_from_string);
    GNC_TEST_ADD_FUNC (suitename, "gnc guid replace", test_gnc_guid_replace);
}

