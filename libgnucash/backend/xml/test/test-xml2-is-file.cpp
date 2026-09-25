/********************************************************************\
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
 * along with this program; if not, contact:                        *
 *                                                                  *
 * Free Software Foundation           Voice:  +1-617-542-5942       *
 * 51 Franklin Street, Fifth Floor    Fax:    +1-617-542-2652       *
 * Boston, MA  02110-1301,  USA       gnu@gnu.org                   *
 *                                                                  *
\********************************************************************/
#include <config.h>
#include <stdlib.h>

#include <string>

#include <glib/gstdio.h>

#include "gnc-backend-xml.h"
#include "test-engine-stuff.h"
#include "io-gncxml-v2.h"
#include "test-file-stuff.h"
#include "test-stuff.h"

#define FILENAME "Money95bank_fr.gml2"

static const char* s_xml_without_encoding =
    "<?xml version=\"1.0\"?>\n"
    "<gnc-v2 xmlns:gnc=\"http://www.gnucash.org/XML/gnc\">\n";

static const char* s_not_gnucash_xml =
    "<?xml version=\"1.0\"?>\n"
    "<document/>\n";

int
main (int argc, char** argv)
{
    const char* directory = g_getenv ("GNC_TEST_FILES");

    if (!directory)
    {
        directory = "test-files/xml2";
    }

    auto filename = std::string{directory} + '/' + FILENAME;
    auto temp_directory = g_dir_make_tmp ("gnc-xml-encoding-XXXXXX", nullptr);
    if (!temp_directory)
    {
        g_printerr ("Unable to create a temporary XML test directory.\n");
        return EXIT_FAILURE;
    }
    auto no_encoding = g_build_filename (temp_directory, "no-encoding.xml",
                                         nullptr);
    auto not_gnucash = g_build_filename (temp_directory, "not-gnucash.xml",
                                         nullptr);

    do_test (gnc_is_xml_data_file_v2 (filename.c_str(), nullptr),
             "gnc_is_xml_data_file_v2");
    do_test (!gnc_xml_file_needs_encoding_conversion (filename.c_str()),
             "encoded XML does not need conversion");
    do_test (g_file_set_contents (no_encoding, s_xml_without_encoding, -1,
                                  nullptr),
             "write XML without encoding");
    do_test (gnc_xml_file_needs_encoding_conversion (no_encoding),
             "XML without encoding needs conversion");
    do_test (g_file_set_contents (not_gnucash, s_not_gnucash_xml, -1,
                                  nullptr),
             "write non-GnuCash XML");
    do_test (!gnc_xml_file_needs_encoding_conversion (not_gnucash),
             "non-GnuCash XML does not need conversion");

    g_remove (no_encoding);
    g_remove (not_gnucash);
    g_rmdir (temp_directory);
    g_free (no_encoding);
    g_free (not_gnucash);
    g_free (temp_directory);

    print_test_results ();
    return get_rv ();
}
