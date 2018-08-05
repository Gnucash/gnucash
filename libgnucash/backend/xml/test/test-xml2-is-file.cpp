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
extern "C"
{
#include <config.h>
#include <stdlib.h>
#include <string.h>

#include "test-engine-stuff.h"
}

#include "io-gncxml-v2.h"
#include "test-file-stuff.h"
#include "test-stuff.h"

#define FILENAME "Money95bank_fr.gml2"

int
main (int argc, char** argv)
{
    const char* directory = g_getenv ("GNC_TEST_FILES");

    if (!directory)
    {
        directory = "test-files/xml2";
    }

    char* filename = static_cast<decltype (filename)> (malloc (strlen (
            directory) + 1 + strlen (FILENAME) + 1));
    sprintf (filename, "%s/%s", directory, FILENAME);
    do_test (gnc_is_xml_data_file_v2 (filename, NULL), "gnc_is_xml_data_file_v2");

    print_test_results ();
    exit (get_rv ());
}
