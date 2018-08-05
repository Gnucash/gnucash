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

#include "test-engine-stuff.h"

#include <stdlib.h>
}

#include "test-file-stuff.h"
#include "sixtp-utils.h"
#include "sixtp-dom-generators.h"
#include "test-stuff.h"

#define GNC_V2_STRING "gnc-v2"
const gchar* gnc_v2_xml_version_string = GNC_V2_STRING;

int
main (int argc, char** argv)
{
    int i;

    for (i = 0; i < 20; i++)
    {
        time64 spec2;
        auto spec1 = get_random_time ();
        auto sec_str = time64_to_string (spec1);
        if (!string_to_time64 (sec_str, &spec2))
        {
            failure_args ("string_to_timespec_secs", __FILE__, __LINE__,
                          "string is %s", sec_str);
        }
        else if (spec1 != spec2)
        {
            failure_args ("timespec_secs", __FILE__, __LINE__,
                          "not equal ints are %" G_GINT64_FORMAT
                          " and %" G_GINT64_FORMAT "\n",
                          spec1, spec2);
        }
        else
        {
            success ("timespec");
        }
        g_free (sec_str);
    }
    print_test_results ();
    exit (get_rv ());
}
