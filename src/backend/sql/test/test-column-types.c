/***************************************************************************
 *            test-column-types.c
 *
 *  Tests the basic SQL column types
 *
 *  Copyright  2008 Phil Longstaff
 *  plongstaff@rogers.com
 ****************************************************************************/

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
#include "qof.h"
#include "cashobjects.h"
#include "test-stuff.h"

#include "gnc-backend-sql.h"

int main( int argc, char ** argv )
{
    qof_init();
    cashobjects_register();
    gnc_sql_init( NULL );
    /*    do_test(
            qof_load_backend_library ("../.libs/", GNC_LIB_NAME),
            " loading gnc-backend-gda GModule failed");
    */
    print_test_results();
    qof_close();
    exit( get_rv() );
}
