/*********************************************************************
 * test-address.c
 * object definition/initialization for Address
 *
 * Copyright (c) 2001 Derek Atkins <warlord@MIT.EDU>
 * Copyright (c) 2005 Neil Williams <linux@codehelp.co.uk>
 *
 * This program is free software; you can redistribute it and/or
 * modify it under the terms of the GNU General Public License as
 * published by the Free Software Foundation; either version 2 of
 * the License, or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, contact:
 *
 * Free Software Foundation           Voice:  +1-617-542-5942
 * 51 Franklin Street, Fifth Floor    Fax:    +1-617-542-2652
 * Boston, MA  02110-1301,  USA       gnu@gnu.org
 *
 *********************************************************************/

#include "config.h"
#include <glib.h>
#include "cashobjects.h"
#include "gncAddressP.h"
#include "test-stuff.h"

static void
test_string_fcn (GncAddress *address, const char *message,
                 void (*set) (GncAddress *, const char *str),
                 const char * (*get)(const GncAddress *));

static void
test_address (void)
{
    QofInstance *ent;
    GncAddress *address;
    QofBook *book = qof_book_new ();

    ent = g_object_new(QOF_TYPE_INSTANCE, "guid", guid_null(), NULL);
    ent->e_type = "asdf";

    /* Test creation/destruction */
    {
        do_test (gncAddressCreate (NULL,  NULL) == NULL, "address create NULL");

        address = gncAddressCreate (book, ent);
        do_test (address != NULL, "address create");

        gncAddressBeginEdit (address);
        gncAddressDestroy (address);
        success ("create/destroy");
    }

    /* Test setting routines */
    {
        address = gncAddressCreate (book, ent);
        test_string_fcn (address, "Name", gncAddressSetName, gncAddressGetName);
        test_string_fcn (address, "Addr1", gncAddressSetAddr1, gncAddressGetAddr1);
        test_string_fcn (address, "Addr2", gncAddressSetAddr2, gncAddressGetAddr2);
        test_string_fcn (address, "Addr3", gncAddressSetAddr3, gncAddressGetAddr3);
        test_string_fcn (address, "Addr4", gncAddressSetAddr4, gncAddressGetAddr4);
        test_string_fcn (address, "Phone", gncAddressSetPhone, gncAddressGetPhone);
        test_string_fcn (address, "Fax", gncAddressSetFax, gncAddressGetFax);
        test_string_fcn (address, "Email", gncAddressSetEmail, gncAddressGetEmail);
    }
}

static void
test_string_fcn (GncAddress *address, const char *message,
                 void (*set) (GncAddress *, const char *str),
                 const char * (*get)(const GncAddress *))
{
    char const *str = get_random_string ();

    do_test (!gncAddressIsDirty (address), "test if start dirty");
    set (address, str);
    do_test (gncAddressIsDirty (address), "test dirty later");
    do_test (safe_strcmp (get (address), str) == 0, message);
    gncAddressClearDirty (address);
}

int
main (int argc, char **argv)
{
    qof_init();
    if (cashobjects_register())
    {
        test_address();
        print_test_results();
    }
    qof_close();
    return 0;
}
