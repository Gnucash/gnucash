/*********************************************************************
 * businessmod-core.c
 * module definition/initialization for the core Business module
 * 
 * Copyright (c) 2001 Derek Atkins <warlord@MIT.EDU>
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
 * 59 Temple Place - Suite 330        Fax:    +1-617-542-2652
 * Boston, MA  02111-1307,  USA       gnu@gnu.org
 *
 *********************************************************************/

#include <glib.h>
#include <libguile.h>

#include "guid.h"
#include "gnc-module.h"
#include "gnc-engine-util.h"

#include "gncBusiness.h"
#include "gncAddress.h"
#include "test-stuff.h"

static void
test_string_fcn (GncAddress *address, const char *message,
		 void (*set) (GncAddress *, const char *str),
		 const char * (*get)(const GncAddress *));

static void
test_address (void)
{
  QofEntity ent;
  GncAddress *address;
  QofBook *book = qof_book_new ();

  ent.e_type = "asdf";
  ent.guid = *guid_null();

  /* Test creation/destruction */
  {
    do_test (gncAddressCreate (NULL,  NULL) == NULL, "address create NULL");

    address = gncAddressCreate (book, &ent);
    do_test (address != NULL, "address create");

    gncAddressDestroy (address);
    success ("create/destroy");
  }

  /* Test setting routines */
  {
    address = gncAddressCreate (book, &ent);
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

static void
main_helper (void *closure, int argc, char **argv)
{
  gnc_module_load("gnucash/business-core", 0);
  test_address();
  print_test_results();
  exit(get_rv());
}

int
main (int argc, char **argv)
{
  scm_boot_guile (argc, argv, main_helper, NULL);
  return 0;
}
