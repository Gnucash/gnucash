/********************************************************************\
 * druid-hbci-utils.c -- hbci creation functionality              *
 * Copyright (C) 2002 Christian Stimming                            *
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
 * along with this program; if not, contact:                        *
 *                                                                  *
 * Free Software Foundation           Voice:  +1-617-542-5942       *
 * 59 Temple Place - Suite 330        Fax:    +1-617-542-2652       *
 * Boston, MA  02111-1307,  USA       gnu@gnu.org                   *
\********************************************************************/

#include "config.h"

#include <gnome.h>
#include <sys/stat.h>
#include <sys/types.h>
#include <unistd.h>

#include "druid-hbci-utils.h"
#include "gnc-hbci-kvp.h"
#include "gnc-hbci-utils.h"

#include "dialog-utils.h"
#include "druid-utils.h"
#include "gnc-ui-util.h"
#include "gnc-ui.h"
/* #include "Group.h" */
/* #include "glade/glade-xml.h" */
/* #include "gnc-amount-edit.h" */
/* #include "gnc-commodity-edit.h" */
/* #include "gnc-general-select.h" */
/* #include "gnc-component-manager.h" */
/* #include "../gnome-utils/gnc-dir.h" */
/* #include "gnc-gui-query.h" */
/* #include "io-example-account.h" */
/* #include "top-level.h" */

#include "gnc-hbci-utils.h"

/**
 * Save the reference strings to the HBCI accounts in the kvp's of the
 * gnucash accounts.*/
static void
accounts_save_kvp_cb (gpointer key, gpointer value, gpointer user_data)
{
  AB_ACCOUNT *hbci_acc = key;
  Account *gnc_acc = value;
  g_assert(hbci_acc);
  g_assert(gnc_acc);

  if (gnc_hbci_get_account_uid(gnc_acc) !=
      AB_Account_GetUniqueId(hbci_acc))
    gnc_hbci_set_account_uid
      (gnc_acc, AB_Account_GetUniqueId(hbci_acc));
}

static gpointer accounts_clear_kvp (Account *gnc_acc, gpointer user_data)
{
  if (gnc_hbci_get_account_uid(gnc_acc))
    gnc_hbci_set_account_uid (gnc_acc, 0);
  return NULL;
}

/* hash is a DIRECT hash from each HBCI account to each gnucash
   account. */
void
accounts_save_kvp (GHashTable *hash)
{
  AccountGroup *grp;
  g_assert(hash);

  grp = gnc_book_get_group (gnc_get_current_book ());
  xaccGroupForEachAccount (grp, 
			   &accounts_clear_kvp,
			   NULL, TRUE);

  g_hash_table_foreach (hash, &accounts_save_kvp_cb, NULL);
}
/*
******************************************************/



void
update_accounts (GtkWidget *parent, AB_BANKING *api, GNCInteractor *inter) 
{
  g_assert(api);

}



struct hbci_acc_cb_data 
{
  AB_BANKING *api;
  GHashTable *hash;
};

static gpointer 
gnc_hbci_new_hash_from_kvp_cb (Account *gnc_acc, gpointer user_data)
{
  struct hbci_acc_cb_data *data = user_data;
  AB_ACCOUNT *hbci_acc = NULL;

  hbci_acc = (AB_ACCOUNT *) gnc_hbci_get_hbci_acc (data->api, gnc_acc);
  if (hbci_acc) {
    g_hash_table_insert (data->hash, hbci_acc, gnc_acc);
  }
  return NULL;
}

GHashTable *
gnc_hbci_new_hash_from_kvp (AB_BANKING *api)
{
  GHashTable *hash;

  hash = g_hash_table_new (&g_direct_hash, &g_direct_equal);
  if (api) {
    struct hbci_acc_cb_data data;
    AccountGroup *grp = gnc_book_get_group (gnc_get_current_book ());
    data.api = api;
    data.hash = hash;
    xaccGroupForEachAccount (grp, 
			     &gnc_hbci_new_hash_from_kvp_cb,
			     &data, TRUE);
  }
  return hash;
}


gboolean 
gnc_verify_exist_or_new_file (GtkWidget *parent, const char *filename)
{
  g_assert (parent);
  
  if (g_file_test (filename, G_FILE_TEST_ISFILE | G_FILE_TEST_ISLINK)) {
    return TRUE;
  }

  return gnc_verify_dialog
    (parent, TRUE,
     _("The file %s does not exist. \n"
"Would you like to create it now?"), 
     filename ? filename : _("(null)"));
}

gboolean
gnc_test_dir_exist_error (GtkWindow *parent, const char *filename) 
{
  char *dirname = g_dirname (filename);
  gboolean dirtest = g_file_test (dirname, G_FILE_TEST_ISDIR);
  g_free (dirname);
  if (!dirtest) {
    gnc_error_dialog
      (GTK_WIDGET (parent), 
       _("The directory for file\n"
"%s\n"
"does not exist. \n"
"Please choose another place for this file."), 
       filename ? filename : _("(null)"));
    return FALSE;
  }
  return TRUE;
}


