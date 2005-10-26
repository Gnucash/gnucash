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
 * 59 Temple Place - Suite 330        Fax:    +1-617-542-2652       *
 * Boston, MA  02111-1307,  USA       gnu@gnu.org                   *
\********************************************************************/
/** @addtogroup Import_Export
    @{ */
/** @internal
     @file gnc-mt940-import.c
     @brief MT940 import module code
     @author Copyright (c) 2002 Benoit Grégoire <bock@step.polymtl.ca>, Copyright (c) 2003 Jan-Pascal van Best <janpascal@vanbest.org>
 */
#define _GNU_SOURCE

#include "config.h"

#include <stdio.h>
#include <string.h>
#include <sys/time.h>

#include <glib.h>
#include <gmodule.h>

#include <openhbci/swiftparser.h>

#include "gnc-hbci-gettrans.h"
#include "import-account-matcher.h"
#include "import-main-matcher.h"

#include "Account.h"
#include "Transaction.h"

#include "gnc-engine.h"
#include "gnc-file.h"
#include "gnc-ui-util.h"
#include "gnc-gconf-utils.h"
#include "gnc-hbci-utils.h"

#include "gnc-mt940-import.h"

#define GCONF_SECTION "dialogs/import/mt940"

static QofLogModule log_module = GNC_MOD_IMPORT;

static void *trans_importer_cb (const HBCI_Transaction *h_trans, 
			    void *user_data);

/********************************************************************\
 * gnc_file_mt940_import
 * Entry point
\********************************************************************/

void gnc_file_mt940_import (void)
{
  char *selected_filename;
  char *default_dir;
  FILE *mt940_file;
  GNCImportMainMatcher *gnc_mt940_importer_gui = NULL;

  /* gnc_should_log(MOD_IMPORT, GNC_LOG_TRACE); */
  DEBUG("gnc_file_mt940_import(): Begin...\n");

  default_dir = gnc_gconf_get_string(GCONF_SECTION, KEY_LAST_PATH, NULL);
  if (default_dir == NULL)
    gnc_init_default_directory(&default_dir);
  selected_filename = gnc_file_dialog(_("Select an MT940 file to process"),
				      NULL,
				      default_dir,
				      GNC_FILE_DIALOG_IMPORT);
  g_free(default_dir);

  if(selected_filename!=NULL)
    {
      /* Remember the directory as the default. */
      gnc_extract_directory(&default_dir, selected_filename);
      gnc_gconf_set_string(GCONF_SECTION, KEY_LAST_PATH, default_dir, NULL);
      g_free(default_dir);

      /*strncpy(file,selected_filename, 255);*/
      DEBUG("Filename found: %s",selected_filename);

      /* Create the Generic transaction importer GUI. */
      gnc_mt940_importer_gui = gnc_gen_trans_list_new(NULL, NULL, FALSE);

      DEBUG("Opening selected file");
      mt940_file = fopen(selected_filename, "r");

      fseek(mt940_file,0,SEEK_END);
      unsigned long filesize=ftell(mt940_file);
      rewind(mt940_file);

      char *mt940_records=g_malloc(filesize+1);

      fread(mt940_records,1,filesize,mt940_file);
      mt940_records[filesize]='\0';

      DEBUG("Read file data: %s\n",mt940_records);

      {
        int pos=0;
        int result;
        HBCI_transactionReport *tr;
        const list_HBCI_Transaction *transactions;
        /*list_HBCI_Transaction_iter *iter;*/

        tr=HBCI_transactionReport_new();
        
        while(pos<filesize){
          DEBUG("Parsing mt940 string, pos=%d\n",pos);

          result=HBCI_SWIFTparser_readMT940(mt940_records,tr,&pos);

          DEBUG("Parsing result: %d\n", result);

          transactions=HBCI_transactionReport_transactions(tr);

          DEBUG("Got %d transactions.\n",
             list_HBCI_Transaction_size(transactions));

          DEBUG("Importing those transaction...\n");
          list_HBCI_Transaction_foreach (transactions, trans_importer_cb, gnc_mt940_importer_gui);
          DEBUG("Imported transactions, looking for next...\n");
        }
        HBCI_transactionReport_delete(tr);
      }
      g_free(mt940_records);
      g_free(selected_filename);
    }
}

/* list_HBCI_Transaction_foreach callback. The Conversion from HBCI to
   GNC transaction is done here, once for each HBCI_Transaction.
   */
static void *trans_importer_cb (const HBCI_Transaction *h_trans, 
			    void *user_data)
{
  Account *gnc_acc;
  GNCImportMainMatcher *gnc_importer_gui = user_data;

  g_assert(gnc_importer_gui);
  g_assert(h_trans);

  char *account_name=(char *)HBCI_Transaction_ourAccountId(h_trans);
  DEBUG("Account name: \"%s\"\n",account_name);
  DEBUG("Transaction code: %d\n", HBCI_Transaction_transactionCode(h_trans));
  const HBCI_Value* value=HBCI_Transaction_value(h_trans);
  DEBUG("Transaction value: %f\n", HBCI_Value_getValue(value));

  gnc_acc = gnc_import_select_account(account_name, 1, NULL, NULL, NO_TYPE, NULL, NULL);
  g_assert(gnc_acc);

  gnc_hbci_trans_import(h_trans,gnc_importer_gui,gnc_acc);
  // trans_importer(h_trans,gnc_importer_gui,gnc_acc);

  return NULL;
}

/** @} */
