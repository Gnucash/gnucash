/********************************************************************\
 * druid-hbci-initial.c -- hbci creation functionality              *
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

#include "druid-hbci-initial.h"
#include "druid-hbci-utils.h"
#include "gnc-hbci-kvp.h"
#include "import-account-matcher.h"
#include "gnc-hbci-utils.h"

#include "dialog-utils.h"
#include "druid-utils.h"
#include "gnc-ui-util.h"
#include "gnc-ui.h"
#include "gnc-html.h"
#include "gnc-component-manager.h"

#include <aqbanking/banking.h>

/* #define DEFAULT_HBCI_VERSION 201 */

typedef enum _infostate {
  INI_UPDATE_ACCOUNTS,
  INI_MATCH_ACCOUNTS,
  UPDATE_ACCOUNTS,
  MATCH_ACCOUNTS
} Infostate;

struct _hbciinitialinfo 
{
  GtkWidget *window;
  GtkWidget *druid;

  /* configfile page */
  GtkWidget *filepage;
  
  /* account match page */
  GtkWidget *accountpage;
  GtkWidget *accountlist;
    
  /* OpenHBCI stuff */
  AB_BANKING *api;
  GNCInteractor *interactor;

  /* account match: row_number (int) -> hbci_account */
  GHashTable *hbci_hash;
  /* hbci_account (direct) -> gnucash_account  -- DO NOT DELETE THE KEYS! */
  GHashTable *gnc_hash;

  /* Status of user's movement through the wizard */
  Infostate state;

};

static gboolean
hash_remove (gpointer key, gpointer value, gpointer user_data) 
{
  free (key);
  return TRUE;
}

static void
delete_hash (GHashTable *hash) 
{
  if (hash != NULL) {
    g_hash_table_foreach_remove (hash, &hash_remove, NULL);
    g_hash_table_destroy (hash);
  }
}
static void
reset_initial_info (HBCIInitialInfo *info)
{
  if (info == NULL) return;

  if (info->api != NULL) {
    gnc_AB_BANKING_delete (info->api);
  }
  info->api = NULL;

  delete_hash (info->hbci_hash);
  info->hbci_hash = NULL;
  if (info->gnc_hash != NULL)
    g_hash_table_destroy (info->gnc_hash);
  info->gnc_hash = NULL;
}

static void
delete_initial_druid (HBCIInitialInfo *info)
{
  if (info == NULL) return;

  reset_initial_info (info);
  
  if (info->interactor)
    GNCInteractor_delete(info->interactor);

  if (info->window != NULL) 
    gtk_widget_destroy (info->window);

  g_free (info);
}


/*******************************************************************
 * update_accountlist widget
 */
static AB_ACCOUNT *
update_accountlist_acc_cb (AB_ACCOUNT *hacc, gpointer user_data)
{
  HBCIInitialInfo *info = user_data;
  gchar *row_text[3];
  Account *gacc;
  int row;
  gint *row_key;

  g_assert(hacc);
  g_assert(info);
  row_text[2] = "";
  
  row_text[0] = g_strdup(AB_Account_GetOwnerName(hacc));
		
  /* Get corresponding gnucash account */
  gacc = g_hash_table_lookup (info->gnc_hash, hacc);

  /* Build the text for the gnucash account. */
  if (gacc == NULL)
    row_text[1] = "";
  else 
    row_text[1] = 
      xaccAccountGetFullName (gacc, gnc_get_account_separator ());

  /* Add this row to the list */
  row = gtk_clist_append (GTK_CLIST (info->accountlist), row_text);

  /* Set the "new" checkbox. */
  gnc_clist_set_check (GTK_CLIST (info->accountlist), row, 2,
		       FALSE);

  /* Store the row_number -> hbci_account hash reference. */
  row_key = g_new(gint, 1);
  *row_key = row;
  g_hash_table_insert (info->hbci_hash, row_key, (AB_ACCOUNT*)hacc);

  return NULL;
}

/* Update the account list GtkCList widget */
static void
update_accountlist (HBCIInitialInfo *info)
{
  int sel_row = 0;

  g_assert(info);
  g_assert(info->api);
  g_assert(info->gnc_hash);

  /* Store old selected row here. */
  sel_row = (GTK_CLIST(info->accountlist))->focus_row;

  /* Delete old list */
  gtk_clist_freeze (GTK_CLIST (info->accountlist));
  gtk_clist_clear (GTK_CLIST (info->accountlist));

  /* Delete old hash with row_number -> hbci_account */
  delete_hash (info->hbci_hash);
  info->hbci_hash = g_hash_table_new (&g_int_hash, &g_int_equal);
  g_hash_table_freeze (info->hbci_hash);
  
  /* Go through all HBCI accounts */
  AB_Account_List2_ForEach (AB_Banking_GetAccounts(info->api),
			    update_accountlist_acc_cb,
			    info);

  //printf("update_accountlist: HBCI hash has %d entries.\n", g_hash_table_size(info->hbci_hash));
  //printf("update_accountlist: GNC hash has %d entries.\n", g_hash_table_size(info->gnc_hash));
  
  g_hash_table_thaw (info->hbci_hash);
  gtk_clist_thaw (GTK_CLIST (info->accountlist));

  /* move to the old selected row */
  (GTK_CLIST(info->accountlist))->focus_row = sel_row;
  gtk_clist_moveto(GTK_CLIST(info->accountlist), sel_row, 0, 0.0, 0.0);
}
/*
 * end update_accountlist 
 *******************************************************************/

/*******************************************************************
 *
 * Button enabling */
static void 
druid_enable_next_button(HBCIInitialInfo *info)
{
  g_assert(info);
  gnome_druid_set_buttons_sensitive (GNOME_DRUID (info->druid),
				     TRUE, TRUE, TRUE);
}
static void 
druid_disable_next_button(HBCIInitialInfo *info)
{
  g_assert(info);
  gnome_druid_set_buttons_sensitive (GNOME_DRUID (info->druid),
				     TRUE, FALSE, TRUE);
}
/*
 * end button enabling
 *******************************************************************/



/*************************************************************
 * GUI callbacks
 */


static void
on_cancel (GnomeDruid *gnomedruid,
	   gpointer user_data)
{
  HBCIInitialInfo *info = user_data;
  
  delete_initial_druid(info);
}

static void
on_finish (GnomeDruidPage *gnomedruidpage,
	   gpointer arg1,
	   gpointer user_data)
{
  HBCIInitialInfo *info = user_data;
  gboolean successful = TRUE;
  g_assert (info);

  if (successful && info->gnc_hash)
    accounts_save_kvp (info->gnc_hash);
  
  gnc_AB_BANKING_save (info->api);
  delete_initial_druid(info);
}



static void
on_aqbutton_prepare (GnomeDruidPage *gnomedruidpage,
		     gpointer arg1,
		     gpointer user_data)
{
  HBCIInitialInfo *info = user_data;
  druid_disable_next_button(info);
}


static gboolean 
on_accountlist_back (GnomeDruidPage  *gnomedruidpage,
		     gpointer         arg1,
		     gpointer         user_data)
{
  HBCIInitialInfo *info = user_data;
  g_assert(info);
  
  switch (info->state) {
  case INI_MATCH_ACCOUNTS:
  case MATCH_ACCOUNTS:
    gnome_druid_set_page (GNOME_DRUID (info->druid), 
			  GNOME_DRUID_PAGE (info->filepage));
    return TRUE;
  default:
    return FALSE;
  }
}

static void
on_accountlist_prepare (GnomeDruidPage *gnomedruidpage,
			gpointer arg1,
			gpointer user_data)
{
  HBCIInitialInfo *info = user_data;

  if (info->gnc_hash == NULL)
    info->gnc_hash = gnc_hbci_new_hash_from_kvp (info->api);
  
  gnome_druid_set_buttons_sensitive (GNOME_DRUID (info->druid),
				     FALSE, TRUE, TRUE);

  update_accountlist(info);
}


static gchar *gnc_hbci_account_longname(const AB_ACCOUNT *hacc)
{
  g_assert(hacc);
  /* Translators: Strings are 1. Account code, 2. Bank name, 3. Bank code. */
  return g_strdup_printf(_("%s at %s (code %s)"),
			 AB_Account_GetAccountNumber (hacc),
			 AB_Account_GetBankName (hacc),
			 AB_Account_GetBankCode (hacc));
}

static void
on_accountlist_select_row (GtkCList *clist, gint row,
			   gint column, GdkEvent *event,
			   gpointer user_data)
{
  HBCIInitialInfo *info = user_data;
  AB_ACCOUNT *hbci_acc;
  Account *gnc_acc, *old_value;
  gchar *longname;
  gnc_commodity *currency = NULL;
  
  hbci_acc = g_hash_table_lookup (info->hbci_hash, &row);
  if (hbci_acc) {
    old_value = g_hash_table_lookup (info->gnc_hash, hbci_acc);

    printf("on_accountlist_select_row: Selected hbci_acc id %s; old_value %p \n",
	   AB_Account_GetAccountNumber(hbci_acc),
	   old_value);

    longname = gnc_hbci_account_longname(hbci_acc);
    if (AB_Account_GetCurrency (hbci_acc) && 
	(strlen(AB_Account_GetCurrency (hbci_acc)) > 0)) {
      currency = gnc_commodity_table_lookup 
	(gnc_book_get_commodity_table (gnc_get_current_book ()), 
	 GNC_COMMODITY_NS_ISO, AB_Account_GetCurrency (hbci_acc));
    }

    gnc_acc = gnc_import_select_account(NULL, TRUE, longname, currency, BANK,
					old_value, NULL);
    g_free(longname);

    if (gnc_acc) {
      if (old_value) 
	g_hash_table_remove (info->gnc_hash, hbci_acc);
      
      g_hash_table_insert (info->gnc_hash, hbci_acc, gnc_acc);
    }
    
    /* update display */
    update_accountlist(info);
  } /* hbci_acc */
}







static void
on_button_clicked (GtkButton *button,
		   gpointer user_data)
{
  HBCIInitialInfo *info = user_data;
  char *name;
  g_assert(info->druid);
  
  name = gtk_widget_get_name (GTK_WIDGET (button));
  if (strcmp (name, "aqhbci_button") == 0) {
/*     info->state = ADD_BANK; */
    /* gnome_druid_set_page (GNOME_DRUID (info->druid), 
       GNOME_DRUID_PAGE (info->bankpage)); */
  } else if (strcmp (name, "updatelist_button") == 0) {
    info->state = UPDATE_ACCOUNTS;
    /* Nothing else to do. */
  } else {
    printf("on_button_clicked: Oops, unknown button: %s\n",
	   name);
  }
}


static void
on_aqhbci_button (GtkButton *button,
		  gpointer user_data)
{
  HBCIInitialInfo *info = user_data;
  GWEN_BUFFER *buf;
  int res;
  g_assert(info->druid);

  buf = GWEN_Buffer_new(NULL, 200, 0, 0);
  AB_Banking_GetWizardPath(info->api, "aqhbci", buf);
  GWEN_Buffer_AppendString(buf, "/kde_wizard");

  if (strlen(GWEN_Buffer_GetStart(buf)) > 0) {
    res = system(GWEN_Buffer_GetStart(buf));
    if (res == 0)
      druid_enable_next_button(info);
    else {
      printf("on_aqhbci_button: Oops, aqhbci wizard return nonzero value: %d. The called program was \"%s\".\n", res, GWEN_Buffer_GetStart(buf));
      druid_disable_next_button(info);
    }
  } else {
    printf("on_aqhbci_button: Oops, no aqhbci wizard found. Cannot start aqhbci wizard.\n");
    druid_disable_next_button(info);
  }
  GWEN_Buffer_free(buf);
}





void gnc_hbci_initial_druid (void)
{
  HBCIInitialInfo *info;
  GladeXML *xml;
  GtkWidget *page;
  
  info = g_new0 (HBCIInitialInfo, 1);

  xml = gnc_glade_xml_new ("hbci.glade", "HBCI Init Druid");

  info->window = glade_xml_get_widget (xml, "HBCI Init Druid");

  info->druid = glade_xml_get_widget (xml, "hbci_init_druid");
  gnc_druid_set_colors (GNOME_DRUID (info->druid));
  
  glade_xml_signal_connect_data (xml, "on_finish", 
				 GTK_SIGNAL_FUNC (on_finish), info);
  glade_xml_signal_connect_data (xml, "on_cancel", 
				 GTK_SIGNAL_FUNC (on_cancel), info);
  
  info->api = gnc_AB_BANKING_new_currentbook(info->window, &(info->interactor));

  {
    /* Page with config file entry widget */
    page = glade_xml_get_widget(xml, "configfile_page");
    info->filepage = page;
    gtk_signal_connect (GTK_OBJECT (page), "prepare",
			GTK_SIGNAL_FUNC (on_aqbutton_prepare), info);
    gtk_signal_connect (GTK_OBJECT 
			(glade_xml_get_widget (xml, "aqhbci_button")), 
			"clicked",
			GTK_SIGNAL_FUNC (on_aqhbci_button), info);
  }
  {
    page = glade_xml_get_widget(xml, "account_match_page");
    info->accountpage = page;
    info->accountlist = glade_xml_get_widget(xml, "account_page_list");
    gtk_signal_connect (GTK_OBJECT (info->accountlist), "select_row",
			GTK_SIGNAL_FUNC (on_accountlist_select_row), info);
    gtk_signal_connect (GTK_OBJECT 
			(glade_xml_get_widget (xml, "aqhbci_again_button")), 
			"clicked",
			GTK_SIGNAL_FUNC (on_aqhbci_button), info);
    gtk_signal_connect (GTK_OBJECT 
			(glade_xml_get_widget (xml, "updatelist_button")), 
			"clicked",
			GTK_SIGNAL_FUNC (on_button_clicked), info);
    gtk_signal_connect (GTK_OBJECT (page), "prepare", 
			GTK_SIGNAL_FUNC (on_accountlist_prepare), info);
    gtk_signal_connect (GTK_OBJECT (page), "back", 
			GTK_SIGNAL_FUNC (on_accountlist_back), info);
  }


  /*gtk_signal_connect (GTK_OBJECT(dialog), "destroy",*/
  /*                   GTK_SIGNAL_FUNC(gnc_hierarchy_destroy_cb), NULL);*/

  gtk_widget_show_all (info->window);
  
}

SCM  scm_hbci_initial_druid (void)
{
  gnc_hbci_initial_druid();
  return SCM_EOL;
}
