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
 * 51 Franklin Street, Fifth Floor    Fax:    +1-617-542-2652       *
 * Boston, MA  02110-1301,  USA       gnu@gnu.org                   *
\********************************************************************/

#include "config.h"

#include <gnome.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <sys/wait.h>
#include <fcntl.h>
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
//#include "import-account-matcher.h"
#include "gnc-component-manager.h"

#include <aqbanking/banking.h>
#include <gwenhywfar/stringlist.h>

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
  
  /* if (info->interactor)
     GNCInteractor_delete(info->interactor);  -- being destroyed by AB_BANKING*/

  if (info->window != NULL) 
    gtk_widget_destroy (info->window);

  g_free (info);
}


static gchar *gnc_hbci_account_longname(const AB_ACCOUNT *hacc)
{
  const char *bankname;
  const char *bankcode;
  g_assert(hacc);
  bankname = AB_Account_GetBankName (hacc);
  bankcode = AB_Account_GetBankCode (hacc);
  /* Translators: Strings are 1. Account code, 2. Bank name, 3. Bank code. */
  if (bankname)
    return g_strdup_printf(_("%s at %s (code %s)"),
			   AB_Account_GetAccountNumber (hacc),
			   bankname,
			   bankcode);
  else
    return g_strdup_printf(_("%s at bank code %s"),
			   AB_Account_GetAccountNumber (hacc),
			   bankcode);
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
  
  row_text[0] = gnc_hbci_account_longname(hacc);
		
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
  AB_BANKING *banking;
  AB_ACCOUNT_LIST2 *acclist;

  g_assert(info);
  banking = info->api;
  g_assert(banking);
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
  acclist = AB_Banking_GetAccounts(banking);
  if (acclist)
    AB_Account_List2_ForEach (acclist,
			      update_accountlist_acc_cb,
			      info);
  else
    printf("update_accountlist: Oops, account list from AB_Banking is NULL.\n");

  /* printf("update_accountlist: HBCI hash has %d entries.\n", g_hash_table_size(info->hbci_hash)); */
  /* printf("update_accountlist: GNC hash has %d entries.\n", g_hash_table_size(info->gnc_hash)); */
  
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
				     TRUE, TRUE, TRUE, TRUE);
}
static void 
druid_disable_next_button(HBCIInitialInfo *info)
{
  g_assert(info);
  gnome_druid_set_buttons_sensitive (GNOME_DRUID (info->druid),
				     TRUE, FALSE, TRUE, TRUE);
}
/*
 * end button enabling
 *******************************************************************/



/*************************************************************
 * GUI callbacks
 */

static gboolean banking_has_accounts(AB_BANKING *banking)
{
  AB_ACCOUNT_LIST2 *accl;
  gboolean result;
  g_assert(banking);

  accl = AB_Banking_GetAccounts(banking);
  
  if (accl && (AB_Account_List2_GetSize(accl) > 0))
    result = TRUE;
  else
    result = FALSE;

  if (accl)
    AB_Account_List2_free(accl);
  return result;
}


static void
on_cancel (GnomeDruid *gnomedruid,
	   gpointer user_data)
{
  HBCIInitialInfo *info = user_data;

  /* FIXME: Need to choose a fixed ending procedure here */
  /* probably not saving because of 'cancel', but for now we save too */
  gnc_AB_BANKING_fini (info->api);
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
  
  gnc_AB_BANKING_fini (info->api);
  delete_initial_druid(info);
}


static void
on_aqbutton_prepare (GnomeDruidPage *gnomedruidpage,
		     gpointer arg1,
		     gpointer user_data)
{
  HBCIInitialInfo *info = user_data;
  AB_BANKING *banking = info->api;
  g_assert(banking);

  if (banking_has_accounts(banking))
    druid_enable_next_button(info);
  else
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
  
  /* Make sure the api reads in the current data */
  AB_Banking_Fini (info->api);
  AB_Banking_Init (info->api);

  if (info->gnc_hash == NULL)
    info->gnc_hash = gnc_hbci_new_hash_from_kvp (info->api);
  
  gnome_druid_set_buttons_sensitive (GNOME_DRUID (info->druid),
				     FALSE, TRUE, TRUE, TRUE);

  update_accountlist(info);
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
  const char *name;
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
  GWEN_PLUGIN_DESCRIPTION_LIST2 *pluginlist;
  const char *backend_name_nc;
  char *backend_name;

  /* This is the point where we look for and start an external
     application shipped with aqhbci that contains the setup druid for
     HBCI related stuff. It requires qt (but not kde). This
     application contains the very verbose step-by-step setup wizard
     for the HBCI account, and the application is shared with other
     AqBanking-based financial managers that offer the HBCI features
     (e.g. KMyMoney). See gnucash-devel discussion here
     https://lists.gnucash.org/pipermail/gnucash-devel/2004-December/012351.html
  */
  gboolean wizard_exists;
  const char *wizard_path;
  AB_BANKING *banking = info->api;
  g_assert(info->druid);

  /* Get list of all backends, active or inactive */
  pluginlist = AB_Banking_GetProviderDescrs (banking);

  /* If there is only one backend, use it, otherwise ask the user */
  if (!pluginlist || (GWEN_PluginDescription_List2_GetSize(pluginlist) < 1))
    /* No backend at all? Try aqhbci */
    backend_name_nc = "aqhbci";
  else {
    GWEN_PLUGIN_DESCRIPTION_LIST2_ITERATOR *pluginlist_it = 
      GWEN_PluginDescription_List2_First(pluginlist);
    GWEN_PLUGIN_DESCRIPTION *plugindescr;
    g_assert (pluginlist_it);

    plugindescr = GWEN_PluginDescription_List2Iterator_Data (pluginlist_it);
    if (GWEN_PluginDescription_List2_GetSize(pluginlist) == 1)
      /* Only one backend? Use it */
      backend_name_nc = GWEN_PluginDescription_GetName(plugindescr);
    else {
      /* Present a selection dialog to select a particular backend */
      GList *radio_list = NULL;
      int x;

      while (plugindescr) {
	radio_list = 
	  g_list_append(radio_list, 
			g_strdup_printf("%s: %s",
					GWEN_PluginDescription_GetName(plugindescr),
					GWEN_PluginDescription_GetShortDescr(plugindescr)));
	plugindescr = GWEN_PluginDescription_List2Iterator_Next (pluginlist_it);
      }
      GWEN_PluginDescription_List2Iterator_free(pluginlist_it);

      x = gnc_choose_radio_option_dialog
	(GTK_WIDGET(info->window),
	 _("Choose AqBanking Backend"),
	 _("Please choose an AqBanking backend to be configured"),
	 0,
	 radio_list);
      g_list_free(radio_list);

      /* User pressed cancel in choice dialog */
      if (x == -1) {
	GWEN_PluginDescription_List2_freeAll(pluginlist);
	GWEN_PluginDescription_List2_free(pluginlist);
	return;
      }

      pluginlist_it = GWEN_PluginDescription_List2_First(pluginlist);
      plugindescr = GWEN_PluginDescription_List2Iterator_Data (pluginlist_it);
      while (x > 0) {
	plugindescr = GWEN_PluginDescription_List2Iterator_Next (pluginlist_it);
	x--;
      }
      backend_name_nc = GWEN_PluginDescription_GetName(plugindescr);
    }
    GWEN_PluginDescription_List2Iterator_free(pluginlist_it);
  }

  /* Allocate the backend name again because the PluginDescr list will
     be freed */
  backend_name = g_strdup (backend_name_nc);
  GWEN_PluginDescription_List2_freeAll (pluginlist);
  GWEN_PluginDescription_List2_free (pluginlist);

  /* ***** */

  /* Now find out the wizard name for that backend */
  buf = GWEN_Buffer_new(NULL, 300, 0, 0);
  AB_Banking_FindWizard(banking, backend_name, NULL, buf);
  wizard_exists = (strlen(GWEN_Buffer_GetStart(buf)) > 0);
  wizard_path = GWEN_Buffer_GetStart(buf);

  if (wizard_exists) {
    /* Really check whether the file exists */
    int fd = open( wizard_path, O_RDONLY );
    if ( fd == -1)
      wizard_exists = FALSE;
    else
      close( fd );
  }

  druid_disable_next_button(info);
  /* AB_Banking_DeactivateProvider(banking, backend_name); */
  if (wizard_exists) {
    int wait_status;
    int wait_result = 0;

    /* Call the qt wizard. See the note above about why this approach
       is chosen. */

    /* In gtk2, this would be g_spawn_async or similar. */
    AB_Banking_Fini (info->api);
    {
      pid_t pid;
      pid = fork();
      switch (pid) {
      case -1:
	printf("Fork call failed. Cannot start AqHBCI setup wizard.");
	res = -1;
	AB_Banking_Init (info->api);
	break;
      case 0: /* child */
	execl(wizard_path, wizard_path, NULL);
	printf("Fork call failed. Cannot start AqHBCI setup wizard.");
	_exit(0);
      default: /* parent */
	res = 0;
	/* wait until child is finished */
	while (wait_result == 0) {
	  gtk_main_iteration();
	  wait_result = waitpid(pid, &wait_status, WNOHANG);
	  if ((wait_result == pid) && WIFEXITED(wait_status))
	    res = WEXITSTATUS(wait_status);
	  else
	    res = -8;
	}
	AB_Banking_Init (info->api);
      }
    }

    if (res == 0) {
      res = AB_Banking_ActivateProvider(banking, backend_name);
      if ((res == 0) || (res == AB_ERROR_FOUND))
	druid_enable_next_button(info);
      else {
	printf("on_aqhbci_button: Oops, after successful wizard the activation return nonzero value: %d. \n", res);
	druid_disable_next_button(info);
      }
    }
    else {
      printf("on_aqhbci_button: Oops, aqhbci wizard return nonzero value: %d. The called program was \"%s\".\n", res, wizard_path);
      gnc_error_dialog
	(info->window,
       /* Each of the %s is the name of the backend, e.g. "aqhbci". */
	 _("The external program \"%s Setup Wizard\" returned a nonzero \n"
	   "exit code which means it has not been finished successfully. \n"
	   "The further HBCI setup can only be finished if the %s \n"
	   "Setup Wizard is run successfully. Please try to start and \n"
	   "successfully finish the %s Setup Wizard program again."),
	 backend_name, backend_name, backend_name);
      druid_disable_next_button(info);
    }
  } else {
    printf("on_aqhbci_button: Oops, no aqhbci setup wizard found.");
    gnc_error_dialog
      (info->window,
       /* Each of the %s is the name of the backend, e.g. "aqhbci". */
       _("The external program \"%s Setup Wizard\" has not been found. \n\n"
	 "The package aqbanking is supposed to install the program \n"
	 "\"%s-qt3-wizard\". Please check your installation of aqbanking."),
       backend_name, backend_name);
    druid_disable_next_button(info);
  }
  g_free (backend_name);
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
  g_assert(info->api);

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
