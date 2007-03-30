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
#include <glib/gi18n.h>
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

#include "gnc-glib-utils.h"
#include "dialog-utils.h"
#include "druid-utils.h"
#include "gnc-ui-util.h"
#include "gnc-ui.h"
#include "gnc-html.h"
//#include "import-account-matcher.h"
#include "gnc-component-manager.h"
#include "gnc-session.h"

#include <aqbanking/banking.h>
#include <aqbanking/version.h>
#include <gwenhywfar/stringlist.h>
#include <gwenhywfar/version.h>

#define DRUID_HBCI_IMPORT_CM_CLASS "druid-hbci-import"
/* #define DEFAULT_HBCI_VERSION 201 */

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

};

/* Is TRUE as long as the druid is opened and running. Is being
   used to catch a window close event during waiting for a child
   process. */
static gboolean hbci_druid_is_active = FALSE;

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

  gnc_unregister_gui_component_by_data(DRUID_HBCI_IMPORT_CM_CLASS, info);

  reset_initial_info (info);
  
  /* if (info->interactor)
     GNCInteractor_delete(info->interactor);  -- being destroyed by AB_BANKING*/

  if (info->window != NULL) 
    gtk_widget_destroy (info->window);

  hbci_druid_is_active = FALSE;
  g_free (info);
}


static gchar *gnc_hbci_account_longname(const AB_ACCOUNT *hacc)
{
  gchar *bankname;
  gchar *result;
  const char *bankcode;
  g_assert(hacc);
  bankname = 
    gnc_utf8_strip_invalid_strdup (AB_Account_GetBankName (hacc));
  bankcode = AB_Account_GetBankCode (hacc);
  /* Translators: Strings are 1. Account code, 2. Bank name, 3. Bank code. */
  if (strlen(bankname) > 0)
    result = g_strdup_printf(_("%s at %s (code %s)"),
			   AB_Account_GetAccountNumber (hacc),
			   bankname,
			   bankcode);
  else
    result = g_strdup_printf(_("%s at bank code %s"),
			   AB_Account_GetAccountNumber (hacc),
			   bankcode);
  g_free (bankname);
  return result;
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
    row_text[1] = xaccAccountGetFullName (gacc);

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
cm_close_handler(gpointer user_data)
{
  HBCIInitialInfo *info = user_data;

  /* FIXME: Need to choose a fixed ending procedure here */
  /* probably not saving because of 'cancel', but for now we save too */
  gnc_AB_BANKING_fini (info->api);
  delete_initial_druid(info);
}

static void
on_cancel (GnomeDruid *gnomedruid,
	   gpointer user_data)
{
  cm_close_handler(user_data);
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
	 GNC_COMMODITY_NS_CURRENCY, AB_Account_GetCurrency (hbci_acc));
    }

    gnc_acc = gnc_import_select_account(info->window,
					NULL, TRUE, longname, currency, BANK,
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







#if (AQBANKING_VERSION_MAJOR > 1) || \
  ((AQBANKING_VERSION_MAJOR == 1) && \
    (AQBANKING_VERSION_MINOR == 9) && \
     (AQBANKING_VERSION_PATCHLEVEL > 0))
/* The wizard choice has changed with aqbanking >= 1.9.1; we don't
   need to specify a "backend" for the wizard anymore but instead
   there is only one wizard for all backends now. */
# define AQBANKING_WIZARD_ALLBACKENDS
#else
# undef AQBANKING_WIZARD_ALLBACKENDS
#endif

static void
on_aqhbci_button (GtkButton *button,
		  gpointer user_data)
{
  HBCIInitialInfo *info = user_data;
  GWEN_BUFFER *buf;
  int res;
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

  /* See note above about wizard choice. */
#ifndef AQBANKING_WIZARD_ALLBACKENDS
  {
  GWEN_PLUGIN_DESCRIPTION_LIST2 *pluginlist;
  const char *backend_name_nc;

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
	 _("Co_nfigure"),
	 0,
	 radio_list);
      g_list_free(radio_list);

      /* User pressed cancel in choice dialog */
      if (x == -1) {
	GWEN_PluginDescription_List2_freeAll(pluginlist);
#if ((GWENHYWFAR_VERSION_MAJOR < 1) || \
     ((GWENHYWFAR_VERSION_MAJOR == 1) && \
      ((GWENHYWFAR_VERSION_MINOR < 98))))
	/* Memory cleanup needed for gwenhywfar<1.98.x but not for
	   gwenhywfar>=1.98.x */
	GWEN_PluginDescription_List2_free(pluginlist);
#endif
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
#if ((GWENHYWFAR_VERSION_MAJOR < 1) || \
     ((GWENHYWFAR_VERSION_MAJOR == 1) && \
      ((GWENHYWFAR_VERSION_MINOR < 98))))
  /* Memory cleanup needed for gwenhywfar<1.98.x but not for
     gwenhywfar>=1.98.x */
  GWEN_PluginDescription_List2_free (pluginlist);
#endif
  }

#else /* AQBANKING_WIZARD_ALLBACKENDS */
  backend_name = g_strdup("");
#endif /* AQBANKING_WIZARD_ALLBACKENDS */

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

    /* Reset existing mapping tables */
    AB_Banking_Fini (info->api);
    delete_hash (info->hbci_hash);
    info->hbci_hash = NULL;
    if (info->gnc_hash != NULL)
      g_hash_table_destroy (info->gnc_hash);
    info->gnc_hash = NULL;
    /* In gtk2, this would be g_spawn_async or similar. */
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
	  hbci_druid_is_active = TRUE;
	  gtk_main_iteration();
	  wait_result = waitpid(pid, &wait_status, WNOHANG);
	  if ((wait_result == pid) && WIFEXITED(wait_status))
	    res = WEXITSTATUS(wait_status);
	  else
	    res = -8;
	}
	if (!hbci_druid_is_active) {
	  /* Just in case the druid has been canceled in the meantime. */
	  g_free (backend_name);
	  GWEN_Buffer_free(buf);
	  return;
	}
	AB_Banking_Init (info->api);
      }
    }

    if (res == 0) {
#ifndef AQBANKING_WIZARD_ALLBACKENDS
      res = AB_Banking_ActivateProvider(banking, backend_name);
#endif
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
	 _("The external program \"%s Setup Wizard\" returned a nonzero "
	   "exit code which means it has not been finished successfully. "
	   "The further HBCI setup can only be finished if the %s "
	   "Setup Wizard is run successfully. Please try to start and "
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
	 "The package aqbanking is supposed to install the program "
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
  gint component_id;
  
  info = g_new0 (HBCIInitialInfo, 1);

  xml = gnc_glade_xml_new ("hbci.glade", "HBCI Init Druid");

  info->window = glade_xml_get_widget (xml, "HBCI Init Druid");

  info->druid = glade_xml_get_widget (xml, "hbci_init_druid");
  gnc_druid_set_colors (GNOME_DRUID (info->druid));
  
  glade_xml_signal_connect_data (xml, "on_finish", 
				 G_CALLBACK (on_finish), info);
  glade_xml_signal_connect_data (xml, "on_cancel", 
				 G_CALLBACK (on_cancel), info);
  
  info->api = gnc_AB_BANKING_new_currentbook(info->window, &(info->interactor));
  g_assert(info->api);

  {
    /* Page with config file entry widget */
    page = glade_xml_get_widget(xml, "configfile_page");
    info->filepage = page;
    g_signal_connect (page, "prepare",
		      G_CALLBACK (on_aqbutton_prepare), info);
    g_signal_connect (glade_xml_get_widget (xml, "aqhbci_button"),
		      "clicked",
		      G_CALLBACK (on_aqhbci_button), info);
  }
  {
    page = glade_xml_get_widget(xml, "account_match_page");
    info->accountpage = page;
    info->accountlist = glade_xml_get_widget(xml, "account_page_list");
    g_signal_connect (info->accountlist, "select_row",
		      G_CALLBACK (on_accountlist_select_row), info);
    g_signal_connect (page, "prepare", 
		      G_CALLBACK (on_accountlist_prepare), info);
  }

  component_id = gnc_register_gui_component(DRUID_HBCI_IMPORT_CM_CLASS,
					    NULL, cm_close_handler,
					    info);
  gnc_gui_component_set_session(component_id, gnc_get_current_session());

  /*g_signal_connect (dialog, "destroy",*/
  /*                  G_CALLBACK(gnc_hierarchy_destroy_cb), NULL);*/

  gtk_widget_show_all (info->window);
}
