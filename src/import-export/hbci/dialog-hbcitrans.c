/********************************************************************\
 * dialog-hbcitrans.c -- dialog for hbci transaction                *
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
#include <openhbci/bank.h>
#include <openhbci/outboxaccjobs.h>
#include <openhbci.h>

#include "dialog-utils.h"
#include "gnc-ui.h"
#include "gnc-amount-edit.h"

#include "gnc-hbci-utils.h"
#include "gnc-hbci-trans-templ.h"
#include "dialog-hbcitrans.h"

/* -------------------------------------- */
/* Data structure */
/* -------------------------------------- */
struct _trans_data 
{
  GtkWidget *parent;
  /* Recipient */
  GtkWidget *recp_name_entry;
  GtkWidget *recp_account_entry;
  GtkWidget *recp_bankcode_entry;

  /* Amount */
  GtkWidget *amount_edit;

  /* Purpose, description */
  GtkWidget *purpose_entry;
  GtkWidget *purpose_cont_entry;

  /* Recipient's bank name (may be filled in automatically sometime later) */
  GtkWidget *recp_bankname_label;

  /* The template choosing option menu */
  GtkWidget *template_option;

  /* GList of GNCTransTempl */
  GList *templ;
};
typedef struct _trans_data TransData;




/* -------------------------------------- */
/* Prototypes; callbacks for dialog function */
/* -------------------------------------- */

void template_selection_cb(GtkButton *b, gpointer user_data);
void add_template_cb(GtkButton *b, gpointer user_data);

static void fill_template_menu_func(gpointer data, gpointer user_data)
{
  GNCTransTempl *templ = data;
  GtkMenu *menu = user_data;
  GtkWidget *item = gtk_menu_item_new_with_label(gnc_trans_templ_get_name(templ));
  gtk_object_set_user_data(GTK_OBJECT(item), templ);
  gtk_menu_append(menu, item);
}


/* -------------------------------------- */
/* Main dialog function */
/* -------------------------------------- */

HBCI_Transaction *
gnc_hbci_trans (GtkWidget *parent,
		HBCI_API *api,
		GNCInteractor *interactor,
		const HBCI_Account *h_acc,
		const HBCI_Customer *customer,
		Account *gnc_acc,
		GNC_HBCI_Transtype trans_type,
		GList **templ)
{
  GtkWidget *dialog;
  GladeXML *xml;
  HBCI_Transaction *trans = NULL;
  gint result;
  const HBCI_Bank *bank;
  gboolean successful;
  TransData td;

  td.parent = parent;
  td.templ = *templ;
  g_assert (api);
  g_assert (h_acc);
  g_assert (customer);
  bank = HBCI_Account_bank (h_acc);
  g_assert (bank);
      
  xml = gnc_glade_xml_new ("hbci.glade", "HBCI_trans_dialog");

  dialog = glade_xml_get_widget (xml, "HBCI_trans_dialog");

  if (parent)
    gnome_dialog_set_parent (GNOME_DIALOG (dialog), GTK_WINDOW (parent));
  
  {
    GtkWidget *heading_label;
    GtkWidget *recp_name_heading;
    GtkWidget *recp_account_heading;
    GtkWidget *recp_bankcode_heading;
    GtkWidget *amount_hbox;
    GtkWidget *orig_name_label;
    GtkWidget *orig_account_label;
    GtkWidget *orig_bankname_label;
    GtkWidget *orig_bankcode_label;
    GtkWidget *orig_name_heading;
    GtkWidget *orig_account_heading;
    GtkWidget *orig_bankname_heading;
    GtkWidget *orig_bankcode_heading;
    GtkWidget *exec_later_button;
    GtkWidget *add_templ_button;
        
    g_assert 
      (heading_label = glade_xml_get_widget (xml, "heading_label"));
    g_assert 
      (td.recp_name_entry = glade_xml_get_widget (xml, "recp_name_entry"));
    g_assert 
      (recp_name_heading = glade_xml_get_widget (xml, "recp_name_heading"));
    g_assert
      (td.recp_account_entry = glade_xml_get_widget (xml, "recp_account_entry"));
    g_assert
      (recp_account_heading = glade_xml_get_widget (xml, "recp_account_heading"));
    g_assert
      (td.recp_bankcode_entry = glade_xml_get_widget (xml, "recp_bankcode_entry"));
    g_assert
      (recp_bankcode_heading = glade_xml_get_widget (xml, "recp_bankcode_heading"));
    g_assert
      (td.recp_bankname_label = glade_xml_get_widget (xml, "recp_bankname_label"));
    g_assert
      (amount_hbox = glade_xml_get_widget (xml, "amount_hbox"));
    g_assert
      (td.purpose_entry = glade_xml_get_widget (xml, "purpose_entry"));
    g_assert
      (td.purpose_cont_entry = glade_xml_get_widget (xml, "purpose_cont_entry"));
    g_assert
      (orig_name_label = glade_xml_get_widget (xml, "orig_name_label"));
    g_assert
      (orig_account_label = glade_xml_get_widget (xml, "orig_account_label"));
    g_assert
      (orig_bankname_label = glade_xml_get_widget (xml, "orig_bankname_label"));
    g_assert
      (orig_bankcode_label = glade_xml_get_widget (xml, "orig_bankcode_label"));
    g_assert
      (orig_name_heading = glade_xml_get_widget (xml, "orig_name_heading"));
    g_assert
      (orig_account_heading = glade_xml_get_widget (xml, "orig_account_heading"));
    g_assert
      (orig_bankname_heading = glade_xml_get_widget (xml, "orig_bankname_heading"));
    g_assert
      (orig_bankcode_heading = glade_xml_get_widget (xml, "orig_bankcode_heading"));
    g_assert
      (exec_later_button = glade_xml_get_widget (xml, "exec_later_button"));
    g_assert
      (td.template_option = glade_xml_get_widget (xml, "template_optionmenu"));
    g_assert
      (add_templ_button = glade_xml_get_widget (xml, "add_templ_button"));

    td.amount_edit = gnc_amount_edit_new();
    gtk_box_pack_start_defaults(GTK_BOX(amount_hbox), td.amount_edit);
    gnc_amount_edit_set_evaluate_on_enter (GNC_AMOUNT_EDIT (td.amount_edit), 
      TRUE);
    gnc_amount_edit_set_fraction (GNC_AMOUNT_EDIT (td.amount_edit),
				  xaccAccountGetCommoditySCU (gnc_acc));

    /* Check for what kind of transaction this should be, and change
       the labels accordingly. */
    switch (trans_type) {
    case SINGLE_TRANSFER:
      /* all labels are already set */
      break;
    case SINGLE_DEBITNOTE:
      gtk_label_set_text (GTK_LABEL (heading_label), 
			  /* Translators: Strings from this file are really only
			   * needed inside Germany (HBCI is not supported anywhere
			   * else). You may safely ignore strings from the
			   * import-export/hbci subdirectory in other countries. */
			  _("Enter an Online Direct Debit Note"));

      gtk_label_set_text (GTK_LABEL (recp_name_heading),
			  _("Debited Account Owner"));
      gtk_label_set_text (GTK_LABEL (recp_account_heading),
			  _("Debited Account Number"));
      gtk_label_set_text (GTK_LABEL (recp_bankcode_heading),
			  _("Debited Account Bank Code"));

      gtk_label_set_text (GTK_LABEL (orig_name_heading),
			  _("Credited Account Owner"));
      gtk_label_set_text (GTK_LABEL (orig_account_heading),
			  _("Credited Account Number"));
      gtk_label_set_text (GTK_LABEL (orig_bankcode_heading),
			  _("Credited Account Bank Code"));
      break;

    default:
      printf("dialog-hbcitrans: Oops, unknown GNC_HBCI_Transtype %d.\n",
	     trans_type);
    }
    
    /* Make this button insensitive since it's still unimplemented. */
    gtk_widget_set_sensitive (GTK_WIDGET (exec_later_button), FALSE);
    
    /* Fill in the values from the objects */
    gtk_label_set_text (GTK_LABEL (orig_name_label), 
			(strlen(HBCI_Customer_custName (customer))>0 ? 
			 HBCI_Customer_custName (customer) :
			 HBCI_Customer_custId (customer)));
    gtk_label_set_text (GTK_LABEL (orig_account_label), 
			HBCI_Account_accountId (h_acc));
    gtk_label_set_text (GTK_LABEL (orig_bankname_label), 
			(strlen(HBCI_Bank_name (bank))>0 ?
			 HBCI_Bank_name (bank) :
			 _("(unknown)")));
    gtk_label_set_text (GTK_LABEL (orig_bankcode_label), 
			HBCI_Bank_bankCode (bank));

    /* fill OptionMenu for choosing a transaction template */
    g_list_foreach(td.templ, fill_template_menu_func, 
		   gtk_option_menu_get_menu 
		   ( GTK_OPTION_MENU (td.template_option)));
    
    /* Connect signals */
    gnc_option_menu_init_w_signal (td.template_option, 
				   GTK_SIGNAL_FUNC(template_selection_cb),
				   &td);
    gtk_signal_connect(GTK_OBJECT (add_templ_button), "clicked",
		       GTK_SIGNAL_FUNC(add_template_cb), &td);

    /* Default button */
    gnome_dialog_set_default (GNOME_DIALOG (dialog), 0);
    
    gtk_widget_grab_focus (td.recp_name_entry);

    /* Hide on close instead of destroy since we still need the values
       from the boxes. */
    gnome_dialog_close_hides (GNOME_DIALOG (dialog), TRUE);

    /* Repeat until HBCI action was successful or user pressed cancel */
    do {

      gtk_widget_show_all (dialog); 

      result = gnome_dialog_run (GNOME_DIALOG (dialog));
      gtk_widget_hide_all (dialog);
      /*result = gnome_dialog_run_and_close (GNOME_DIALOG (dialog));*/
      /* printf("hbci_trans: result button was %d.\n", result); */
	
      *templ = td.templ;

      /* Was cancel pressed or dialog closed? 0 == execute now, 1 ==
	 scheduled for later execution (currently unimplemented) */
      if ((result != 0) && (result != 1)) {
	gtk_widget_destroy (GTK_WIDGET (dialog));
	return NULL;
      }
	
      /* Fill in the user-entered values */
      trans = HBCI_Transaction_new();
	
      /* OpenHBCI newer than 0.9.8: use account's bankCode values
       * instead of the bank's ones since this is what some banks
       * require. */
      HBCI_Transaction_setOurCountryCode (trans, 
					  HBCI_Account_countryCode (h_acc));
      HBCI_Transaction_setOurBankCode (trans, 
				       HBCI_Account_instituteCode (h_acc));
      HBCI_Transaction_setOurAccountId (trans, HBCI_Account_accountId (h_acc));
      HBCI_Transaction_setOurSuffix (trans, HBCI_Account_accountSuffix (h_acc));
	
      HBCI_Transaction_setOtherCountryCode (trans, 280);
      HBCI_Transaction_setOtherBankCode 
	(trans, gtk_entry_get_text (GTK_ENTRY (td.recp_bankcode_entry)));
      /* printf("Got otherBankCode %s.\n",
	 HBCI_Transaction_otherBankCode (trans)); */
      HBCI_Transaction_setOtherAccountId
	(trans, gtk_entry_get_text (GTK_ENTRY (td.recp_account_entry)));
      /* printf("Got otherAccountId %s.\n",
	 HBCI_Transaction_otherAccountId (trans)); */
      HBCI_Transaction_addOtherName
	(trans, gtk_entry_get_text (GTK_ENTRY (td.recp_name_entry)));
	
      HBCI_Transaction_addDescription
	(trans, gtk_entry_get_text (GTK_ENTRY (td.purpose_entry)));
      HBCI_Transaction_addDescription
	(trans, gtk_entry_get_text (GTK_ENTRY (td.purpose_cont_entry)));
	
      HBCI_Transaction_setValue 
	(trans, HBCI_Value_new_double 
	 (gnc_amount_edit_get_damount (GNC_AMOUNT_EDIT (td.amount_edit)), "EUR"));
      /* FIXME: Replace "EUR" by account-dependent string here. */
      /*printf("dialog-hbcitrans: Got value as %s .\n", 
	HBCI_Value_toReadableString (HBCI_Transaction_value (trans)));*/
      if (HBCI_Value_getValue (HBCI_Transaction_value (trans)) == 0.0) {
	printf("dialog-hbcitrans: Oops, value is zero. Cancelling HBCI job.\n");
	gtk_widget_destroy (GTK_WIDGET (dialog));
	HBCI_Transaction_delete (trans);
	return NULL;
      }

      {
	/* Create a Do-Transaction (Transfer) job. */
	HBCI_OutboxJobTransfer *transfer_job;
	HBCI_OutboxJobDebitNote *debit_job;
	HBCI_OutboxJob *job = NULL;
    
	switch (trans_type) {
	case SINGLE_DEBITNOTE:
	  debit_job =
	    HBCI_OutboxJobDebitNote_new (customer, (HBCI_Account *)h_acc, trans);
	  job = HBCI_OutboxJobDebitNote_OutboxJob (debit_job);
	  break;
	case SINGLE_TRANSFER:
	  transfer_job = 
	    HBCI_OutboxJobTransfer_new (customer, (HBCI_Account *)h_acc, trans);
	  job = HBCI_OutboxJobTransfer_OutboxJob (transfer_job);
	  break;
	default:
	  printf("dialog-hbcitrans: Oops, unknown GNC_HBCI_Transtype %d.\n",
		 trans_type);
	  transfer_job = 
	    HBCI_OutboxJobTransfer_new (customer, (HBCI_Account *)h_acc, trans);
	  job = HBCI_OutboxJobTransfer_OutboxJob (transfer_job);
	}

	HBCI_API_clearQueueByStatus (api, HBCI_JOB_STATUS_NONE);

	g_assert (job);
	HBCI_API_addJob (api, job);

	if (result == 0) {

	  /* If the user pressed "execute now", then execute this job now. */
	  successful = gnc_hbci_api_execute (parent, api, job, interactor);

	  /*printf("dialog-hbcitrans: Ok, result of api_execute was %d.\n", 
	    successful);*/
	  
	  if (!successful) {
	    /* HBCI_API_executeOutbox failed. */
	    if ((HBCI_OutboxJob_status (job) == HBCI_JOB_STATUS_DONE) &&
		(HBCI_OutboxJob_result (job) == HBCI_JOB_RESULT_FAILED)) 
	      	successful = !gnc_verify_dialog_parented
		  (parent, 
		   FALSE,
		   _("The job was successfully sent to the bank, but the \n"
		     "bank is refusing to execute the job. Please check \n"
		     "the log window for the exact error message of the \n"
		     "bank. The line with the error message contains a \n"
		     "code number that is greater than 9000.\n"
		     "\n"
		     "Do you want to enter the job again?"));

	    HBCI_Transaction_delete (trans);
	    trans = NULL;
	    HBCI_API_clearQueueByStatus (api, HBCI_JOB_STATUS_NONE);
	  }
	} /* result == 0 */
	else {
	  /* huh? Only result == 0 should be possible. Simply ignore
	     this case. */
	  break;
	} /* result == 0 */
	  
      } /* Create a do-transaction (transfer) job */
	
    } while (!successful);
    
  } /* GtkWidget declarations/definitions */
  
  HBCI_API_clearQueueByStatus (api, HBCI_JOB_STATUS_NONE);
  gtk_widget_destroy (GTK_WIDGET (dialog));
  return trans;
}



/* -------------------------------------- */
/* Callbacks */
/* -------------------------------------- */
static void fill_entry(const char *str, GtkWidget *entry) { 
  gtk_entry_set_text (GTK_ENTRY (entry), str ? str : ""); 
}

void template_selection_cb(GtkButton *b,
			   gpointer user_data)
{
  TransData *td = user_data;
  g_assert(td);
  unsigned index = gnc_option_menu_get_active (td->template_option);
  /*printf("template_selection_cd: %d is active \n", index);*/
  if ((index > 0) && (index <= g_list_length(td->templ)))
    {
      GNCTransTempl *templ = g_list_nth_data(td->templ, index-1);
      /*printf("template_selection_cd: using template %s \n", 
	gnc_trans_templ_get_name(templ));*/

      fill_entry(gnc_trans_templ_get_recp_name(templ), td->recp_name_entry);
      fill_entry(gnc_trans_templ_get_recp_account(templ), td->recp_account_entry);
      fill_entry(gnc_trans_templ_get_recp_bankcode(templ), td->recp_bankcode_entry);
      fill_entry(gnc_trans_templ_get_purpose(templ), td->purpose_entry);
      fill_entry(gnc_trans_templ_get_purpose_cont(templ), td->purpose_cont_entry);
      
      gnc_amount_edit_set_amount (GNC_AMOUNT_EDIT (td->amount_edit), 
				  gnc_trans_templ_get_amount (templ));
    }
}

/* -------------------------------------- */
/* -------------------------------------- */
/* Copied from window-help.c */
static void
goto_string_cb(char * string, gpointer data)
{
  if(!data) return;
  if(!string) {
    *(char **)data = NULL;
  }
  else {
    *(char **)data = g_strdup(string);
  }
}
void add_template_cb(GtkButton *b,
		     gpointer user_data)
{
  TransData *td = user_data;
  GtkWidget *dlg;
  char *name;
  int retval = -1;
  g_assert(td);

  dlg = gnome_request_dialog(FALSE,
			     _("Enter name for new template:"), "", 250,
			     &goto_string_cb, &name, GTK_WINDOW(td->parent));
  retval = gnome_dialog_run_and_close(GNOME_DIALOG(dlg));

  if ((retval == 0) && name && (strlen(name) > 0)) {
    GNCTransTempl *r;
    /*printf("add_template_cb: adding template '%s'\n", name);*/
    r = gnc_trans_templ_new_full
      (name, 
       gtk_entry_get_text (GTK_ENTRY (td->recp_name_entry)),
       gtk_entry_get_text (GTK_ENTRY (td->recp_account_entry)),
       gtk_entry_get_text (GTK_ENTRY (td->recp_bankcode_entry)),
       gnc_amount_edit_get_amount (GNC_AMOUNT_EDIT (td->amount_edit)),
       gtk_entry_get_text (GTK_ENTRY (td->purpose_entry)),
       gtk_entry_get_text (GTK_ENTRY (td->purpose_cont_entry)));

    /* Append new template to the list. */
    td->templ = g_list_append(td->templ, r);
    
    /* Also append that template to the OptionMenu */
    fill_template_menu_func(r, 
			    gtk_option_menu_get_menu 
			    ( GTK_OPTION_MENU (td->template_option)));
    /* the show_all is necessary since otherwise the new item doesn't show up */
    gtk_widget_show_all (GTK_WIDGET (gtk_option_menu_get_menu 
				     ( GTK_OPTION_MENU (td->template_option))));
    gnc_option_menu_init_w_signal (td->template_option, 
				   GTK_SIGNAL_FUNC(template_selection_cb),
				   td);
  }
}

