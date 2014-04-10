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

#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#include <gnome.h>
#include <openhbci/bank.h>
#include <openhbci/outboxaccjobs.h>
#include <openhbci.h>

#include "dialog-utils.h"
#include "gnc-ui.h"
#include "gnc-amount-edit.h"
#include "dialog-transfer.h"

#include "gnc-hbci-utils.h"
#include "gnc-hbci-trans-templ.h"
#include "dialog-hbcitrans.h"
#if HAVE_KTOBLZCHECK_H
#  include <ktoblzcheck.h>
#endif

/* -------------------------------------- */
/* Data structure */
/* -------------------------------------- */
struct _trans_data 
{
  /* The dialog itself */
  GtkWidget *dialog;
  GtkWidget *parent;

  /* Whether this is a transfer or a direct debit */
  GNC_HBCI_Transtype trans_type;

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

  /* The HBCI transaction that got created here */
  HBCI_Transaction *hbci_trans;
  
  /* The gnucash transaction dialog where the user specifies the gnucash transaction. */
  XferDialog *gnc_trans_dialog;
  
  /* The Gnucash transaction that got created here */
  Transaction *gnc_trans;
  
#if HAVE_KTOBLZCHECK_H
  /* object for Account number checking */
  AccountNumberCheck *blzcheck;
#endif
};


void gnc_hbci_dialog_delete(HBCITransDialog *td)
{
  if (!td) return;
  /* Unregister handler for transaction creation callback */
  if (td->gnc_trans_dialog)
    gnc_xfer_dialog_set_txn_cb(td->gnc_trans_dialog, NULL, NULL);
  if (td->hbci_trans)
    HBCI_Transaction_delete (td->hbci_trans);

  gtk_widget_destroy (GTK_WIDGET (td->dialog));
#if HAVE_KTOBLZCHECK_H
  AccountNumberCheck_delete(td->blzcheck);
#endif    
}

GList *gnc_hbci_dialog_get_templ(const HBCITransDialog *td)
{
  g_assert(td);
  return td->templ;
}
GtkWidget *gnc_hbci_dialog_get_parent(const HBCITransDialog *td)
{
  g_assert(td);
  return td->parent;
}
const HBCI_Transaction *gnc_hbci_dialog_get_htrans(const HBCITransDialog *td)
{
  g_assert(td);
  return td->hbci_trans;
}
Transaction *gnc_hbci_dialog_get_gtrans(const HBCITransDialog *td)
{
  g_assert(td);
  return td->gnc_trans;
}
void gnc_hbci_dialog_hide(HBCITransDialog *td)
{
  g_assert(td);
  gtk_widget_hide_all (td->dialog);
}
void gnc_hbci_dialog_show(HBCITransDialog *td)
{
  g_assert(td);
  gtk_widget_show_all (td->dialog);
}


/* -------------------------------------- */
/* Prototypes; callbacks for dialog function */
/* -------------------------------------- */

HBCI_Transaction *
hbci_trans_fill_values(const HBCI_Account *h_acc, HBCITransDialog *td);
gboolean
check_ktoblzcheck(GtkWidget *parent, const HBCITransDialog *td, 
		  const HBCI_Transaction *trans);

void template_selection_cb(GtkButton *b, gpointer user_data);
void add_template_cb(GtkButton *b, gpointer user_data);
void blz_changed_cb(GtkEditable *e, gpointer user_data);



/* -------------------------------------- */
/* Main dialog function */
/* -------------------------------------- */

/* doesn't exist any longer */

/* ************************************************************
 * constructor 
 */

static void fill_template_menu_func(gpointer data, gpointer user_data)
{
  GNCTransTempl *templ = data;
  GtkMenu *menu = user_data;
  GtkWidget *item;

  g_assert(templ);
  g_assert(menu);
  
  item = gtk_menu_item_new_with_label(gnc_trans_templ_get_name(templ));
  g_assert(item);
  
  gtk_object_set_user_data(GTK_OBJECT(item), templ);
  gtk_menu_append(menu, item);
}

HBCITransDialog *
gnc_hbci_dialog_new (GtkWidget *parent,
		const HBCI_Account *h_acc,
		const HBCI_Customer *customer,
		Account *gnc_acc,
		GNC_HBCI_Transtype trans_type,
		GList *templates)
{
  GladeXML *xml;
  const HBCI_Bank *bank;
  HBCITransDialog *td;

  td = g_new0(HBCITransDialog, 1);
  
  td->parent = parent;
  td->templ = templates;
  td->trans_type = trans_type;
  g_assert (h_acc);
  g_assert (customer);
  bank = HBCI_Account_bank (h_acc);
  g_assert (bank);
#if HAVE_KTOBLZCHECK_H
  td->blzcheck = AccountNumberCheck_new();
#endif
  
  xml = gnc_glade_xml_new ("hbci.glade", "HBCI_trans_dialog");

  td->dialog = glade_xml_get_widget (xml, "HBCI_trans_dialog");

  if (parent)
    gnome_dialog_set_parent (GNOME_DIALOG (td->dialog), GTK_WINDOW (parent));
  
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
      (td->recp_name_entry = glade_xml_get_widget (xml, "recp_name_entry"));
    g_assert 
      (recp_name_heading = glade_xml_get_widget (xml, "recp_name_heading"));
    g_assert
      (td->recp_account_entry = glade_xml_get_widget (xml, "recp_account_entry"));
    g_assert
      (recp_account_heading = glade_xml_get_widget (xml, "recp_account_heading"));
    g_assert
      (td->recp_bankcode_entry = glade_xml_get_widget (xml, "recp_bankcode_entry"));
    g_assert
      (recp_bankcode_heading = glade_xml_get_widget (xml, "recp_bankcode_heading"));
    g_assert
      (td->recp_bankname_label = glade_xml_get_widget (xml, "recp_bankname_label"));
    g_assert
      (amount_hbox = glade_xml_get_widget (xml, "amount_hbox"));
    g_assert
      (td->purpose_entry = glade_xml_get_widget (xml, "purpose_entry"));
    g_assert
      (td->purpose_cont_entry = glade_xml_get_widget (xml, "purpose_cont_entry"));
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
      (td->template_option = glade_xml_get_widget (xml, "template_optionmenu"));
    g_assert
      (add_templ_button = glade_xml_get_widget (xml, "add_templ_button"));

    td->amount_edit = gnc_amount_edit_new();
    gtk_box_pack_start_defaults(GTK_BOX(amount_hbox), td->amount_edit);
    gnc_amount_edit_set_evaluate_on_enter (GNC_AMOUNT_EDIT (td->amount_edit), 
      TRUE);
    gnc_amount_edit_set_fraction (GNC_AMOUNT_EDIT (td->amount_edit),
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
    g_list_foreach(td->templ, fill_template_menu_func, 
		   gtk_option_menu_get_menu 
		   ( GTK_OPTION_MENU (td->template_option)));
    
    /* Connect signals */
    gnc_option_menu_init_w_signal (td->template_option, 
				   GTK_SIGNAL_FUNC(template_selection_cb),
				   td);
    gtk_signal_connect(GTK_OBJECT (add_templ_button), "clicked",
		       GTK_SIGNAL_FUNC(add_template_cb), td);
    gtk_signal_connect(GTK_OBJECT (td->recp_bankcode_entry), "changed",
		       GTK_SIGNAL_FUNC(blz_changed_cb), td);

    /* Default button */
    gnome_dialog_set_default (GNOME_DIALOG (td->dialog), 0);
    
    gtk_widget_grab_focus (td->recp_name_entry);

    /* Hide on close instead of destroy since we still need the values
       from the boxes. */
    gnome_dialog_close_hides (GNOME_DIALOG (td->dialog), TRUE);

  } /* GtkWidget declarations/definitions */
  
  return td;
}


/* ************************************************************
 * Now all the functions where the action happens.
 */

int gnc_hbci_dialog_run_until_ok(HBCITransDialog *td, 
				 const HBCI_Account *h_acc)
{
  int result;
  gboolean values_ok;

  /* Repeat until entered values make sense */
  do {

    /* Make sure to show the dialog here */
    gtk_widget_show_all (td->dialog); 

    /* Now run the dialog until it gets closed by a button press. */
    result = gnome_dialog_run (GNOME_DIALOG (td->dialog));
    /* printf("hbci_trans: result button was %d.\n", result); */

    /* The dialog gets hidden anyway as soon as any button is pressed. */
    gtk_widget_hide_all (td->dialog);

    /* Was cancel pressed or dialog closed? 0 == execute now, 1 ==
       scheduled for later execution (currently unimplemented) */
    if ((result != 0) && (result != 1)) {
      return -1;
    }

    /* Now fill in the values from the entry fields into a new
       HBCI_Transaction. */
    td->hbci_trans = hbci_trans_fill_values(h_acc, td);
    values_ok = TRUE;

    /*printf("dialog-hbcitrans: Got value as %s .\n", 
      HBCI_Value_toReadableString (HBCI_Transaction_value (trans)));*/
    if (HBCI_Value_getValue (HBCI_Transaction_value (td->hbci_trans)) == 0.0) {
      gtk_widget_show_all (td->dialog); 
      values_ok = !gnc_verify_dialog
	(GTK_WIDGET (td->dialog),
	 TRUE,
	 "%s",
	 _("The amount is zero or the amount field could not be \n"
	   "interpreted correctly. You might have mixed up decimal \n"
	   "point and comma, compared to your locale settings. \n"
	   "\n"
	   "This does not result in a valid online transfer job.\n"
	   "Do you want to enter the job again?"));
      if (values_ok) {
	HBCI_Transaction_delete (td->hbci_trans);
	return -1;
      }
      continue;
    } /* check Transaction_value */

    /* FIXME: If this is a direct debit, set the textkey/ "Textschluessel"/
       transactionCode according to some GUI selection here!! */
    /*if (td->trans_type == SINGLE_DEBITNOTE)
      HBCI_Transaction_setTransactionCode (td->hbci_trans, 05);*/

    /* And finally check the account code, if ktoblzcheck is available. */
    values_ok = check_ktoblzcheck(GTK_WIDGET (td->dialog), td, td->hbci_trans);

  } while (!values_ok);

  return result;
}


/** Create a new HBCI_Transaction, fill the values from the entry
    fields into it and return it. The caller must
    HBCI_Transaction_delete() it when finished. */
HBCI_Transaction *
hbci_trans_fill_values(const HBCI_Account *h_acc, HBCITransDialog *td)
{
  /* Fill in the user-entered values */
  HBCI_Transaction *trans = HBCI_Transaction_new();
	
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
    (trans, gtk_entry_get_text (GTK_ENTRY (td->recp_bankcode_entry)));
  /* printf("Got otherBankCode %s.\n",
     HBCI_Transaction_otherBankCode (trans)); */
  HBCI_Transaction_setOtherAccountId
    (trans, gtk_entry_get_text (GTK_ENTRY (td->recp_account_entry)));
  /* printf("Got otherAccountId %s.\n",
     HBCI_Transaction_otherAccountId (trans)); */
  HBCI_Transaction_addOtherName
    (trans, gtk_entry_get_text (GTK_ENTRY (td->recp_name_entry)));
	
  HBCI_Transaction_addDescription
    (trans, gtk_entry_get_text (GTK_ENTRY (td->purpose_entry)));
  HBCI_Transaction_addDescription
    (trans, gtk_entry_get_text (GTK_ENTRY (td->purpose_cont_entry)));
	
  /* FIXME: Replace "EUR" by account-dependent string here. */
  HBCI_Transaction_setValue 
    (trans, HBCI_Value_new_double 
     (gnc_amount_edit_get_damount (GNC_AMOUNT_EDIT (td->amount_edit)), "EUR"));

  /* If this is a direct debit, a textkey/ "Textschluessel"/
     transactionCode different from the default has to be set. */
  if (td->trans_type == SINGLE_DEBITNOTE)
    HBCI_Transaction_setTransactionCode (trans, 05);

  return trans;
}

/** Checks the account code in the HBCI_Transaction, if the
    ktoblzcheck package is available. Returns TRUE if everything is
    fine, or FALSE if this transaction should be entered again. */
gboolean
check_ktoblzcheck(GtkWidget *parent, const HBCITransDialog *td, 
		  const HBCI_Transaction *trans)	
{
#if HAVE_KTOBLZCHECK_H
  int blzresult;
  const char *blztext;
  gboolean values_ok = TRUE;
  
  blzresult = AccountNumberCheck_check
    (td->blzcheck, 
     HBCI_Transaction_otherBankCode (trans),
     HBCI_Transaction_otherAccountId (trans));
  switch (blzresult) {
  case 2:
    gtk_widget_show_all (parent); 
    values_ok = gnc_verify_dialog
      (parent,
       TRUE,
       _("The internal check of the destination account number '%s' \n"
	 "at the specified bank with bank code '%s' failed. This means \n"
	 "the account number might contain an error. Should the online \n"
	 "transfer job be sent with this account number anyway?"),
       HBCI_Transaction_otherAccountId (trans),
       HBCI_Transaction_otherBankCode (trans));
    blztext = "Kontonummer wahrscheinlich falsch";
    break;
  case 0:
    blztext = "Kontonummer ok";
    break;
  case 3:
    blztext = "bank unbekannt";
    break;
  default:
  case 1:
    blztext = "unbekannt aus unbekanntem grund";
    break;
  }
	
  printf("gnc_hbci_trans: KtoBlzCheck said check is %d = %s\n",
	 blzresult, blztext);
  return values_ok;
#else
  return TRUE;
#endif    
}

HBCI_OutboxJob *
gnc_hbci_trans_dialog_enqueue(HBCITransDialog *td, HBCI_API *api,
			      const HBCI_Customer *customer, 
			      HBCI_Account *h_acc, 
			      GNC_HBCI_Transtype trans_type) 
{
  HBCI_OutboxJob *job;
      
  /* Create a Do-Transaction (Transfer) job. */
  switch (trans_type) {
  case SINGLE_DEBITNOTE:
    {
      HBCI_OutboxJobDebitNote *debit_job =
	HBCI_OutboxJobDebitNote_new (customer, h_acc, td->hbci_trans);
      job = HBCI_OutboxJobDebitNote_OutboxJob (debit_job);
    }
    break;
  case SINGLE_TRANSFER:
    {
      HBCI_OutboxJobTransfer *transfer_job = 
	HBCI_OutboxJobTransfer_new (customer, h_acc, td->hbci_trans);
      job = HBCI_OutboxJobTransfer_OutboxJob (transfer_job);
    }
    break;
  default:
    {
      /*printf("dialog-hbcitrans: Oops, unknown GNC_HBCI_Transtype %d.\n",
	trans_type);*/
      HBCI_OutboxJobTransfer *transfer_job = 
	HBCI_OutboxJobTransfer_new (customer, h_acc, td->hbci_trans);
      job = HBCI_OutboxJobTransfer_OutboxJob (transfer_job);
    }
  }
  g_assert (job);

  /* Make really sure there is no other job in the queue */
  HBCI_API_clearQueueByStatus (api, HBCI_JOB_STATUS_NONE);

  /* Add job to queue */
  HBCI_API_addJob (api, job);

  return job;
}

gboolean 
gnc_hbci_trans_dialog_execute(HBCITransDialog *td, HBCI_API *api, 
			      HBCI_OutboxJob *job, GNCInteractor *interactor)
{
  gboolean successful;
  g_assert(td);
  g_assert(api);
  g_assert(job);

  successful = gnc_hbci_api_execute (td->parent, api, job, interactor);

  /*printf("dialog-hbcitrans: Ok, result of api_execute was %d.\n", 
    successful);*/
	  
  if (!successful) {
    /* HBCI_API_executeOutbox failed. */
    if ((HBCI_OutboxJob_status (job) == HBCI_JOB_STATUS_DONE) &&
	(HBCI_OutboxJob_result (job) == HBCI_JOB_RESULT_FAILED)) 
      successful = !gnc_verify_dialog
	(td->parent, 
	 FALSE,
	 "%s",
	 _("The job was successfully sent to the bank, but the \n"
	   "bank is refusing to execute the job. Please check \n"
	   "the log window for the exact error message of the \n"
	   "bank. The line with the error message contains a \n"
	   "code number that is greater than 9000.\n"
	   "\n"
	   "Do you want to enter the job again?"));

    HBCI_Transaction_delete (td->hbci_trans);
    td->hbci_trans = NULL;
  }
  /* Watch out! The job *has* to be removed from the queue
     here because otherwise it might be executed again. */
  HBCI_API_clearQueueByStatus (api, HBCI_JOB_STATUS_NONE);
  return successful;
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
  HBCITransDialog *td = user_data;
  unsigned index;
  g_assert(td);
  index = gnc_option_menu_get_active (td->template_option);
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

void blz_changed_cb(GtkEditable *e, gpointer user_data)
{
#if HAVE_KTOBLZCHECK_H
  HBCITransDialog *td = user_data;
  const AccountNumberCheck_Record *record;
  g_assert(td);

  record = AccountNumberCheck_findBank
    (td->blzcheck, 
     gtk_entry_get_text (GTK_ENTRY (td->recp_bankcode_entry)));
  
  if (record) {
    const char *bankname = AccountNumberCheck_Record_bankName (record);
    gtk_label_set_text (GTK_LABEL (td->recp_bankname_label),
			(strlen(bankname)>0 ? bankname : _("(unknown)")));
    gtk_widget_show_all (td->recp_bankname_label);

    /*printf("blz_changed_cb: KtoBlzCheck said check is bank is '%s' at '%s'.\n",
      bankname,
      AccountNumberCheck_Record_location (record));*/

  } else {
    gtk_label_set_text (GTK_LABEL (td->recp_bankname_label),
			_("(unknown)"));
    gtk_widget_show_all (td->recp_bankname_label);
  }
#endif    
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
  HBCITransDialog *td = user_data;
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

void gnc_hbci_dialog_xfer_cb(Transaction *trans, gpointer user_data)
{
  HBCITransDialog *td = user_data;
  g_assert(td);
  if (trans) {
    td->gnc_trans = trans;
    /* Unregister handler for transaction creation callback */
    if (td->gnc_trans_dialog)
      gnc_xfer_dialog_set_txn_cb(td->gnc_trans_dialog, NULL, NULL);
    td->gnc_trans_dialog = NULL;
  }
  else {
    if (td->gnc_trans_dialog) {
      gnc_xfer_dialog_set_txn_cb(td->gnc_trans_dialog, NULL, NULL);
      td->gnc_trans_dialog = NULL;
    }
  }
  return;
}

