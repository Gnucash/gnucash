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

#include "dialog-utils.h"
#include "gnc-ui.h"
#include "dialog-hbcitrans.h"

HBCI_Transaction *
gnc_hbci_trans (GtkWidget *parent,
		HBCI_API *api,
		const HBCI_Account *h_acc,
		const HBCI_Customer *customer)
{
  GtkWidget *dialog;
  GladeXML *xml;
  HBCI_Transaction *res_trans = NULL;
  gint result;
  const HBCI_Bank *bank;
  
  g_assert (api);
  g_assert (h_acc);
  g_assert (customer);
  bank = HBCI_Account_bank (h_acc);
    
  xml = gnc_glade_xml_new ("hbci.glade", "HBCI_trans_dialog");

  dialog = glade_xml_get_widget (xml, "HBCI_trans_dialog");

  if (parent)
    gnome_dialog_set_parent (GNOME_DIALOG (dialog), GTK_WINDOW (parent));
  
  {
    GtkWidget *recp_name_entry;
    GtkWidget *recp_account_entry;
    GtkWidget *recp_bankcode_entry;
    GtkWidget *recp_bankname_label;
    GtkWidget *amount_entry;
    GtkWidget *purpose_entry;
    GtkWidget *purpose_cont_entry;
    GtkWidget *orig_name_label;
    GtkWidget *orig_account_label;
    GtkWidget *orig_bankname_label;
    GtkWidget *orig_bankcode_label;
    HBCI_Transaction *trans;
    
    g_assert 
      (recp_name_entry = glade_xml_get_widget (xml, "recp_name_entry"));
    g_assert
      (recp_account_entry = glade_xml_get_widget (xml, "recp_account_entry"));
    g_assert
      (recp_bankcode_entry = glade_xml_get_widget (xml, "recp_bankcode_entry"));
    g_assert
      (recp_bankname_label = glade_xml_get_widget (xml, "recp_bankname_label"));
    g_assert
      (amount_entry = glade_xml_get_widget (xml, "amount_entry"));
    g_assert
      (purpose_entry = glade_xml_get_widget (xml, "purpose_entry"));
    g_assert
      (purpose_cont_entry = glade_xml_get_widget (xml, "purpose_cont_entry"));
    g_assert
      (orig_name_label = glade_xml_get_widget (xml, "orig_name_label"));
    g_assert
      (orig_account_label = glade_xml_get_widget (xml, "orig_account_label"));
    g_assert
      (orig_bankname_label = glade_xml_get_widget (xml, "orig_bankname_label"));
    g_assert
      (orig_bankcode_label = glade_xml_get_widget (xml, "orig_bankcode_label"));

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
    
    gnome_dialog_set_default (GNOME_DIALOG (dialog), 0);
    
    gtk_widget_grab_focus (recp_name_entry);

    gnome_dialog_close_hides (GNOME_DIALOG (dialog), TRUE);
  
    result = gnome_dialog_run_and_close (GNOME_DIALOG (dialog));
    
    /* Was cancel pressed? */
    if (result != 0) {
      gtk_widget_destroy (GTK_WIDGET (dialog));
      return NULL;
    }
      
    /* Fill in the user-entered values */
    trans = HBCI_Transaction_new();

    HBCI_Transaction_setOurCountryCode (trans, HBCI_Bank_countryCode (bank));
    HBCI_Transaction_setOurBankCode (trans, HBCI_Bank_bankCode (bank));
    HBCI_Transaction_setOurAccountId (trans, HBCI_Account_accountId (h_acc));
    HBCI_Transaction_setOurSuffix (trans, HBCI_Account_accountSuffix (h_acc));

    HBCI_Transaction_setOtherCountryCode (trans, 280);
    HBCI_Transaction_setOtherBankCode 
      (trans, gtk_entry_get_text (GTK_ENTRY (recp_bankcode_entry)));
    printf("Got otherBankCode %s.\n",
	   HBCI_Transaction_otherBankCode (trans));
    HBCI_Transaction_setOtherAccountId
      (trans, gtk_entry_get_text (GTK_ENTRY (recp_account_entry)));
    printf("Got otherAccountId %s.\n",
	   HBCI_Transaction_otherAccountId (trans));
    HBCI_Transaction_addOtherName
      (trans, gtk_entry_get_text (GTK_ENTRY (recp_name_entry)));
    
    HBCI_Transaction_addDescription
      (trans, gtk_entry_get_text (GTK_ENTRY (purpose_entry)));
    HBCI_Transaction_addDescription
      (trans, gtk_entry_get_text (GTK_ENTRY (purpose_cont_entry)));
    
    {
      const char *amount = gtk_entry_get_text (GTK_ENTRY (amount_entry));
      HBCI_Value *val = HBCI_Value_new_double (atof(amount), "EUR");
      printf("Got value as %s .\n", 
	     HBCI_Value_toReadableString (val));
      HBCI_Transaction_setValue (trans, val);
    }

    res_trans = trans;
  }
  
  gtk_widget_destroy (GTK_WIDGET (dialog));
  return res_trans;
}
