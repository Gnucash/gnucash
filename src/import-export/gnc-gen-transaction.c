/********************************************************************\
 * gnc-gen-transaction.c -- window/dialog for generic transaction 
 *                                                       processing *
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

#include "gnc-gen-transaction.h"

#include "dialog-utils.h"
#include "gnc-ui.h"
#include "global-options.h"
#include "gnc-ui-util.h"
#include "gnc-engine-util.h"
#include "dialog-account-pick.h"

#include "gnc-import-match-map.h"
#include "Transaction-matcher.h"

struct _generic_transaction_info
{
  GtkWidget *dialog;
  GtkWidget *clist;

  int clear_threshold;
  int add_threshold;
  int display_threshold;

  Account *source_acc;
};

/* The GtkCList widget has 7 columns. */
#define GEN_TRANS_CLIST_COLUMNS 7
/* Column with index 3 (i.e. the fourth column) is the one with the
   checkmark. */
#define GEN_TRANS_CLIST_CHECKCOL 3
#define GEN_TRANS_CLIST_DUPDESCR 4
#define GEN_TRANS_CLIST_DUPACC 5
#define GEN_TRANS_CLIST_DUPAMOUNT 6
/*static short module = MOD_IMPORT;*/

static void
refresh_clist_row (GNCGenTransaction *gui, 
		   int row_number, GNCImportTransInfo *info);

void gnc_gen_trans_delete (GNCGenTransaction *info)
{
  if (info == NULL) 
    return;
  gtk_widget_destroy (GTK_WIDGET (info->dialog));
  g_free (info);
}


static void 
on_matcher_ok_clicked (GtkButton *button,
			   gpointer user_data)
{
  GNCGenTransaction *info = user_data;
  g_assert (info);
  /*   DEBUG ("Begin") */
  gnc_import_process_trans_clist (GTK_CLIST (info->clist), NULL);
  /* DEBUG ("Deleting") */
  gnc_gen_trans_delete (info);
  /* DEBUG ("End") */
}

static void 
on_matcher_cancel_clicked (GtkButton *button,
			   gpointer user_data)
{
  GNCGenTransaction *info = user_data;
  gnc_gen_trans_delete (info);
}

static void 
run_account_picker_dialog (GNCGenTransaction *info, 
			   gint row, GNCImportTransInfo *trans_info)
{
  Account *old_acc, *new_acc;
  old_acc = gnc_import_TransInfo_get_destacc (trans_info);
  new_acc = gnc_account_picker_dialog(old_acc);
  if (old_acc != new_acc) {
    gnc_import_TransInfo_set_destacc (trans_info, new_acc);
    refresh_clist_row (info, row, trans_info);
  }
}

static void 
run_match_dialog (GNCGenTransaction *info, 
		  gint row, GNCImportTransInfo *trans_info)
{
  GNCImportMatchInfo *old = 
    gnc_import_TransInfo_get_selected_match (trans_info);
  
  gnc_import_match_picker_run_and_close (trans_info);

  if (old != gnc_import_TransInfo_get_selected_match (trans_info))
    refresh_clist_row (info, row, trans_info);
}

static void
clist_select_row_cb (GtkCList *clist,
		     gint row,
		     gint column,
		     GdkEventButton *event,
		     gpointer user_data) 
{
  GNCGenTransaction *info = user_data; 
  GNCImportTransInfo *trans_info;
  /*DEBUG("row: %d%s%d",row,", column: ",column);*/

  trans_info = gtk_clist_get_row_data (clist, row);
  if (trans_info == NULL)
    return;

  if (gnc_import_TransInfo_get_action (trans_info) == GNCImport_ADD) {
    if (column == GEN_TRANS_CLIST_CHECKCOL) {
      /* Checkmark was clicked for a new transaction. */
      gnc_import_TransInfo_set_action (trans_info, GNCImport_CLEAR);
      refresh_clist_row (info, row, trans_info);
      if (gnc_import_TransInfo_get_selected_match (trans_info) == NULL) 
	run_match_dialog (info, row, trans_info);
    }
    else if (column > GEN_TRANS_CLIST_CHECKCOL) {
      /* Destination account was clicked, for new transaction. */
      run_account_picker_dialog (info, row, trans_info);
    }
  }
  else {
    if (column == GEN_TRANS_CLIST_CHECKCOL) {
      /* Checkmark was clicked for a duplicate transaction. */
      gnc_import_TransInfo_set_action (trans_info, GNCImport_ADD);
      refresh_clist_row (info, row, trans_info);
      if (gnc_import_TransInfo_get_destacc (trans_info) == NULL)
	run_account_picker_dialog (info, row, trans_info);
    }
    else if (column > GEN_TRANS_CLIST_CHECKCOL) {
      /* Matching existing transaction was clicked, for duplicate
	 transaction. */
      run_match_dialog (info, row, trans_info);
    }
  }
}

static void
clist_unselect_row_cb (GtkCList *clist,
		     gint row,
		     gint column,
		     GdkEventButton *event,
		     gpointer user_data) 
{
  /*DEBUG("row: %d column: %d event.type %d send_event %d",
    row,column, event->type, event->send_event);*/
}

void gnc_gen_trans_freeze (GNCGenTransaction *gui)
{
  g_assert (gui);
  gtk_clist_freeze (GTK_CLIST (gui->clist));
}

void gnc_gen_trans_thaw (GNCGenTransaction *gui)
{
  g_assert (gui);
  gtk_clist_thaw (GTK_CLIST (gui->clist));
}

GNCGenTransaction *gnc_gen_trans_new (GtkWidget *parent, 
				      const gchar* heading)
{
  GNCGenTransaction *info;
  GladeXML *xml;
  GtkWidget *heading_label;
  
  gnc_should_log(MOD_IMPORT, GNC_LOG_TRACE);

  /* DEBUG("%s", "Begin..."); */

  info = g_new0 (GNCGenTransaction, 1);

  /* Initialize misc. data members */
  info->clear_threshold =
    gnc_lookup_number_option("Transaction Matcher","Auto-CLEAR threshold",
			     DEFAULT_CLEAR_THRESHOLD);
  info->add_threshold =
    gnc_lookup_number_option("Transaction Matcher","Auto-ADD threshold",
			     DEFAULT_ADD_THRESHOLD);
  info->display_threshold =
    gnc_lookup_number_option("Transaction Matcher","Match display threshold",
			     DEFAULT_DISPLAY_THRESHOLD);

  /* Initialize the GnomeDialog. */
  xml = gnc_glade_xml_new ("generic-import.glade", "transaction_list_dialog");

  g_assert
    (info->dialog = glade_xml_get_widget (xml, "transaction_list_dialog"));
  g_assert 
    (info->clist = glade_xml_get_widget (xml, "downloaded_clist"));
  g_assert
    (heading_label = glade_xml_get_widget (xml, "heading_label"));

  if (parent)
    gnome_dialog_set_parent (GNOME_DIALOG (info->dialog), 
			     GTK_WINDOW (parent));

  /* Connect signals */
  glade_xml_signal_connect_data(xml, "downloaded_transaction_select_cb",
				GTK_SIGNAL_FUNC(clist_select_row_cb), 
				info);
  glade_xml_signal_connect_data(xml, "downloaded_transaction_unselect_cb", 
				GTK_SIGNAL_FUNC(clist_unselect_row_cb),
				info);
  glade_xml_signal_connect_data(xml, "on_matcher_ok_clicked", 
				GTK_SIGNAL_FUNC(on_matcher_ok_clicked),
				info);
  glade_xml_signal_connect_data(xml, "on_matcher_cancel_clicked", 
				GTK_SIGNAL_FUNC(on_matcher_cancel_clicked),
				info);

  gnome_dialog_set_default (GNOME_DIALOG (info->dialog), 0);

  if (heading)
    gtk_label_set_text (GTK_LABEL (heading_label), heading);

  gtk_widget_grab_focus (glade_xml_get_widget (xml, "ok_button"));

  /* Hide on close instead of destroy since we still need the values
     from the boxes. */
  gnome_dialog_close_hides (GNOME_DIALOG (info->dialog), TRUE);
  gtk_widget_show_all (GTK_WIDGET (info->dialog));

  return info;
}

gboolean gnc_gen_trans_run (GNCGenTransaction *info) 
{
  gboolean result;
  
  /* DEBUG("Begin"); */

  result = gnome_dialog_run_and_close (GNOME_DIALOG (info->dialog));

  /* DEBUG("Result was %d", result); */

  /* No destroying here since the dialog was already destroyed through
     the ok_clicked handlers. */
  return result;
}

/* For cleaning up dangling row data. */
static void
trans_clist_row_destroy_cb (gpointer data)
{
  GNCImportTransInfo * transaction_info = data;
  /*DEBUG("Begin");*/
  gnc_import_TransInfo_delete (transaction_info);
}

static char **text_for_transInfo (GNCImportTransInfo *info)
{
  char **res;
  Transaction *trans = gnc_import_TransInfo_get_trans (info);
  Split *split = xaccTransGetSplit (trans, 0);
  int i;

  res = g_new0(char*, GEN_TRANS_CLIST_COLUMNS);
  /* FIXME: yes, yes, these allocated strings have to be g_free'd
     somewhere else, but currently they won't. */
  res[0] = xaccPrintDateSecs (xaccTransGetDate (trans));
  res[1] = g_strdup (xaccTransGetDescription (trans));
  res[2] = g_strdup 
    (xaccPrintAmount (xaccSplitGetValue (split), 
		      gnc_split_value_print_info(split, TRUE)));
  for (i = GEN_TRANS_CLIST_CHECKCOL; i < GEN_TRANS_CLIST_COLUMNS; i++)
    res[i] = "";
  return res;
}

static void
refresh_clist_row (GNCGenTransaction *gui, 
		   int row_number, GNCImportTransInfo *info)
{
  GNCImportMatchInfo *match;
  gchar *tmp;
  g_assert (gui);
  g_assert (info);

  /* Update the check mark */
  gnc_clist_set_check (GTK_CLIST (gui->clist), row_number,
		       GEN_TRANS_CLIST_CHECKCOL,
		       gnc_import_TransInfo_get_action(info)==GNCImport_ADD);

  /* Update the last two columns. */
  if (gnc_import_TransInfo_get_action (info) == GNCImport_ADD) {
    /* New transaction, so set the destination here. */
    gtk_clist_set_text (GTK_CLIST (gui->clist), row_number, 
			GEN_TRANS_CLIST_DUPDESCR, "");
    gtk_clist_set_text (GTK_CLIST (gui->clist), row_number, 
			GEN_TRANS_CLIST_DUPAMOUNT, "");
    if (gnc_import_TransInfo_get_destacc (info) != NULL) {
      tmp = xaccAccountGetFullName 
	(gnc_import_TransInfo_get_destacc (info),
	 gnc_get_account_separator ());
      gtk_clist_set_text (GTK_CLIST (gui->clist), row_number, 
			  GEN_TRANS_CLIST_DUPACC, tmp);
      free (tmp);
    }
    else {
    gtk_clist_set_text (GTK_CLIST (gui->clist), row_number, 
			GEN_TRANS_CLIST_DUPACC, _("Unspecified"));
    }
  }
  else {
    /* Duplicate of old transaction, so use that here */
    match = gnc_import_TransInfo_get_selected_match (info);
    if (match != NULL) {
      Split *match_split = gnc_import_MatchInfo_get_split (match);
      gtk_clist_set_text (GTK_CLIST (gui->clist), row_number, 
			  GEN_TRANS_CLIST_DUPDESCR, 
			  xaccTransGetDescription
			  (xaccSplitGetParent (match_split)));
      tmp = xaccSplitGetCorrAccountFullName (match_split,
					     gnc_get_account_separator ());
      gtk_clist_set_text (GTK_CLIST (gui->clist), row_number, 
			  GEN_TRANS_CLIST_DUPACC, tmp);
      g_free (tmp);
      gtk_clist_set_text 
	(GTK_CLIST (gui->clist), row_number, 
	 GEN_TRANS_CLIST_DUPAMOUNT, 
	 xaccPrintAmount (xaccSplitGetValue (match_split), 
			  gnc_split_value_print_info(match_split, TRUE)));
    } 
    else {
      gtk_clist_set_text (GTK_CLIST (gui->clist), row_number, 
			  GEN_TRANS_CLIST_DUPDESCR, _("Unspecified"));
      gtk_clist_set_text (GTK_CLIST (gui->clist), row_number, 
			  GEN_TRANS_CLIST_DUPACC, "");
      gtk_clist_set_text (GTK_CLIST (gui->clist), row_number, 
			  GEN_TRANS_CLIST_DUPAMOUNT, "");
    }
  }
}



void gnc_gen_trans_add_trans(GNCGenTransaction *gui, Transaction *trans)
{
  GNCImportTransInfo * transaction_info = NULL;
  gint row_number;
  g_assert (gui);
  g_assert (trans);
  
  if (gnc_import_exists_online_id (trans))
    return;
  else
    {
      transaction_info = gnc_import_TransInfo_new(trans, NULL);
      
      gnc_import_TransInfo_init_matches (transaction_info, 
					 gui->clear_threshold, 
					 gui->clear_threshold,
					 gui->display_threshold,
					 0.0);

      row_number = gtk_clist_append(GTK_CLIST (gui->clist),
				    text_for_transInfo (transaction_info));
      gtk_clist_set_row_data_full(GTK_CLIST (gui->clist),
				  row_number,
				  transaction_info,
				  trans_clist_row_destroy_cb);
      refresh_clist_row (gui, row_number, transaction_info);
    }
  return;
}/* end gnc_import_add_trans() */

