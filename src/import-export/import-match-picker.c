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
 * 51 Franklin Street, Fifth Floor    Fax:    +1-617-542-2652       *
 * Boston, MA  02110-1301,  USA       gnu@gnu.org                   *
\********************************************************************/
/** @addtogroup Import_Export
    @{ */
/** @internal
@file import-match-picker.c
   @brief The transaction match picker dialog 
   implementation
   @author Copyright (C) 2002 Benoit Grégoire
   @author Copyright (c) 2006 David Hampton <hampton@employees.org>
*/
 
#include "config.h"

#include <gtk/gtk.h>
#include <glib/gi18n.h>

#include "import-backend.h"
#include "import-match-picker.h"

#include "qof.h"
#include "gnc-ui-util.h"
#include <glade/glade.h>
#include "dialog-utils.h"
/********************************************************************\
 *   Constants   *
\********************************************************************/

#define GCONF_SECTION "dialogs/import/generic_matcher/match_picker"

enum downloaded_cols {
  DOWNLOADED_COL_ACCOUNT = 0,
  DOWNLOADED_COL_DATE,
  DOWNLOADED_COL_AMOUNT,
  DOWNLOADED_COL_DESCRIPTION,
  DOWNLOADED_COL_MEMO,
  DOWNLOADED_COL_BALANCED,
  DOWNLOADED_COL_INFO_PTR,
  NUM_DOWNLOADED_COLS
};

enum matcher_cols {
  MATCHER_COL_CONFIDENCE = 0,
  MATCHER_COL_CONFIDENCE_PIXBUF,
  MATCHER_COL_DATE,
  MATCHER_COL_AMOUNT,
  MATCHER_COL_DESCRIPTION,
  MATCHER_COL_MEMO,
  MATCHER_COL_INFO_PTR,
  NUM_MATCHER_COLS
};

/* Needs to be commented in again if any DEBUG() macro is used here. */
/*static short module = MOD_IMPORT;*/

/********************************************************************\
 *   Constants, should idealy be defined a user preference dialog    *
\********************************************************************/

static const int SHOW_NUMERIC_SCORE = FALSE;

/********************************************************************\
 *               Structures passed between the functions             *
\********************************************************************/

struct _transpickerdialog {
  GtkWidget * transaction_matcher;
  GtkTreeView * downloaded_view;
  GtkTreeView * match_view;
  GNCImportSettings * user_settings;
  struct _transactioninfo * selected_trans_info;
  GNCImportMatchInfo * selected_match_info;
};



static void
downloaded_transaction_append(GNCImportMatchPicker * matcher,
			      GNCImportTransInfo * transaction_info)
{
  GtkListStore *store;
  GtkTreeIter iter;
  GtkTreeSelection *selection;
  Transaction *trans;
  Split *split;
  gchar *text;
  const gchar *ro_text;
  gboolean found = FALSE;
  GNCImportTransInfo *local_info;

  g_assert(matcher);
  g_assert(transaction_info);

  /*DEBUG("Begin");*/

  /* Has the transaction already been added? */
  store = GTK_LIST_STORE(gtk_tree_view_get_model(matcher->downloaded_view));
  if (gtk_tree_model_get_iter_first(GTK_TREE_MODEL(store), &iter)) {
    do {
      gtk_tree_model_get(GTK_TREE_MODEL(store), &iter,
			 DOWNLOADED_COL_INFO_PTR, &local_info,
			 -1);
      if (local_info == transaction_info) {
	found = TRUE;
	break;
      }
    } while (gtk_tree_model_iter_next(GTK_TREE_MODEL(store), &iter));
  }
  if (!found)
    gtk_list_store_append(store, &iter);
  
  split = gnc_import_TransInfo_get_fsplit(transaction_info);
  trans = gnc_import_TransInfo_get_trans(transaction_info);
  
  /*Account*/
  ro_text = xaccAccountGetName(xaccSplitGetAccount(split));
  gtk_list_store_set(store, &iter, DOWNLOADED_COL_ACCOUNT, ro_text, -1);

  /*Date*/
  text = qof_print_date(xaccTransGetDate(trans));
  gtk_list_store_set(store, &iter, DOWNLOADED_COL_DATE, text, -1);
  g_free(text);
  
  /*Amount*/
  ro_text = xaccPrintAmount(xaccSplitGetAmount(split),
			    gnc_split_amount_print_info(split, TRUE));
  gtk_list_store_set(store, &iter, DOWNLOADED_COL_AMOUNT, ro_text, -1);
 
  /*Description*/
  ro_text = xaccTransGetDescription(trans);
  gtk_list_store_set(store, &iter, DOWNLOADED_COL_DESCRIPTION, ro_text, -1);
  
  /*Memo*/
  ro_text = xaccSplitGetMemo(split);
  gtk_list_store_set(store, &iter, DOWNLOADED_COL_MEMO, ro_text, -1);

  /*Imbalance*/
  ro_text = xaccPrintAmount(xaccTransGetImbalance(trans), 
			    gnc_default_print_info(TRUE));
  gtk_list_store_set(store, &iter, DOWNLOADED_COL_BALANCED, ro_text, -1);

  gtk_list_store_set(store, &iter, DOWNLOADED_COL_INFO_PTR,
		     transaction_info, -1);

  selection = gtk_tree_view_get_selection(matcher->downloaded_view);
  gtk_tree_selection_select_iter(selection, &iter);
}

/********************************************************************\
 *                                                                   *
 *                       GUI callbacks                               *
 *                                                                   *
\********************************************************************/

static void
downloaded_transaction_changed_cb (GtkTreeSelection *selection,
				   GNCImportMatchPicker *matcher) 
{
  GNCImportMatchInfo * match_info;
  GtkTreeModel *dl_model;
  GtkListStore *match_store;
  GtkTreeIter iter;
  GList * list_element;
  gchar *text;
  const gchar *ro_text;
  /*DEBUG("row: %d%s%d",row,", column: ",column);*/

  /* Get the transaction info from the "downloaded" model.  */
  if (!gtk_tree_selection_get_selected(selection, &dl_model, &iter)) {
      matcher->selected_trans_info=NULL;
      return;
  }
  gtk_tree_model_get(dl_model, &iter,
		     DOWNLOADED_COL_INFO_PTR, &matcher->selected_trans_info,
		     -1);
  
  /* Now rewrite the "match" model based on that trans. */
  match_store = GTK_LIST_STORE(gtk_tree_view_get_model(matcher->match_view));
  gtk_list_store_clear(match_store);
  list_element = g_list_first (gnc_import_TransInfo_get_match_list
			       (matcher->selected_trans_info));
  while(list_element!=NULL)
    {
      match_info = list_element->data;
      
      gtk_list_store_append(match_store, &iter);

      /* Print fields. */

      /* Probability */
      text = g_strdup_printf("%d", gnc_import_MatchInfo_get_probability (match_info));
      gtk_list_store_set(match_store, &iter, MATCHER_COL_CONFIDENCE, text, -1);
      g_free(text);

      /* Date */
      text =
	qof_print_date 
	( xaccTransGetDate
	  ( xaccSplitGetParent
	    ( gnc_import_MatchInfo_get_split(match_info) ) ));
      gtk_list_store_set(match_store, &iter, MATCHER_COL_DATE, text, -1);
      g_free(text);
      
      /* Amount */
      ro_text =
	xaccPrintAmount( xaccSplitGetAmount ( gnc_import_MatchInfo_get_split(match_info)  ), 
				  gnc_split_amount_print_info(gnc_import_MatchInfo_get_split(match_info), TRUE) 
				  );
      gtk_list_store_set(match_store, &iter, MATCHER_COL_AMOUNT, ro_text, -1);
      
      /*Description*/
      ro_text = xaccTransGetDescription
	( xaccSplitGetParent( gnc_import_MatchInfo_get_split(match_info)) );
      gtk_list_store_set(match_store, &iter, MATCHER_COL_DESCRIPTION, ro_text, -1);
      
      /*Split memo*/    
      ro_text = xaccSplitGetMemo(gnc_import_MatchInfo_get_split(match_info) );
      gtk_list_store_set(match_store, &iter, MATCHER_COL_MEMO, ro_text, -1);
      
      gtk_list_store_set(match_store, &iter, MATCHER_COL_INFO_PTR, match_info, -1);
      if(gnc_import_MatchInfo_get_probability(match_info) != 0)
	{
	  if(SHOW_NUMERIC_SCORE==TRUE)
	    {
	      gtk_list_store_set(match_store, &iter,
				     MATCHER_COL_CONFIDENCE_PIXBUF,
				     gen_probability_pixbuf(gnc_import_MatchInfo_get_probability(match_info), 
							    matcher->user_settings, 
							    GTK_WIDGET(matcher->match_view)),
				     -1);
	    }
	  else
	    {
	      gtk_list_store_set(match_store, &iter,
				    MATCHER_COL_CONFIDENCE_PIXBUF,
				    gen_probability_pixbuf(gnc_import_MatchInfo_get_probability(match_info),
							   matcher->user_settings, 
							   GTK_WIDGET(matcher->match_view)),
				    -1);
	    }
	}
      
      if(match_info == 
	 gnc_import_TransInfo_get_selected_match (matcher->selected_trans_info))
	{
	  GtkTreeSelection *selection;

	  selection = gtk_tree_view_get_selection(matcher->match_view);
	  gtk_tree_selection_select_iter(selection, &iter);
	}
      
      list_element=g_list_next(list_element);
    }
}

static void
match_transaction_changed_cb (GtkTreeSelection *selection,
			      GNCImportMatchPicker *matcher) 
{
  GtkTreeModel *model;
  GtkTreeIter iter;

  if (!gtk_tree_selection_get_selected (selection, &model, &iter)) {
      matcher->selected_match_info=NULL;
      return;
  }

  gtk_tree_model_get(model, &iter,
		     MATCHER_COL_INFO_PTR, &matcher->selected_match_info,
		     -1);
}

static void
match_transaction_row_activated_cb (GtkTreeView *view, GtkTreePath *path,
				    GtkTreeViewColumn *column,
				    GNCImportMatchPicker *matcher)
{
  g_return_if_fail (matcher && matcher->transaction_matcher);

  gtk_dialog_response (GTK_DIALOG (matcher->transaction_matcher),
		       GTK_RESPONSE_OK);
}

static void
add_column(GtkTreeView *view, const gchar *title, int col_num)
{
  GtkCellRenderer *renderer;
  GtkTreeViewColumn *column;
 
  renderer = gtk_cell_renderer_text_new();
  column = gtk_tree_view_column_new_with_attributes(title, renderer,
						    "text", col_num,
						    NULL);
  gtk_tree_view_append_column(view, column);
  g_object_set(G_OBJECT(column),
	       "reorderable", TRUE,
	       "resizable", TRUE,
	       NULL);
}

static void
gnc_import_match_picker_init_downloaded_view (GNCImportMatchPicker * matcher)
{
  GtkTreeView *view;
  GtkListStore *store;
  GtkTreeSelection *selection;

  view = matcher->downloaded_view;
  store = gtk_list_store_new(NUM_DOWNLOADED_COLS,
			     G_TYPE_STRING, G_TYPE_STRING, G_TYPE_STRING,
			     G_TYPE_STRING, G_TYPE_STRING, G_TYPE_STRING,
			     G_TYPE_POINTER);
  gtk_tree_view_set_model(view, GTK_TREE_MODEL(store));
  g_object_unref(store);

  add_column(view, _("Account"),     DOWNLOADED_COL_ACCOUNT);
  add_column(view, _("Date"),        DOWNLOADED_COL_DATE);
  add_column(view, _("Amount"),      DOWNLOADED_COL_AMOUNT);
  add_column(view, _("Description"), DOWNLOADED_COL_DESCRIPTION);
  add_column(view, _("Memo"),        DOWNLOADED_COL_MEMO);
  add_column(view, _("Balanced"),    DOWNLOADED_COL_BALANCED);

  selection = gtk_tree_view_get_selection(view);
  g_signal_connect(selection, "changed",
		   G_CALLBACK(downloaded_transaction_changed_cb), matcher);
}

static void
gnc_import_match_picker_init_match_view (GNCImportMatchPicker * matcher)
{
  GtkTreeView *view;
  GtkListStore *store;
  GtkCellRenderer *renderer;
  GtkTreeViewColumn *column;
  GtkTreeSelection *selection;

  view = matcher->match_view;
  store = gtk_list_store_new(NUM_MATCHER_COLS,
			     G_TYPE_STRING, GDK_TYPE_PIXBUF, G_TYPE_STRING, 
			     G_TYPE_STRING, G_TYPE_STRING, G_TYPE_STRING,
			     G_TYPE_POINTER);
  gtk_tree_view_set_model(view, GTK_TREE_MODEL(store));
  g_object_unref(store);

  renderer = gtk_cell_renderer_pixbuf_new();
  g_object_set(renderer, "xalign", 0.0, NULL);
  column = gtk_tree_view_column_new_with_attributes(_("Confidence"), renderer,
				      "pixbuf", MATCHER_COL_CONFIDENCE_PIXBUF,
				      NULL);
  renderer = gtk_cell_renderer_text_new();
  gtk_tree_view_column_pack_start(column, renderer, TRUE);
  gtk_tree_view_column_set_attributes(column, renderer,
				      "text", MATCHER_COL_CONFIDENCE,
				      NULL);
  gtk_tree_view_append_column(view, column);

  add_column(view, _("Date"),        MATCHER_COL_DATE);
  add_column(view, _("Amount"),      MATCHER_COL_AMOUNT);
  add_column(view, _("Description"), MATCHER_COL_DESCRIPTION);
  add_column(view, _("Memo"),        MATCHER_COL_MEMO);

  selection = gtk_tree_view_get_selection(view);
  g_signal_connect(selection, "changed",
		   G_CALLBACK(match_transaction_changed_cb), matcher);
  g_signal_connect(view, "row-activated",
		   G_CALLBACK(match_transaction_row_activated_cb), matcher);
}

/********************************************************************\
 * init_match_picker_gui()
 * -- GUI initialization for the Match_Picker Dialog
\********************************************************************/
static void
init_match_picker_gui(GNCImportMatchPicker * matcher)
{
  GladeXML *xml;
  
  /* DEBUG("Begin..."); */

  /* Initialize user Settings. */
  matcher->user_settings = gnc_import_Settings_new ();

  /* load the interface */
  xml = gnc_glade_xml_new ("generic-import.glade", "match_picker");
  g_return_if_fail (xml != NULL);

  matcher->transaction_matcher = glade_xml_get_widget (xml, "match_picker");
  matcher->downloaded_view = (GtkTreeView *)glade_xml_get_widget (xml, "downloaded_view");
  matcher->match_view = (GtkTreeView *)glade_xml_get_widget (xml, "matched_view");

  gnc_import_match_picker_init_downloaded_view(matcher);
  gnc_import_match_picker_init_match_view(matcher);

  /* DEBUG("User prefs:%s%d%s%d%s%d%s%d%s%d",
     " action_replace_enabled:",matcher->action_replace_enabled,
     ", action_skip_enabled:",matcher->action_skip_enabled,
     ", clear_threshold:",matcher->clear_threshold,
     ", add_threshold:",matcher->add_threshold,
     ", display_threshold:",matcher->display_threshold); */
  
  gnc_restore_window_size(GCONF_SECTION,
			  GTK_WINDOW (matcher->transaction_matcher));
  gtk_widget_show(matcher->transaction_matcher);  
  
}/* end init_match_picker_gui */

/** 
 * Run a match_picker dialog so that the selected-MatchInfo in the
 * given trans_info is updated accordingly. This functions will only
 * return after the user clicked Ok, Cancel, or Window-Close.
 */
void 
gnc_import_match_picker_run_and_close (GNCImportTransInfo *transaction_info)
{
  GNCImportMatchPicker *matcher;
  gint response;
  GNCImportMatchInfo *old;
  g_assert (transaction_info);
  
  /* Create a new match_picker, even though it's stored in a
     transmatcher struct :-) */
  matcher = g_new0(GNCImportMatchPicker, 1);
  /* DEBUG("Init match_picker"); */
  init_match_picker_gui(matcher);
 
  /* Append this single transaction to the view and select it */
  downloaded_transaction_append(matcher, transaction_info);
  
  old = gnc_import_TransInfo_get_selected_match(transaction_info);
  
  /* Let this dialog run and close. */
  /*DEBUG("Right before run and close");*/
  gtk_window_set_modal(GTK_WINDOW(matcher->transaction_matcher), TRUE);
  response = gtk_dialog_run (GTK_DIALOG (matcher->transaction_matcher));
  gnc_save_window_size(GCONF_SECTION,
		       GTK_WINDOW (matcher->transaction_matcher));
  gtk_widget_destroy (matcher->transaction_matcher);
  /*DEBUG("Right after run and close");*/
  /* DEBUG("Response was %d.", response); */
  if (response == GTK_RESPONSE_OK && matcher->selected_match_info != old)
    {    /* OK was pressed */
      gnc_import_TransInfo_set_selected_match (transaction_info,
					       matcher->selected_match_info,
					       TRUE);
    }
}

/** @} */
