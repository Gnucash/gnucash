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
@file import-match-picker.c
   @brief The transaction match picker dialog 
   implementation
   @author Copyright (C) 2002 Benoit Grégoire
*/
 
#define _GNU_SOURCE

#include "config.h"

#include <glib.h>

#include "import-backend.h"
#include "import-match-picker.h"

#include "gnc-engine-util.h"
#include "gnc-ui-util.h"
#include <glade/glade.h>
#include "dialog-utils.h"
/********************************************************************\
 *   Constants   *
\********************************************************************/

#define NUM_COLUMNS_DOWNLOADED_CLIST 6
static const int DOWNLOADED_CLIST_ACCOUNT = 0;
static const int DOWNLOADED_CLIST_DATE = 1;
static const int DOWNLOADED_CLIST_AMOUNT = 2;
static const int DOWNLOADED_CLIST_DESCRIPTION = 3;
static const int DOWNLOADED_CLIST_MEMO = 4;
static const int DOWNLOADED_CLIST_BALANCED = 5;

#define NUM_COLUMNS_MATCHER_CLIST 5
static const int MATCHER_CLIST_CONFIDENCE = 0;
static const int MATCHER_CLIST_DATE = 1;
static const int MATCHER_CLIST_AMOUNT = 2;
static const int MATCHER_CLIST_DESCRIPTION = 3;
static const int MATCHER_CLIST_MEMO = 4;

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
  GtkCList * downloaded_clist;
  GtkCList * match_clist;
  GNCImportSettings * user_settings;
  struct _transactioninfo * selected_trans_info;
  GNCImportMatchInfo * selected_match_info;
};



static gint  
downloaded_transaction_append(GNCImportMatchPicker * matcher,
				   GNCImportTransInfo * transaction_info)
{
  gint row_number;
  const char * clist_text[NUM_COLUMNS_DOWNLOADED_CLIST];

  g_assert(matcher);
  g_assert(transaction_info);
  /*DEBUG("Begin");*/
  row_number = gtk_clist_find_row_from_data(matcher->downloaded_clist,
					    transaction_info);
  
  /*Account*/
  clist_text[DOWNLOADED_CLIST_ACCOUNT] =
    g_strdup ( xaccAccountGetName
	       ( xaccSplitGetAccount
		 ( gnc_import_TransInfo_get_fsplit (transaction_info))));
  
  
  /*Date*/
  clist_text[DOWNLOADED_CLIST_DATE] = 
    g_strdup ( xaccPrintDateSecs 
	       ( xaccTransGetDate
		 ( gnc_import_TransInfo_get_trans(transaction_info) ) ));
  
  /*Amount*/
  clist_text[DOWNLOADED_CLIST_AMOUNT] =
    g_strdup( xaccPrintAmount( xaccSplitGetAmount ( gnc_import_TransInfo_get_fsplit(transaction_info) ),
			       gnc_split_amount_print_info( gnc_import_TransInfo_get_fsplit(transaction_info), TRUE) 
			       ) );
 
  /*Description*/
  clist_text[DOWNLOADED_CLIST_DESCRIPTION] = g_strdup(xaccTransGetDescription(gnc_import_TransInfo_get_trans(transaction_info) ) );
  
  /*Memo*/
  clist_text[DOWNLOADED_CLIST_MEMO] =
    g_strdup(xaccSplitGetMemo(gnc_import_TransInfo_get_fsplit(transaction_info) ) );

  /*Imbalance*/
  clist_text[DOWNLOADED_CLIST_BALANCED] =
    g_strdup (xaccPrintAmount (xaccTransGetImbalance(gnc_import_TransInfo_get_trans(transaction_info) ), 
			       gnc_default_print_info (TRUE) )
	      );

  row_number = gtk_clist_append(matcher->downloaded_clist,
				(char **)(clist_text));
  gtk_clist_set_row_data(matcher->downloaded_clist,
			 row_number,
			 transaction_info);
  return row_number;
}

/********************************************************************\
 *                                                                   *
 *                       GUI callbacks                               *
 *                                                                   *
\********************************************************************/

static void
downloaded_transaction_select_cb (GtkCList *clist,
				  gint row,
				  gint column,
				  GdkEventButton *event,
				  gpointer user_data) {
  GNCImportMatchPicker * matcher = user_data;
  GNCImportMatchInfo * match_info;
  GList * list_element;
  gint row_number;
  const char * clist_text[NUM_COLUMNS_MATCHER_CLIST];
  /*DEBUG("row: %d%s%d",row,", column: ",column);*/
  
  matcher->selected_trans_info = gtk_clist_get_row_data(clist, row);
  

  gtk_clist_clear(matcher->match_clist);
  list_element = g_list_first (gnc_import_TransInfo_get_match_list
			       (matcher->selected_trans_info));
  while(list_element!=NULL)
    {
      match_info = list_element->data;
      
      /* Print fields. */

      /* Probability */
      clist_text[MATCHER_CLIST_CONFIDENCE] = 
	g_strdup_printf("%d", gnc_import_MatchInfo_get_probability (match_info));
      
      /* Date */
      clist_text[MATCHER_CLIST_DATE]=
	g_strdup( xaccPrintDateSecs 
		  ( xaccTransGetDate
		    ( xaccSplitGetParent
		      ( gnc_import_MatchInfo_get_split(match_info) ) )));
      
      /* Amount */
      clist_text[MATCHER_CLIST_AMOUNT]=
	g_strdup(xaccPrintAmount( xaccSplitGetAmount ( gnc_import_MatchInfo_get_split(match_info)  ), 
				  gnc_split_amount_print_info(gnc_import_MatchInfo_get_split(match_info), TRUE) 
				  ) );
      
      /*Description*/
      clist_text[MATCHER_CLIST_DESCRIPTION] =
	g_strdup( xaccTransGetDescription
		  ( xaccSplitGetParent( gnc_import_MatchInfo_get_split(match_info)) ));
      
      /*Split memo*/    
      clist_text[MATCHER_CLIST_MEMO]=
	g_strdup(xaccSplitGetMemo(gnc_import_MatchInfo_get_split(match_info) ) );
      
      row_number = gtk_clist_append(matcher->match_clist,
				    (char **)(clist_text)); 
      gtk_clist_set_row_data          (matcher->match_clist,
				       row_number,
				       match_info);
      if(gnc_import_MatchInfo_get_probability(match_info) != 0)
	{
	  if(SHOW_NUMERIC_SCORE==TRUE)
	    {
	      gtk_clist_set_pixtext (matcher->match_clist,
				     row_number,
				     MATCHER_CLIST_CONFIDENCE,
				     clist_text[MATCHER_CLIST_CONFIDENCE],
				     3,
				     gen_probability_pixmap(gnc_import_MatchInfo_get_probability(match_info), 
							    matcher->user_settings, 
							    GTK_WIDGET(matcher->match_clist)),
				     NULL);
	    }
	  else
	    {
	      gtk_clist_set_pixmap (matcher->match_clist,
				    row_number,
				    MATCHER_CLIST_CONFIDENCE,
				    gen_probability_pixmap(gnc_import_MatchInfo_get_probability(match_info),
							   matcher->user_settings, 
							   GTK_WIDGET(matcher->match_clist)),
				    NULL);
	    }
	}
      
      gtk_clist_set_row_height        (matcher->match_clist,
				       0);

      if(match_info == 
	 gnc_import_TransInfo_get_selected_match (matcher->selected_trans_info))
	{
	  gtk_clist_select_row            (matcher->match_clist,
					   row_number,
					   0);
	}
      
      list_element=g_list_next(list_element);
    }
}

static void
match_transaction_select_cb (GtkCList *clist,
			     gint row,
			     gint column,
			     GdkEventButton *event,
			     gpointer user_data) {
  GNCImportMatchPicker * matcher = user_data;
  /*DEBUG("row: %d%s%d",row,", column: ",column);*/
  matcher->selected_match_info =
    gtk_clist_get_row_data(clist, row);
}

static void
match_transaction_unselect_cb(GtkCList *clist,
			      gint row,
			      gint column,
			      GdkEventButton *event,
			      gpointer user_data) {
  GNCImportMatchPicker * matcher = user_data;
  /*DEBUG("row: %d%s%d",row,", column: ",column);*/
  matcher->selected_match_info=NULL;

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
  g_assert
    (xml = gnc_glade_xml_new ("generic-import.glade", "match_picker"));
  /* connect the signals in the interface */
  glade_xml_signal_connect_data(xml,
				"match_transaction_select_cb", 
				GTK_SIGNAL_FUNC(match_transaction_select_cb),
				matcher);
  glade_xml_signal_connect_data(xml,
				"match_transaction_unselect_cb", 
				GTK_SIGNAL_FUNC(match_transaction_unselect_cb),
				matcher);
  
  matcher->transaction_matcher = glade_xml_get_widget (xml, "match_picker");
  matcher->downloaded_clist = (GtkCList *)glade_xml_get_widget (xml, "downloaded_clist");
  matcher->match_clist =  (GtkCList *)glade_xml_get_widget (xml, "matched_clist");

  /*Ajust column size*/
  gtk_clist_set_column_auto_resize (GTK_CLIST (matcher->downloaded_clist),
				    DOWNLOADED_CLIST_DATE,
				    TRUE);
  gtk_clist_set_column_auto_resize (GTK_CLIST (matcher->downloaded_clist),
				    DOWNLOADED_CLIST_AMOUNT,
				    TRUE);

  gtk_clist_set_column_auto_resize (GTK_CLIST (matcher->downloaded_clist),
				    DOWNLOADED_CLIST_BALANCED,
				    TRUE);

  gtk_clist_set_column_auto_resize (GTK_CLIST (matcher->match_clist),
				    MATCHER_CLIST_CONFIDENCE,
				    TRUE);
  gtk_clist_set_column_auto_resize (GTK_CLIST (matcher->match_clist),
				    MATCHER_CLIST_DATE,
				    TRUE);
  gtk_clist_set_column_auto_resize (GTK_CLIST (matcher->match_clist),
				    MATCHER_CLIST_AMOUNT,
				    TRUE);

  /* DEBUG("User prefs:%s%d%s%d%s%d%s%d%s%d",
     " action_replace_enabled:",matcher->action_replace_enabled,
     ", action_skip_enabled:",matcher->action_skip_enabled,
     ", clear_threshold:",matcher->clear_threshold,
     ", add_threshold:",matcher->add_threshold,
     ", display_threshold:",matcher->display_threshold); */
  
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
  gint row_number, result;
  GNCImportMatchInfo *old;
  g_assert (transaction_info);
  
  /* Create a new match_picker, even though it's stored in a
     transmatcher struct :-) */
  matcher = g_new0(GNCImportMatchPicker, 1);
  /* DEBUG("Init match_picker"); */
  init_match_picker_gui(matcher);
 
  /* Append this single transaction to the downloaded_clist */
  row_number = downloaded_transaction_append(matcher,
					     transaction_info);
  
  /* Now fake a selection of that transaction. */
  downloaded_transaction_select_cb (matcher->downloaded_clist,
				    row_number,
				    2,
				    NULL,
				    matcher);
  gtk_widget_set_sensitive (GTK_WIDGET (matcher->downloaded_clist), FALSE);
  
  old = gnc_import_TransInfo_get_selected_match(transaction_info);
  
  /* Let this dialog run and close. */
  /*DEBUG("Right before run and close");*/
  result = 
    gnome_dialog_run_and_close (GNOME_DIALOG (matcher->transaction_matcher));
  /*DEBUG("Right after run and close");*/
  /* DEBUG("Result was %d.", result); */
  if (result == 0 && matcher->selected_match_info != old)
    {    /* OK was pressed */
      gnc_import_TransInfo_set_selected_match (transaction_info,
					       matcher->selected_match_info,
					       TRUE);
    }
}

/** @} */
