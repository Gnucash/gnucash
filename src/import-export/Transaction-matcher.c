/********************************************************************\
 * Transaction-matcher.c --                                         *
 * See file generic-import-design.txt for                           *
 * description                                                      *
 *                        (GnuCash)                                 *
 * Copyright (C) 2002 Benoit Grégoire <bock@step.polymtl.ca>        *
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

#define _GNU_SOURCE

#include "config.h"


#include <glib.h>
#include <gmodule.h>

#include <glade/glade.h>

#include <stdlib.h> 
#include <math.h>
#include "Transaction-matcher.h"
#include "Account-matcher.h"
#include "Commodity-matcher.h"
#include "gnc-gen-utilities.h"
#include "Account.h"
#include "Transaction.h"
#include "dialog-utils.h"
#include "global-options.h"

#include "gnc-engine-util.h"

#include "gnc-ui-util.h"

/********************************************************************\
 *   Constants   *
\********************************************************************/

#define NUM_COLUMNS_DOWNLOADED_CLIST 7
static const int DOWNLOADED_CLIST_ACTION = 0;
static const int DOWNLOADED_CLIST_ACCOUNT = 1;
static const int DOWNLOADED_CLIST_DATE = 2;
static const int DOWNLOADED_CLIST_AMOUNT = 3;
static const int DOWNLOADED_CLIST_DESCRIPTION = 4;
static const int DOWNLOADED_CLIST_MEMO = 5;
static const int DOWNLOADED_CLIST_BALANCED = 6;

#define NUM_COLUMNS_MATCHER_CLIST 5
static const int MATCHER_CLIST_CONFIDENCE = 0;
static const int MATCHER_CLIST_DATE = 1;
static const int MATCHER_CLIST_AMOUNT = 2;
static const int MATCHER_CLIST_DESCRIPTION = 3;
static const int MATCHER_CLIST_MEMO = 4;

/********************************************************************\
 *               Structures passed between the functions             *
\********************************************************************/


struct _transmatcherdialog {
  gboolean initialised;
  GtkWidget * transaction_matcher;
  GtkCList * downloaded_clist;
  GtkCList * match_clist;
  struct _transactioninfo * selected_trans_info;

  char * action_add_text;
  char * action_clear_text;       
  char * action_replace_text;       
  char * action_skip_text;

  gboolean action_skip_enabled;
  gboolean action_replace_enabled;
  gboolean action_add_enabled;
  gboolean action_clear_enabled;

  /*Transaction who's best match probability is equal or higher than
    this will reconcile their best match by default */
  int clear_threshold;
  /*Transaction who's best match probability is below or equal to 
    this will be added as new by default */
  int add_threshold;
  /*Transaction's match probability must be at least this much to be 
    displayed in the match list.  Dont set this to 0 except for 
    debugging purposes, otherwise all transactions of every accounts 
    will be shown in the list */
  int display_threshold;
  /*Transaction's who have an online_id kvp frame have been downloaded 
    online can probably be skipped in the match list, since it is very 
    unlikely that they would match a transaction downloaded at a later
    date and yet not have the same online_id.  This also increases
    performance of the matcher. */
};

struct _transactioninfo
{

  Transaction * trans;
  Split * first_split;
  const char * action_text;
  char date_text[20];
  char amount_text[20];
  char balance_text[20];
  const char * clist_text[NUM_COLUMNS_DOWNLOADED_CLIST];

  /* GList of GNCImportMatchInfo's, one for each possible duplicate match. */
  GList * match_list;
  GNCImportMatchInfo * selected_match_info;

  GNCImportAction action;
  GNCImportAction previous_action;

  /* In case of a single destination account it is stored here. */
  Account *dest_acc;
};



struct _matchinfo
{
  Transaction * trans;
  Split * split;
  //GNC_match_probability probability;
  gint probability;
  char probability_text[10];
  char date_text[20];
  char amount_text[20];
  const char * clist_text[NUM_COLUMNS_MATCHER_CLIST];
};

/* Some simple getters and setters for the above data types. */
GList *
gnc_import_TransInfo_get_match_list (const GNCImportTransInfo *info)
{
  g_assert (info);
  return info->match_list;
}

Transaction *
gnc_import_TransInfo_get_trans (const GNCImportTransInfo *info)
{
  g_assert (info);
  return info->trans;
}

Split *
gnc_import_TransInfo_get_fsplit (const GNCImportTransInfo *info)
{
  g_assert (info);
  return info->first_split;
}

GNCImportMatchInfo *
gnc_import_TransInfo_get_selected_match (const GNCImportTransInfo *info)
{
  g_assert (info);
  return info->selected_match_info;
}

void
gnc_import_TransInfo_set_selected_match (GNCImportTransInfo *info,
				       GNCImportMatchInfo *match)
{
  g_assert (info);
  info->selected_match_info = match;
}

GNCImportAction
gnc_import_TransInfo_get_action (const GNCImportTransInfo *info)
{
  g_assert (info);
  return info->action;
}
void
gnc_import_TransInfo_set_action (GNCImportTransInfo *info, 
				 GNCImportAction action)
{
  g_assert (info);
  info->action = action;
}
Account *
gnc_import_TransInfo_get_destacc (const GNCImportTransInfo *info)
{
  g_assert (info);
  return info->dest_acc;
}
void gnc_import_TransInfo_set_destacc (GNCImportTransInfo *info, 
				       Account *acc)
{
  g_assert (info);
  info->dest_acc = acc;
}

Split * 
gnc_import_MatchInfo_get_split (const GNCImportMatchInfo * info)
{
  g_assert (info);
  return info->split;
}
void gnc_import_TransInfo_delete (GNCImportTransInfo *info)
{
  if (info) {
    g_list_free (info->match_list);
    /*If the transaction is still open, it must be destroyed*/
    if(xaccTransIsOpen(info->trans)==TRUE)
      {
	xaccTransDestroy(info->trans);
	xaccTransCommitEdit(info->trans);
      }
    g_free(info);
  }
}



/********************************************************************\
 *   Constants, should idealy be defined a user preference dialog    *
\********************************************************************/

static short module = MOD_IMPORT;
static const int MATCH_DATE_THRESHOLD=4; /*within 4 days*/
static const int MATCH_DATE_NOT_THRESHOLD = 16;
static const int SHOW_TRANSACTIONS_WITH_UNIQUE_ID = FALSE;
static const int SHOW_NUMERIC_SCORE = FALSE;

static const int DEFAULT_ACTION_ADD_ENABLED = TRUE;
static const int DEFAULT_ACTION_CLEAR_ENABLED = TRUE;

/********************************************************************\
 * The folowing are default values for user prefs.  If you modify   *
 * the value of any of these, you must do the same in               *
 * generic-import.scm                                               *
\********************************************************************/
/*Transaction's who have an online_id kvp frame have been downloaded 
  online can probably be skipped in the match list, since it is very 
  unlikely that they would match a transaction downloaded at a later
  date and yet not have the same online_id.  This also increases
  performance of the matcher. */

static const int DEFAULT_ACTION_SKIP_ENABLED = TRUE;
static const int DEFAULT_ACTION_REPLACE_ENABLED = FALSE;

/* XPM */
static char * fleche_xpm[] = {
"17 22 41 1",
" 	c None",
".	c #FFFFFF",
"+	c #000000",
"@	c #FFFAFF",
"#	c #F6FFF6",
"$	c #EEEEE6",
"%	c #B4B29C",
"&	c #F6F6F6",
"*	c #F6F2F6",
"=	c #EFF7EF",
"-	c #EEF2EE",
";	c #EEEEEE",
">	c #F6EEF6",
",	c #E6EEE6",
"'	c #EEEAEE",
")	c #E6EAE6",
"!	c #EEE6EE",
"~	c #E6E6E6",
"{	c #DEE2DE",
"]	c #E6E2E6",
"^	c #DEDEDE",
"/	c #E6DEE6",
"(	c #DEDADE",
"_	c #D5DED5",
":	c #D5DAD5",
"<	c #DED6DE",
"[	c #D5D6D5",
"}	c #D5D2D5",
"|	c #CDD6CD",
"1	c #CDD2CD",
"2	c #CDCECD",
"3	c #D5CED5",
"4	c #CDCACD",
"5	c #C5CAC5",
"6	c #C5C6C5",
"7	c #CDC6CD",
"8	c #BDC6BD",
"9	c #C5C2C5",
"0	c #C5BEC5",
"a	c #BDC2BD",
"b	c #BDBEBD",
".+++++++++++++++.",
"+@.............#+",
"+.$$$$$$$$$$$$$%+",
"+.$&&*&&&&&=&&&%+",
"+.$*--*-**-*-*-%+",
"+.$;;;;>-;;;;;-%+",
"+.$,',';;';',';%+",
"+.$)!)!)!))))')%+",
"+.$~~~~~~~~~~~~%+",
"+.${]+++++++]]]%+",
"+.${^/+++++/{^{%+",
"+.$(^(_+++((_(^%+",
"+.$:<:(<+<::<(<%+",
"+.$[[<[[[[[<[[[%+",
"+.$}|}}}}|}}}}}%+",
"+.$111112131131%+",
"+.$242442422424%+",
"+.$454545444545%+",
"+.$676676656767%+",
"+.$689689696966%+",
"+0%%%%%%%%%%%%%a+",
"b+++++++++++++++b"};

static  GdkPixmap* gen_probability_pixmap(gint score, struct _transmatcherdialog * matcher)
{
  GdkPixmap* retval = NULL;
  gint i, j;
  const gint height = 15;
  const gint width_each_bar = 7;
  gchar * green_bar = ("bggggb ");
  gchar * yellow_bar = ("byyyyb ");
  gchar * red_bar = ("brrrrb ");
  gchar * black_bar = ("bbbbbb ");
  const gint num_colors = 5;
  gchar * size_str = g_strdup_printf("%d%s%d%s%d%s",width_each_bar*score/*width*/," ",height," ",num_colors," 1"/*characters per pixel*/);
  gchar * none_color_str = g_strdup_printf("  c None");
  gchar * green_color_str = g_strdup_printf("g c green");
  gchar * yellow_color_str = g_strdup_printf("y c yellow");
  gchar * red_color_str = g_strdup_printf("r c red");
  gchar * black_color_str = g_strdup_printf("b c black");
  gchar * xpm[2+num_colors+height];

  /*DEBUG("Begin");*/
  xpm[0]=size_str;
  xpm[1]=none_color_str;
  xpm[2]=green_color_str;
  xpm[3]=yellow_color_str;
  xpm[4]=red_color_str; 
  xpm[5]=black_color_str;
  
  for(i=0;i<height;i++)
    {
      xpm[num_colors+1+i]= g_new0(char,(width_each_bar*score)+1);
      for(j=0;j<score;j++)
	{
	  if(i==0||i==height-1)
	    {
	      strcat(xpm[num_colors+1+i],black_bar);
	    }
	  else
	    {
	      if (j<=(matcher->add_threshold)-1)
		{
		  strcat(xpm[num_colors+1+i],red_bar);
		}
	      else if (j>=(matcher->clear_threshold)-1)
		{
		  strcat(xpm[num_colors+1+i],green_bar);
		}
	      else
		{
		  strcat(xpm[num_colors+1+i],yellow_bar);
		}
	    }
	}
    }  

    if(score!=0)
      {
	retval =  gdk_pixmap_colormap_create_from_xpm_d    (NULL,
							    gtk_widget_get_colormap(matcher->transaction_matcher),
							    NULL,
							    NULL,
							    xpm);
      }
   for(i=0;i<=num_colors+height;i++)
    { 
      /*DEBUG("free_loop i=%d%s%s",i,": ",xpm[i]);*/
      g_free(xpm[i]);
    }

  return retval;
}

static GNCImportAction 
get_next_action(struct _transmatcherdialog * matcher, 
		GNCImportAction current_action)
{
  GNCImportAction retval = GNCImport_INVALID_ACTION;
  GNCImportAction i = current_action;
  DEBUG("Begin, action=%d",current_action);
  do
    {
      if (i == GNCImport_LAST_ACTION)
	{
	  i=GNCImport_SKIP;
	}
      else
	{
	  i++;
	}
      DEBUG("i=%d",i);
      if(i==GNCImport_SKIP&&matcher->action_skip_enabled==TRUE)
	retval = i;
      if(i==GNCImport_ADD&&matcher->action_add_enabled==TRUE)
	retval = i;
      if(i==GNCImport_CLEAR&&matcher->action_clear_enabled==TRUE)
	retval = i;
      if(i==GNCImport_REPLACE&&matcher->action_replace_enabled==TRUE)
	retval = i;
    }while(retval==GNCImport_INVALID_ACTION&&i!=current_action);
      DEBUG("retval=%d",i);
  return retval;
}

static void 
downloaded_transaction_refresh_gui(struct _transmatcherdialog * matcher,
				   GNCImportTransInfo * transaction_info)
{
  gint row_number;
  gint i;
  GdkPixmap* fleche;

  DEBUG("Begin, transaction_info ptr: %p%s%d",transaction_info," action: ", transaction_info->action);
  row_number = gtk_clist_find_row_from_data(matcher->downloaded_clist,
					    transaction_info);
  switch(transaction_info->action)
    {
    case GNCImport_ADD: transaction_info->action_text = matcher->action_add_text;
      break;
    case GNCImport_CLEAR: transaction_info->action_text= matcher->action_clear_text;
      break;
    case GNCImport_REPLACE:transaction_info->action_text=matcher->action_replace_text;
      break;
    case GNCImport_SKIP: transaction_info->action_text=matcher->action_skip_text;
      break;
    default:
      PERR("Unknown action");
    }
 
  transaction_info->clist_text[DOWNLOADED_CLIST_ACTION] =
    transaction_info->action_text; /*Action*/
  transaction_info->clist_text[DOWNLOADED_CLIST_ACCOUNT] =
    xaccAccountGetName(xaccSplitGetAccount(gnc_import_TransInfo_get_fsplit (transaction_info)));
  /*Account*/
 
  printDateSecs(transaction_info->date_text, 
		xaccTransGetDate(transaction_info->trans));
  transaction_info->clist_text[DOWNLOADED_CLIST_DATE] =
    transaction_info->date_text; /*Date*/

  {
    Split *split = gnc_import_TransInfo_get_fsplit (transaction_info);
    xaccSPrintAmount (transaction_info->amount_text,
		      xaccSplitGetAmount (split), 
		      gnc_split_value_print_info (split, TRUE));
  }
  transaction_info->clist_text[DOWNLOADED_CLIST_AMOUNT] =
    transaction_info->amount_text;
  
  transaction_info->clist_text[DOWNLOADED_CLIST_DESCRIPTION] =
    xaccTransGetDescription(transaction_info->trans);
  transaction_info->clist_text[DOWNLOADED_CLIST_MEMO] =
    xaccSplitGetMemo(gnc_import_TransInfo_get_fsplit (transaction_info));
 
  xaccSPrintAmount (transaction_info->balance_text,
		    xaccTransGetImbalance (transaction_info->trans), 
		    gnc_default_print_info (TRUE));
  transaction_info->clist_text[DOWNLOADED_CLIST_BALANCED] =
    transaction_info->balance_text;

  for(i=0;i<NUM_COLUMNS_DOWNLOADED_CLIST;i++)
    {
      gtk_clist_set_text              (matcher->downloaded_clist,
				       row_number,
				       i,
				       transaction_info->clist_text[i]);
    } 

  fleche =  gdk_pixmap_colormap_create_from_xpm_d    (NULL,
						      gtk_widget_get_colormap(matcher->transaction_matcher),
						      NULL,
						      NULL,
						      fleche_xpm);
  
  gtk_clist_set_pixtext           (matcher->downloaded_clist,
				   row_number,
				   DOWNLOADED_CLIST_ACTION,
				   transaction_info->action_text,
				   3,
				   fleche,
				   NULL);
    
  gtk_clist_set_row_height        (matcher->downloaded_clist,
				   23);
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
  struct _transmatcherdialog * matcher = user_data;
  GNCImportMatchInfo * match_info;
  GList * list_element;
  gint row_number;
  gboolean valid_action_found;
  /* DEBUG("row: %d%s%d",row,", column: ",column); */
  
  if(  matcher->selected_trans_info != gtk_clist_get_row_data(clist, row) 
       && matcher->selected_trans_info != NULL 
       && column == DOWNLOADED_CLIST_ACTION)
    {
      /* We just screwed up the action of a previous selection because
	 of the way GTK automatically calls unselect row.  Let's fix
	 it. */
      matcher->selected_trans_info->action=matcher->selected_trans_info->previous_action;
      downloaded_transaction_refresh_gui(matcher,matcher->selected_trans_info);
      row_number = gtk_clist_find_row_from_data(matcher->downloaded_clist,
						matcher->selected_trans_info);
      gtk_clist_unselect_row (clist,
			      row_number,
			      DOWNLOADED_CLIST_AMOUNT);/* Anything but DOWNLOADED_CLIST_ACTION */
    }

  matcher->selected_trans_info = gtk_clist_get_row_data(clist, row);
  

  gtk_clist_clear(matcher->match_clist);
  list_element = g_list_first (gnc_import_TransInfo_get_match_list
			       (matcher->selected_trans_info));
  while(list_element!=NULL)
    {
      match_info = list_element->data;
      row_number = gtk_clist_append(matcher->match_clist,
				    (char **)(match_info->clist_text)); 
      gtk_clist_set_row_data          (matcher->match_clist,
				       row_number,
				       match_info);
      if(match_info->probability!=0)
	{
	  if(SHOW_NUMERIC_SCORE==TRUE)
	    {
	      gtk_clist_set_pixtext (matcher->match_clist,
				     row_number,
				     MATCHER_CLIST_CONFIDENCE,
				     match_info->probability_text,
				     3,
				     gen_probability_pixmap(match_info->probability, matcher),
				     NULL);
	    }
	  else
	    {
	      gtk_clist_set_pixmap (matcher->match_clist,
				    row_number,
				    MATCHER_CLIST_CONFIDENCE,
				    gen_probability_pixmap(match_info->probability, matcher),
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

  if(column == DOWNLOADED_CLIST_ACTION)
    {
      matcher->selected_trans_info->previous_action=matcher->selected_trans_info->action;
      valid_action_found=FALSE;
      while(valid_action_found==FALSE)
	{
	  gnc_import_TransInfo_set_action
	    (matcher->selected_trans_info, 
	     get_next_action(matcher, gnc_import_TransInfo_get_action
			     (matcher->selected_trans_info)));
	  valid_action_found=TRUE;
	  if ((gnc_import_TransInfo_get_selected_match
	       (matcher->selected_trans_info) == NULL) &&
	      ((gnc_import_TransInfo_get_action
		(matcher->selected_trans_info) == GNCImport_REPLACE) ||
	       (gnc_import_TransInfo_get_action
		(matcher->selected_trans_info) == GNCImport_CLEAR)))
	    {
	      valid_action_found=FALSE;
	    }
	}
    }

  downloaded_transaction_refresh_gui(matcher,matcher->selected_trans_info);
}

static void
downloaded_transaction_unselect_cb(GtkCList *clist,
				   gint row,
				   gint column,
				   GdkEventButton *event,
				   gpointer user_data) {
  struct _transmatcherdialog * matcher = user_data;
  /* DEBUG("row: %d%s%d",row,", column: ",column); */

  if(  matcher->selected_trans_info == gtk_clist_get_row_data(clist, row) && column == DOWNLOADED_CLIST_ACTION)
    {
      gtk_clist_select_row (clist,
			    row,
			    column);
    }
  else
    {
      matcher->selected_trans_info = NULL;
      gtk_clist_clear(matcher->match_clist);
      
    }
}

static void
match_transaction_select_cb (GtkCList *clist,
			     gint row,
			     gint column,
			     GdkEventButton *event,
			     gpointer user_data) {
  struct _transmatcherdialog * matcher = user_data;
  /* DEBUG("row: %d%s%d",row,", column: ",column); */
  matcher->selected_trans_info->selected_match_info =
    gtk_clist_get_row_data(clist, row);
}

static void
match_transaction_unselect_cb(GtkCList *clist,
			      gint row,
			      gint column,
			      GdkEventButton *event,
			      gpointer user_data) {
  struct _transmatcherdialog * matcher = user_data;
  /* DEBUG("row: %d%s%d",row,", column: ",column); */
  matcher->selected_trans_info->selected_match_info=NULL;

  /*You can't replace or reconcile if no match is selected*/
  while((gnc_import_TransInfo_get_action
	 (matcher->selected_trans_info) == GNCImport_REPLACE) ||
	(gnc_import_TransInfo_get_action
	 (matcher->selected_trans_info) == GNCImport_CLEAR))
    {
      gnc_import_TransInfo_set_action
	(matcher->selected_trans_info, 
	 get_next_action(matcher, gnc_import_TransInfo_get_action
			 (matcher->selected_trans_info)));
    } 
  downloaded_transaction_refresh_gui(matcher,matcher->selected_trans_info);
}

static void 
on_matcher_apply_clicked (GtkButton *button,
			  gpointer user_data)
{
  struct _transmatcherdialog * matcher = user_data;
  g_assert (matcher);

  gnc_import_process_trans_clist (matcher->downloaded_clist, NULL);
}


static void 
on_matcher_cancel_clicked (GtkButton *button,
			   gpointer user_data)
{
  struct _transmatcherdialog * matcher = user_data;
  /* DEBUG("Begin"); */
  gtk_clist_clear(matcher->downloaded_clist);
  matcher->initialised=FALSE;
  gtk_widget_destroy(matcher->transaction_matcher);
  g_free (matcher->action_add_text);
  g_free (matcher->action_clear_text);
  g_free (matcher->action_replace_text);    
  g_free (matcher->action_skip_text);
}

static void 
on_matcher_ok_clicked (GtkButton *button,
		       gpointer user_data)
{
  /* DEBUG("Begin"); */
  on_matcher_apply_clicked (button,
			    user_data);
  on_matcher_cancel_clicked (button,
			     user_data);
}

/********************************************************************\
 * downloaded_trans_row_destroy_cb()
\********************************************************************/
static void
downloaded_trans_row_destroy_cb(gpointer data)
{
  GNCImportTransInfo * transaction_info = data;
  /* DEBUG("Begin"); */
  gnc_import_TransInfo_delete (transaction_info);
}



/********************************************************************\
 * init_matcher_gui()
 * -- GUI Initialization for the Transaction-Matcher
\********************************************************************/
static void
init_matcher_gui(struct _transmatcherdialog * matcher)
{
  GladeXML *xml;
  
  /* DEBUG("Begin..."); */
  /* load the interface */
  xml = gnc_glade_xml_new ("generic-import.glade", "transaction_matcher");
  /* connect the signals in the interface */
  if(xml==NULL)
    {
      PERR("Error opening the glade interface\n");
    }
  
  glade_xml_signal_connect_data(xml,
				"downloaded_transaction_select_cb",
				GTK_SIGNAL_FUNC(downloaded_transaction_select_cb), 
				matcher);
  glade_xml_signal_connect_data(xml,
				"downloaded_transaction_unselect_cb", 
				GTK_SIGNAL_FUNC(downloaded_transaction_unselect_cb),
				matcher);
  glade_xml_signal_connect_data(xml,
				"match_transaction_select_cb", 
				GTK_SIGNAL_FUNC(match_transaction_select_cb),
				matcher);
  glade_xml_signal_connect_data(xml,
				"match_transaction_unselect_cb", 
				GTK_SIGNAL_FUNC(match_transaction_unselect_cb),
				matcher);
  glade_xml_signal_connect_data(xml,
				"on_matcher_ok_clicked", 
				GTK_SIGNAL_FUNC(on_matcher_ok_clicked),
				matcher);
  glade_xml_signal_connect_data(xml,
				"on_matcher_apply_clicked", 
				GTK_SIGNAL_FUNC(on_matcher_apply_clicked),
				matcher);
  glade_xml_signal_connect_data(xml,
				"on_matcher_cancel_clicked", 
				GTK_SIGNAL_FUNC(on_matcher_cancel_clicked),
				matcher);


  matcher->transaction_matcher = glade_xml_get_widget (xml, "transaction_matcher");
  matcher->downloaded_clist = (GtkCList *)glade_xml_get_widget (xml, "downloaded_clist");
  matcher->match_clist =  (GtkCList *)glade_xml_get_widget (xml, "match_clist");

  matcher->action_add_text = g_strdup(_("ADD"));
  matcher->action_clear_text =  g_strdup(_("CLEAR"));       
  matcher->action_replace_text =  g_strdup(_("REPLACE"));       
  matcher->action_skip_text =  g_strdup(_("SKIP"));
  matcher->action_skip_enabled=gnc_lookup_boolean_option("Transaction Matcher","Enable SKIP transaction action",
							 DEFAULT_ACTION_SKIP_ENABLED);
  matcher->action_replace_enabled=gnc_lookup_boolean_option("Transaction Matcher","Enable REPLACE match action",
							    DEFAULT_ACTION_REPLACE_ENABLED);
  matcher->action_add_enabled=DEFAULT_ACTION_ADD_ENABLED;
  matcher->action_clear_enabled=DEFAULT_ACTION_CLEAR_ENABLED;
  matcher->clear_threshold=gnc_lookup_number_option("Transaction Matcher","Auto-CLEAR threshold",
						    DEFAULT_CLEAR_THRESHOLD);
  matcher->add_threshold=gnc_lookup_number_option("Transaction Matcher","Auto-ADD threshold",
						  DEFAULT_ADD_THRESHOLD);
  matcher->display_threshold=gnc_lookup_number_option("Transaction Matcher","Match display threshold",
						      DEFAULT_DISPLAY_THRESHOLD);
  DEBUG("User prefs:%s%d%s%d%s%d%s%d%s%d",
	" action_replace_enabled:",matcher->action_replace_enabled,
	", action_skip_enabled:",matcher->action_skip_enabled,
	", clear_threshold:",matcher->clear_threshold,
	", add_threshold:",matcher->add_threshold,
	", display_threshold:",matcher->display_threshold);
  

  gtk_widget_show(matcher->transaction_matcher);  
  matcher->initialised=TRUE;  
    
}/* end init_matcher_gui */






/********************************************************************\
 * init_match_picker_gui()
 * -- GUI initialization for the Match_Picker Dialog
\********************************************************************/
static void
init_match_picker_gui(struct _transmatcherdialog * matcher)
{
  GladeXML *xml;
  
  /* DEBUG("Begin..."); */
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

  matcher->action_add_text = g_strdup(_("ADD"));
  matcher->action_clear_text =  g_strdup(_("CLEAR"));       
  matcher->action_replace_text =  g_strdup(_("REPLACE"));       
  matcher->action_skip_text =  g_strdup(_("SKIP"));
  matcher->action_skip_enabled = 
    gnc_lookup_boolean_option("Transaction Matcher",
			      "Enable SKIP transaction action",
			      DEFAULT_ACTION_SKIP_ENABLED);
  matcher->action_replace_enabled =
    gnc_lookup_boolean_option("Transaction Matcher",
			      "Enable REPLACE match action",
			      DEFAULT_ACTION_REPLACE_ENABLED);
  matcher->action_add_enabled=DEFAULT_ACTION_ADD_ENABLED;
  matcher->action_clear_enabled=DEFAULT_ACTION_CLEAR_ENABLED;
  matcher->clear_threshold=gnc_lookup_number_option("Transaction Matcher",
						    "Auto-CLEAR threshold",
						    DEFAULT_CLEAR_THRESHOLD);
  matcher->add_threshold=gnc_lookup_number_option("Transaction Matcher",
						  "Auto-ADD threshold",
						  DEFAULT_ADD_THRESHOLD);
  matcher->display_threshold =
    gnc_lookup_number_option("Transaction Matcher","Match display threshold",
			     DEFAULT_DISPLAY_THRESHOLD);
  /* DEBUG("User prefs:%s%d%s%d%s%d%s%d%s%d",
     " action_replace_enabled:",matcher->action_replace_enabled,
     ", action_skip_enabled:",matcher->action_skip_enabled,
     ", clear_threshold:",matcher->clear_threshold,
     ", add_threshold:",matcher->add_threshold,
     ", display_threshold:",matcher->display_threshold); */

  gtk_widget_show(matcher->transaction_matcher);  
  matcher->initialised=TRUE;  
    
}/* end init_match_picker_gui */

/** 
 * Run a match_picker dialog so that the selected-MatchInfo in the
 * given trans_info is updated accordingly. This functions will only
 * return after the user clicked Ok, Cancel, or Window-Close.
 */
void 
gnc_import_match_picker_run_and_close (GNCImportTransInfo *transaction_info)
{
  struct _transmatcherdialog *matcher;
  gint row_number, result;
  GNCImportMatchInfo *old;
  g_assert (transaction_info);

  /* Create a new match_picker, even though it's stored in a
     transmatcher struct :-) */
  matcher = g_new0(struct _transmatcherdialog, 1);
  /* DEBUG("Init match_picker"); */
  init_match_picker_gui(matcher);

  /* Append this single transaction to the downloaded_clist */
  row_number = gtk_clist_append(matcher->downloaded_clist,
				(char **)(transaction_info->clist_text));
  gtk_clist_set_row_data(matcher->downloaded_clist,
			 row_number,
			 transaction_info);
  downloaded_transaction_refresh_gui(matcher,
				     transaction_info);

  /* Now fake a selection of that transaction. */
  downloaded_transaction_select_cb (matcher->downloaded_clist,
				    row_number,
				    2,
				    NULL,
				    matcher);
  gtk_widget_set_sensitive (GTK_WIDGET (matcher->downloaded_clist), FALSE);
  
  old = transaction_info->selected_match_info;

  /* Let this dialog run and close. */
  result = 
    gnome_dialog_run_and_close (GNOME_DIALOG (matcher->transaction_matcher));

  /* DEBUG("Result was %d.", result); */
  if (result != 0)
    /* Cancel was pressed, or the window was closed. Revert change. */
    transaction_info->selected_match_info = old;
}



/*************************************************************************
 * MatchMap- related functions (storing and retrieving)
 */
static Account *
matchmap_find_destination (GncImportMatchMap *matchmap, 
				    GNCImportTransInfo *info)
{
  GncImportMatchMap *tmp_map;
  Account *result;
  g_assert (info);
  
  tmp_map = ((matchmap != NULL) ? matchmap : 
	     gnc_imap_create_from_account 
	     (xaccSplitGetAccount
	      (gnc_import_TransInfo_get_fsplit (info))));

  result = gnc_imap_find_account 
    (tmp_map, GNCIMPORT_DESC, 
     xaccTransGetDescription (gnc_import_TransInfo_get_trans (info)));
  if (result == NULL)
    result = gnc_imap_find_account 
      (tmp_map, GNCIMPORT_MEMO, 
       xaccSplitGetMemo (gnc_import_TransInfo_get_fsplit (info)));
  
  if (matchmap == NULL)
    gnc_imap_destroy (tmp_map);

  return result;
}

/* Store the destination account from trans_info in the matchmap. If
   'use_match' is true, the destination account of the selected
   matching/duplicate transaction is used; otherwise, the stored
   destination_acc pointer is used. */
static void 
matchmap_store_destination (GncImportMatchMap *matchmap, 
			    GNCImportTransInfo *trans_info,
			    gboolean use_match)
{
  GncImportMatchMap *tmp_matchmap = NULL;
  Account *dest;
  g_assert (trans_info);

  dest = ((use_match) ?
	  xaccSplitGetAccount
	  (xaccSplitGetOtherSplit 
	   (gnc_import_MatchInfo_get_split 
	    (gnc_import_TransInfo_get_selected_match (trans_info)))) :
	  gnc_import_TransInfo_get_destacc (trans_info));
  if (dest == NULL)
    return;
  
  tmp_matchmap = ((matchmap != NULL) ? 
		  matchmap : 
		  gnc_imap_create_from_account 
		  (xaccSplitGetAccount
		   (gnc_import_TransInfo_get_fsplit (trans_info))));

  gnc_imap_add_account (tmp_matchmap, 
			GNCIMPORT_DESC, 
			xaccTransGetDescription 
			(gnc_import_TransInfo_get_trans (trans_info)), 
			dest);
  gnc_imap_add_account (tmp_matchmap, 
			GNCIMPORT_MEMO, 
			xaccSplitGetMemo 
			(gnc_import_TransInfo_get_fsplit (trans_info)),
			dest);

  if (matchmap == NULL)
    gnc_imap_destroy (tmp_matchmap);
}



/********************************************************************\
 * split_find_match() 
 * The main function for transaction matching.  The heuristics are
 * here. 
\********************************************************************/
static void split_find_match (GNCImportTransInfo * trans_info,
			      Split * split, 
			      gint display_threshold,
			      double fuzzy_amount_difference)
{
  /* DEBUG("Begin"); */
  
  /*Ignore the split if the transaction is open for edit, meaning it
    was just downloaded.  Ignore the split if the transaction has an
    online ID , unless overriden in prefs (i.e. do not ignore the
    split if the online_id kvp is NULL or if it has zero length). */
  if ((xaccTransIsOpen(xaccSplitGetParent(split)) == FALSE) &&
      ((gnc_import_get_trans_online_id(xaccSplitGetParent(split))==NULL) ||
       (strlen(gnc_import_get_trans_online_id(xaccSplitGetParent(split))) == 0) ||
       SHOW_TRANSACTIONS_WITH_UNIQUE_ID==TRUE))
    {
      GNCImportMatchInfo * match_info;
      gint prob = 0;
      double downloaded_split_amount, match_split_amount;
      time_t match_time, download_time;
      int datediff_day;
    
      /* Matching heuristics */
    
      /* Amount heuristics */
      downloaded_split_amount = 
	gnc_numeric_to_double
	(xaccSplitGetAmount(gnc_import_TransInfo_get_fsplit (trans_info)));
      /*DEBUG(" downloaded_split_amount=%f", downloaded_split_amount);*/
      match_split_amount = gnc_numeric_to_double(xaccSplitGetAmount(split));
      /*DEBUG(" match_split_amount=%f", match_split_amount);*/
      if(gnc_numeric_equal(xaccSplitGetAmount
			   (gnc_import_TransInfo_get_fsplit (trans_info)),
			   xaccSplitGetAmount(split)))
	{
	  prob = prob+3;
	  DEBUG("heuristics:  probability + 3 (amount)");
	}
      else if (fabs (downloaded_split_amount - match_split_amount) <= 
	       fuzzy_amount_difference)
	{
	  /* ATM fees are sometimes added directly in the transaction.
	     So you withdraw 100$ and get charged 101,25$ in the same
	     transaction */ 
	  prob = prob+1;
	  DEBUG("heuristics:  probability + 1 (amount)");
	}
      else
	{
	  /* If a transaction's amount doesn't match within the
	     threshold, it's very unlikely to be the same transaction
	     so we give it an extra -1 penality */
	  prob = prob-1;
	  /* DEBUG("heuristics:  probability - 1 (amount)"); */
	}
      
      /* Date heuristics */
      match_time = xaccTransGetDate (xaccSplitGetParent (split));
      download_time = 
	xaccTransGetDate (gnc_import_TransInfo_get_trans (trans_info));
      datediff_day = abs(match_time - download_time)/86400;
      /* Sorry, there are not really functions around at all that
	 provide for less hacky calculation of days of date
	 differences. Whatever. On the other hand, the difference
	 calculation itself will work regardless of month/year
	 turnarounds. */
      /*DEBUG("diff day %d", datediff_day);*/
      if (datediff_day == 0)
	{
	  prob = prob+2;
	  DEBUG("heuristics:  probability + 2 (date)");
	}
      else if (datediff_day <= MATCH_DATE_THRESHOLD)
	{
	  prob = prob+1;
	  DEBUG("heuristics:  probability + 1 (date)");
	}
      else if (datediff_day > MATCH_DATE_NOT_THRESHOLD)
	{
	  /* Extra penalty if that split lies awfully far away
	     from the given one. */
	  prob = prob-1;
	  /* DEBUG("heuristics:  probability - 1 (date)"); */
	}
      
    
      /* Memo heuristics */  
      if((strcmp(xaccSplitGetMemo(gnc_import_TransInfo_get_fsplit (trans_info)),
		 xaccSplitGetMemo(split))
	  ==0))
	{	
	  /* An exact match of description gives a +2 */
	  prob = prob+2;
	  DEBUG("heuristics:  probability + 2 (memo)");
	}
      else if((strncmp(xaccSplitGetMemo(gnc_import_TransInfo_get_fsplit (trans_info)),
		       xaccSplitGetMemo(split),
		       strlen(xaccSplitGetMemo(split))/2)
	       ==0))
	{
	  /* Very primitive fuzzy match worth +1.  This matches the
	     first 50% of the strings to skip annoying transaction
	     number some banks seem to include in the memo but someone
	     should write something more sophisticated */ 
      	  prob = prob+1;
	  DEBUG("heuristics:  probability + 1 (memo)");	
	}

      /* Description heuristics */  
      if((strcmp(xaccTransGetDescription
		 (gnc_import_TransInfo_get_trans (trans_info)),
		 xaccTransGetDescription(xaccSplitGetParent(split)))
	  ==0))
	{	
	  /*An exact match of Description gives a +2 */
	  prob = prob+2;
	  DEBUG("heuristics:  probability + 2 (description)");
	}
      else if((strncmp(xaccTransGetDescription
		       (gnc_import_TransInfo_get_trans (trans_info)),
		       xaccTransGetDescription(xaccSplitGetParent(split)),
		       strlen(xaccTransGetDescription
			      (gnc_import_TransInfo_get_trans (trans_info)))/2)
	  ==0))
	{
	  /* Very primitive fuzzy match worth +1.  This matches the
	     first 50% of the strings to skip annoying transaction
	     number some banks seem to include in the memo but someone
	     should write something more sophisticated */ 
      	  prob = prob+1;
	  DEBUG("heuristics:  probability + 1 (description)");	
	}

      if ((gnc_import_get_trans_online_id(xaccSplitGetParent(split))!=NULL) &&
	  (strlen(gnc_import_get_trans_online_id(xaccSplitGetParent(split)))>0))
	{
	  /* If the pref is to show match even with online ID's,
	     reverse the confidence value to distinguish them */
	  prob = 0-prob;
	}

      /* Is the probability high enough? Otherwise do nothing and return. */
      if(prob < display_threshold)
	{
	  return;
	}

      /* The probability is high enough, so allocate an object
	 here. Allocating it only when it's actually being used is
	 probably quite some performance gain. */
      match_info = g_new0(GNCImportMatchInfo,1);
    
      match_info->probability = prob;
      match_info->split = split;
      match_info->trans = xaccSplitGetParent(split);
    
      /* Print fields. */

      /* Probability */
      sprintf(match_info->probability_text, 
	      "%d",
	      match_info->probability);
      match_info->clist_text[MATCHER_CLIST_CONFIDENCE] = 
	match_info->probability_text;

      /* Date */
      printDateSecs(match_info->date_text,
		    xaccTransGetDate(xaccSplitGetParent(split)));
      match_info->clist_text[MATCHER_CLIST_DATE]=match_info->date_text;

      /* Amount */
      xaccSPrintAmount (match_info->amount_text,
			xaccSplitGetAmount (split), 
			gnc_split_value_print_info (split, TRUE));
      match_info->clist_text[MATCHER_CLIST_AMOUNT]=match_info->amount_text;
    
      /*Description*/
      match_info->clist_text[MATCHER_CLIST_DESCRIPTION] =
	xaccTransGetDescription(xaccSplitGetParent(split));

      /*Split memo*/    
      match_info->clist_text[MATCHER_CLIST_MEMO]=xaccSplitGetMemo(split);

      /* Append that to the list. */
      trans_info->match_list = 
	g_list_append(trans_info->match_list,
		      match_info);
    }
}/* end split_find_match */


/* Iterate through all splits of the originating account of the given
   transaction, and find all matching splits there. */
void gnc_import_find_split_matches(GNCImportTransInfo *trans_info,
				   gint process_threshold, 
				   double fuzzy_amount_difference)
{
  GList * list_element;
  g_assert (trans_info);
  
  /* Get list of splits of the originating account. */
  list_element = 
    g_list_first
    (xaccAccountGetSplitList
     (xaccSplitGetAccount (gnc_import_TransInfo_get_fsplit (trans_info))));

  /* Traverse that list, calling split_find_match on each one. Note
     that xaccAccountForEachSplit is declared in Account.h but
     implemented nowhere :-( */
  while(list_element!=NULL)
    {
      split_find_match (trans_info, list_element->data, 
			process_threshold, fuzzy_amount_difference);
      list_element = g_list_next (list_element);
    }
}

/***********************************************************************
 */


/* gnc_import_process_trans_clist -- Processes every selected match
   according to its selected action. Then, each entry is removed from
   the clist. */
void
gnc_import_process_trans_clist (GtkCList *clist, 
				GncImportMatchMap *matchmap)
{
  GNCImportTransInfo * trans_info;
  gint row_number = 0, i = 0;
  g_assert (clist);
  
  /* DEBUG("Begin"); */
  gtk_clist_freeze (clist);
  trans_info = 
    (GNCImportTransInfo *) gtk_clist_get_row_data(clist, 0);
  
  for(i = 1; trans_info != NULL; i++)
    {
      g_assert (trans_info);
      /*DEBUG("Iteration %d, action %d, split %s", i, 
	trans_info->action,
	xaccTransGetDescription (gnc_import_TransInfo_get_trans 
	(trans_info)))*/
      switch (gnc_import_TransInfo_get_action (trans_info))
	{
	case GNCImport_SKIP:
	  break;
	case GNCImport_ADD:
	  /* Transaction gets imported. */

	  /* Is there a non-NULL destination account? */
	  if (gnc_import_TransInfo_get_destacc (trans_info) != NULL) {
	    /* Create the 'other' split. */
	    Split *split = 
	      xaccMallocSplit
	      (xaccAccountGetBook
	       (gnc_import_TransInfo_get_destacc (trans_info)));
	    xaccTransAppendSplit
	      (gnc_import_TransInfo_get_trans (trans_info), split);
	    xaccAccountInsertSplit
	      (gnc_import_TransInfo_get_destacc (trans_info), split);
	    xaccSplitSetValue
	      (split, gnc_numeric_neg(xaccSplitGetValue 
				      (gnc_import_TransInfo_get_fsplit (trans_info))));
	    xaccSplitSetMemo (split, _("Imported Transaction"));

	    /* Store the mapping to the other account in the MatchMap. */
	    matchmap_store_destination (matchmap, trans_info, FALSE);
	  }
	  
	  xaccSplitSetReconcile(gnc_import_TransInfo_get_fsplit (trans_info), CREC);
	  /*Set reconcile date to today*/
	  xaccSplitSetDateReconciledSecs(gnc_import_TransInfo_get_fsplit (trans_info),
					 time(NULL));
	  /* Done editing. */
	  xaccTransCommitEdit 
	    (gnc_import_TransInfo_get_trans (trans_info));
	  break;
	case GNCImport_CLEAR:
	  /* Transaction gets not imported but the matching one gets 
	     reconciled. */
	  if(gnc_import_MatchInfo_get_split 
	     (gnc_import_TransInfo_get_selected_match (trans_info)) ==NULL)
	    {
	      PERR("The split I am trying to reconcile is NULL, shouldn't happen!")
	    }
	  else
	    {
	      /* Reconcile the matching transaction */
	      /*DEBUG("BeginEdit selected_match")*/
	      xaccTransBeginEdit(trans_info->selected_match_info->trans);

	      if (xaccSplitGetReconcile 
		  (trans_info->selected_match_info->split) == NREC)
		xaccSplitSetReconcile
		  (trans_info->selected_match_info->split, CREC);
	      /* Set reconcile date to today */
	      xaccSplitSetDateReconciledSecs
		(trans_info->selected_match_info->split,time(NULL));

	      /* Copy the online id to the reconciled transaction, so
		 the match will be remembered */ 
	      if (gnc_import_get_trans_online_id(trans_info->trans)
		  != NULL)
		gnc_import_set_trans_online_id
		  (trans_info->selected_match_info->trans, 
		   gnc_import_get_trans_online_id(trans_info->trans));
	      
	      /* Done editing. */
	      /*DEBUG("CommitEdit selected_match")*/
	      xaccTransCommitEdit
		(trans_info->selected_match_info->trans);
	      
	      /* Store the mapping to the other account in the MatchMap. */
	      matchmap_store_destination (matchmap, trans_info, TRUE);

	      /* Erase the downloaded transaction */
	      xaccTransDestroy(trans_info->trans);
	      /* FIXME: Why did we use xaccTransRollbackEdit instead
		 of xaccTransDestroy here? It doesn't make sense since
		 we want to *erase* it here. */
	      DEBUG("CommitEdit trans")
	      xaccTransCommitEdit(trans_info->trans);
	    }
	  break;
	case GNCImport_REPLACE:
	  /* Imported transaction replaces the selected matching one. */
	  if(trans_info->selected_match_info->split==NULL)
	    PERR("The split I am trying to replace is NULL, shouldn't happen!")
	  else
	    {
	      /* Store the mapping to the other account in the MatchMap. */
	      matchmap_store_destination (matchmap, trans_info, TRUE);

	      /* DEBUG("Deleting the previous transaction"); */
	      /*Erase the matching transaction*/
	      xaccTransBeginEdit(trans_info->selected_match_info->trans);
	      xaccTransDestroy(trans_info->selected_match_info->trans);
	      xaccTransCommitEdit(trans_info->selected_match_info->trans);
	      
	      /*Replace it with the new one*/
	      xaccSplitSetReconcile(gnc_import_TransInfo_get_fsplit (trans_info),CREC);
	      /*Set reconcile date to today*/
	      xaccSplitSetDateReconciledSecs(gnc_import_TransInfo_get_fsplit (trans_info),
					     time(NULL));
	      xaccTransCommitEdit(trans_info->trans);
	    } 
	  break;
	default:
	  DEBUG("Invalid GNCImportAction for this imported transaction.");
	}

      /* For all actions except SKIP delete this transaction now. */
      if(trans_info->action != GNCImport_SKIP) {
	row_number = gtk_clist_find_row_from_data(clist, trans_info);
	gtk_clist_remove (clist, row_number);
	/* decrement the iteration counter since we've just removed
	   one row. */
	i--;
      }

      /* Get next trans_info, NULL if finished */
      trans_info =
	(GNCImportTransInfo *) gtk_clist_get_row_data(clist, i);
    }
  
  /*DEBUG("End");*/
  gtk_clist_thaw (clist);
  /*DEBUG("Thawed.")*/
}

/********************************************************************\
 * check_trans_online_id() Callback function to be used by
 * gnc_import_exists_online_id.  Takes pointers to two transaction and
 * returns TRUE if their online_id kvp_frame do NOT match, or if both
 * pointers point to the same transaction.
 * \********************************************************************/
static gboolean check_trans_online_id(Transaction *trans1, void *user_data)
{
  Transaction *trans2 = user_data;
  const gchar *online_id1 = gnc_import_get_trans_online_id(trans1);
  const gchar *online_id2 = gnc_import_get_trans_online_id(trans2);

  if ((trans1 == trans2) || (online_id1 == NULL) || 
      (online_id2 == NULL) || (strcmp(online_id1, online_id2) != 0))
    {
      return TRUE;
    }
  else
    {
      //printf("test_trans_online_id(): Duplicate found\n");
      return FALSE;
    }
}

/* Checks whether the given transaction's online_id already exists in
 * its parent account. */
gboolean gnc_import_exists_online_id (Transaction *trans)
{
  int i;
  gboolean online_id_exists = FALSE;
  Account *dest_acct;
  Split *source_split;
  
  /* For each split in the transaction, check whether the parent account
     contains a transaction with the same online id. */
  for (i=0; 
       ((source_split = xaccTransGetSplit(trans, i)) != NULL) &&
	 (online_id_exists == FALSE);
       i++)
    {
      /* DEBUG("%s%d%s","Checking split ",i," for duplicates"); */
      dest_acct = xaccSplitGetAccount(source_split);
      online_id_exists = !xaccAccountForEachTransaction(dest_acct,
							check_trans_online_id,
							trans);
    }

  /* If it does, abort the process for this transaction, since it is
     already in the system. */
  if (online_id_exists == TRUE)
    {
      DEBUG("%s","Transaction with same online ID exists, destroying current transaction");
      xaccTransDestroy(trans);
      xaccTransCommitEdit(trans);
    }
  return online_id_exists;
}


/* ******************************************************************
 */

/* Create a new object of GNCImportTransInfo here. */
GNCImportTransInfo *
gnc_import_TransInfo_new (Transaction *trans, GncImportMatchMap *matchmap)
{
  GNCImportTransInfo *transaction_info;
  g_assert (trans);
  
  transaction_info = g_new0(GNCImportTransInfo,1);
      
  transaction_info->trans=trans;
  /* Only use first split, the source split */
  transaction_info->first_split = xaccTransGetSplit(trans,0);

  transaction_info->dest_acc = 
    matchmap_find_destination (matchmap, transaction_info);
  
  return transaction_info;
}


/* compare_probability() is used by g_list_sort to sort by probability */
static gint compare_probability (gconstpointer a,
				 gconstpointer b)
{
  return(((GNCImportMatchInfo *)b)->probability - 
	 ((GNCImportMatchInfo *)a)->probability);
}

/** Iterates through all splits of the originating account of
 * trans_info. Sorts the resulting list and sets the selected_match
 * and action fields in the trans_info. 
 */
void 
gnc_import_TransInfo_init_matches (GNCImportTransInfo *trans_info,
				   gint clear_threshold, 
				   gint add_threshold,
				   gint process_threshold, 
				   double fuzzy_amount_difference)
{
  GNCImportMatchInfo * best_match;
  g_assert (trans_info);
  
  /* Find all split matches in originating account. */
  gnc_import_find_split_matches(trans_info, process_threshold,
				fuzzy_amount_difference);

  if (trans_info->match_list != NULL) 
    {
      trans_info->match_list = g_list_sort(trans_info->match_list,
					   compare_probability);
      best_match = g_list_nth_data(trans_info->match_list,0);
      if(best_match != NULL && 
	 best_match->probability >= clear_threshold)
	{
	  trans_info->action = GNCImport_CLEAR;
	  trans_info->selected_match_info = best_match;
	}
      else if(best_match == NULL ||
	      best_match->probability <= add_threshold)
	trans_info->action=GNCImport_ADD;
      else
	trans_info->action=GNCImport_SKIP;
    }
  else
    trans_info->action = GNCImport_ADD;
  
  trans_info->previous_action=trans_info->action;
}


/********************************************************************\
 * gnc_import_add_trans(Transaction *trans) 
 * The transaction passed to this function must contain at least
 * one split, and this split must have been associated with an account
 * Only the first split will be used for matching.  The 
 * transaction should not be commited.
\********************************************************************/
void gnc_import_add_trans(Transaction *trans)
{
  static struct _transmatcherdialog * matcher;
  GNCImportTransInfo * transaction_info = NULL;
  gint row_number;

  gnc_should_log(MOD_IMPORT, GNC_LOG_TRACE);

  DEBUG("%s", "Begin...");

  if (gnc_import_exists_online_id (trans))
    {
      return;
    }
  else
    {
      if(matcher == NULL)
	{
	  DEBUG("Gui not yet opened");
	  matcher = g_new0(struct _transmatcherdialog, 1);
	  init_matcher_gui(matcher);
	  DEBUG("Gui init done");
	}
      else if(matcher->initialised==FALSE)
	{
	  init_matcher_gui(matcher);
	  DEBUG("Matcher reinitialised");
	}
      
      transaction_info = gnc_import_TransInfo_new(trans, NULL);
      
      gnc_import_TransInfo_init_matches (transaction_info, 
					 matcher->clear_threshold, 
					 matcher->add_threshold,
					 matcher->display_threshold,
					 MATCH_ATM_FEE_THRESHOLD);


      row_number = gtk_clist_append(matcher->downloaded_clist,
				    (char **)(transaction_info->clist_text));
      gtk_clist_set_row_data_full(matcher->downloaded_clist,
				  row_number,
				  transaction_info,
				  downloaded_trans_row_destroy_cb);
      downloaded_transaction_refresh_gui(matcher,
					 transaction_info);
    }
  return;
}/* end gnc_import_add_trans() */


