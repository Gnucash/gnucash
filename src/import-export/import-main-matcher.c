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
    @file import-main-matcher.c
    @brief Transaction matcher main window
    @author Copyright (C) 2002 Benoit Grégoire
    @author Christian Stimming    
*/
#include "config.h"

#include <gnome.h>

#include "import-main-matcher.h"

#include "dialog-utils.h"
#include "gnc-ui.h"
#include "global-options.h"
#include "gnc-ui-util.h"
#include "gnc-engine-util.h"
#include "import-settings.h"
#include "import-match-map.h"
#include "import-match-picker.h"
#include "import-backend.h"
#include "import-account-matcher.h"

struct _main_matcher_info
{
  GtkWidget *dialog;
  GtkWidget *clist;
  GNCImportSettings *user_settings;
  GdkPixmap* fleche_pixmap;
  GdkPixmap* checkbox_checked_pixmap;
  GdkPixmap* checkbox_unchecked_pixmap;
  GdkColor color_back_white;
  GdkColor color_back_red;
  GdkColor color_back_green;
  GdkColor color_back_yellow;
  int selected_row;
};

#define NUM_COLUMNS_DOWNLOADED_CLIST 9
#define DOWNLOADED_CLIST_ACCOUNT 1
#define DOWNLOADED_CLIST_DATE 0
#define DOWNLOADED_CLIST_AMOUNT 2
#define DOWNLOADED_CLIST_DESCRIPTION 3
#define DOWNLOADED_CLIST_MEMO 4
#define DOWNLOADED_CLIST_ACTION_ADD 5
#define DOWNLOADED_CLIST_ACTION_CLEAR 6
#define DOWNLOADED_CLIST_ACTION_EDIT 7
#define DOWNLOADED_CLIST_ACTION_INFO 8
static short module = MOD_IMPORT;

/* Local prototypes */
static void automatch_clist_transactions(GNCImportMainMatcher *info, GtkCList *clist);



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

static char * checkbox_checked_xpm[] = {
"16 16 28 1",
" 	c None",
".	c #20449C",
"+	c #000000",
"@	c #FFFFFF",
"#	c #D5D2D5",
"$	c #EEEEEE",
"%	c #B4B6B4",
"&	c #20409C",
"*	c #DEE2DE",
"=	c #E6E6E6",
"-	c #EEEAEE",
";	c #838183",
">	c #184094",
",	c #CDCACD",
"'	c #8B898B",
")	c #949594",
"!	c #9C999C",
"~	c #A4A5A4",
"{	c #C5C6C5",
"]	c #949194",
"^	c #A4A1A4",
"/	c #BDBABD",
"(	c #DEDADE",
"_	c #C5C2C5",
":	c #BDBEBD",
"<	c #DEDEDE",
"[	c #D5D6D5",
"}	c #ACAAAC",
" .............. ",
"................",
"..++++++++++++..",
"..+@@@@@@@@@#+..",
"..+@$$$$$$$+%+..",
"..+@$$$$$$++++&.",
"..+@$+*=-+++;+>.",
"..+@+++,+++''+&.",
"..+@=+++++)!~+..",
"..+@-{+++]^//+..",
"..+@$(_+]^_#:+..",
"..+@<[#/}/#[:+..",
"..+#:::/%/:::+..",
"..++++++++++++..",
"................",
" .............. "};

static char * checkbox_unchecked_xpm[] = {
"12 12 14 1",
" 	c None",
".	c #000000",
"+	c #FFFFFF",
"@	c #A4A5A4",
"#	c #DEE2DE",
"$	c #7B7D7B",
"%	c #DEDEDE",
"&	c #D5D6D5",
"*	c #CDD2CD",
"=	c #CDCACD",
"-	c #C5C6C5",
";	c #C5C2C5",
">	c #BDBEBD",
",	c #ACAEAC",
"............",
".+++++++++@.",
".+########$.",
".+%%%%%%%%$.",
".+&&&&&&&&$.",
".+********$.",
".+========$.",
".+--;---;-$.",
".+>>>>>>>>$.",
".+,,,,,,,,$.",
".@$$$$$$$$$.",
"............"};

static void
refresh_clist_row (GNCImportMainMatcher *gui, 
		   int row_number, GNCImportTransInfo *info);

void gnc_gen_trans_list_delete (GNCImportMainMatcher *info)
{
  if (info == NULL) 
    return;

  gnc_import_Settings_delete (info->user_settings);
  gtk_widget_destroy (GTK_WIDGET (info->dialog));
  g_free (info);
}

static void 
on_matcher_ok_clicked (GtkButton *button,
			   gpointer user_data)
{
  GNCImportMainMatcher *info = user_data;
  g_assert (info);
  /*   DEBUG ("Begin") */
  gnc_import_process_trans_clist (GTK_CLIST (info->clist), NULL);
  /* DEBUG ("Deleting") */
  gnc_gen_trans_list_delete (info);
  /* DEBUG ("End") */
}

static void 
on_matcher_cancel_clicked (GtkButton *button,
			   gpointer user_data)
{
  GNCImportMainMatcher *info = user_data;
  gnc_gen_trans_list_delete (info);
}

static void 
run_account_picker_dialog (GNCImportMainMatcher *info, 
			   gint row, GNCImportTransInfo *trans_info)
{
  Account *old_acc, *new_acc;
  gboolean ok_pressed;
  g_assert (trans_info);
  old_acc = gnc_import_TransInfo_get_destacc (trans_info);
  new_acc = gnc_import_select_account(NULL,
				      TRUE,
				      _("Destination account for the auto-balance split."),
				      xaccTransGetCurrency(gnc_import_TransInfo_get_trans(trans_info)),
				      NO_TYPE,
				      old_acc,
				      &ok_pressed);
  if(ok_pressed)
    {
      gnc_import_TransInfo_set_destacc (trans_info,
					new_acc,
					TRUE);

      /* Iterate through the transactions in a given clist to auto match them */
      automatch_clist_transactions(info, (GtkCList*)info->clist);
    }
}

static void 
run_match_dialog (GNCImportMainMatcher *info, 
		  gint row, GNCImportTransInfo *trans_info)
{
  gnc_import_match_picker_run_and_close (trans_info);
}

static void
clist_select_row_cb (GtkCList *clist,
		     gint row_number,
		     gint column,
		     GdkEventButton *event,
		     gpointer user_data) 
{
  GNCImportMainMatcher *gui = user_data; 
  GNCImportTransInfo *trans_info;
  gboolean should_refresh = TRUE;
  /*DEBUG("row_number: %d%s%d",row_number,", column: ",column);*/
  trans_info = gtk_clist_get_row_data (clist, row_number);
  if (trans_info == NULL)
    return;

  switch(column)
    {
    case DOWNLOADED_CLIST_ACTION_ADD:
     if( gnc_import_TransInfo_get_action(trans_info)==GNCImport_ADD 
	 && gnc_import_Settings_get_action_skip_enabled (gui->user_settings)==TRUE)
       {
	 gnc_import_TransInfo_set_action(trans_info, GNCImport_SKIP);
       }
     else
       {
	 gnc_import_TransInfo_set_action(trans_info, GNCImport_ADD);
       }
      break;
    case DOWNLOADED_CLIST_ACTION_CLEAR:
      if( gnc_import_TransInfo_get_action(trans_info)==GNCImport_CLEAR
	  && gnc_import_Settings_get_action_skip_enabled (gui->user_settings)==TRUE)
	{
	  gnc_import_TransInfo_set_action(trans_info, GNCImport_SKIP);
	}
      else
	{
	  gnc_import_TransInfo_set_action(trans_info, GNCImport_CLEAR);
	}
      break;
    case DOWNLOADED_CLIST_ACTION_EDIT: 
      if( gnc_import_TransInfo_get_action(trans_info)==GNCImport_EDIT
	  && gnc_import_Settings_get_action_skip_enabled (gui->user_settings)==TRUE)
	{
	  gnc_import_TransInfo_set_action(trans_info, GNCImport_SKIP);
	}
      else
	{
	  gnc_import_TransInfo_set_action(trans_info, GNCImport_EDIT);
	}
      break;
    case DOWNLOADED_CLIST_ACTION_INFO:
      switch(gnc_import_TransInfo_get_action (trans_info))
	{
	case GNCImport_ADD:
	  if(gnc_import_TransInfo_is_balanced(trans_info)==FALSE)
	    {
	      run_account_picker_dialog (gui, row_number, trans_info);
	    }
	  break;
	case GNCImport_CLEAR:
	  run_match_dialog (gui, row_number, trans_info);
	  break;
	case GNCImport_SKIP:
	  /*The information displayed is only informative, until you select an action*/
	  break;
	default:
	  PERR("I don't know what to do! (Yet...)");
	}
      break;
    default:
      /*Do nothing for other columns*/
      should_refresh = FALSE;
    }
  
  if(should_refresh == TRUE)/*If there was a change, refresh the GUI for that row*/
    {
      refresh_clist_row (gui, row_number, trans_info);
    }
  gtk_clist_unselect_row (clist,
			  row_number,
			  column);
}

static void gnc_gen_trans_list_freeze (GNCImportMainMatcher *gui)
{
  g_assert (gui);
  gtk_clist_freeze (GTK_CLIST (gui->clist));
}

static void gnc_gen_trans_list_thaw (GNCImportMainMatcher *gui)
{
  g_assert (gui);
  gtk_clist_thaw (GTK_CLIST (gui->clist));
}

GNCImportMainMatcher *gnc_gen_trans_list_new (GtkWidget *parent, 
					   const gchar* heading,
					   gboolean all_from_same_account)
{
  GNCImportMainMatcher *info;
  GladeXML *xml;
  GtkWidget *heading_label;
  
  gnc_should_log(MOD_IMPORT, GNC_LOG_TRACE);

  info = g_new0 (GNCImportMainMatcher, 1);

  /* Initialize user Settings. */
  info->user_settings = gnc_import_Settings_new ();

  /* Initialize the GnomeDialog. */
  xml = gnc_glade_xml_new ("generic-import.glade", "transaction_matcher");

  g_assert
    (info->dialog = glade_xml_get_widget (xml, "transaction_matcher"));
  g_assert 
    (info->clist = glade_xml_get_widget (xml, "downloaded_clist"));
  g_assert
    (heading_label = glade_xml_get_widget (xml, "heading_label"));

  /*if (parent)
    gnome_dialog_set_parent (GNOME_DIALOG (info->dialog), 
			     GTK_WINDOW (parent));*/

  /* Connect signals */
  glade_xml_signal_connect_data(xml, "downloaded_transaction_select_cb",
				GTK_SIGNAL_FUNC(clist_select_row_cb), 
				info);
  glade_xml_signal_connect_data(xml, "on_matcher_ok_clicked", 
				GTK_SIGNAL_FUNC(on_matcher_ok_clicked),
				info);
  glade_xml_signal_connect_data(xml, "on_matcher_cancel_clicked", 
				GTK_SIGNAL_FUNC(on_matcher_cancel_clicked),
				info);

  /*Initialise pixmaps*/
  info->fleche_pixmap =  gdk_pixmap_colormap_create_from_xpm_d (NULL,
								gtk_widget_get_colormap(info->dialog),
								NULL,
								NULL,
								fleche_xpm);
  info->checkbox_checked_pixmap =  gdk_pixmap_colormap_create_from_xpm_d (NULL,
									  gtk_widget_get_colormap(info->dialog),
									  NULL,
									  NULL,
									  checkbox_checked_xpm);
  info->checkbox_unchecked_pixmap =  gdk_pixmap_colormap_create_from_xpm_d (NULL,
									    gtk_widget_get_colormap(info->dialog),
									    NULL,
									    NULL,
									    checkbox_unchecked_xpm);
 /*Initialise the colors */
  info->color_back_red.red=65535;
  info->color_back_red.green=16383;
  info->color_back_red.blue=16383;
  info->color_back_green.red=49151;
  info->color_back_green.green=65535;
  info->color_back_green.blue=49151;
  info->color_back_yellow.red=65535;
  info->color_back_yellow.green=55255;
  info->color_back_yellow.blue=0;
  info->color_back_white.red=65535;
  info->color_back_white.green=65535;
  info->color_back_white.blue=65535;
  /*Ajust column size*/
  gtk_clist_set_column_auto_resize (GTK_CLIST (info->clist),
				    DOWNLOADED_CLIST_DATE,
				    TRUE);
  gtk_clist_set_column_auto_resize (GTK_CLIST (info->clist),
				    DOWNLOADED_CLIST_AMOUNT,
				    TRUE);
  gtk_clist_set_column_auto_resize (GTK_CLIST (info->clist),
				    DOWNLOADED_CLIST_ACTION_ADD,
				    TRUE);
  gtk_clist_set_column_auto_resize (GTK_CLIST (info->clist),
				    DOWNLOADED_CLIST_ACTION_CLEAR,
				    TRUE);
  gtk_clist_set_column_auto_resize (GTK_CLIST (info->clist),
				    DOWNLOADED_CLIST_ACTION_EDIT,
				    TRUE);
  gtk_clist_set_column_auto_resize (GTK_CLIST (info->clist),
				    DOWNLOADED_CLIST_ACTION_INFO,
				    TRUE);
  /*Set column visibility*/
  if(all_from_same_account==TRUE)
    {
      gtk_clist_set_column_visibility (GTK_CLIST (info->clist),
				       DOWNLOADED_CLIST_ACCOUNT,
				       FALSE);
    }
  if(gnc_import_Settings_get_action_edit_enabled (info->user_settings)==FALSE)
    {
      gtk_clist_set_column_visibility (GTK_CLIST (info->clist),
				       DOWNLOADED_CLIST_ACTION_EDIT,
				       FALSE);
    }
  

  if (heading)
    gtk_label_set_text (GTK_LABEL (heading_label), heading);
  
  /* Hide on close instead of destroy since we still need the values
     from the boxes. */
  /*gnome_dialog_close_hides (GNOME_DIALOG (info->dialog), TRUE);*/
  gtk_widget_show_all (GTK_WIDGET (info->dialog));
  return info;
}

gboolean gnc_gen_trans_list_run (GNCImportMainMatcher *info) 
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


static char ** gen_clist_row_text (GNCImportTransInfo *info)
{
  static char *text[NUM_COLUMNS_DOWNLOADED_CLIST];
  gint i;
  g_assert (info);
  for(i = 0; i < NUM_COLUMNS_DOWNLOADED_CLIST; i++)
    {
      text[i]=g_strdup("");
    }
  return text;
}


static void
refresh_clist_row (GNCImportMainMatcher *gui, 
		   int row_number, GNCImportTransInfo *info)
{
  char *text[NUM_COLUMNS_DOWNLOADED_CLIST];
  char **old_text = g_new(char *, NUM_COLUMNS_DOWNLOADED_CLIST);/* Should be g_new?*/
  gint i;
  gchar *tmp,*imbalance;
  g_assert (gui);
  g_assert (info);
  /*DEBUG("Begin");*/
  gnc_gen_trans_list_freeze(gui);
  
  for (i = 0; i < NUM_COLUMNS_DOWNLOADED_CLIST; i++)
    {
      gtk_clist_get_text (GTK_CLIST (gui->clist), row_number, 
	i, 
	&(old_text[i]));
      text[i]=NULL;
    }

  /* Note that ALL strings must be duplicated with g_strdup or equivalent, so that they can
     be freed by the widget*/
  
  /*Account:*/
  text[DOWNLOADED_CLIST_ACCOUNT] = 
    g_strdup(xaccAccountGetName(xaccSplitGetAccount(gnc_import_TransInfo_get_fsplit (info))));
  gtk_clist_set_text (GTK_CLIST (gui->clist), row_number, 
		      DOWNLOADED_CLIST_ACCOUNT, 
		      text[DOWNLOADED_CLIST_ACCOUNT]);

  /*Date*/

  text[DOWNLOADED_CLIST_DATE] = 
    g_strdup( xaccPrintDateSecs ( xaccTransGetDate( gnc_import_TransInfo_get_trans(info) ) ));
  gtk_clist_set_text (GTK_CLIST (gui->clist), row_number, 
		      DOWNLOADED_CLIST_DATE, 
		      text[DOWNLOADED_CLIST_DATE]);
  
  /*Amount*/
  text[DOWNLOADED_CLIST_AMOUNT] = 
    g_strdup(xaccPrintAmount 
	     (xaccSplitGetAmount (gnc_import_TransInfo_get_fsplit(info) ), 
	      gnc_split_amount_print_info(gnc_import_TransInfo_get_fsplit(info), TRUE) 
	      ) );
  gtk_clist_set_text (GTK_CLIST (gui->clist), row_number, 
		      DOWNLOADED_CLIST_AMOUNT, 
		      text[DOWNLOADED_CLIST_AMOUNT]);
  
  /*Description*/
  text[DOWNLOADED_CLIST_DESCRIPTION] = 
    g_strdup(xaccTransGetDescription(gnc_import_TransInfo_get_trans(info) ) );
  gtk_clist_set_text (GTK_CLIST (gui->clist), row_number, 
		      DOWNLOADED_CLIST_DESCRIPTION, 
		      text[DOWNLOADED_CLIST_DESCRIPTION]);

  /*Memo*/
  text[DOWNLOADED_CLIST_MEMO] = 
    g_strdup(xaccSplitGetMemo(gnc_import_TransInfo_get_fsplit(info) ) );
  gtk_clist_set_text (GTK_CLIST (gui->clist), row_number, 
		      DOWNLOADED_CLIST_MEMO, 
		      text[DOWNLOADED_CLIST_MEMO]);
  
  /*Actions*/
  text[DOWNLOADED_CLIST_ACTION_ADD] = g_strdup("");
  text[DOWNLOADED_CLIST_ACTION_CLEAR] = g_strdup("");
  text[DOWNLOADED_CLIST_ACTION_EDIT] = g_strdup("");
  
  /* Action informations */
  gtk_clist_set_background (GTK_CLIST (gui->clist), row_number, 
			    &(gui->color_back_white));
  switch(gnc_import_TransInfo_get_action(info))
    {
    case GNCImport_ADD:
      if(gnc_import_TransInfo_is_balanced(info)==TRUE)
	{
	  text[DOWNLOADED_CLIST_ACTION_INFO] = 
	    g_strdup(_("New, already balanced"));
	  gtk_clist_set_background (GTK_CLIST (gui->clist), row_number, 
				    &(gui->color_back_green));
	}
      else
	{
	  imbalance = 
	    g_strdup 
	    (xaccPrintAmount
	     (gnc_numeric_neg(xaccTransGetImbalance
			      (gnc_import_TransInfo_get_trans(info) )), 
	      gnc_commodity_print_info 
	      (xaccTransGetCurrency(gnc_import_TransInfo_get_trans (info)),
	       TRUE) ));
	  if (gnc_import_TransInfo_get_destacc (info) != NULL)
	    {
	      gtk_clist_set_background (GTK_CLIST (gui->clist), row_number, 
					&(gui->color_back_green));
	      tmp = xaccAccountGetFullName 
		(gnc_import_TransInfo_get_destacc (info),
		 gnc_get_account_separator ());
	      if(gnc_import_TransInfo_get_destacc_selected_manually(info)
		 == TRUE)
		{
		  text[DOWNLOADED_CLIST_ACTION_INFO] = 
		    /* Translators: %1$s is the amount to be 
		       transferred. %2$s is the destination account. */
		    g_strdup_printf(_("New, transfer %s to (manual) \"%s\""),
				    imbalance, tmp);
		}
	      else
		{
		  text[DOWNLOADED_CLIST_ACTION_INFO] = 
		    /* Translators: %1$s is the amount to be 
		       transferred. %2$s is the destination account. */
		    g_strdup_printf(_("New, transfer %s to (auto) \"%s\""),
				    imbalance,tmp);
		}
	      g_free (tmp);

	    }
	  else
	    {
	      gtk_clist_set_background (GTK_CLIST (gui->clist), row_number, 
					&(gui->color_back_yellow));
	      text[DOWNLOADED_CLIST_ACTION_INFO] = 
		/* Translators: %s is the amount to be transferred. */
		g_strdup_printf(_("New, UNBALANCED (need acct to transfer %s)!"),
				imbalance);
	    }
	      g_free (imbalance);
	}
      break;
    case GNCImport_CLEAR: 
      if(gnc_import_TransInfo_get_selected_match(info))
	{
	  gtk_clist_set_background (GTK_CLIST (gui->clist), row_number, 
				    &(gui->color_back_green));
	  if(gnc_import_TransInfo_get_match_selected_manually(info)==TRUE)
	    {
	      text[DOWNLOADED_CLIST_ACTION_INFO] = 
		g_strdup(_("Reconcile (manual) match"));
	    }
	  else
	    {
	      text[DOWNLOADED_CLIST_ACTION_INFO] = 
		g_strdup(_("Reconcile (auto) match"));
	    }
	}
      else
	{
	  gtk_clist_set_background (GTK_CLIST (gui->clist), row_number, 
				    &(gui->color_back_red));
	  text[DOWNLOADED_CLIST_ACTION_INFO] = 
	    g_strdup_printf(_("Match missing!"));
	}
      break;
    case GNCImport_EDIT: 
      text[DOWNLOADED_CLIST_ACTION_INFO] = g_strdup("NOT SUPPORTED YET!");
      break;
    case GNCImport_SKIP: 
      text[DOWNLOADED_CLIST_ACTION_INFO] = 
	g_strdup(_("Do not import (no action selected)"));
      gtk_clist_set_background (GTK_CLIST (gui->clist), row_number, 
				&(gui->color_back_red));
      break;
    default:
      text[DOWNLOADED_CLIST_ACTION_INFO] = 
	g_strdup("WRITEME, this is an unknown action");
    }
  gtk_clist_set_text (GTK_CLIST (gui->clist), row_number, 
		      DOWNLOADED_CLIST_ACTION_INFO, 
		      text[DOWNLOADED_CLIST_ACTION_INFO]);

  /* Set the pixmaps */
  if(gnc_import_TransInfo_get_action(info)==GNCImport_ADD)
    {
      gtk_clist_set_pixtext (GTK_CLIST (gui->clist), row_number,
			     DOWNLOADED_CLIST_ACTION_ADD,
			     text[DOWNLOADED_CLIST_ACTION_ADD],
			     3,
			     gui->checkbox_checked_pixmap,
			     NULL);
      
      if(gnc_import_TransInfo_is_balanced(info)==FALSE)
	{
	  /*Show the arrow button*/
	  gtk_clist_set_pixtext (GTK_CLIST (gui->clist), row_number,
				 DOWNLOADED_CLIST_ACTION_INFO,
				 text[DOWNLOADED_CLIST_ACTION_INFO],
				 3,
				 gui->fleche_pixmap,
				 NULL);
	}
    }
  else
    {
      gtk_clist_set_pixtext (GTK_CLIST (gui->clist), row_number,
			     DOWNLOADED_CLIST_ACTION_ADD,
			     text[DOWNLOADED_CLIST_ACTION_ADD],
			     3,
			     gui->checkbox_unchecked_pixmap,
			     NULL);
    }
  
  if(gnc_import_TransInfo_get_action(info)==GNCImport_SKIP)
    {      
      /*Show the best match's confidence pixmap in the info column*/
      gtk_clist_set_pixtext (GTK_CLIST (gui->clist), row_number,
			     DOWNLOADED_CLIST_ACTION_INFO,
			     text[DOWNLOADED_CLIST_ACTION_INFO],
			     3,
			     gen_probability_pixmap( gnc_import_MatchInfo_get_probability 
						     ( gnc_import_TransInfo_get_selected_match (info)),
						     gui->user_settings, 
						     GTK_WIDGET(gui->clist)),
			     NULL);
    }
  
  if(gnc_import_TransInfo_get_action(info)==GNCImport_CLEAR)
    {
      gtk_clist_set_pixtext (GTK_CLIST (gui->clist), row_number,
			     DOWNLOADED_CLIST_ACTION_CLEAR,
			     text[DOWNLOADED_CLIST_ACTION_CLEAR],
			     3,
			     gui->checkbox_checked_pixmap,
			     NULL);
      /*Show the best match's confidence pixmap in the info column*/
      gtk_clist_set_pixtext (GTK_CLIST (gui->clist), row_number,
			     DOWNLOADED_CLIST_ACTION_INFO,
			     text[DOWNLOADED_CLIST_ACTION_INFO],
			     3,
			     gen_probability_pixmap( gnc_import_MatchInfo_get_probability 
						     ( gnc_import_TransInfo_get_selected_match (info)),
						     gui->user_settings, 
						     GTK_WIDGET(gui->clist)),
			     NULL);
    }
  else
    {
      gtk_clist_set_pixtext (GTK_CLIST (gui->clist), row_number,
			     DOWNLOADED_CLIST_ACTION_CLEAR,
			     text[DOWNLOADED_CLIST_ACTION_CLEAR],
			     3,
			     gui->checkbox_unchecked_pixmap,
			     NULL);
    }
  
  if(gnc_import_TransInfo_get_action(info)==GNCImport_EDIT)
    {
      gtk_clist_set_pixtext (GTK_CLIST (gui->clist), row_number,
			     DOWNLOADED_CLIST_ACTION_EDIT,
			     text[DOWNLOADED_CLIST_ACTION_EDIT],
			     3,
			     gui->checkbox_checked_pixmap,
			     NULL);
    }
  else
    {
      gtk_clist_set_pixtext (GTK_CLIST (gui->clist), row_number,
			     DOWNLOADED_CLIST_ACTION_EDIT,
			     text[DOWNLOADED_CLIST_ACTION_EDIT],
			     3,
			     gui->checkbox_unchecked_pixmap,
			     NULL);
    }
  
  gtk_clist_set_row_height        (GTK_CLIST (gui->clist),
				   24);
  
  
  gnc_gen_trans_list_thaw(gui);
}


void gnc_gen_trans_list_add_trans(GNCImportMainMatcher *gui, Transaction *trans)
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
					 gui->user_settings);

      row_number = gtk_clist_append(GTK_CLIST (gui->clist),
				    gen_clist_row_text (transaction_info));
      gtk_clist_set_row_data_full(GTK_CLIST (gui->clist),
				  row_number,
				  transaction_info,
				  trans_clist_row_destroy_cb);
      refresh_clist_row (gui, 
			 row_number, 
			 transaction_info);
    }
  return;
}/* end gnc_import_add_trans() */

/* Iterate through the rows of the clist and try to automatch each of them */
static void automatch_clist_transactions(GNCImportMainMatcher *info, GtkCList *clist)
{
  int row;
  GNCImportTransInfo *trans_info;
  
  for(row = 0; row < clist->rows; row++)
    {
      trans_info = gtk_clist_get_row_data(clist, row);
      
      /* returns TRUE if we changed this row, so update it */
      if(gnc_import_TransInfo_refresh_destacc(trans_info, NULL))
	{
	  refresh_clist_row(info, row, trans_info);
	}
    }
}

/** @} */
