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
    @file import-main-matcher.c
    @brief Transaction matcher main window
    @author Copyright (C) 2002 Benoit Grégoire
    @author Christian Stimming    
    @author Copyright (c) 2006 David Hampton <hampton@employees.org>
*/
#include "config.h"

#include <gtk/gtk.h>
#include <glib/gi18n.h>

#include "import-main-matcher.h"

#include "dialog-utils.h"
#include "gnc-ui.h"
#include "gnc-ui-util.h"
#include "gnc-engine.h"
#include "import-settings.h"
#include "import-match-map.h"
#include "import-match-picker.h"
#include "import-backend.h"
#include "import-account-matcher.h"

#define GCONF_SECTION "dialogs/import/generic_matcher/transaction_list"

struct _main_matcher_info
{
  GtkWidget *dialog;
  GtkTreeView *view;
  GNCImportSettings *user_settings;
  GdkColor color_back_red;
  GdkColor color_back_green;
  GdkColor color_back_yellow;
  int selected_row;
};

enum downloaded_cols {
  DOWNLOADED_COL_DATE = 0,
  DOWNLOADED_COL_ACCOUNT,
  DOWNLOADED_COL_AMOUNT,
  DOWNLOADED_COL_DESCRIPTION,
  DOWNLOADED_COL_MEMO,
  DOWNLOADED_COL_ACTION_ADD,
  DOWNLOADED_COL_ACTION_CLEAR,
  DOWNLOADED_COL_ACTION_EDIT,
  DOWNLOADED_COL_ACTION_INFO,
  DOWNLOADED_COL_ACTION_PIXBUF,
  DOWNLOADED_COL_DATA,
  DOWNLOADED_COL_COLOR,
  NUM_DOWNLOADED_COLS
};

#define COLOR_RED    "brown1"
#define COLOR_YELLOW "gold"
#define COLOR_GREEN  "DarkSeaGreen1"

static QofLogModule log_module = GNC_MOD_IMPORT;

/* Local prototypes */
static void
automatch_store_transactions(GNCImportMainMatcher *info,
			     GtkTreeModel *model,
			     GtkTreeIter *iter,
			     GNCImportTransInfo *trans_info);
static void
refresh_model_row(GNCImportMainMatcher *gui, GtkTreeModel *model,
		  GtkTreeIter *iter, GNCImportTransInfo *info);

void gnc_gen_trans_list_delete (GNCImportMainMatcher *info)
{
  GtkTreeModel *model;
  GtkTreeIter iter;
  GNCImportTransInfo *trans_info;

  if (info == NULL) 
    return;

  model = gtk_tree_view_get_model(info->view);
  if (gtk_tree_model_get_iter_first(model, &iter)) {
    do {
      gtk_tree_model_get(model, &iter,
		         DOWNLOADED_COL_DATA, &trans_info,
		         -1);
      gnc_import_TransInfo_delete(trans_info);
    } while (gtk_tree_model_iter_next (model, &iter));
  }

  gnc_save_window_size(GCONF_SECTION, GTK_WINDOW(info->dialog));
  gnc_import_Settings_delete (info->user_settings);
  gtk_widget_destroy (GTK_WIDGET (info->dialog));
  g_free (info);
}

static void 
on_matcher_ok_clicked (GtkButton *button,
		       GNCImportMainMatcher *info)
{
  GtkTreeModel *model;
  GtkTreePath *path;
  GtkTreeRowReference *ref;
  GtkTreeIter iter;
  GNCImportTransInfo *trans_info;
  GSList *refs_list = NULL, *item;

  g_assert (info);

  /*   DEBUG ("Begin") */

  model = gtk_tree_view_get_model(info->view);
  if (!gtk_tree_model_get_iter_first(model, &iter))
    return;

  do {
    gtk_tree_model_get(model, &iter,
		       DOWNLOADED_COL_DATA, &trans_info,
		       -1);
    if (gnc_import_process_trans_item (NULL, trans_info)) {
      path = gtk_tree_model_get_path(model, &iter);
      ref = gtk_tree_row_reference_new(model, path);
      refs_list = g_slist_append(refs_list, ref);
      gtk_tree_path_free(path);
    }
  } while (gtk_tree_model_iter_next (model, &iter));

  /* DEBUG ("Deleting") */
  /* DRH: Is this necessary. Isn't the call to trans_list_delete at
     the end of this routine going to destroy the entire list store
     anyway? */
  for (item = refs_list; item; item = g_slist_next(item)) {
    ref = item->data;
    path =  gtk_tree_row_reference_get_path(ref);
    if (gtk_tree_model_get_iter(model, &iter, path))
      gtk_list_store_remove(GTK_LIST_STORE(model), &iter);
    gtk_tree_path_free(path);
    gtk_tree_row_reference_free(ref);
  }
  g_slist_free(refs_list);

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
on_matcher_help_close_clicked (GtkButton *button,
			       gpointer user_data)
{
  GtkWidget *help_dialog = user_data;

  gtk_widget_destroy(help_dialog);
}

static void 
on_matcher_help_clicked (GtkButton *button,
			 gpointer user_data)
{
  GNCImportMainMatcher *info = user_data;
  GladeXML *xml;
  GtkWidget *help_dialog, *box;
  
  xml = gnc_glade_xml_new ("generic-import.glade", "matcher_help");

  box = glade_xml_get_widget (xml, "red");
  gtk_widget_modify_bg(box, GTK_STATE_NORMAL, &info->color_back_red);
  box = glade_xml_get_widget (xml, "yellow");
  gtk_widget_modify_bg(box, GTK_STATE_NORMAL, &info->color_back_yellow);
  box = glade_xml_get_widget (xml, "green");
  gtk_widget_modify_bg(box, GTK_STATE_NORMAL, &info->color_back_green);

  help_dialog = glade_xml_get_widget (xml, "matcher_help");
  gtk_window_set_transient_for(GTK_WINDOW(help_dialog),
			       GTK_WINDOW(info->dialog));

  glade_xml_signal_connect_data(xml, "on_matcher_help_close_clicked",
				G_CALLBACK(on_matcher_help_close_clicked), 
				help_dialog);

  gtk_widget_show(help_dialog);
}

static void 
run_account_picker_dialog (GNCImportMainMatcher *info, 
			   GtkTreeModel *model,
			   GtkTreeIter *iter,
			   GNCImportTransInfo *trans_info)
{
  Account *old_acc, *new_acc;
  gboolean ok_pressed;
  g_assert (trans_info);
  old_acc = gnc_import_TransInfo_get_destacc (trans_info);
  new_acc = gnc_import_select_account(info->dialog,
				      NULL,
				      TRUE,
				      _("Destination account for the auto-balance split."),
				      xaccTransGetCurrency(gnc_import_TransInfo_get_trans(trans_info)),
				      ACCT_TYPE_NONE,
				      old_acc,
				      &ok_pressed);
  if(ok_pressed)
    {
      gnc_import_TransInfo_set_destacc (trans_info,
					new_acc,
					TRUE);

      /* Iterate through the transactions in a given clist to auto match them */
      automatch_store_transactions(info, model, iter, trans_info);
    }
}

static void 
run_match_dialog (GNCImportMainMatcher *info, 
		  GNCImportTransInfo *trans_info)
{
  gnc_import_match_picker_run_and_close (trans_info);
}

static void
gnc_gen_trans_add_toggled_cb (GtkCellRendererToggle *cell_renderer,
			      gchar                 *path,
			      GNCImportMainMatcher  *gui)
{
  GtkTreeModel *model;
  GtkTreeIter iter;
  GNCImportTransInfo *trans_info;

  model = gtk_tree_view_get_model(gui->view);
  if (!gtk_tree_model_get_iter_from_string(model, &iter, path))
    return;
  gtk_tree_model_get(model, &iter, DOWNLOADED_COL_DATA, &trans_info, -1);

  if( gnc_import_TransInfo_get_action(trans_info)==GNCImport_ADD 
      && gnc_import_Settings_get_action_skip_enabled (gui->user_settings)==TRUE)
    {
      gnc_import_TransInfo_set_action(trans_info, GNCImport_SKIP);
    }
  else
    {
      gnc_import_TransInfo_set_action(trans_info, GNCImport_ADD);
    }
  refresh_model_row(gui, model, &iter, trans_info);
}

static void
gnc_gen_trans_clear_toggled_cb (GtkCellRendererToggle *cell_renderer,
				gchar                 *path,
				GNCImportMainMatcher  *gui)
{
  GtkTreeModel *model;
  GtkTreeIter iter;
  GNCImportTransInfo *trans_info;

  model = gtk_tree_view_get_model(gui->view);
  if (!gtk_tree_model_get_iter_from_string(model, &iter, path))
    return;
  gtk_tree_model_get(model, &iter, DOWNLOADED_COL_DATA, &trans_info, -1);

  if( gnc_import_TransInfo_get_action(trans_info)==GNCImport_CLEAR
      && gnc_import_Settings_get_action_skip_enabled (gui->user_settings)==TRUE)
    {
      gnc_import_TransInfo_set_action(trans_info, GNCImport_SKIP);
    }
  else
    {
      gnc_import_TransInfo_set_action(trans_info, GNCImport_CLEAR);
    }
  refresh_model_row(gui, model, &iter, trans_info);
}

static void
gnc_gen_trans_edit_toggled_cb (GtkCellRendererToggle *cell_renderer,
			       gchar                 *path,
			       GNCImportMainMatcher  *gui)
{
  GtkTreeModel *model;
  GtkTreeIter iter;
  GNCImportTransInfo *trans_info;

  model = gtk_tree_view_get_model(gui->view);
  if (!gtk_tree_model_get_iter_from_string(model, &iter, path))
    return;
  gtk_tree_model_get(model, &iter, DOWNLOADED_COL_DATA, &trans_info, -1);

  if( gnc_import_TransInfo_get_action(trans_info)==GNCImport_EDIT
      && gnc_import_Settings_get_action_skip_enabled (gui->user_settings)==TRUE)
    {
      gnc_import_TransInfo_set_action(trans_info, GNCImport_SKIP);
    }
  else
    {
      gnc_import_TransInfo_set_action(trans_info, GNCImport_EDIT);
    }
  refresh_model_row(gui, model, &iter, trans_info);
}

static void
gnc_gen_trans_row_activated_cb (GtkTreeView           *view,
				GtkTreePath           *path,
				GtkTreeViewColumn     *column,
				GNCImportMainMatcher  *gui)    
{
  GtkTreeModel *model;
  GtkTreeIter iter;
  GNCImportTransInfo *trans_info;

  model = gtk_tree_view_get_model(gui->view);
  if (!gtk_tree_model_get_iter(model, &iter, path))
    return;
  gtk_tree_model_get(model, &iter, DOWNLOADED_COL_DATA, &trans_info, -1);

  switch(gnc_import_TransInfo_get_action (trans_info))
    {
    case GNCImport_ADD:
      if (gnc_import_TransInfo_is_balanced(trans_info) == FALSE) {
	run_account_picker_dialog (gui, model, &iter, trans_info);
      }
      break;
    case GNCImport_CLEAR:
      run_match_dialog (gui, trans_info);
      break;
    case GNCImport_SKIP:
      /*The information displayed is only informative, until you select an action*/
      break;
    default:
      PERR("I don't know what to do! (Yet...)");
    }
  refresh_model_row(gui, model, &iter, trans_info);
}

static void
gnc_gen_trans_row_changed_cb (GtkTreeSelection *selection,
			      GNCImportMainMatcher *gui)
{
  GtkTreeModel *model;
  GtkTreeIter iter;

  if (!gtk_tree_selection_get_selected(selection, &model, &iter))
    return;
  gtk_tree_selection_unselect_iter(selection, &iter);
}

static GtkTreeViewColumn *
add_text_column(GtkTreeView *view, const gchar *title, int col_num)
{
  GtkCellRenderer *renderer;
  GtkTreeViewColumn *column;
 
  renderer = gtk_cell_renderer_text_new();
  column = gtk_tree_view_column_new_with_attributes
    (title, renderer,
     "text", col_num,
     "background", DOWNLOADED_COL_COLOR,
     NULL);
  gtk_tree_view_column_set_sort_column_id(column, col_num);
  g_object_set(G_OBJECT(column),
	       "reorderable", TRUE,
	       "resizable", TRUE,
	       NULL);
  gtk_tree_view_append_column(view, column);
  return column;
}

static GtkTreeViewColumn *
add_toggle_column(GtkTreeView *view, const gchar *title, int col_num,
		  GCallback cb_fn, gpointer cb_arg)
{
  GtkCellRenderer *renderer;
  GtkTreeViewColumn *column;
 
  renderer = gtk_cell_renderer_toggle_new();
  column = gtk_tree_view_column_new_with_attributes
    (title, renderer,
     "active", col_num,
     "cell-background", DOWNLOADED_COL_COLOR,
     NULL);
  gtk_tree_view_column_set_sort_column_id(column, col_num);
  g_object_set(G_OBJECT(column),
	       "reorderable", TRUE,
	       NULL);
  g_signal_connect(renderer, "toggled", cb_fn, cb_arg);
  gtk_tree_view_append_column(view, column);
  return column;
}

static void
gnc_gen_trans_init_view (GNCImportMainMatcher *info,
			 gboolean show_account,
			 gboolean show_edit)
{
  GtkTreeView *view;
  GtkListStore *store;
  GtkCellRenderer *renderer;
  GtkTreeViewColumn *column;
  GtkTreeSelection *selection;

  view = info->view;
  store = gtk_list_store_new(NUM_DOWNLOADED_COLS,
			     G_TYPE_STRING, G_TYPE_STRING, G_TYPE_STRING,
			     G_TYPE_STRING, G_TYPE_STRING, G_TYPE_BOOLEAN,
			     G_TYPE_BOOLEAN, G_TYPE_BOOLEAN, G_TYPE_STRING,
			     GDK_TYPE_PIXBUF, G_TYPE_POINTER, G_TYPE_STRING);
  gtk_tree_view_set_model(view, GTK_TREE_MODEL(store));
  g_object_unref(store);

  /* Add the columns */
  add_text_column(view, _("Date"), DOWNLOADED_COL_DATE);
  column = add_text_column(view, _("Account"), DOWNLOADED_COL_ACCOUNT);
  gtk_tree_view_column_set_visible(column, show_account);
  add_text_column(view, _("Amount"), DOWNLOADED_COL_AMOUNT);
  add_text_column(view, _("Description"), DOWNLOADED_COL_DESCRIPTION);
  add_text_column(view, _("Memo"), DOWNLOADED_COL_MEMO);
  add_toggle_column(view, _("A"), DOWNLOADED_COL_ACTION_ADD,
		    G_CALLBACK(gnc_gen_trans_add_toggled_cb), info);
  add_toggle_column(view, _("R"), DOWNLOADED_COL_ACTION_CLEAR,
		    G_CALLBACK(gnc_gen_trans_clear_toggled_cb), info);
  column = add_toggle_column(view, _("Edit"), DOWNLOADED_COL_ACTION_EDIT,
			     G_CALLBACK(gnc_gen_trans_edit_toggled_cb), info);
  gtk_tree_view_column_set_visible(column, show_edit);


  /* The last column has multiple renderers */
  renderer = gtk_cell_renderer_pixbuf_new();
  g_object_set(renderer, "xalign", 0.0, NULL);
  column = gtk_tree_view_column_new_with_attributes(_("Info"), renderer,
				      "pixbuf", DOWNLOADED_COL_ACTION_PIXBUF,
				      "cell-background", DOWNLOADED_COL_COLOR,
				      NULL);
  renderer = gtk_cell_renderer_text_new();
  gtk_tree_view_column_pack_start(column, renderer, TRUE);
  gtk_tree_view_column_set_attributes(column, renderer,
				      "text", DOWNLOADED_COL_ACTION_INFO,
				      "background", DOWNLOADED_COL_COLOR,
				      NULL);
  gtk_tree_view_column_set_sort_column_id(column, DOWNLOADED_COL_ACTION_INFO);
  g_object_set(G_OBJECT(column),
	       "reorderable", TRUE,
	       "resizable", TRUE,
	       NULL);
  gtk_tree_view_append_column(info->view, column);


  selection = gtk_tree_view_get_selection(info->view);
  g_signal_connect(info->view, "row-activated",
		   G_CALLBACK(gnc_gen_trans_row_activated_cb), info);
  g_signal_connect(selection, "changed",
		   G_CALLBACK(gnc_gen_trans_row_changed_cb), info);
}

GNCImportMainMatcher *gnc_gen_trans_list_new (GtkWidget *parent, 
					      const gchar* heading,
					      gboolean all_from_same_account,
					      gint match_date_hardlimit)
{
  GNCImportMainMatcher *info;
  GladeXML *xml;
  GtkWidget *heading_label;
  gboolean show_edit;
  
  info = g_new0 (GNCImportMainMatcher, 1);

  /* Initialize user Settings. */
  info->user_settings = gnc_import_Settings_new ();
  gnc_import_Settings_set_match_date_hardlimit (info->user_settings, match_date_hardlimit);

  /* Initialize the GtkDialog. */
  xml = gnc_glade_xml_new ("generic-import.glade", "transaction_matcher");

  info->dialog = glade_xml_get_widget (xml, "transaction_matcher");
  g_assert (info->dialog != NULL);
  info->view = GTK_TREE_VIEW(glade_xml_get_widget (xml, "downloaded_view"));
  g_assert (info->view != NULL);

  show_edit = gnc_import_Settings_get_action_edit_enabled (info->user_settings);
  gnc_gen_trans_init_view(info, all_from_same_account, show_edit);
  heading_label = glade_xml_get_widget (xml, "heading_label");
  g_assert (heading_label != NULL);

  /* if (parent)
    gtk_window_set_transient_for (GTK_WINDOW (info->dialog), 
				  GTK_WINDOW (parent));*/

  /* Connect signals */
  glade_xml_signal_connect_data(xml, "on_matcher_ok_clicked", 
				G_CALLBACK(on_matcher_ok_clicked),
				info);
  glade_xml_signal_connect_data(xml, "on_matcher_cancel_clicked", 
				G_CALLBACK(on_matcher_cancel_clicked),
				info);
  glade_xml_signal_connect_data(xml, "on_matcher_help_clicked", 
				G_CALLBACK(on_matcher_help_clicked),
				info);

 /*Initialise the colors */
  gdk_color_parse(COLOR_RED,    &info->color_back_red);
  gdk_color_parse(COLOR_YELLOW, &info->color_back_yellow);
  gdk_color_parse(COLOR_GREEN,  &info->color_back_green);

  if (heading)
    gtk_label_set_text (GTK_LABEL (heading_label), heading);
  
  gnc_restore_window_size(GCONF_SECTION, GTK_WINDOW(info->dialog));
  gtk_widget_show_all (GTK_WIDGET (info->dialog));
  return info;
}

gboolean gnc_gen_trans_list_run (GNCImportMainMatcher *info) 
{
  gboolean result;
  
  /* DEBUG("Begin"); */

  result = gtk_dialog_run (GTK_DIALOG (info->dialog));

  /* DEBUG("Result was %d", result); */

  /* No destroying here since the dialog was already destroyed through
     the ok_clicked handlers. */
  return result;
}


static void
refresh_model_row (GNCImportMainMatcher *gui,
		   GtkTreeModel *model,
		   GtkTreeIter *iter,
		   GNCImportTransInfo *info)
{
  GtkListStore *store;
  GtkTreeSelection *selection;
  gchar *tmp,*imbalance,*text,*color;
  const gchar *ro_text;
  g_assert (gui);
  g_assert (model);
  g_assert (info);
  /*DEBUG("Begin");*/
  
  store = GTK_LIST_STORE(model);
  gtk_list_store_set(store, iter, DOWNLOADED_COL_DATA, info, -1);

  /*Account:*/
  ro_text =
    xaccAccountGetName(xaccSplitGetAccount(gnc_import_TransInfo_get_fsplit (info)));
  gtk_list_store_set(store, iter, DOWNLOADED_COL_ACCOUNT, ro_text, -1);

  /*Date*/

  text =
    qof_print_date ( xaccTransGetDate( gnc_import_TransInfo_get_trans(info) ) );
  gtk_list_store_set(store, iter, DOWNLOADED_COL_DATE, text, -1);
  g_free(text);
  
  /*Amount*/
  ro_text = xaccPrintAmount 
	     (xaccSplitGetAmount (gnc_import_TransInfo_get_fsplit(info) ), 
	      gnc_split_amount_print_info(gnc_import_TransInfo_get_fsplit(info), TRUE) 
	      );
  gtk_list_store_set(store, iter, DOWNLOADED_COL_AMOUNT, ro_text, -1);
  
  /*Description*/
  ro_text = xaccTransGetDescription(gnc_import_TransInfo_get_trans(info) );
  gtk_list_store_set(store, iter, DOWNLOADED_COL_DESCRIPTION, ro_text, -1);

  /*Memo*/
  ro_text = xaccSplitGetMemo(gnc_import_TransInfo_get_fsplit(info) );
  gtk_list_store_set(store, iter, DOWNLOADED_COL_MEMO, ro_text, -1);
  
  /*Actions*/
  
  /* Action informations */
  ro_text = text = color = NULL;
  switch(gnc_import_TransInfo_get_action(info))
    {
    case GNCImport_ADD:
      if(gnc_import_TransInfo_is_balanced(info)==TRUE)
	{
	  ro_text = _("New, already balanced");
	  color = COLOR_GREEN;
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
	      color = COLOR_GREEN;
	      tmp = gnc_account_get_full_name 
		(gnc_import_TransInfo_get_destacc (info));
	      if(gnc_import_TransInfo_get_destacc_selected_manually(info)
		 == TRUE)
		{
		  text = 
		    /* Translators: %1$s is the amount to be 
		       transferred. %2$s is the destination account. */
		    g_strdup_printf(_("New, transfer %s to (manual) \"%s\""),
				    imbalance, tmp);
		}
	      else
		{
		  text = 
		    /* Translators: %1$s is the amount to be 
		       transferred. %2$s is the destination account. */
		    g_strdup_printf(_("New, transfer %s to (auto) \"%s\""),
				    imbalance,tmp);
		}
	      g_free (tmp);

	    }
	  else
	    {
	      color = COLOR_YELLOW;
	      text = 
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
	  color = COLOR_GREEN;
	  if(gnc_import_TransInfo_get_match_selected_manually(info)==TRUE)
	    {
	      ro_text = _("Reconcile (manual) match");
	    }
	  else
	    {
	      ro_text = _("Reconcile (auto) match");
	    }
	}
      else
	{
	  color = COLOR_RED;
	  ro_text = _("Match missing!");
	}
      break;
    case GNCImport_EDIT: 
      color = "white";
      ro_text = "NOT SUPPORTED YET!";
      break;
    case GNCImport_SKIP: 
      color = COLOR_RED;
      ro_text = _("Do not import (no action selected)");
      break;
    default:
      color = "white";
      ro_text = "WRITEME, this is an unknown action";
    }

  gtk_list_store_set(store, iter,
		     DOWNLOADED_COL_COLOR, color,
		     DOWNLOADED_COL_ACTION_INFO, ro_text ? ro_text : text,
		     -1);
  if (text)
    g_free(text);

  /* Set the pixmaps */
  gtk_list_store_set(store, iter,
		     DOWNLOADED_COL_ACTION_ADD,
		     gnc_import_TransInfo_get_action(info)==GNCImport_ADD,
		     -1);
  if(gnc_import_TransInfo_get_action(info)==GNCImport_SKIP)
    {      
      /*Show the best match's confidence pixmap in the info column*/
      gtk_list_store_set(store, iter,
			 DOWNLOADED_COL_ACTION_PIXBUF,
			 gen_probability_pixbuf( gnc_import_MatchInfo_get_probability 
						     ( gnc_import_TransInfo_get_selected_match (info)),
						     gui->user_settings, 
						     GTK_WIDGET(gui->view)),
			 -1);
    }
  
  gtk_list_store_set(store, iter,
		     DOWNLOADED_COL_ACTION_CLEAR,
		     gnc_import_TransInfo_get_action(info)==GNCImport_CLEAR,
		     -1);
  if(gnc_import_TransInfo_get_action(info)==GNCImport_CLEAR)
    {
      /*Show the best match's confidence pixmap in the info column*/
      gtk_list_store_set(store, iter,
			 DOWNLOADED_COL_ACTION_PIXBUF,
			 gen_probability_pixbuf( gnc_import_MatchInfo_get_probability 
						     ( gnc_import_TransInfo_get_selected_match (info)),
						     gui->user_settings, 
						     GTK_WIDGET(gui->view)),
			 -1);
    }
  
  gtk_list_store_set(store, iter,
		     DOWNLOADED_COL_ACTION_EDIT,
		     gnc_import_TransInfo_get_action(info)==GNCImport_EDIT,
		     -1);

  selection = gtk_tree_view_get_selection(gui->view);
  gtk_tree_selection_unselect_all(selection);
}


void gnc_gen_trans_list_add_trans(GNCImportMainMatcher *gui, Transaction *trans)
{
  GNCImportTransInfo * transaction_info = NULL;
  GtkTreeModel *model;
  GtkTreeIter iter;
  g_assert (gui);
  g_assert (trans);
  

  if (gnc_import_exists_online_id (trans))
    return;
  else
    {
      transaction_info = gnc_import_TransInfo_new(trans, NULL);
      
      gnc_import_TransInfo_init_matches (transaction_info, 
					 gui->user_settings);

      model = gtk_tree_view_get_model(gui->view);
      gtk_list_store_append(GTK_LIST_STORE(model), &iter);
      refresh_model_row (gui, model, &iter, transaction_info);
    }
  return;
}/* end gnc_import_add_trans() */

/* Iterate through the rows of the clist and try to automatch each of them */
static void
automatch_store_transactions (GNCImportMainMatcher *info,
			      GtkTreeModel *model,
			      GtkTreeIter *iter,
			      GNCImportTransInfo *trans_info)
{
      /* returns TRUE if we changed this row, so update it */
      if(gnc_import_TransInfo_refresh_destacc(trans_info, NULL))
	{
	  refresh_model_row(info, model, iter, trans_info);
	}
}

/** @} */
