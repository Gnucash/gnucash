/********************************************************************\
 * druid-qif-import.c -- window for importing QIF files             *
 *                        (GnuCash)                                 *
 * Copyright (C) 2000 Bill Gribble <grib@billgribble.com>           *
 * Copyright (c) 2006 David Hampton <hampton@employees.org>         *
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
#include <glib/gstdio.h>
#ifndef HAVE_GLIB_2_8
#include <gstdio-2.8.h>
#endif
#include <libguile.h>
#include <sys/time.h>
#include <unistd.h>

#include "Account.h"
#include "Transaction.h"
#include "dialog-account-picker.h"
#include "dialog-commodity.h"
#include "dialog-utils.h"
#include "druid-qif-import.h"
#include "druid-utils.h"
#include "gnc-component-manager.h"
#include "qof.h"
#include "gnc-file.h"
#include "gnc-gui-query.h"
#include "gnc-ui-util.h"
#include "gnc-gconf-utils.h"
#include "gnc-gtk-utils.h"
#include "gnc-ui.h"
#include "guile-mappings.h"

#include "swig-runtime.h"

#define DRUID_QIF_IMPORT_CM_CLASS "druid-qif-import"
#define GCONF_SECTION "dialogs/import/qif"
#define GCONF_NAME_SHOW_DOC "show_doc"

#define PREV_ROW "prev_row"

enum filename_cols {
  FILENAME_COL_INDEX = 0,
  FILENAME_COL_NAME,
  NUM_FILENAME_COLS
};

enum account_cols {
  ACCOUNT_COL_INDEX = 0,
  ACCOUNT_COL_QIF_NAME,
  ACCOUNT_COL_GNC_NAME,
  ACCOUNT_COL_NEW,
  NUM_ACCOUNT_COLS
};

enum qif_trans_cols {
  QIF_TRANS_COL_INDEX = 0,
  QIF_TRANS_COL_DATE,
  QIF_TRANS_COL_DESCRIPTION,
  QIF_TRANS_COL_AMOUNT,
  QIF_TRANS_COL_CHECKED,
  NUM_QIF_TRANS_COLS
};

struct _qifimportwindow {
  GtkWidget * window;
  GtkWidget * druid;
  GtkWidget * filename_entry;
  GtkWidget * acct_entry;
  GtkWidget * date_format_combo;
  GtkWidget * selected_file_view;
  GtkWidget * acct_view;
  GtkWidget * cat_view;
  GtkWidget * memo_view;
  GtkWidget * currency_picker;
  GtkWidget * new_transaction_view;
  GtkWidget * old_transaction_view;
  
  GList     * pre_comm_pages;
  GList     * commodity_pages;
  GList     * post_comm_pages;
  GList     * doc_pages;

  gboolean  show_doc_pages;

  SCM       imported_files;
  SCM       selected_file;

  SCM       acct_map_info; 
  SCM       acct_display_info;

  SCM       cat_map_info;
  SCM       cat_display_info;

  SCM       memo_map_info;
  SCM       memo_display_info;

  SCM       gnc_acct_info;
  SCM       stock_hash;
  SCM       new_stocks;
  SCM       ticker_map;

  SCM       imported_account_tree;
  SCM       match_transactions;
  int       selected_transaction;
};

struct _qifdruidpage {
  GtkWidget * page;
  GtkWidget * new_type_combo;
  GtkWidget * new_name_entry;
  GtkWidget * new_mnemonic_entry;
  gnc_commodity * commodity;
};  

typedef struct _qifdruidpage QIFDruidPage;
static QIFDruidPage * make_qif_druid_page(gnc_commodity * comm);

static void update_file_page(QIFImportWindow * win);
static void update_accounts_page(QIFImportWindow * win);
static void update_categories_page(QIFImportWindow * win);
static void update_memo_page(QIFImportWindow * win);

static void update_account_picker_page(QIFImportWindow * wind,
				       SCM make_display, GtkWidget *view,
				       SCM map_info, SCM * display_info);

static void gnc_ui_qif_import_commodity_prepare_cb(GnomeDruidPage * page,
                                                   gpointer arg1,
                                                   gpointer user_data);

static GdkColor std_bg_color = { 0, 39835, 49087, 40092 };
static GdkColor std_logo_bg_color = { 0, 65535, 65535, 65535 };
static GdkColor std_title_color =  { 0, 65535, 65535, 65535 };

#define NUM_PRE_PAGES 13
#define NUM_POST_PAGES 3
#define NUM_DOC_PAGES  6

static GnomeDruidPage *
get_named_page(QIFImportWindow * w, const char * name)
{
  return GNOME_DRUID_PAGE(gnc_glade_lookup_widget(w->window, name));
}


/********************************************************************\
 * gnc_ui_qif_import_druid_destroy
 * close the QIF Import druid window
\********************************************************************/

void
gnc_ui_qif_import_druid_destroy (QIFImportWindow * window)
{
  if (!window)
    return;

  /* FIXME -- commodity pages */

  gnc_unregister_gui_component_by_data(DRUID_QIF_IMPORT_CM_CLASS, window);

  gtk_widget_destroy(window->window);

  scm_gc_unprotect_object(window->imported_files);
  scm_gc_unprotect_object(window->selected_file);
  scm_gc_unprotect_object(window->gnc_acct_info);
  scm_gc_unprotect_object(window->cat_display_info);
  scm_gc_unprotect_object(window->cat_map_info);
  scm_gc_unprotect_object(window->memo_display_info);
  scm_gc_unprotect_object(window->memo_map_info);
  scm_gc_unprotect_object(window->acct_display_info);
  scm_gc_unprotect_object(window->acct_map_info);
  scm_gc_unprotect_object(window->stock_hash);
  scm_gc_unprotect_object(window->new_stocks);
  scm_gc_unprotect_object(window->ticker_map);
  scm_gc_unprotect_object(window->imported_account_tree);
  scm_gc_unprotect_object(window->match_transactions);

  g_free(window);
}


/********************************************************************\
 * get_next_druid_page
 *
 * Determine which page to show after the current page. If NULL is
 * ever returned, then a bug has been detected! (We really ought to
 * do a better job of notifying someone.)
\********************************************************************/

static GtkWidget * 
get_next_druid_page(QIFImportWindow * wind, GnomeDruidPage * page)
{
  GList     * current = NULL;
  GList     * next;
  int       where = 0;
  
  /* Figure out which stage of the druid we're in. */
  if((current = g_list_find(wind->pre_comm_pages, page)) == NULL) {
    if((current = g_list_find(wind->commodity_pages, page)) == NULL) {
      if((current = g_list_find(wind->post_comm_pages, page)) == NULL) {
        /* Where are we? */
        g_critical("QIF import: BUG DETECTED in get_next_druid_page! I'm lost!");
        return FALSE;
      }
      else {
        where = 3;
      }
    }
    else {
      where = 2;
    }
  }
  else {
    where = 1;
  }
  
  next = current->next;
  while (!next ||
         (!wind->show_doc_pages && g_list_find(wind->doc_pages, next->data)) ||
         (wind->new_stocks == SCM_BOOL_F &&
          GNOME_DRUID_PAGE(next->data) == get_named_page(wind, "commodity_doc_page"))) {
    if(next && next->next) {
      next = next->next;
    }
    else {
      where ++;
      switch(where) {
      case 2:
        next = wind->commodity_pages;
        break;
      case 3:
        next = wind->post_comm_pages;
        break;
      default:
        g_critical("QIF import: BUG DETECTED in get_next_druid_page!");
        next = NULL;
        if (where > 3)
          return NULL;
        break;
      }              
    }
  }

  if(next) return (GtkWidget *)next->data;
  else return NULL;
}


/********************************************************************
 * get_prev_druid_page
 *
 * Determine which page was shown before the current page. If NULL
 * is ever returned, then a bug has been detected! (We really ought
 * to do a better job of notifying someone.)
 ********************************************************************/

static GtkWidget * 
get_prev_druid_page(QIFImportWindow * wind, GnomeDruidPage * page)
{
  GList     * current = NULL;
  GList     * prev;
  int       where = 0;
  
  /* Figure out which stage of the druid we're in. */
  if((current = g_list_find(wind->pre_comm_pages, page)) == NULL) {
    if((current = g_list_find(wind->commodity_pages, page)) == NULL) {
      if((current = g_list_find(wind->post_comm_pages, page)) == NULL) {
        /* Where are we? */
        g_critical("QIF import: BUG DETECTED in get_prev_druid_page! I'm lost!");
        return NULL;
      }
      else {
        where = 3;
      }
    }
    else {
      where = 2;
    }
  }
  else {
    where = 1;
  }
  
  /* If no duplicates were found, skip all post-conversion pages. */
  if (where == 3 && SCM_NULLP(wind->match_transactions))
    prev = NULL;
  else
    prev = current->prev;

  /* Keep going back through the sets of available pages as long as:
   * (a) there are no remaining pages in this set, or
   * (b) the page is a doc page and we're not supposed to show them, or
   * (c) the page is commodity related and the are no new commodities. */
  while (!prev ||
         (!wind->show_doc_pages && g_list_find(wind->doc_pages, prev->data)) ||
         (wind->new_stocks == SCM_BOOL_F &&
          GNOME_DRUID_PAGE(prev->data) == get_named_page(wind, "commodity_doc_page"))) {
    /* We're either out of pages for this stage, or we've reached
     * an optional doc page that shouldn't be shown. */

    if(prev && prev->prev) {
      /* Go back another page within the same stage. */
      prev = prev->prev;
    }
    else {
      /* Start looking at the end of the previous stage. */
      where --;
      switch(where) {
      case 1:
        prev = g_list_last(wind->pre_comm_pages);
        break;
      case 2:
        if(wind->new_stocks != SCM_BOOL_F) {
          prev = g_list_last(wind->commodity_pages);
        }
        else {
           prev = g_list_last(wind->pre_comm_pages);
        }
        break;
      default:
        if (wind->show_doc_pages)
          g_critical("QIF import: BUG DETECTED in get_prev_druid_page!");
        prev = NULL;
        if (where < 1)
          return NULL;
        break;
      }              
    }
  }

  if(prev)
    return (GtkWidget *)prev->data;
  else 
    return NULL;
}


/********************************************************************
 * gnc_ui_qif_import_generic_next_cb
 * close the QIF Import druid window
 ********************************************************************/

static gboolean
gnc_ui_qif_import_generic_next_cb(GnomeDruidPage * page, gpointer arg1, 
                                  gpointer user_data)
{
  QIFImportWindow * wind = user_data;
  GtkWidget * next_page = get_next_druid_page(wind, page);
  
  if(next_page) {
    gnome_druid_set_page(GNOME_DRUID(wind->druid),
                         GNOME_DRUID_PAGE(next_page));
    
    return TRUE;
  }
  else {
    return FALSE;
  }
}

/********************************************************************
 * gnc_ui_qif_import_generic_back_cb
 * close the QIF Import druid window
 ********************************************************************/

static gboolean
gnc_ui_qif_import_generic_back_cb(GnomeDruidPage * page, gpointer arg1, 
                                  gpointer user_data)
{
  QIFImportWindow * wind = user_data;
  GtkWidget * back_page = get_prev_druid_page(wind, page);
  
  if(back_page) {
    gnome_druid_set_page(GNOME_DRUID(wind->druid),
                         GNOME_DRUID_PAGE(back_page));
    return TRUE;
  }
  else {
    return FALSE;
  }
}


/********************************************************************
 * gnc_ui_qif_import_select_file_cb
 * invoked when the "select file" button is clicked
 * this is just to pick a file name and reset-to-defaults all the 
 * fields describing how to parse the file.
 ********************************************************************/

static void
gnc_ui_qif_import_select_file_cb(GtkButton * button,
                                 gpointer user_data)
{
  QIFImportWindow * wind = user_data;
  GtkFileFilter *filter;
  char * new_file_name;
  char *file_name, *default_dir;

  /* Default to whatever's already present */
  default_dir = gnc_get_default_directory(GCONF_SECTION);

  filter = gtk_file_filter_new ();
  gtk_file_filter_set_name (filter, "*.qif");
  gtk_file_filter_add_pattern (filter, "*.[Qq][Ii][Ff]");
  new_file_name = gnc_file_dialog (_("Select QIF File"),
				   g_list_prepend (NULL, filter),
				   default_dir,
				   GNC_FILE_DIALOG_IMPORT);

  /* Insure valid data, and something that can be freed. */
  if (new_file_name == NULL) {
    file_name = g_strdup(default_dir);
  } else if (!g_path_is_absolute(new_file_name)) {
    file_name = g_build_filename(default_dir, new_file_name, NULL);
    g_free(new_file_name);
  } else {
    file_name = new_file_name;
    /* Update the working directory */
    g_free(default_dir);
    default_dir = g_path_get_dirname(file_name);
    gnc_set_default_directory(GCONF_SECTION, default_dir);
  }
  g_free(default_dir);

  /* set the filename entry for what was selected */
  gtk_entry_set_text(GTK_ENTRY(wind->filename_entry), file_name);
  g_free(file_name);
}


/********************************************************************
 * gnc_ui_qif_import_load_file_back_cb
 * 
 * Invoked when the "back" button is clicked on the load file page.
 ********************************************************************/

static gboolean
gnc_ui_qif_import_load_file_back_cb(GnomeDruidPage * page, gpointer arg1, 
				    gpointer user_data)
{
  QIFImportWindow * wind = user_data;

  if (SCM_LISTP(wind->imported_files) &&
      (scm_ilength(wind->imported_files) > 0)) {
    gnome_druid_set_page(GNOME_DRUID(wind->druid), 
			 get_named_page(wind, "loaded_files_page"));
    return TRUE;
  }

  gnome_druid_set_page(GNOME_DRUID(wind->druid), 
		       get_named_page(wind, "start_page"));
  return TRUE;
}


/********************************************************************
 * gnc_ui_qif_import_load_file_next_cb
 * 
 * Invoked when the "next" button is clicked on the load file page.
 ********************************************************************/

static gboolean
gnc_ui_qif_import_load_file_next_cb(GnomeDruidPage * page, 
                                    gpointer arg1,
                                    gpointer user_data)
{
  QIFImportWindow * wind = user_data;

  const char * path_to_load;
  const gchar * default_acctname = NULL;

  SCM make_qif_file   = scm_c_eval_string("make-qif-file");
  SCM qif_file_load   = scm_c_eval_string("qif-file:read-file");
  SCM qif_file_parse  = scm_c_eval_string("qif-file:parse-fields");
  SCM qif_file_loaded = scm_c_eval_string("qif-dialog:qif-file-loaded?");
  SCM unload_qif_file = scm_c_eval_string("qif-dialog:unload-qif-file");
  SCM check_from_acct = scm_c_eval_string("qif-file:check-from-acct");
  SCM default_acct    = scm_c_eval_string("qif-file:path-to-accountname");
  SCM qif_file_parse_results  = scm_c_eval_string("qif-file:parse-fields-results");
  SCM date_formats;
  SCM scm_filename;
  SCM scm_qiffile;
  SCM imported_files = SCM_EOL;
  SCM load_return, parse_return;
  SCM window;
  int ask_date_format = FALSE;

  /* get the file name */ 
  path_to_load = gtk_entry_get_text(GTK_ENTRY(wind->filename_entry));
  window = SWIG_NewPointerObj(wind->window, SWIG_TypeQuery("_p_GtkWidget"), 0);

  /* check a few error conditions before we get started */
  if(strlen(path_to_load) == 0) {
    /* stay here if no file specified */
    gnc_error_dialog(wind->window, _("Please select a file to load."));
    return TRUE;
  }
  else if (g_access(path_to_load, R_OK) < 0) {
    /* stay here if bad file */
    gnc_error_dialog(wind->window, 
		     _("File not found or read permission denied. "
		       "Please select another file."));
    return TRUE;
  }
  else {
    /* convert filename to scm */
    scm_filename   = scm_makfrom0str(path_to_load);
    imported_files = wind->imported_files;
    
    if(scm_call_2(qif_file_loaded, scm_filename, wind->imported_files)
       == SCM_BOOL_T) {
      gnc_error_dialog(wind->window,
                                _("That QIF file is already loaded. "
                                  "Please select another file."));
      return TRUE;
    }
    
    /* turn on the busy cursor */
    gnc_set_busy_cursor(NULL, TRUE);
    
    /* create the <qif-file> object */
    scm_qiffile          = scm_call_0(make_qif_file);    
    imported_files       = scm_cons(scm_qiffile, imported_files);    

    scm_gc_unprotect_object(wind->selected_file);      
    wind->selected_file  = scm_qiffile;    
    scm_gc_protect_object(wind->selected_file);      
    
    /* load the file */
    load_return = scm_call_4(qif_file_load, SCM_CAR(imported_files),
			     scm_filename, wind->ticker_map, window);
    
    /* turn back the cursor */
    gnc_unset_busy_cursor(NULL);

    /* a list returned is (#f error-message) for an error, 
     * (#t error-message) for a warning, or just #f for an 
     * exception. */
    if(SCM_LISTP(load_return) &&
       (SCM_CAR(load_return) == SCM_BOOL_T)) {
      const gchar *warn_str = SCM_STRING_CHARS(SCM_CADR(load_return));
      gnc_warning_dialog(GTK_WIDGET(wind->window),
			 _("QIF file load warning: %s"),
			 warn_str ? warn_str : "(null)");
    }

    /* check success of the file load */
    if(load_return == SCM_BOOL_F) {
      gnc_error_dialog(wind->window, 
		       _( "An error occurred while loading the QIF file."));
      return TRUE;
    }
    else if ((load_return != SCM_BOOL_T) &&
             (!SCM_LISTP(load_return) || 
              (SCM_CAR(load_return) != SCM_BOOL_T))) {
      const gchar *warn_str = SCM_STRING_CHARS(SCM_CADR(load_return));
      gnc_error_dialog(wind->window,
		       _("QIF file load failed: %s"),
		       warn_str ? warn_str : "(null)");

      imported_files = 
        scm_call_2(unload_qif_file, scm_qiffile, imported_files);
            
      scm_gc_unprotect_object(wind->imported_files);
      wind->imported_files = imported_files;
      scm_gc_protect_object(wind->imported_files);

      return TRUE;
    }
    else {
      /* turn on the busy cursor */
      gnc_set_busy_cursor(NULL, TRUE);

      /* call the field parser */
      parse_return = scm_call_1(qif_file_parse, SCM_CAR(imported_files));
      
      /* parser returns:
       *   success:	#t
       *   failure:	(#f . ((type . errror) ...))
       *   warning:	(#t . ((type . error) ...))
       *
       * warning means that (potentially) the date format is
       * ambiguous.  So search the results for the "date" type and if
       * it's found, set up the format selector page.
       */
      if(SCM_LISTP(parse_return) && 
         (SCM_CAR(parse_return) == SCM_BOOL_T)) {
	gint n_items;

	/* clear the combo box */
	gtk_combo_box_set_active(GTK_COMBO_BOX(wind->date_format_combo), -1);
	n_items = gtk_tree_model_iter_n_children(
	  gtk_combo_box_get_model(GTK_COMBO_BOX(wind->date_format_combo)), NULL);
	while (n_items-- > 0)
	  gtk_combo_box_remove_text(GTK_COMBO_BOX(wind->date_format_combo), 0);

	if ((date_formats = scm_call_2(qif_file_parse_results,
				       SCM_CDR(parse_return),
				       scm_str2symbol("date"))) != SCM_BOOL_F) {
	  while(SCM_LISTP(date_formats) && !SCM_NULLP(date_formats)) {
	    gtk_combo_box_append_text(GTK_COMBO_BOX(wind->date_format_combo),
				      SCM_SYMBOL_CHARS(SCM_CAR(date_formats)));
	    date_formats = SCM_CDR(date_formats);
	  }
	gtk_combo_box_set_active(GTK_COMBO_BOX(wind->date_format_combo), 0);
        
	  ask_date_format = TRUE;

	} else {
	  /* FIXME: we've got a "warning" but it's not the date! */
	  ;
	}
      }

      /* turn back the cursor */
      gnc_unset_busy_cursor(NULL);

      /* Can this ever happen??? */
      if(parse_return == SCM_BOOL_F) {
        gnc_error_dialog(wind->window,
			 _("An error occurred while parsing the QIF file."));
        imported_files = 
          scm_call_2(unload_qif_file, scm_qiffile, imported_files);
        return TRUE;
      }
      else if((parse_return != SCM_BOOL_T) &&
         (!SCM_LISTP(parse_return) ||
          (SCM_CAR(parse_return) != SCM_BOOL_T))) {
        const gchar *warn_str = SCM_STRING_CHARS(SCM_CDADR(parse_return));
        gnc_error_dialog(wind->window,
			 _("QIF file parse failed: %s"),
			 warn_str ? warn_str : "(null)");

        imported_files = 
          scm_call_2(unload_qif_file, scm_qiffile, imported_files);
        
        return TRUE;
      } 
    }
    
    scm_gc_unprotect_object(wind->imported_files);
    wind->imported_files = imported_files;
    scm_gc_protect_object(wind->imported_files);
    
    if(ask_date_format) {
      /* we need to get a date format, so go to the next page */
      return gnc_ui_qif_import_generic_next_cb(page, arg1, wind);
    }
    else if(scm_call_1(check_from_acct, SCM_CAR(imported_files)) != SCM_BOOL_T) {
      /* skip to the "ask account name" page */
      default_acctname =
	SCM_STRING_CHARS(scm_call_1(default_acct, SCM_CAR(imported_files)));
      gtk_entry_set_text(GTK_ENTRY(wind->acct_entry), default_acctname);
      
      gnome_druid_set_page(GNOME_DRUID(wind->druid),
                           get_named_page(wind, "account_name_page"));
      return TRUE;
    }
    else {
      /* skip ahead to the "loaded files" page */
      gnome_druid_set_page(GNOME_DRUID(wind->druid), 
                           get_named_page(wind, "loaded_files_page"));
      return TRUE;      
    }
  }
  
  return FALSE;
}

static gboolean
gnc_ui_qif_import_date_format_next_cb(GnomeDruidPage * page, 
                                      gpointer arg1,
                                      gpointer user_data)
{
  QIFImportWindow * wind = user_data;

  SCM  reparse_dates   = scm_c_eval_string("qif-file:reparse-dates");
  SCM  check_from_acct = scm_c_eval_string("qif-file:check-from-acct");
  SCM  format_sym;
  gchar *text;
  
  text = gtk_combo_box_get_active_text(GTK_COMBO_BOX(wind->date_format_combo));
  format_sym = scm_str2symbol(text);
  g_free(text);
  scm_call_2(reparse_dates, wind->selected_file, format_sym);
  
  if(scm_call_1(check_from_acct, wind->selected_file) != SCM_BOOL_T) {
    SCM default_acct    = scm_c_eval_string("qif-file:path-to-accountname");
    const gchar * default_acctname;

    default_acctname = SCM_STRING_CHARS(scm_call_1(default_acct,
						wind->selected_file));
    gtk_entry_set_text(GTK_ENTRY(wind->acct_entry), default_acctname);

    return FALSE;
  }
  else {
    /* skip ahead to the "loaded files" page */
    gnome_druid_set_page(GNOME_DRUID(wind->druid), 
                         get_named_page(wind, "loaded_files_page"));
    
    return TRUE;      
  }
}


/****************************************************************
 * gnc_ui_qif_import_select_loaded_file_cb
 * callback when a file is clicked in the "loaded files" page
 ****************************************************************/

static void
gnc_ui_qif_import_select_loaded_file_cb (GtkTreeSelection *selection,
					 gpointer          user_data)   
{
  QIFImportWindow * wind = user_data;
  GtkTreeModel *model;
  GtkTreeIter iter;
  gint row;
  GtkWidget *button;

  button = gnc_glade_lookup_widget(wind->window, "unload_file_button");
  if (gtk_tree_selection_get_selected(selection, &model, &iter)) {
    gtk_tree_model_get(model, &iter, FILENAME_COL_INDEX, &row, -1);
    if(SCM_LISTP(wind->imported_files) && 
       (scm_ilength(wind->imported_files) > row)) {
      scm_gc_unprotect_object(wind->selected_file);
      wind->selected_file = scm_list_ref(wind->imported_files,
					 scm_int2num(row));   
      scm_gc_protect_object(wind->selected_file);
      g_object_set(button, "sensitive", TRUE, (gchar*)NULL);
      gnome_druid_set_buttons_sensitive(GNOME_DRUID(wind->druid),
                                        TRUE, TRUE, TRUE, TRUE);
    } 
  } else {
    scm_gc_unprotect_object(wind->selected_file);
    wind->selected_file = SCM_BOOL_F;
    scm_gc_protect_object(wind->selected_file);
      g_object_set(button, "sensitive", FALSE, (gchar*)NULL);
    gnome_druid_set_buttons_sensitive(GNOME_DRUID(wind->druid),
                                      FALSE, TRUE, TRUE, TRUE);
  }
}

/********************************************************************
 * gnc_ui_qif_import_loaded_files_prepare_cb
 * 
 * Get the loaded files page ready for viewing
 ********************************************************************/

static void
gnc_ui_qif_import_loaded_files_prepare_cb(GnomeDruidPage * page,
                                          gpointer arg1,
                                          gpointer user_data)
{
  QIFImportWindow * wind = user_data;

  gnome_druid_set_buttons_sensitive(GNOME_DRUID(wind->druid),
                                    FALSE, TRUE, TRUE, TRUE);
  update_file_page(wind);
}


/********************************************************************
 * gnc_ui_qif_import_load_another_cb
 * Invoked when the "load another" button is clicked on the loaded
 * files page.
 ********************************************************************/

static void
gnc_ui_qif_import_load_another_cb(GtkButton * button,
                                  gpointer user_data)
{
  QIFImportWindow * wind = user_data;
  
  gnome_druid_set_page(GNOME_DRUID(wind->druid),
                       get_named_page(wind, "load_file_page"));
  gnome_druid_set_buttons_sensitive(GNOME_DRUID(wind->druid),
                                    TRUE, TRUE, TRUE, TRUE); 
}


/********************************************************************
 * gnc_ui_qif_import_unload_cb
 * Invoked when the "unload" button is clicked on the loaded files
 * page.
 ********************************************************************/

static void
gnc_ui_qif_import_unload_file_cb(GtkButton * button,
                                 gpointer user_data)
{
  QIFImportWindow * wind = user_data;

  SCM unload_qif_file = scm_c_eval_string("qif-dialog:unload-qif-file");
  SCM imported_files;
  
  if(wind->selected_file != SCM_BOOL_F) {
    imported_files = 
      scm_call_2(unload_qif_file, wind->selected_file, wind->imported_files);
  
    scm_gc_unprotect_object(wind->imported_files);
    wind->imported_files = imported_files;
    scm_gc_protect_object(wind->imported_files);

    scm_gc_unprotect_object(wind->selected_file);
    wind->selected_file = SCM_BOOL_F;
    scm_gc_protect_object(wind->selected_file);
     
    update_file_page(wind);
  }
}


/********************************************************************
 * update_file_page
 * update the list of loaded files 
 ********************************************************************/

static void
update_file_page(QIFImportWindow * wind)
{
  
  SCM       loaded_file_list = wind->imported_files;
  SCM       scm_qiffile = SCM_BOOL_F;
  SCM       qif_file_path;
  int       row = 0;
  char      * row_text;
  GtkTreeView *view;
  GtkListStore *store;
  GtkTreeIter iter;
  GtkTreePath *path;
  GtkTreeRowReference *reference = NULL;

  /* clear the list */
  view = GTK_TREE_VIEW(wind->selected_file_view);
  store = GTK_LIST_STORE(gtk_tree_view_get_model(view));
  gtk_list_store_clear(store);
  qif_file_path = scm_c_eval_string("qif-file:path");
  
  while(!SCM_NULLP(loaded_file_list)) {  
    scm_qiffile = SCM_CAR(loaded_file_list);
    row_text    = SCM_STRING_CHARS(scm_call_1(qif_file_path, scm_qiffile));

    gtk_list_store_append(store, &iter);
    gtk_list_store_set(store, &iter,
		       FILENAME_COL_INDEX, row++,
		       FILENAME_COL_NAME, row_text,
		       -1);
    if(scm_qiffile == wind->selected_file) {
      path = gtk_tree_model_get_path(GTK_TREE_MODEL(store), &iter);
      reference = gtk_tree_row_reference_new(GTK_TREE_MODEL(store), path);
      gtk_tree_path_free(path);
    }

    loaded_file_list = SCM_CDR(loaded_file_list);
  }

  if (reference) {
    GtkTreeSelection* selection = gtk_tree_view_get_selection(view);
    path = gtk_tree_row_reference_get_path(reference);
    if (path) {
      gtk_tree_selection_select_path(selection, path);
      gtk_tree_path_free(path);
    }
    gtk_tree_row_reference_free(reference);
  }
}


/********************************************************************
 * gnc_ui_qif_import_default_acct_next_cb
 * 
 * Invoked when the "next" button is clicked on the default acct page.
 ********************************************************************/

static gboolean
gnc_ui_qif_import_default_acct_next_cb(GnomeDruidPage * page,
                                       gpointer arg1,
                                       gpointer user_data)
{
  QIFImportWindow * wind = user_data;
  const char   * acct_name = gtk_entry_get_text(GTK_ENTRY(wind->acct_entry));
  SCM    fix_default = scm_c_eval_string("qif-import:fix-from-acct");
  SCM    scm_name;

  g_return_val_if_fail(wind->selected_file != SCM_BOOL_F, FALSE);
  if(!acct_name || acct_name[0] == 0) {
    gnc_warning_dialog(wind->window, _("You must enter an account name."));
    return TRUE;
  }
  else {
    scm_name = scm_makfrom0str(acct_name);
    scm_call_2(fix_default, wind->selected_file, scm_name);
    return FALSE;
  }
}

/********************************************************************
 * gnc_ui_qif_import_default_acct_back_cb
 * 
 * Invoked when the "back" button is clicked on the default acct page.
 * this unloads the current file.  
 ********************************************************************/

static gboolean
gnc_ui_qif_import_default_acct_back_cb(GnomeDruidPage * page,
                                       gpointer arg1,
                                       gpointer user_data)
{
  QIFImportWindow * wind = user_data;
  SCM unload = scm_c_eval_string("qif-dialog:unload-qif-file");
  SCM files_list;

  files_list = scm_call_2(unload, wind->selected_file, wind->imported_files);

  scm_gc_unprotect_object(wind->imported_files);
  wind->imported_files = files_list;
  scm_gc_protect_object(wind->imported_files);
  
  scm_gc_unprotect_object(wind->selected_file);
  wind->selected_file = SCM_BOOL_F;
  scm_gc_protect_object(wind->selected_file);
  
  gnome_druid_set_page(GNOME_DRUID(wind->druid),
                       get_named_page(wind, "load_file_page"));
  gnome_druid_set_buttons_sensitive(GNOME_DRUID(wind->druid),
                                    TRUE, TRUE, TRUE, TRUE); 
  return TRUE;
}


/****************************************************************
 * update_account_picker_page 
 * Generic function to update an account_picker page.  This
 * generalizes the code shared whenever any QIF -> GNC mapper is
 * updating it's LIST STORE.  It asks the Scheme side to guess some account
 * translations and then shows the account name and suggested
 * translation in the Accounts page view (acount picker list).
 ****************************************************************/

static void
update_account_picker_page(QIFImportWindow * wind, SCM make_display,
			   GtkWidget *view, SCM map_info, SCM * display_info)
{

  SCM  get_qif_name = scm_c_eval_string("qif-map-entry:qif-name");
  SCM  get_gnc_name = scm_c_eval_string("qif-map-entry:gnc-name");
  SCM  get_new      = scm_c_eval_string("qif-map-entry:new-acct?");
  SCM  accts_left;
  const gchar *qif_name, *gnc_name;
  gboolean checked;
  gint row = 0;
  gint prev_row;
  GtkListStore *store;
  GtkTreeIter iter;
  GtkTreePath *path;
  GtkTreeSelection *selection;

  store = GTK_LIST_STORE(gtk_tree_view_get_model(GTK_TREE_VIEW(view)));

  /* now get the list of strings to display in the gtk_list_store widget */
  accts_left = scm_call_3(make_display,
			  wind->imported_files,
			  map_info, 
			  wind->gnc_acct_info);

  scm_gc_unprotect_object(*display_info);
  *display_info = accts_left;  
  scm_gc_protect_object(*display_info);
  
  /* clear the list */
  gtk_list_store_clear(store);

  while(!SCM_NULLP(accts_left)) {
    qif_name = SCM_STRING_CHARS(scm_call_1(get_qif_name, SCM_CAR(accts_left)));
    gnc_name = SCM_STRING_CHARS(scm_call_1(get_gnc_name, SCM_CAR(accts_left)));
    checked  = (scm_call_1(get_new, SCM_CAR(accts_left)) == SCM_BOOL_T);

    gtk_list_store_append(store, &iter);
    gtk_list_store_set(store, &iter,
		       ACCOUNT_COL_INDEX,    row++,
		       ACCOUNT_COL_QIF_NAME, qif_name,
		       ACCOUNT_COL_GNC_NAME, gnc_name,
		       ACCOUNT_COL_NEW,      checked,
		       -1);
    accts_left = SCM_CDR(accts_left);
  }

  /* move to the old selected row */
  prev_row = GPOINTER_TO_INT(g_object_get_data(G_OBJECT(store), PREV_ROW));
  if (prev_row != -1) {
    selection = gtk_tree_view_get_selection(GTK_TREE_VIEW(view));
    path = gtk_tree_path_new_from_indices (prev_row, -1);
    gtk_tree_selection_select_path(selection, path);
    gtk_tree_path_free(path);
  }
}


/****************************************************************
 * update_accounts_page 
 * update the QIF account -> GNC Account picker
 ****************************************************************/

static void
update_accounts_page(QIFImportWindow * wind)
{

  SCM  make_account_display = scm_c_eval_string("qif-dialog:make-account-display");

  update_account_picker_page (wind, make_account_display, wind->acct_view,
			      wind->acct_map_info, &(wind->acct_display_info));
}

/****************************************************************
 * update_categories_page 
 * update the QIF category -> GNC Account picker
 ****************************************************************/

static void
update_categories_page(QIFImportWindow * wind)
{
  SCM  make_category_display = scm_c_eval_string("qif-dialog:make-category-display");

  update_account_picker_page (wind, make_category_display, wind->cat_view,
			      wind->cat_map_info, &(wind->cat_display_info));
}

/****************************************************************
 * update_memo_page 
 * update the QIF memo -> GNC Account picker
 ****************************************************************/

static void
update_memo_page(QIFImportWindow * wind)
{
  SCM  make_memo_display = scm_c_eval_string("qif-dialog:make-memo-display");

  update_account_picker_page (wind, make_memo_display, wind->memo_view,
			      wind->memo_map_info, &(wind->memo_display_info));
}


/********************************************************************
 ********************************************************************/

static void
create_account_picker_view(GtkWidget *widget,
			   const gchar *col_name,
			   GCallback callback,
			   gpointer user_data)
{
  GtkTreeView *view = GTK_TREE_VIEW(widget);
  GtkListStore *store;
  GtkCellRenderer *renderer;
  GtkTreeViewColumn *column;

  store = gtk_list_store_new(NUM_ACCOUNT_COLS, G_TYPE_INT, G_TYPE_STRING,
			     G_TYPE_STRING, G_TYPE_BOOLEAN);
  gtk_tree_view_set_model(view, GTK_TREE_MODEL(store));
  g_object_unref(store);

  renderer = gtk_cell_renderer_text_new();
  column = gtk_tree_view_column_new_with_attributes(col_name, renderer,
						    "text", ACCOUNT_COL_QIF_NAME,
						    NULL);
  g_object_set(column, "expand", TRUE, NULL);
  gtk_tree_view_append_column(view, column);

  renderer = gtk_cell_renderer_text_new();
  column = gtk_tree_view_column_new_with_attributes(_("GnuCash account name"), renderer,
						    "text", ACCOUNT_COL_GNC_NAME,
						    NULL);
  g_object_set(column, "expand", TRUE, NULL);
  gtk_tree_view_append_column(view, column);

  renderer = gtk_cell_renderer_toggle_new();
  g_object_set(renderer, "activatable", FALSE, NULL);
  column = gtk_tree_view_column_new_with_attributes(_("New?"), renderer,
						    "active", ACCOUNT_COL_NEW,
						    NULL);
  gtk_tree_view_append_column(view, column);

  g_object_set_data(G_OBJECT(store), PREV_ROW, GINT_TO_POINTER(-1));
  g_signal_connect(view, "row-activated", G_CALLBACK(callback), user_data);
}

/********************************************************************
 * select_line
 * generic function to process the selection when a user tries to edit
 * an account mapping in one of the "map QIF * to GNC" pages.  This
 * calls out to the account picker, and then then updates the
 * appropriate data structures.  Finally, it will call the update_page
 * function.
 ********************************************************************/
static void
select_line (QIFImportWindow *wind, GtkTreeSelection *selection,
	     SCM display_info, SCM map_info,
	     void (*update_page)(QIFImportWindow *))
{
  SCM   get_name = scm_c_eval_string("qif-map-entry:qif-name");
  SCM   selected_acct;
  GtkTreeModel *model;
  GtkTreeIter iter;
  gint row;

  if (!gtk_tree_selection_get_selected (selection, &model, &iter))
    return;
  gtk_tree_model_get(model, &iter, ACCOUNT_COL_INDEX, &row, -1);
  g_object_set_data(G_OBJECT(model), PREV_ROW, GINT_TO_POINTER(row));
  if (row == -1)
    return;

  /* find the <qif-map-entry> corresponding to the selected row */
  selected_acct = scm_list_ref(display_info, scm_int2num(row));
  
  /* call the account picker to update it */
  selected_acct = qif_account_picker_dialog(wind, selected_acct);

  scm_hash_set_x(map_info, scm_call_1(get_name, selected_acct), selected_acct);

  /* update display */
  update_page(wind);
}

/********************************************************************
 * gnc_ui_qif_import_account_line_select_cb
 * when an account is clicked for editing in the "map QIF accts to GNC"
 * page.
 ********************************************************************/

static void
gnc_ui_qif_import_account_line_select_cb(GtkTreeView *view, GtkTreePath *path,
					 GtkTreeViewColumn *column,
					 gpointer user_data)
{
  QIFImportWindow *wind = user_data;
  GtkTreeSelection *selection;

  g_return_if_fail (view && wind);
  selection = gtk_tree_view_get_selection (view);

  select_line (wind, selection, wind->acct_display_info, wind->acct_map_info,
	       update_accounts_page);
}

/********************************************************************
 * gnc_ui_qif_import_category_line_select_cb
 * when a cat is clicked for editing in the "map QIF cats to GNC"
 * page.
 ********************************************************************/

static void
gnc_ui_qif_import_category_line_select_cb(GtkTreeView *view, GtkTreePath *path,
					 GtkTreeViewColumn *column,
					 gpointer user_data)
{
  QIFImportWindow *wind = user_data;
  GtkTreeSelection *selection;

  g_return_if_fail (view && wind);
  selection = gtk_tree_view_get_selection (view);

  select_line (wind, selection, wind->cat_display_info, wind->cat_map_info,
	       update_categories_page);
}

/********************************************************************
 *  gnc_ui_qif_import_memo_line_select_cb
 *  when a memo is clicked for editing in the "map QIF memos to GNC"
 *  page.
 ********************************************************************/

static void
gnc_ui_qif_import_memo_line_select_cb(GtkTreeView *view, GtkTreePath *path,
					 GtkTreeViewColumn *column,
					 gpointer user_data)
{
  QIFImportWindow *wind = user_data;
  GtkTreeSelection *selection;

  g_return_if_fail (view && wind);
  selection = gtk_tree_view_get_selection (view);

  select_line (wind, selection, wind->memo_display_info, wind->memo_map_info,
	       update_memo_page);
}


/********************************************************************
 * gnc_ui_qif_import_accounts_prepare_cb
 ********************************************************************/

static void
gnc_ui_qif_import_accounts_prepare_cb(GnomeDruidPage * page,
                                      gpointer arg1,
                                      gpointer user_data)
{
  QIFImportWindow * wind = user_data;

  gnc_set_busy_cursor(NULL, TRUE);
  update_accounts_page(wind);
  gnc_unset_busy_cursor(NULL);
}


/********************************************************************
 * gnc_ui_qif_import_categories_prepare_cb
 ********************************************************************/

static void
gnc_ui_qif_import_categories_prepare_cb(GnomeDruidPage * page,
                                        gpointer arg1,
                                        gpointer user_data)
{
  QIFImportWindow * wind = user_data;

  gnc_set_busy_cursor(NULL, TRUE);
  update_categories_page(wind);
  gnc_unset_busy_cursor(NULL);
}

/****************************************************************
 * gnc_ui_qif_import_categories_next_cb
 * Check to see if there are any payees and memos to show. If not
 * jump to currency page.
 ****************************************************************/
static gboolean
gnc_ui_qif_import_categories_next_cb(GnomeDruidPage * page,
                                     gpointer arg1,
                                     gpointer user_data)
{
  QIFImportWindow * wind = user_data;
  SCM  make_memo_display = scm_c_eval_string("qif-dialog:make-memo-display");
  SCM  accts_left;
  
  gnc_set_busy_cursor(NULL, TRUE);
  /*
   * Hack. Call make-memo-display to see if there are any memos to display.
   * This will get called again when we actually do make the memo display.
   */
  accts_left = scm_call_3(make_memo_display,
			  wind->imported_files,
			  wind->memo_map_info,
			  wind->gnc_acct_info);
  
  gnc_unset_busy_cursor(NULL);
  
  if (SCM_NULLP(accts_left)) {
    gnome_druid_set_page(GNOME_DRUID(wind->druid),
                         get_named_page(wind, "currency_page"));
    return TRUE;
  } else {
      return gnc_ui_qif_import_generic_next_cb(page, arg1, user_data);
  }
}

/********************************************************************
 * gnc_ui_qif_import_memo_prepare_cb
 ********************************************************************/

static void
gnc_ui_qif_import_memo_prepare_cb(GnomeDruidPage * page,
                                        gpointer arg1,
                                        gpointer user_data)
{
  QIFImportWindow * wind = user_data;

  gnc_set_busy_cursor(NULL, TRUE);
  update_memo_page(wind);
  gnc_unset_busy_cursor(NULL);
}


/****************************************************************
 * gnc_ui_qif_import_commodity_update
 *
 * This function updates the commodities based on the values for
 * mnemonic, namespace, and name approved by the user.
 ****************************************************************/

static void
gnc_ui_qif_import_commodity_update(QIFImportWindow * wind)
{
  GList          *pageptr;
  GnomeDruidPage *gtkpage;
  QIFDruidPage   *page;
  const char     *mnemonic = NULL;
  gchar          *namespace = NULL;
  const char     *fullname = NULL;
  gnc_commodity  *old_commodity;

  for (pageptr = wind->commodity_pages; pageptr; pageptr=pageptr->next)
  {
    gtkpage   = GNOME_DRUID_PAGE(pageptr->data);
    page      = g_object_get_data(G_OBJECT(gtkpage), "page_struct");

    /* Get any changes from the commodity page. */
    mnemonic  = gtk_entry_get_text(GTK_ENTRY(page->new_mnemonic_entry));
    namespace = gnc_ui_namespace_picker_ns(page->new_type_combo);
    fullname  = gtk_entry_get_text(GTK_ENTRY(page->new_name_entry));

    /* Update the commodity with the new values. */
    gnc_commodity_set_namespace(page->commodity, namespace);
    gnc_commodity_set_fullname(page->commodity, fullname);
    gnc_commodity_set_mnemonic(page->commodity, mnemonic);

    g_free(namespace);

    /* Add the commodity to the commodity table. */
    old_commodity = page->commodity;
    page->commodity = gnc_commodity_table_insert(gnc_get_current_commodities(),
                                                 page->commodity);
    /* If the table already contains the same namespace and mnemonic...
     * ...blow away any existing entry in the hash table? Using the fullname
     * as the key, which the user may have changed? Does this make any sense?
     * Getting a match would just be luck!
     *
     * Shouldn't we do a hash-fold to update the Schema hash table, replacing
     * each reference to the "old" commodity with a reference to the commodity
     * returned by gnc_commodity_table_insert? */
    if (old_commodity != page->commodity)
      scm_hash_remove_x(wind->stock_hash, scm_makfrom0str(fullname));
  }
}


/****************************************************************
 * gnc_ui_qif_import_prepare_duplicates
 *
 * This function prepares the duplicates checking page.
 ****************************************************************/

static void
gnc_ui_qif_import_prepare_duplicates(QIFImportWindow * wind)
{
  GtkTreeView      *view;
  GtkListStore     *store;
  SCM               duplicates;
  SCM               current_xtn;
  Transaction      *gnc_xtn;
  Split            *gnc_split;
  GtkTreeIter       iter;
  GtkTreeSelection *selection;
  GtkTreePath      *path;
  const gchar      *amount_str;
  int               rownum = 0;

  view = GTK_TREE_VIEW(wind->new_transaction_view);
  store = GTK_LIST_STORE(gtk_tree_view_get_model(view));
  gtk_list_store_clear(store);

  /* Loop through the list of new, potentially duplicate transactions. */
  duplicates = wind->match_transactions;
  while (!SCM_NULLP(duplicates))
  {
    current_xtn = SCM_CAAR(duplicates);
    #define FUNC_NAME "xaccTransCountSplits"
    gnc_xtn     = SWIG_MustGetPtr(current_xtn,
    SWIG_TypeQuery("_p_Transaction"), 1, 0);
    #undef FUNC_NAME
    if (xaccTransCountSplits(gnc_xtn) > 2)
      amount_str = _("(split)");
    else
    {
      gnc_split = xaccTransGetSplit(gnc_xtn, 0);
      amount_str =
        xaccPrintAmount(gnc_numeric_abs(xaccSplitGetValue(gnc_split)),
                        gnc_account_print_info
                        (xaccSplitGetAccount(gnc_split), TRUE));
    }

    gtk_list_store_append(store, &iter);
    gtk_list_store_set
      (store, &iter,
       QIF_TRANS_COL_INDEX, rownum++,
       QIF_TRANS_COL_DATE,
       gnc_print_date(xaccTransRetDatePostedTS(gnc_xtn)),
       QIF_TRANS_COL_DESCRIPTION, xaccTransGetDescription(gnc_xtn),
       QIF_TRANS_COL_AMOUNT, amount_str,
       -1);

    duplicates = SCM_CDR(duplicates);
  }

  selection = gtk_tree_view_get_selection(view);
  path = gtk_tree_path_new_from_indices (0, -1);
  gtk_tree_selection_select_path(selection, path);
  gtk_tree_path_free(path);
}


/****************************************************************
 * gnc_ui_qif_import_convert
 *
 * This function launches the Scheme procedures that actually do
 * the work of (a) converting the QIF data into GnuCash accounts
 * transactions, and (b) checking for possible duplication. Then
 * the next druid page is prepared and displayed.
 ****************************************************************/

static gboolean
gnc_ui_qif_import_convert(QIFImportWindow * wind)
{
  SCM   qif_to_gnc      = scm_c_eval_string("qif-import:qif-to-gnc");
  SCM   find_duplicates = scm_c_eval_string("gnc:account-tree-find-duplicates");
  SCM   retval;
  SCM   window;

  /* Get the default currency. */
  const char * currname =
    gtk_combo_box_get_active_text(GTK_COMBO_BOX(wind->currency_picker));

  /* Let the user know we're busy. */
  gnc_suspend_gui_refresh ();
  gnc_set_busy_cursor(NULL, TRUE);

  /* Update the commodities. */
  gnc_ui_qif_import_commodity_update(wind);

  /* Call a Scheme function to do the work.  The return value is the
   * root account of an account tree containing all the new accounts
   * and transactions */
  window = SWIG_NewPointerObj(wind->window, SWIG_TypeQuery("_p_GtkWidget"), 0);
  retval = scm_apply(qif_to_gnc,
                     SCM_LIST7(wind->imported_files,
                               wind->acct_map_info,
                               wind->cat_map_info,
                               wind->memo_map_info,
                               wind->stock_hash,
                               scm_makfrom0str(currname),
                               window),
                     SCM_EOL);
  gnc_unset_busy_cursor(NULL);

  if (retval == SCM_BOOL_F)
  {
    /* An error occurred during conversion. */

    /* There's no imported account tree. */
    scm_gc_unprotect_object(wind->imported_account_tree);
    wind->imported_account_tree = SCM_BOOL_F;
    scm_gc_protect_object(wind->imported_account_tree);

    /* We don't know what data structures may have become corrupted,
     * so we shouldn't allow further action. Display the failure
     * page next, and just allow the user to cancel. */
    gnome_druid_set_page(GNOME_DRUID(wind->druid),
                         get_named_page(wind, "failed_page"));
    gnome_druid_set_buttons_sensitive(GNOME_DRUID(wind->druid),
                                      FALSE, FALSE, TRUE, TRUE);
  }
  else
  {
    /* Save the imported account tree. */
    scm_gc_unprotect_object(wind->imported_account_tree);
    wind->imported_account_tree = retval;
    scm_gc_protect_object(wind->imported_account_tree);

    /* Detect duplicate transactions. */
    gnc_set_busy_cursor(NULL, TRUE);
    retval = scm_call_3(find_duplicates,
                        scm_c_eval_string("(gnc-get-current-root-account)"),
                        wind->imported_account_tree, window);
    gnc_unset_busy_cursor(NULL);

    /* Save the results. */
    scm_gc_unprotect_object(wind->match_transactions);
    wind->match_transactions = retval;
    scm_gc_protect_object(wind->match_transactions);

    /* Were any potential duplicates found? */
    if (retval == SCM_BOOL_F)
    {
      /* An error occurred during duplicate checking. */
      gnome_druid_set_page(GNOME_DRUID(wind->druid),
                           get_named_page(wind, "failed_page"));
      gnome_druid_set_buttons_sensitive(GNOME_DRUID(wind->druid),
                                        FALSE, FALSE, TRUE, TRUE);
    }
    else if (SCM_NULLP(retval))
    {
      /* No potential duplicates, so skip to the last page. */
      gnome_druid_set_page(GNOME_DRUID(wind->druid),
                           get_named_page(wind, "end_page"));
    }
    else
    {
      /* Prepare the duplicates page. */
      gnc_ui_qif_import_prepare_duplicates(wind);

      /* Display the next page. */
      if (wind->show_doc_pages)
        gnome_druid_set_page(GNOME_DRUID(wind->druid),
                             get_named_page(wind, "match_doc_page"));
      else
        gnome_druid_set_page(GNOME_DRUID(wind->druid),
                             get_named_page(wind, "match_duplicates_page"));
    }
  }

  gnc_resume_gui_refresh();
  return TRUE;
}


/********************************************************************
 * gnc_ui_qif_import_memo_next_cb
 ********************************************************************/

static gboolean
gnc_ui_qif_import_memo_next_cb(GnomeDruidPage * page,
                               gpointer arg1,
                               gpointer user_data)
{
  QIFImportWindow * wind = user_data;
  SCM any_new      = scm_c_eval_string("qif-import:any-new-accts?");
  SCM update_stock = scm_c_eval_string("qif-import:update-stock-hash");

  
  /* if any accounts are new, ask about the currency; else,
     just skip that page */
  if ((scm_call_1(any_new, wind->acct_map_info) == SCM_BOOL_T) ||
      (scm_call_1(any_new, wind->cat_map_info) == SCM_BOOL_T))
    /* go to currency page */ 
    return gnc_ui_qif_import_generic_next_cb(page, arg1, wind);
  else
  {
    /* if we need to look at stocks, do that, otherwise import
       xtns and go to the duplicates page */
    scm_gc_unprotect_object(wind->new_stocks);
    wind->new_stocks = scm_call_3(update_stock, wind->stock_hash,
				  wind->ticker_map, wind->acct_map_info);
    scm_gc_protect_object(wind->new_stocks);
    
    if (wind->new_stocks != SCM_BOOL_F)
    {
      if (wind->show_doc_pages)
        gnome_druid_set_page(GNOME_DRUID(wind->druid),
                             get_named_page(wind, "commodity_doc_page"));
      else
      {
        gnc_ui_qif_import_commodity_prepare_cb(page, arg1, wind);
        gnome_druid_set_page(GNOME_DRUID(wind->druid),
                             GNOME_DRUID_PAGE(wind->commodity_pages->data));
      }
    }
    else
      /* It's time to import the accounts. */
      gnc_ui_qif_import_convert(wind);

    return TRUE;
  }
}

/****************************************************************
 * gnc_ui_qif_import_currency_back_cb
 * Check to see if there are any payees and memos to show. If not
 * jump to category match page.
 ****************************************************************/
static gboolean
gnc_ui_qif_import_currency_back_cb(GnomeDruidPage * page, gpointer arg1, 
                                  gpointer user_data)
{
  QIFImportWindow * wind = user_data;
  
  if (!wind->memo_display_info ||
      (wind->memo_display_info == SCM_BOOL_F) ||
       SCM_NULLP(wind->memo_display_info))
  {
    gnome_druid_set_page(GNOME_DRUID(wind->druid),
                         get_named_page(wind, "category_match_page"));
    return TRUE;
  } else {
      return gnc_ui_qif_import_generic_back_cb(page, arg1, user_data);
  }
}

/********************************************************************
 * gnc_ui_qif_import_currency_next_cb
 ********************************************************************/

static gboolean
gnc_ui_qif_import_currency_next_cb(GnomeDruidPage * page,
                                   gpointer arg1,
                                   gpointer user_data)
{
  QIFImportWindow * wind = user_data;
  SCM update_stock = scm_c_eval_string("qif-import:update-stock-hash");

  gnc_set_busy_cursor(NULL, TRUE);
  scm_gc_unprotect_object(wind->new_stocks);
  wind->new_stocks =  scm_call_3(update_stock, wind->stock_hash, 
				 wind->ticker_map, wind->acct_map_info);
  scm_gc_protect_object(wind->new_stocks);
  
  if (wind->new_stocks != SCM_BOOL_F)
  {
    if (wind->show_doc_pages)
      gnome_druid_set_page(GNOME_DRUID(wind->druid),
                           get_named_page(wind, "commodity_doc_page"));
    else
    {
      gnc_ui_qif_import_commodity_prepare_cb(page, arg1, user_data);
      gnome_druid_set_page(GNOME_DRUID(wind->druid),
                           GNOME_DRUID_PAGE(wind->commodity_pages->data));
    }
  }
  else
    /* it's time to import the accounts. */
    gnc_ui_qif_import_convert(wind);

  gnc_unset_busy_cursor(NULL);
  return TRUE;
}


static gboolean
gnc_ui_qif_import_comm_check_cb(GnomeDruidPage * page,
                                gpointer arg1,
                                gpointer user_data)
{
  QIFImportWindow * wind = user_data;
  QIFDruidPage    * qpage = g_object_get_data(G_OBJECT(page), "page_struct");
  
  gchar *namespace       = gnc_ui_namespace_picker_ns(qpage->new_type_combo);
  const char * name      = gtk_entry_get_text(GTK_ENTRY(qpage->new_name_entry));
  const char * mnemonic  = gtk_entry_get_text(GTK_ENTRY(qpage->new_mnemonic_entry));

  if(!namespace || (namespace[0] == 0)) {
    gnc_warning_dialog(wind->window,
		       _("You must enter a Type for the commodity."));
    if (namespace)
      g_free(namespace);
    return TRUE;
  }
  else if(!name || (name[0] == 0)) {
    gnc_warning_dialog(wind->window,
		       _("You must enter a name for the commodity."));
    return TRUE;
  }
  else if(!mnemonic || (mnemonic[0] == 0)) {
    gnc_warning_dialog
      (wind->window, _("You must enter an abbreviation for the commodity."));
    return TRUE;
  }

  if (gnc_commodity_namespace_is_iso (namespace) &&
      !gnc_commodity_table_lookup (gnc_get_current_commodities (),
                                   namespace, mnemonic))
  {
    gnc_warning_dialog(wind->window,
		       _("You must enter an existing national "
			 "currency or enter a different type."));

    g_free(namespace);
    return TRUE;
  }
  g_free(namespace);

  if(page == (g_list_last(wind->commodity_pages))->data)
  {
    /* it's time to import the accounts. */
    gnc_ui_qif_import_convert(wind);
    return TRUE;
  }
  else
    return FALSE;
}


/********************************************************************
 * gnc_ui_qif_import_commodity_prepare_cb
 * build a mapping of QIF stock name to a gnc_commodity 
 ********************************************************************/

static void
gnc_ui_qif_import_commodity_prepare_cb(GnomeDruidPage * page,
                                       gpointer arg1,
                                       gpointer user_data)
{
  QIFImportWindow * wind = user_data;

  SCM   hash_ref  = scm_c_eval_string("hash-ref");
  SCM   stocks;
  SCM   comm_ptr_token;

  gnc_commodity  * commodity;
  GnomeDruidPage * back_page = get_named_page(wind, "commodity_doc_page");  
  QIFDruidPage   * new_page;
  
  /* only set up once */
  if (wind->commodity_pages) return;
  
  /* this shouldn't happen, but DTRT if it does */
  if (SCM_NULLP(wind->new_stocks))
  {
    g_warning("QIF import: BUG DETECTED! Reached commodity doc page with nothing to do!");
    gnc_ui_qif_import_convert(wind);
  }

  /* insert new pages, one for each stock */
  gnc_set_busy_cursor(NULL, TRUE);
  stocks = wind->new_stocks;
  while (!SCM_NULLP(stocks) && (stocks != SCM_BOOL_F))
  {
    comm_ptr_token = scm_call_2(hash_ref, wind->stock_hash, SCM_CAR(stocks));
    #define FUNC_NAME "make_qif_druid_page"
    commodity      = SWIG_MustGetPtr(comm_ptr_token,
                                     SWIG_TypeQuery("_p_gnc_commodity"), 1, 0);
    #undef FUNC_NAME
    new_page = make_qif_druid_page(commodity);

    g_signal_connect(new_page->page, "back",
		     G_CALLBACK(gnc_ui_qif_import_generic_back_cb),
		     wind);

    g_signal_connect(new_page->page, "next",
		     G_CALLBACK(gnc_ui_qif_import_comm_check_cb),
		     wind);

    wind->commodity_pages = g_list_append(wind->commodity_pages, 
                                          new_page->page);

    gnome_druid_insert_page(GNOME_DRUID(wind->druid),
                            back_page, 
                            GNOME_DRUID_PAGE(new_page->page));
    back_page = GNOME_DRUID_PAGE(new_page->page);
    
    stocks = SCM_CDR(stocks);
    gtk_widget_show_all(new_page->page);
  }
  gnc_unset_busy_cursor(NULL);

  gnc_druid_set_colors (GNOME_DRUID (wind->druid));
}

static QIFDruidPage *
make_qif_druid_page(gnc_commodity * comm)
{
  
  QIFDruidPage * retval = g_new0(QIFDruidPage, 1);
  GtkWidget * top_vbox;
  GtkWidget * info_label;
  GtkWidget * next_label;
  GtkWidget * temp;
  char      * title = NULL;
  const char * str;
  GnomeDruidPageStandard * page;

  /* make the page widget */
  retval->page = gnome_druid_page_standard_new_with_vals("", NULL, NULL);
  retval->commodity = comm;
  g_object_set_data(G_OBJECT(retval->page), "page_struct", retval);

  page = GNOME_DRUID_PAGE_STANDARD(retval->page);

  /* save the old commodity name */
  str = gnc_commodity_get_mnemonic(comm);
  str = str ? str : "";
  title = g_markup_printf_escaped(_("Enter information about \"%s\""), str);

  gnome_druid_page_standard_set_background(page, & std_bg_color);  
  gnome_druid_page_standard_set_logo_background(page, & std_logo_bg_color);
  gnome_druid_page_standard_set_title_foreground (page, & std_title_color);
  gnome_druid_page_standard_set_title(page, title);
  g_free(title);
  
  top_vbox = gtk_vbox_new(FALSE, 3);
  gtk_box_pack_start(GTK_BOX(page->vbox), top_vbox, FALSE, FALSE, 0);
                     
  info_label = 
    gtk_label_new(_("Pick the commodity's exchange or listing "
                    "(NASDAQ, NYSE, etc)."));

  gtk_label_set_justify (GTK_LABEL(info_label), GTK_JUSTIFY_LEFT);
  gtk_box_pack_start(GTK_BOX(top_vbox), info_label, TRUE, TRUE, 0);

  temp = gtk_hbox_new(FALSE, 0);
  gtk_box_pack_start(GTK_BOX(top_vbox), temp, FALSE, FALSE, 0);

  info_label = gtk_label_new("");
  gtk_box_pack_start(GTK_BOX(temp), info_label, TRUE, TRUE, 0);

  retval->new_type_combo = gtk_combo_box_entry_new_text();
  gnc_cbe_require_list_item(GTK_COMBO_BOX_ENTRY(retval->new_type_combo));
  gtk_box_pack_start(GTK_BOX(temp), retval->new_type_combo, TRUE, TRUE, 0);

  info_label = gtk_label_new("");
  gtk_box_pack_start(GTK_BOX(temp), info_label, TRUE, TRUE, 0);

  gnc_ui_update_namespace_picker(retval->new_type_combo, 
                                 gnc_commodity_get_namespace(comm),
                                 DIAG_COMM_ALL);

  info_label = 
    gtk_label_new(_("Enter the full name of the commodity, "
                    "such as \"Red Hat Stock\""));
  
  gtk_label_set_justify (GTK_LABEL(info_label), GTK_JUSTIFY_LEFT);
  gtk_box_pack_start(GTK_BOX(top_vbox), info_label, TRUE, TRUE, 0);
  
  temp = gtk_hbox_new(FALSE, 0);
  gtk_box_pack_start(GTK_BOX(top_vbox), temp, FALSE, FALSE, 0);

  info_label = gtk_label_new("");
  gtk_box_pack_start(GTK_BOX(temp), info_label, TRUE, TRUE, 0);

  retval->new_name_entry = gtk_entry_new();
  gtk_box_pack_start(GTK_BOX(temp), retval->new_name_entry,
                     TRUE, TRUE, 0);
  gtk_entry_set_text(GTK_ENTRY(retval->new_name_entry),
                     gnc_commodity_get_fullname(comm));
  
  info_label = gtk_label_new("");
  gtk_box_pack_start(GTK_BOX(temp), info_label, TRUE, TRUE, 0);

  info_label = 
    gtk_label_new(_("Enter the ticker symbol (such as \"RHAT\") or "
                    "other unique abbreviation for the name."));
  
  gtk_label_set_justify (GTK_LABEL(info_label), GTK_JUSTIFY_LEFT);
  gtk_box_pack_start(GTK_BOX(top_vbox), info_label, TRUE, TRUE, 0);
 
  temp = gtk_hbox_new(FALSE, 0);
  gtk_box_pack_start(GTK_BOX(top_vbox), temp, FALSE, FALSE, 0);

  info_label = gtk_label_new("");
  gtk_box_pack_start(GTK_BOX(temp), info_label, TRUE, TRUE, 0);

  retval->new_mnemonic_entry = gtk_entry_new();
  gtk_box_pack_start(GTK_BOX(temp), retval->new_mnemonic_entry,
                     TRUE, TRUE, 0);
  gtk_entry_set_text(GTK_ENTRY(retval->new_mnemonic_entry),
                     gnc_commodity_get_mnemonic(comm));
  
  info_label = gtk_label_new("");
  gtk_box_pack_start(GTK_BOX(temp), info_label, TRUE, TRUE, 0);

  next_label = gtk_label_new(_("Click \"Forward\" to accept the information "
                               "and move on."));
  gtk_label_set_justify (GTK_LABEL(next_label), GTK_JUSTIFY_LEFT);
  gtk_box_pack_end(GTK_BOX(top_vbox), next_label, TRUE, TRUE, 0);

  
  return retval;
}


static void
refresh_old_transactions(QIFImportWindow * wind, int selection)
{
  SCM          possible_matches;
  SCM          current_xtn;
  SCM          selected;
  Transaction  * gnc_xtn;
  Split        * gnc_split;
  const gchar  * amount_str;
  int          rownum = 0;
  GtkTreeView *view;
  GtkListStore *store;
  GtkTreeIter iter;

  view = GTK_TREE_VIEW(wind->old_transaction_view);
  store = GTK_LIST_STORE(gtk_tree_view_get_model(view));
  gtk_list_store_clear(store);

  if(wind->match_transactions != SCM_BOOL_F) {
    possible_matches = SCM_CDR(scm_list_ref(wind->match_transactions,
                                  scm_int2num(wind->selected_transaction)));
    scm_call_2(scm_c_eval_string("qif-import:refresh-match-selection"),
	       possible_matches, scm_int2num(selection));

    while(!SCM_NULLP(possible_matches)) {
      current_xtn = SCM_CAR(possible_matches);
      #define FUNC_NAME "make_qif_druid_page"
      gnc_xtn     = SWIG_MustGetPtr(SCM_CAR(current_xtn),
                                    SWIG_TypeQuery("_p_Transaction"), 1, 0);
      #undef FUNC_NAME
      selected    = SCM_CDR(current_xtn);
      
      if(xaccTransCountSplits(gnc_xtn) > 2) {
        amount_str = _("(split)"); 
      }
      else {
	gnc_split = xaccTransGetSplit(gnc_xtn, 0);  
        amount_str = 
          xaccPrintAmount(gnc_numeric_abs(xaccSplitGetValue(gnc_split)),
                          gnc_account_print_info
                          (xaccSplitGetAccount(gnc_split), TRUE));
      }

      gtk_list_store_append(store, &iter);
      gtk_list_store_set
	(store, &iter,
	 QIF_TRANS_COL_INDEX, rownum++,
	 QIF_TRANS_COL_DATE, gnc_print_date(xaccTransRetDatePostedTS(gnc_xtn)),
	 QIF_TRANS_COL_DESCRIPTION, xaccTransGetDescription(gnc_xtn),
	 QIF_TRANS_COL_AMOUNT, amount_str,
	 QIF_TRANS_COL_CHECKED, selected != SCM_BOOL_F,
	 -1);

      possible_matches = SCM_CDR(possible_matches);
    }
  }
}

static void
gnc_ui_qif_import_duplicate_new_select_cb (GtkTreeSelection *selection,
					   QIFImportWindow  *wind)
{
  GtkTreeModel *model;
  GtkTreeIter iter;

  if (gtk_tree_selection_get_selected(selection, &model, &iter))
    gtk_tree_model_get(model, &iter,
		       QIF_TRANS_COL_INDEX, &wind->selected_transaction,
		       -1);
  refresh_old_transactions(wind, -1);
}

static gboolean
reset_ignore_old_select(gboolean *ignore)
{
  *ignore = FALSE;
  return FALSE;
}

static void
gnc_ui_qif_import_duplicate_old_select_cb (GtkTreeSelection *selection,
					   QIFImportWindow  *wind)
{
  GtkTreeModel *model;
  GtkTreeIter iter;
  gint row;
  static gboolean ignore_old_select = FALSE;

  /* Get the current selection then clear it.  We're about to clear
   * the entire list store and rebuild it so this prevents errors. */
  if (!gtk_tree_selection_get_selected(selection, &model, &iter))
    return;
  gtk_tree_selection_unselect_all(selection);

  /* Getting a weird double call the first time a line is clicked.
   * Once via gtk_tree_view_button_press and then again via
   * gtk_tree_view_grab_focus. */
  if (ignore_old_select)
    return;
  ignore_old_select = TRUE;
  g_idle_add((GSourceFunc)reset_ignore_old_select, &ignore_old_select);

  /* Get the row the user clicked on and update the scheme
   * code/rebuild the list store.  */
  gtk_tree_model_get(model, &iter,
		     QIF_TRANS_COL_INDEX, &row,
		     -1);
  refresh_old_transactions(wind, row);
}

static void
gnc_ui_qif_import_finish_cb(GnomeDruidPage * gpage, 
                            gpointer arg1, 
                            gpointer user_data)
{
  
  SCM   save_map_prefs = scm_c_eval_string("qif-import:save-map-prefs");
  SCM   cat_and_merge = scm_c_eval_string("gnc:account-tree-catenate-and-merge");
  SCM   prune_xtns = scm_c_eval_string("gnc:prune-matching-transactions");
  
  QIFImportWindow * wind = user_data;

  gnc_suspend_gui_refresh();

  /* prune the old transactions marked as dupes */
  if(wind->match_transactions != SCM_BOOL_F) {
    scm_call_1(prune_xtns, wind->match_transactions);
  }

  /* actually add in the new transactions. */
  if (wind->imported_account_tree != SCM_BOOL_F)
    scm_call_2(cat_and_merge,
	       scm_c_eval_string("(gnc-get-current-root-account)"),
	       wind->imported_account_tree);
  
  gnc_resume_gui_refresh();
  
  /* write out mapping info before destroying the window */
  scm_apply(save_map_prefs, 
	    SCM_LIST4(wind->acct_map_info, wind->cat_map_info,
		      wind->memo_map_info, wind->stock_hash),
	    SCM_EOL);
  
  gnc_ui_qif_import_druid_destroy(wind);  
}

static void
gnc_ui_qif_import_cancel_cb (GnomeDruid * druid, 
                             gpointer user_data)
{
  QIFImportWindow * wind = user_data;
  
  gnc_ui_qif_import_druid_destroy(wind);
}

SCM
gnc_ui_qif_import_druid_get_mappings(QIFImportWindow * w)
{
  return SCM_LIST3(w->acct_map_info, 
                   w->cat_map_info,
                   w->memo_map_info);
}


/* ======================================================== */

static gboolean
show_handler (const char *class, gint component_id,
	      gpointer user_data, gpointer iter_data)
{
  QIFImportWindow *qif_win = user_data;

  if (!qif_win)
    return(FALSE);
  gtk_window_present (GTK_WINDOW(qif_win->window));
  return(TRUE);
}

void
gnc_file_qif_import (void) 
{
  if (gnc_forall_gui_components (DRUID_QIF_IMPORT_CM_CLASS,
				 show_handler, NULL))
      return;

  /* pop up the QIF File Import dialog box */
  gnc_ui_qif_import_druid_make();
}

/********************************************************************
 * gnc_ui_qif_import_druid_make() 
 * build the druid.
 ********************************************************************/

QIFImportWindow *
gnc_ui_qif_import_druid_make(void)
{
  
  QIFImportWindow * retval;
  GladeXML        * xml;
  GError * err = NULL;
  SCM  load_map_prefs;
  SCM  mapping_info;
  SCM  create_ticker_map;
  int  i;
  GtkTreeView *view;
  GtkListStore *store;
  GtkCellRenderer *renderer;
  GtkTreeViewColumn *column;
  GtkTreeSelection *selection;


  char * pre_page_names[NUM_PRE_PAGES] = {
    "start_page", "load_file_page", "date_format_page", "account_name_page",
    "loaded_files_page", "account_doc_page", "account_match_page", 
    "category_doc_page", "category_match_page", "memo_doc_page",
    "memo_match_page", "currency_page", "commodity_doc_page"
  };

  char * post_page_names[NUM_POST_PAGES] = {
    "match_doc_page", "match_duplicates_page", "end_page"
  };

  char * doc_page_names[NUM_DOC_PAGES] = {
    "start_page", "account_doc_page", "category_doc_page", 
    "commodity_doc_page", "memo_doc_page", "match_doc_page"    
  };

  retval = g_new0(QIFImportWindow, 1);

  xml = gnc_glade_xml_new ("qif.glade", "QIF Import Druid");

  glade_xml_signal_connect_data
    (xml, "gnc_ui_qif_import_cancel_cb",
     G_CALLBACK (gnc_ui_qif_import_cancel_cb), retval);

  glade_xml_signal_connect_data
    (xml, "gnc_ui_qif_import_generic_next_cb",
     G_CALLBACK (gnc_ui_qif_import_generic_next_cb), retval);

  glade_xml_signal_connect_data
    (xml, "gnc_ui_qif_import_generic_back_cb",
     G_CALLBACK (gnc_ui_qif_import_generic_back_cb), retval);

  glade_xml_signal_connect_data
    (xml, "gnc_ui_qif_import_select_file_cb",
     G_CALLBACK (gnc_ui_qif_import_select_file_cb), retval);

  glade_xml_signal_connect_data
    (xml, "gnc_ui_qif_import_load_file_back_cb",
     G_CALLBACK (gnc_ui_qif_import_load_file_back_cb), retval);

  glade_xml_signal_connect_data
    (xml, "gnc_ui_qif_import_load_file_next_cb",
     G_CALLBACK (gnc_ui_qif_import_load_file_next_cb), retval);

  glade_xml_signal_connect_data
    (xml, "gnc_ui_qif_import_date_format_next_cb",
     G_CALLBACK (gnc_ui_qif_import_date_format_next_cb), retval);

  glade_xml_signal_connect_data
    (xml, "gnc_ui_qif_import_loaded_files_prepare_cb",
     G_CALLBACK (gnc_ui_qif_import_loaded_files_prepare_cb), retval);

  glade_xml_signal_connect_data
    (xml, "gnc_ui_qif_import_load_another_cb",
     G_CALLBACK (gnc_ui_qif_import_load_another_cb), retval);

  glade_xml_signal_connect_data
    (xml, "gnc_ui_qif_import_unload_file_cb",
     G_CALLBACK (gnc_ui_qif_import_unload_file_cb), retval);

  glade_xml_signal_connect_data
    (xml, "gnc_ui_qif_import_default_acct_next_cb",
     G_CALLBACK (gnc_ui_qif_import_default_acct_next_cb), retval);

  glade_xml_signal_connect_data
    (xml, "gnc_ui_qif_import_default_acct_back_cb",
     G_CALLBACK (gnc_ui_qif_import_default_acct_back_cb), retval);

  glade_xml_signal_connect_data
    (xml, "gnc_ui_qif_import_accounts_prepare_cb",
     G_CALLBACK (gnc_ui_qif_import_accounts_prepare_cb), retval);

  glade_xml_signal_connect_data
    (xml, "gnc_ui_qif_import_categories_prepare_cb",
     G_CALLBACK (gnc_ui_qif_import_categories_prepare_cb), retval);
  
  glade_xml_signal_connect_data
    (xml, "gnc_ui_qif_import_categories_next_cb",
     G_CALLBACK (gnc_ui_qif_import_categories_next_cb), retval);
  
  glade_xml_signal_connect_data
    (xml, "gnc_ui_qif_import_memo_prepare_cb",
     G_CALLBACK (gnc_ui_qif_import_memo_prepare_cb), retval);

  glade_xml_signal_connect_data
    (xml, "gnc_ui_qif_import_memo_next_cb",
     G_CALLBACK (gnc_ui_qif_import_memo_next_cb), retval);
  
  glade_xml_signal_connect_data
    (xml, "gnc_ui_qif_import_currency_back_cb",
     G_CALLBACK (gnc_ui_qif_import_currency_back_cb), retval);
  
  glade_xml_signal_connect_data
    (xml, "gnc_ui_qif_import_currency_next_cb",
     G_CALLBACK (gnc_ui_qif_import_currency_next_cb), retval);

  glade_xml_signal_connect_data
    (xml, "gnc_ui_qif_import_commodity_prepare_cb",
     G_CALLBACK (gnc_ui_qif_import_commodity_prepare_cb), retval);

  glade_xml_signal_connect_data
    (xml, "gnc_ui_qif_import_finish_cb",
     G_CALLBACK (gnc_ui_qif_import_finish_cb), retval);

  retval->window = glade_xml_get_widget (xml, "QIF Import Druid");

  retval->imported_files    =  SCM_EOL;
  retval->selected_file     =  SCM_BOOL_F;
  retval->gnc_acct_info     =  SCM_BOOL_F;
  retval->cat_display_info  =  SCM_BOOL_F;
  retval->cat_map_info      =  SCM_BOOL_F;
  retval->acct_display_info =  SCM_BOOL_F;
  retval->acct_map_info     =  SCM_BOOL_F;
  retval->memo_display_info =  SCM_BOOL_F;
  retval->memo_map_info     =  SCM_BOOL_F;
  retval->stock_hash        =  SCM_BOOL_F;
  retval->new_stocks        =  SCM_BOOL_F;
  retval->ticker_map        =  SCM_BOOL_F;
  retval->imported_account_tree   = SCM_BOOL_F;
  retval->match_transactions = SCM_BOOL_F;
  retval->selected_transaction = 0;
  
  retval->druid          = glade_xml_get_widget (xml, "qif_import_druid");
  retval->filename_entry = glade_xml_get_widget (xml, "qif_filename_entry");
  retval->acct_entry     = glade_xml_get_widget (xml, "qif_account_entry");
  retval->date_format_combo = glade_xml_get_widget (xml, "date_format_combobox");
  retval->selected_file_view = glade_xml_get_widget(xml, "selected_file_view");
  retval->currency_picker = glade_xml_get_widget (xml, "currency_comboboxentry");
  retval->acct_view      = glade_xml_get_widget (xml, "account_page_view");
  retval->cat_view       = glade_xml_get_widget (xml, "category_page_view");
  retval->memo_view      = glade_xml_get_widget (xml, "memo_page_view");
  retval->new_transaction_view = 
    glade_xml_get_widget (xml, "new_transaction_view");
  retval->old_transaction_view = 
    glade_xml_get_widget (xml, "old_transaction_view");
  
  retval->pre_comm_pages   = NULL;
  retval->post_comm_pages  = NULL;
  retval->doc_pages        = NULL;
  retval->commodity_pages = NULL;

  /* Get the user's preference for showing documentation pages. */
  retval->show_doc_pages = 
    gnc_gconf_get_bool(GCONF_SECTION, GCONF_NAME_SHOW_DOC, &err);
  if (err != NULL) {
    g_warning("QIF import: gnc_gconf_get_bool error: %s\n", err->message);
    g_error_free(err);

    /* Show documentation pages by default. */
    g_warning("QIF import: Couldn't get %s setting from gconf.\n",
              GCONF_NAME_SHOW_DOC);
    g_warning("QIF import: Documentation pages will be shown by default.\n");
    retval->show_doc_pages = TRUE;
  }

  for(i=0; i < NUM_PRE_PAGES; i++) {
    retval->pre_comm_pages = 
      g_list_append(retval->pre_comm_pages, 
                    glade_xml_get_widget (xml, pre_page_names[i]));
  }
  for(i=0; i < NUM_POST_PAGES; i++) {
    retval->post_comm_pages = 
      g_list_append(retval->post_comm_pages, 
                    glade_xml_get_widget (xml, post_page_names[i]));
  }
  for(i=0; i < NUM_DOC_PAGES; i++) {
    retval->doc_pages = 
      g_list_append(retval->doc_pages, 
                    glade_xml_get_widget (xml, doc_page_names[i]));
  }
  
  /* Set up the selected file view */
  view = GTK_TREE_VIEW(retval->selected_file_view);
  store = gtk_list_store_new(NUM_FILENAME_COLS, G_TYPE_INT, G_TYPE_STRING);
  gtk_tree_view_set_model(view, GTK_TREE_MODEL(store));
  g_object_unref(store);

  renderer = gtk_cell_renderer_text_new();
  column = gtk_tree_view_column_new_with_attributes(_("Account"), renderer,
						    "text", FILENAME_COL_NAME,
						    NULL);
  gtk_tree_view_append_column(view, column);

  selection = gtk_tree_view_get_selection(view);
  g_signal_connect(selection, "changed",
		   G_CALLBACK(gnc_ui_qif_import_select_loaded_file_cb),
		   retval);
  
  create_account_picker_view(retval->acct_view, _("QIF account name"),
			     G_CALLBACK(gnc_ui_qif_import_account_line_select_cb),
			     retval);
  create_account_picker_view(retval->cat_view,  _("QIF category name"),
			     G_CALLBACK(gnc_ui_qif_import_category_line_select_cb),
			     retval);
  create_account_picker_view(retval->memo_view, _("QIF payee/memo"),
			     G_CALLBACK(gnc_ui_qif_import_memo_line_select_cb),
			     retval);

  /* Set up the new transaction view */
  view = GTK_TREE_VIEW(retval->new_transaction_view);
  store = gtk_list_store_new(NUM_QIF_TRANS_COLS, G_TYPE_INT, G_TYPE_STRING,
			     G_TYPE_STRING, G_TYPE_STRING, G_TYPE_BOOLEAN);
  gtk_tree_view_set_model(view, GTK_TREE_MODEL(store));
  g_object_unref(store);

  renderer = gtk_cell_renderer_text_new();
  column = gtk_tree_view_column_new_with_attributes(_("Date"), renderer,
						    "text", QIF_TRANS_COL_DATE,
						    NULL);
  gtk_tree_view_append_column(view, column);

  renderer = gtk_cell_renderer_text_new();
  column = gtk_tree_view_column_new_with_attributes(_("Description"), renderer,
						    "text", QIF_TRANS_COL_DESCRIPTION,
						    NULL);
  gtk_tree_view_append_column(view, column);
  gtk_tree_view_column_set_expand(column, TRUE);

  renderer = gtk_cell_renderer_text_new();
  column = gtk_tree_view_column_new_with_attributes(_("Amount"), renderer,
						    "text", QIF_TRANS_COL_AMOUNT,
						    NULL);
  gtk_tree_view_append_column(view, column);

  selection = gtk_tree_view_get_selection(view);
  g_signal_connect(selection, "changed",
		   G_CALLBACK(gnc_ui_qif_import_duplicate_new_select_cb),
		   retval);


  /* Set up the old transaction view */
  view = GTK_TREE_VIEW(retval->old_transaction_view);
  store = gtk_list_store_new(NUM_QIF_TRANS_COLS, G_TYPE_INT, G_TYPE_STRING,
			     G_TYPE_STRING, G_TYPE_STRING, G_TYPE_BOOLEAN);
  gtk_tree_view_set_model(view, GTK_TREE_MODEL(store));
  g_object_unref(store);

  renderer = gtk_cell_renderer_text_new();
  column = gtk_tree_view_column_new_with_attributes(_("Date"), renderer,
						    "text", QIF_TRANS_COL_DATE,
						    NULL);
  gtk_tree_view_append_column(view, column);

  renderer = gtk_cell_renderer_text_new();
  column = gtk_tree_view_column_new_with_attributes(_("Description"), renderer,
						    "text", QIF_TRANS_COL_DESCRIPTION,
						    NULL);
  gtk_tree_view_append_column(view, column);
  gtk_tree_view_column_set_expand(column, TRUE);

  renderer = gtk_cell_renderer_text_new();
  column = gtk_tree_view_column_new_with_attributes(_("Amount"), renderer,
						    "text", QIF_TRANS_COL_AMOUNT,
						    NULL);
  gtk_tree_view_append_column(view, column);

  renderer = gtk_cell_renderer_toggle_new();
  column = gtk_tree_view_column_new_with_attributes(_("Dup?"), renderer,
						    "active", QIF_TRANS_COL_CHECKED,
						    NULL);
  gtk_tree_view_append_column(view, column);

  selection = gtk_tree_view_get_selection(view);
  g_signal_connect(selection, "changed",
		   G_CALLBACK(gnc_ui_qif_import_duplicate_old_select_cb),
		   retval);


  /* load the saved-state of the mappings from Quicken accounts and
   * categories to gnucash accounts */
  load_map_prefs = scm_c_eval_string("qif-import:load-map-prefs");

  mapping_info = scm_call_0(load_map_prefs);
  retval->gnc_acct_info    = scm_list_ref(mapping_info, scm_int2num(0));
  retval->acct_map_info    = scm_list_ref(mapping_info, scm_int2num(1));
  retval->cat_map_info     = scm_list_ref(mapping_info, scm_int2num(2));
  retval->memo_map_info    = scm_list_ref(mapping_info, scm_int2num(3));
  retval->stock_hash       = scm_list_ref(mapping_info, scm_int2num(4));

  create_ticker_map = scm_c_eval_string("make-ticker-map");
  retval->ticker_map = scm_call_0(create_ticker_map);
  
  scm_gc_protect_object(retval->imported_files);
  scm_gc_protect_object(retval->selected_file);
  scm_gc_protect_object(retval->gnc_acct_info);
  scm_gc_protect_object(retval->cat_display_info);
  scm_gc_protect_object(retval->cat_map_info);
  scm_gc_protect_object(retval->memo_display_info);
  scm_gc_protect_object(retval->memo_map_info);
  scm_gc_protect_object(retval->acct_display_info);
  scm_gc_protect_object(retval->acct_map_info);
  scm_gc_protect_object(retval->stock_hash);
  scm_gc_protect_object(retval->new_stocks);
  scm_gc_protect_object(retval->ticker_map);
  scm_gc_protect_object(retval->imported_account_tree);
  scm_gc_protect_object(retval->match_transactions);
  
  /* set a default currency for new accounts */
  gnc_cbe_require_list_item(GTK_COMBO_BOX_ENTRY(retval->currency_picker));
  gnc_ui_update_commodity_picker(retval->currency_picker,
                                 GNC_COMMODITY_NS_CURRENCY, 
                                 gnc_commodity_get_printname
                                 (gnc_default_currency()));
  
  if(!retval->show_doc_pages) {
    gnome_druid_set_page(GNOME_DRUID(retval->druid),
                         get_named_page(retval, "load_file_page"));
  }

  gnc_druid_set_colors (GNOME_DRUID (retval->druid));

  gnc_register_gui_component(DRUID_QIF_IMPORT_CM_CLASS, NULL, NULL, retval);

  gtk_widget_show_all(retval->window);
  gtk_window_present (GTK_WINDOW(retval->window));

  return retval;
}
