/********************************************************************\
 * druid-qif-import.c -- window for importing QIF files            *
 *                        (GnuCash)                                 *
 * Copyright (C) 2000 Bill Gribble <grib@billgribble.com>           *
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

#include <gnome.h>
#include <stdio.h>
#include <sys/time.h>
#include <unistd.h>

#include <guile/gh.h>
#include "druid-qif-import.h"
#include "dialog-account-picker.h"
#include "dialog-commodity.h"
#include "window-help.h"
#include "messages.h"
#include "gnc-ui.h"

#include "Account.h"
#include "FileDialog.h"
#include "FileBox.h"
#include "dialog-utils.h"
#include "query-user.h"
#include "gnc-ui-util.h"

#include <g-wrap-runtime-guile.h>

struct _qifimportwindow {
  GtkWidget * window;
  GtkWidget * druid;
  GtkWidget * filename_entry;
  GtkWidget * acct_entry;
  GtkWidget * date_format_combo;
  GtkWidget * date_format_entry;
  GtkWidget * selected_file_list;
  GtkWidget * acct_list;
  GtkWidget * cat_list;
  GtkWidget * currency_picker;
  GtkWidget * currency_entry;

  GtkWidget * loaded_files_page;
  GtkWidget * load_file_page;
  GtkWidget * date_format_page;
  GtkWidget * account_name_page;
  GtkWidget * commodity_page;
  GtkWidget * end_page;

  GList     * pages;
  
  SCM       imported_files;
  SCM       selected_file;

  SCM       acct_map_info; 
  SCM       acct_display_info;

  SCM       cat_map_info;
  SCM       cat_display_info;

  SCM       gnc_acct_info;
  SCM       stock_hash;
};

struct _qifdruidpage {
  GtkWidget * page;
  GtkWidget * new_type_combo;
  GtkWidget * new_type_entry;
  GtkWidget * new_name_entry;
  GtkWidget * new_mnemonic_entry;
  gnc_commodity * commodity;
};  

typedef struct _qifdruidpage QIFDruidPage;
static QIFDruidPage * make_qif_druid_page(gnc_commodity * comm);

static void update_file_page(QIFImportWindow * win);
static void update_accounts_page(QIFImportWindow * win);
static void update_categories_page(QIFImportWindow * win);

static GdkColor std_bg_color = { 0, 39835, 49087, 40092 };
static GdkColor std_logo_bg_color = { 0, 65535, 65535, 65535 };
static GdkColor std_title_color =  { 0, 65535, 65535, 65535 };


/********************************************************************
 * gnc_ui_qif_import_druid_make() 
 * build the druid.
 ********************************************************************/

QIFImportWindow *
gnc_ui_qif_import_druid_make(void)  {
  
  QIFImportWindow * retval;
  GtkObject * wobj;
  SCM  load_map_prefs;
  SCM  mapping_info;
  SCM  lookup_option;
  SCM  lookup_value;

  retval = g_new0(QIFImportWindow, 1);
  
  retval->window = create_QIF_Import_Druid();
  wobj           = GTK_OBJECT(retval->window);

  retval->imported_files    =  SCM_EOL;
  retval->selected_file     =  SCM_BOOL_F;
  retval->gnc_acct_info     =  SCM_BOOL_F;
  retval->cat_display_info  =  SCM_BOOL_F;
  retval->cat_map_info      =  SCM_BOOL_F;
  retval->acct_display_info =  SCM_BOOL_F;
  retval->acct_map_info     =  SCM_BOOL_F;
  retval->stock_hash        =  SCM_BOOL_F;

  retval->druid          = gtk_object_get_data(wobj, "qif_import_druid");
  retval->filename_entry = gtk_object_get_data(wobj, "qif_filename_entry");
  retval->acct_entry     = gtk_object_get_data(wobj, "qif_account_entry");
  retval->date_format_combo = gtk_object_get_data(wobj, "date_format_combo");
  retval->date_format_entry = gtk_object_get_data(wobj, "date_format_entry");
  retval->selected_file_list = gtk_object_get_data(wobj, "selected_file_list");
  retval->currency_picker = gtk_object_get_data(wobj, "currency_combo");
  retval->currency_entry = gtk_object_get_data(wobj, "currency_entry");
  retval->acct_list      = gtk_object_get_data(wobj, "account_page_list");
  retval->cat_list       = gtk_object_get_data(wobj, "category_page_list");

  retval->load_file_page = gtk_object_get_data(wobj, "load_file_page");
  retval->loaded_files_page = gtk_object_get_data(wobj, "loaded_files_page");
  retval->commodity_page  = gtk_object_get_data(wobj, "commodity_page");  
  retval->account_name_page  = gtk_object_get_data(wobj, "account_name_page"); 
  retval->date_format_page  = gtk_object_get_data(wobj, "date_format_page"); 
  retval->end_page  = gtk_object_get_data(wobj, "end_page");
  
  retval->pages = NULL;
  
  gtk_object_set_data(wobj, "qif_window_struct", retval);
  
  /* load the saved-state of the mappings from Quicken accounts and
   * categories to gnucash accounts */
  load_map_prefs = gh_eval_str("qif-import:load-map-prefs");
  lookup_option = gh_eval_str("gnc:lookup-global-option");
  lookup_value  = gh_eval_str("gnc:option-value");

  mapping_info = gh_call0(load_map_prefs);
  retval->gnc_acct_info    = gh_car(mapping_info);
  retval->acct_map_info    = gh_cadr(mapping_info);
  retval->cat_map_info     = gh_caddr(mapping_info);

  scm_protect_object(retval->imported_files);
  scm_protect_object(retval->selected_file);
  scm_protect_object(retval->gnc_acct_info);
  scm_protect_object(retval->cat_display_info);
  scm_protect_object(retval->cat_map_info);
  scm_protect_object(retval->acct_display_info);
  scm_protect_object(retval->acct_map_info);
  scm_protect_object(retval->stock_hash);
  
  /* set a default currency for new accounts */
  gnc_ui_update_commodity_picker(retval->currency_picker,
                                 GNC_COMMODITY_NS_ISO, 
                                 gnc_commodity_get_printname
                                 (gnc_locale_default_currency()));
  
  gtk_widget_show_all(retval->window);
  
  return retval;
}


/********************************************************************\
 * gnc_ui_qif_import_druid_destroy
 * close the QIF Import druid window
\********************************************************************/

void
gnc_ui_qif_import_druid_destroy (QIFImportWindow * window) {
  if(window) {
    gtk_widget_destroy(window->window);
  }
  
  scm_unprotect_object(window->imported_files);
  scm_unprotect_object(window->selected_file);
  scm_unprotect_object(window->gnc_acct_info);
  scm_unprotect_object(window->cat_display_info);
  scm_unprotect_object(window->cat_map_info);
  scm_unprotect_object(window->acct_display_info);
  scm_unprotect_object(window->acct_map_info);
  scm_unprotect_object(window->stock_hash);
  
  g_free(window);
}


/********************************************************************
 * gnc_ui_qif_import_select_file_cb
 * invoked when the "select file" button is clicked
 * this is just to pick a file name and reset-to-defaults all the 
 * fields describing how to parse the file.
 ********************************************************************/

void
gnc_ui_qif_import_select_file_cb(GtkButton * button,
                                 gpointer user_data) {
  GtkWidget       * druid = GTK_WIDGET(user_data);
  QIFImportWindow * wind = 
    gtk_object_get_data(GTK_OBJECT(druid), "qif_window_struct");
  
  const char * new_file_name;

  new_file_name = fileBox(_("Select QIF File"), "*.qif", "");

  /* set the filename entry for what was selected */
  if(wind->filename_entry) {
    if(new_file_name) {
      gtk_entry_set_text(GTK_ENTRY(wind->filename_entry),
                         new_file_name);
    }
    else {
      gtk_entry_set_text(GTK_ENTRY(wind->filename_entry),
                         "");
    }      
  }
}


/********************************************************************
 * gnc_ui_qif_import_load_file_cb
 * 
 * Invoked when the "next" button is clicked on the load file page.
 ********************************************************************/

gboolean
gnc_ui_qif_import_load_file_next_cb(GnomeDruidPage * page, 
                                    gpointer arg1,
                                    gpointer user_data) {
  
  GtkWidget       * druid = GTK_WIDGET(user_data);
  QIFImportWindow * wind = 
    gtk_object_get_data(GTK_OBJECT(druid), "qif_window_struct");
  
  char * path_to_load;
  char * error_string = NULL;
  
  GList * format_strings;
  GList * listit;

  SCM make_qif_file   = gh_eval_str("make-qif-file");
  SCM qif_file_load   = gh_eval_str("qif-file:read-file");
  SCM qif_file_parse  = gh_eval_str("qif-file:parse-fields");
  SCM qif_file_loaded = gh_eval_str("qif-dialog:qif-file-loaded?");
  SCM unload_qif_file = gh_eval_str("qif-dialog:unload-qif-file");
  SCM check_from_acct = gh_eval_str("qif-file:check-from-acct");
  SCM date_formats;
  SCM scm_filename;
  SCM scm_qiffile;
  SCM imported_files = SCM_EOL;
  SCM load_return, parse_return;

  int ask_date_format = FALSE;

  /* get the file name */ 
  path_to_load = gtk_entry_get_text(GTK_ENTRY(wind->filename_entry));

  /* check a few error conditions before we get started */
  if(strlen(path_to_load) == 0) {
    /* stay here if no file specified */
    gnc_error_dialog_parented(GTK_WINDOW(wind->window), 
                              _("Please select a file to load.\n"));
    return TRUE;
  }
  else if ((strlen(path_to_load) > 0) && access(path_to_load, R_OK) < 0) {
    /* stay here if bad file */
    gnc_error_dialog_parented(GTK_WINDOW(wind->window), 
                              _("File not found or read permission denied.\n"
                                "Please select another file."));
    return TRUE;
  }
  else {
    /* convert filename to scm */
    scm_filename   = gh_str02scm(path_to_load);
    imported_files = wind->imported_files;
    
    if(gh_call2(qif_file_loaded, scm_filename, wind->imported_files)
       == SCM_BOOL_T) {
      gnc_error_dialog_parented(GTK_WINDOW(wind->window),
                                _("That QIF file is already loaded.\n"
                                  "Please select another file."));
      return TRUE;
    }
    
    /* turn on the busy cursor */
    gnc_set_busy_cursor(NULL);
    
    /* create the <qif-file> object */
    scm_qiffile          = gh_call0(make_qif_file);    
    imported_files       = gh_cons(scm_qiffile, imported_files);    
    wind->selected_file  = scm_qiffile;
    
    scm_protect_object(wind->selected_file);      
    
    /* load the file */
    load_return = gh_call2(qif_file_load,  gh_car(imported_files),
                           scm_filename);
    
    /* a list returned is (#f error-message) for an error, 
     * (#t error-message) for a warning */
    if(gh_list_p(load_return) &&
       (gh_car(load_return) == SCM_BOOL_T)) {
      char *warn_str = gh_scm2newstr(gh_cadr(load_return), NULL);
      error_string = g_strdup_printf(_("QIF file load warning:\n%s"),
                                     warn_str);
      gnc_warning_dialog_parented(GTK_WIDGET(wind->window), error_string);
      g_free(error_string);
      free (warn_str);
    }

    /* check success of the file load */
    if((load_return != SCM_BOOL_T) &&
       (!gh_list_p(load_return) || 
        (gh_car(load_return) != SCM_BOOL_T))) {
      char *warn_str = gh_scm2newstr(gh_cadr(load_return), NULL);
      error_string = g_strdup_printf(_("QIF file load failed:\n%s"),
                                     warn_str);
      gnc_error_dialog_parented(GTK_WINDOW(wind->window), error_string);
      g_free(error_string);
      free (warn_str);

      imported_files = 
        gh_call2(unload_qif_file, scm_qiffile, imported_files);
            
      scm_unprotect_object(wind->imported_files);
      wind->imported_files = imported_files;
      scm_protect_object(wind->imported_files);

      return TRUE;
    }
    else {
      /* call the field parser */
      parse_return = gh_call1(qif_file_parse, gh_car(imported_files));
      gh_display(parse_return); gh_newline();

      /* warning means the date format is ambiguous. Set up the 
       * format selector page. */
      if(gh_list_p(parse_return) && 
         (gh_car(parse_return) == SCM_BOOL_T)) {
        date_formats   = gh_cadr(parse_return);
        format_strings = NULL;
        while(gh_list_p(date_formats) && !gh_null_p(date_formats)) {
          format_strings = 
            g_list_append(format_strings, 
                          gh_symbol2newstr(gh_car(date_formats), NULL));
          date_formats = gh_cdr(date_formats);
        }
        gtk_combo_set_popdown_strings(GTK_COMBO(wind->date_format_combo),
                                      format_strings);

        for(listit = format_strings; listit; listit=listit->next) {
          free(listit->data);
          listit->data = NULL;
        }
        g_list_free(format_strings);
        
        ask_date_format = TRUE;
      }

      if((parse_return != SCM_BOOL_T) &&
         (!gh_list_p(parse_return) ||
          (gh_car(parse_return) != SCM_BOOL_T))) {
        char *warn_str = gh_scm2newstr(gh_cadr(parse_return), NULL);
        error_string = g_strdup_printf(_("QIF file parse failed:\n%s"),
                                       warn_str);
        gnc_error_dialog_parented(GTK_WINDOW(wind->window), error_string);
        g_free(error_string);
        free(warn_str);

        imported_files = 
          gh_call2(unload_qif_file, scm_qiffile, imported_files);
        
        return TRUE;
      } 
    }
    
    scm_unprotect_object(wind->imported_files);
    wind->imported_files = imported_files;
    scm_protect_object(wind->imported_files);
    
    /* turn back the cursor */
    gnc_unset_busy_cursor(NULL);

    /* we're leaving the page, so clear the entry text */
    gtk_entry_set_text(GTK_ENTRY(wind->filename_entry), "");

    if(ask_date_format) {
      /* we need to get a date format, so go to the next page */
      return FALSE;
    }
    else if(gh_call1(check_from_acct, gh_car(imported_files)) != SCM_BOOL_T) {
      /* skip to the "ask account name" page */
      gnome_druid_set_page(GNOME_DRUID(wind->druid),
                           GNOME_DRUID_PAGE(wind->account_name_page));
      return TRUE;
    }
    else {
      /* skip ahead to the "loaded files" page */
      gnome_druid_set_page(GNOME_DRUID(wind->druid), 
                           GNOME_DRUID_PAGE(wind->loaded_files_page));
      
      return TRUE;      
    }
  }
  
  return FALSE;
}

gboolean
gnc_ui_qif_import_date_format_next_cb(GnomeDruidPage * page, 
                                      gpointer arg1,
                                      gpointer user_data) {
  GtkWidget       * druid = GTK_WIDGET(user_data);
  QIFImportWindow * wind = 
    gtk_object_get_data(GTK_OBJECT(druid), "qif_window_struct");  

  SCM  reparse_dates   = gh_eval_str("qif-file:reparse-dates");
  SCM  check_from_acct = gh_eval_str("qif-file:check-from-acct");
  SCM  format_sym = 
    gh_symbol2scm(gtk_entry_get_text(GTK_ENTRY(wind->date_format_entry)));
  
  gh_call2(reparse_dates, wind->selected_file, format_sym);
  
  if(gh_call1(check_from_acct, wind->selected_file) != SCM_BOOL_T) {
    return FALSE;
  }
  else {
    /* skip ahead to the "loaded files" page */
    gnome_druid_set_page(GNOME_DRUID(wind->druid), 
                         GNOME_DRUID_PAGE(wind->loaded_files_page));
    
    return TRUE;      
  }
}


/****************************************************************
 * gnc_ui_qif_import_select_loaded_file_cb
 * callback when a file is clicked in the "loaded files" page
 ****************************************************************/

void
gnc_ui_qif_import_select_loaded_file_cb(GtkCList   * list,
                                        int row, int column,
                                        GdkEvent   * event,
                                        gpointer  user_data) {
  GtkWidget       * druid = GTK_WIDGET(user_data);
  QIFImportWindow * wind = 
    gtk_object_get_data(GTK_OBJECT(druid), "qif_window_struct");  
  
  if(gh_list_p(wind->imported_files) && 
     (gh_length(wind->imported_files) > row)) {
    scm_unprotect_object(wind->selected_file);
    wind->selected_file = gh_list_ref(wind->imported_files,
                                      gh_int2scm(row));   
    scm_protect_object(wind->selected_file);
  } 
}

/********************************************************************
 * gnc_ui_qif_import_loaded_files_prepare_cb
 * 
 * Get the loaded files page ready for viewing
 ********************************************************************/

void
gnc_ui_qif_import_loaded_files_prepare_cb(GnomeDruidPage * page,
                                          gpointer arg1,
                                          gpointer user_data) {
  QIFImportWindow * wind = 
    gtk_object_get_data(GTK_OBJECT(user_data), "qif_window_struct");

  update_file_page(wind);
  gnome_druid_set_buttons_sensitive(GNOME_DRUID(wind->druid),
                                    FALSE, TRUE, TRUE); 
}


/********************************************************************
 * gnc_ui_qif_import_load_another_cb
 * Invoked when the "load another" button is clicked on the loaded
 * files page.
 ********************************************************************/

void
gnc_ui_qif_import_load_another_cb(GtkButton * button,
                                  gpointer user_data) {
  QIFImportWindow * wind = 
    gtk_object_get_data(GTK_OBJECT(user_data), "qif_window_struct");
  
  gnome_druid_set_page(GNOME_DRUID(wind->druid),
                       GNOME_DRUID_PAGE(wind->load_file_page));
  gnome_druid_set_buttons_sensitive(GNOME_DRUID(wind->druid),
                                    TRUE, TRUE, TRUE); 
}


/********************************************************************
 * gnc_ui_qif_import_unload_cb
 * Invoked when the "unload" button is clicked on the loaded files
 * page.
 ********************************************************************/

void
gnc_ui_qif_import_unload_file_cb(GtkButton * button,
                                 gpointer user_data) {
  QIFImportWindow * wind = 
    gtk_object_get_data(GTK_OBJECT(user_data), "qif_window_struct");
  
  SCM unload_qif_file = gh_eval_str("qif-dialog:unload-qif-file");
  SCM imported_files;
  
  if(wind->selected_file != SCM_BOOL_F) {
    imported_files = 
      gh_call2(unload_qif_file, wind->selected_file, wind->imported_files);
  
    scm_unprotect_object(wind->imported_files);
    wind->imported_files = imported_files;
    scm_protect_object(wind->imported_files);

    scm_unprotect_object(wind->selected_file);
    wind->selected_file = SCM_BOOL_F;
    scm_protect_object(wind->selected_file);
     
    update_file_page(wind);
  }
}


/********************************************************************
 * update_file_page
 * update the list of loaded files 
 ********************************************************************/

static void
update_file_page(QIFImportWindow * wind) {
  
  SCM       loaded_file_list = wind->imported_files;
  SCM       scm_qiffile = SCM_BOOL_F;
  SCM       qif_file_path;
  int       row;
  int       sel_item=-1;
  char      * row_text;

  /* clear the list */
  gtk_clist_clear(GTK_CLIST(wind->selected_file_list));
  qif_file_path = gh_eval_str("qif-file:path");
  
  /* iterate over all the imported files */
  gtk_clist_freeze(GTK_CLIST(wind->selected_file_list));
  
  while(!gh_null_p(loaded_file_list)) {  
    scm_qiffile = gh_car(loaded_file_list);
    row_text    = gh_scm2newstr(gh_call1(qif_file_path, scm_qiffile), NULL);

    row = gtk_clist_append(GTK_CLIST(wind->selected_file_list),
                           &row_text);
    free (row_text);

    if(scm_qiffile == wind->selected_file) {
      sel_item = row;
    }

    loaded_file_list = gh_cdr(loaded_file_list);
  }
  gtk_clist_thaw(GTK_CLIST(wind->selected_file_list));

  if(sel_item >= 0) {
    gtk_clist_select_row(GTK_CLIST(wind->selected_file_list), sel_item, 0);
  }  
}


/********************************************************************
 * gnc_ui_qif_import_default_acct_next_cb
 * 
 * Invoked when the "next" button is clicked on the default acct page.
 ********************************************************************/

gboolean
gnc_ui_qif_import_default_acct_next_cb(GnomeDruidPage * page,
                                       gpointer arg1,
                                       gpointer user_data) {
  QIFImportWindow * wind = 
    gtk_object_get_data(GTK_OBJECT(user_data), "qif_window_struct");

  char   * acct_name = gtk_entry_get_text(GTK_ENTRY(wind->acct_entry));
  SCM    fix_default = gh_eval_str("qif-import:fix-from-acct");
  SCM    scm_name;

  if(!acct_name || acct_name[0] == 0) {
    gnc_warning_dialog(_("You must enter an account name."));
    return TRUE;
  }
  else {
    scm_name = gh_str02scm(acct_name);
    gh_call2(fix_default, wind->selected_file, scm_name);
    gtk_entry_set_text(GTK_ENTRY(wind->acct_entry), "");
    return FALSE;
  }
}

/********************************************************************
 * gnc_ui_qif_import_default_acct_back_cb
 * 
 * Invoked when the "back" button is clicked on the default acct page.
 ********************************************************************/

gboolean
gnc_ui_qif_import_default_acct_back_cb(GnomeDruidPage * page,
                                       gpointer arg1,
                                       gpointer user_data) {
  QIFImportWindow * wind = 
    gtk_object_get_data(GTK_OBJECT(user_data), "qif_window_struct");
  SCM unload = gh_eval_str("qif-dialog:unload-qif-file");
  SCM files_list;

  files_list = gh_call2(unload, wind->selected_file, wind->imported_files);

  scm_unprotect_object(wind->imported_files);
  wind->imported_files = files_list;
  scm_protect_object(wind->imported_files);
  
  scm_unprotect_object(wind->selected_file);
  wind->imported_files = SCM_BOOL_F;
  scm_protect_object(wind->selected_file);
  
  return FALSE;
}


/****************************************************************
 * update_accounts_page 
 * Ask the Scheme side to guess some account translations , then show
 * the account name and suggested translation in the Accounts page
 * clist.
 ****************************************************************/

static void
update_accounts_page(QIFImportWindow * wind) {

  SCM  make_account_display = gh_eval_str("qif-dialog:make-account-display");
  SCM  get_qif_name         = gh_eval_str("qif-map-entry:qif-name");
  SCM  get_gnc_name         = gh_eval_str("qif-map-entry:gnc-name");
  SCM  get_new              = gh_eval_str("qif-map-entry:new-acct?");
  SCM  accts_left;
  int  sel_row=0;
  char * row_text[3];

  /* get the old selection row */
  sel_row = (GTK_CLIST(wind->acct_list))->focus_row;

  /* now get the list of strings to display in the clist widget */
  accts_left = gh_call3(make_account_display,
                        wind->imported_files,
                        wind->acct_map_info, 
                        wind->gnc_acct_info);

  scm_unprotect_object(wind->acct_display_info);
  wind->acct_display_info = accts_left;  
  scm_protect_object(wind->acct_display_info);
  
  /* clear the list */
  gtk_clist_clear(GTK_CLIST(wind->acct_list));

  /* update the text in the boxes */
  gtk_clist_freeze(GTK_CLIST(wind->acct_list));

  gtk_clist_set_column_justification(GTK_CLIST(wind->acct_list),
                                     0,
                                     GTK_JUSTIFY_RIGHT);
  gtk_clist_set_column_justification(GTK_CLIST(wind->acct_list),
                                     1,
                                     GTK_JUSTIFY_RIGHT);
  while(!gh_null_p(accts_left)) {
    row_text[0] = gh_scm2newstr(gh_call1(get_qif_name, gh_car(accts_left)),
                                NULL);
    row_text[1] = gh_scm2newstr(gh_call1(get_gnc_name, gh_car(accts_left)),
                                NULL);
    if(gh_call1(get_new, gh_car(accts_left)) == SCM_BOOL_T) {
      row_text[2] = "Y";
    }
    else {
      row_text[2] = "N";
    }
    
    gtk_clist_append(GTK_CLIST(wind->acct_list), row_text);
    accts_left = gh_cdr(accts_left);

    free(row_text[0]);
    free(row_text[1]);
  }

  gtk_clist_thaw(GTK_CLIST(wind->acct_list));
  
  /* move to the old selected row */
  (GTK_CLIST(wind->acct_list))->focus_row = sel_row;
  gtk_clist_moveto(GTK_CLIST(wind->acct_list), sel_row, 0, 0.0, 0.0);
  
}


/****************************************************************
 * update_categories_page 
 * Ask the Scheme side to guess some account translations , then show
 * the category name and suggested translation in the Accounts page
 * clist.
 ****************************************************************/

static void
update_categories_page(QIFImportWindow * wind) {
  SCM  make_category_display = gh_eval_str("qif-dialog:make-category-display");
  SCM  get_qif_name         = gh_eval_str("qif-map-entry:qif-name");
  SCM  get_gnc_name         = gh_eval_str("qif-map-entry:gnc-name");
  SCM  get_new              = gh_eval_str("qif-map-entry:new-acct?");
  SCM  cats_left;
  int  sel_row=0;
  char * row_text[3];

  /* get the old selection row */
  sel_row = (GTK_CLIST(wind->cat_list))->focus_row;

  /* now get the list of strings to display in the clist widget */
  cats_left = gh_call3(make_category_display, 
                       wind->imported_files,
                       wind->cat_map_info, 
                       wind->gnc_acct_info);
  
  scm_unprotect_object(wind->cat_display_info);
  wind->cat_display_info = cats_left;  
  scm_protect_object(wind->cat_display_info);
  
  /* clear the list */
  gtk_clist_clear(GTK_CLIST(wind->cat_list));

  /* update the text in the boxes */
  gtk_clist_freeze(GTK_CLIST(wind->cat_list));

  gtk_clist_set_column_justification(GTK_CLIST(wind->cat_list),
                                     0,
                                     GTK_JUSTIFY_RIGHT);
  gtk_clist_set_column_justification(GTK_CLIST(wind->cat_list),
                                     1,
                                     GTK_JUSTIFY_RIGHT);
  while(!gh_null_p(cats_left)) {
    row_text[0] = gh_scm2newstr(gh_call1(get_qif_name, gh_car(cats_left)),
                                NULL);
    row_text[1] = gh_scm2newstr(gh_call1(get_gnc_name, gh_car(cats_left)),
                                NULL);
    if(gh_call1(get_new, gh_car(cats_left)) == SCM_BOOL_T) {
      row_text[2] = "Y";
    }
    else {
      row_text[2] = "N";
    }
    
    gtk_clist_append(GTK_CLIST(wind->cat_list), row_text);
    cats_left = gh_cdr(cats_left);

    free (row_text[0]);
    free (row_text[1]);
  }

  gtk_clist_thaw(GTK_CLIST(wind->cat_list));

  /* move to the old selected row */
  (GTK_CLIST(wind->cat_list))->focus_row = sel_row;
  gtk_clist_moveto(GTK_CLIST(wind->cat_list), sel_row, 0, 0.0, 0.0);

}



/********************************************************************
 * gnc_ui_qif_import_account_line_select_cb
 * when an account is clicked for editing in the "map QIF accts to GNC"
 * page.
 ********************************************************************/

void
gnc_ui_qif_import_account_line_select_cb(GtkCList * clist, gint row,
                                         gint column, GdkEvent * event,
                                         gpointer user_data) {
  QIFImportWindow * wind = 
    gtk_object_get_data(GTK_OBJECT(user_data), "qif_window_struct");
  SCM   get_gnc_name = gh_eval_str("qif-map-entry:gnc-name");
  SCM   get_allowed_types = gh_eval_str("qif-map-entry:allowed-types");
  SCM   set_gnc_name = gh_eval_str("qif-map-entry:set-gnc-name!");
  SCM   set_allowed_types = gh_eval_str("qif-map-entry:set-allowed-types!");
  SCM   set_description = gh_eval_str("qif-map-entry:set-description!");
  SCM   selected_acct;
  SCM   new_acct_info;
  char  * gnc_name;
  int   initial_type; 

  /* find the <qif-map-entry> corresponding to the selected row */
  selected_acct = gh_list_ref(wind->acct_display_info,
                              gh_int2scm(row));

  /* call the account picker to get the new account */
  gnc_name     = gh_scm2newstr(gh_call1(get_gnc_name, selected_acct), NULL);
  initial_type = 
    gh_scm2int(gh_car(gh_call1(get_allowed_types, selected_acct)));

  new_acct_info = accountPickerBox(gnc_name, initial_type);

  if(gh_list_p(new_acct_info)) {
    gh_call2(set_gnc_name, selected_acct, gh_car(new_acct_info));
    gh_call2(set_allowed_types, selected_acct, 
             SCM_LIST1(gh_cadr(new_acct_info)));    
    gh_call2(set_description, selected_acct, gh_caddr(new_acct_info));    
  }

  update_accounts_page(wind);

  free (gnc_name);
}



/********************************************************************
 * gnc_ui_qif_import_category_line_select_cb
 * when a cat is clicked for editing in the "map QIF cats to GNC"
 * page.
 ********************************************************************/

void
gnc_ui_qif_import_category_line_select_cb(GtkCList * clist, gint row,
                                          gint column, GdkEvent * event,
                                          gpointer user_data) {
  QIFImportWindow * wind = 
    gtk_object_get_data(GTK_OBJECT(user_data), "qif_window_struct");
  SCM   get_gnc_name = gh_eval_str("qif-map-entry:gnc-name");
  SCM   get_allowed_types = gh_eval_str("qif-map-entry:allowed-types");
  SCM   set_gnc_name = gh_eval_str("qif-map-entry:set-gnc-name!");
  SCM   set_allowed_types = gh_eval_str("qif-map-entry:set-allowed-types!");
  SCM   set_description = gh_eval_str("qif-map-entry:set-description!");
  SCM   selected_acct;
  SCM   new_acct_info;
  char  * gnc_name;
  int   initial_type; 
  
  /* find the <qif-map-entry> corresponding to the selected row */
  selected_acct = gh_list_ref(wind->cat_display_info,
                              gh_int2scm(row));
  
  /* call the account picker to get the new account */
  gnc_name     = gh_scm2newstr(gh_call1(get_gnc_name, selected_acct), NULL);
  initial_type =
    gh_scm2int(gh_car(gh_call1(get_allowed_types, selected_acct)));

  new_acct_info = accountPickerBox(gnc_name, initial_type);

  if(gh_list_p(new_acct_info)) {
    gh_call2(set_gnc_name, selected_acct, gh_car(new_acct_info));
    gh_call2(set_allowed_types, selected_acct, 
             SCM_LIST1(gh_cadr(new_acct_info)));    
    gh_call2(set_description, selected_acct, gh_caddr(new_acct_info));    
  }

  update_categories_page(wind);

  free(gnc_name);
}


/********************************************************************
 * gnc_ui_qif_import_accounts_prepare_cb
 * 
 * Invoked when the "next" button is clicked on the loaded files page.
 ********************************************************************/

void
gnc_ui_qif_import_accounts_prepare_cb(GnomeDruidPage * page,
                                      gpointer arg1,
                                      gpointer user_data) {
  QIFImportWindow * wind = 
    gtk_object_get_data(GTK_OBJECT(user_data), "qif_window_struct");
  
  update_accounts_page(wind);
}


/********************************************************************
 * gnc_ui_qif_import_categories_prepare_cb
 * 
 * Invoked when the "next" button is clicked on the loaded files page.
 ********************************************************************/

void
gnc_ui_qif_import_categories_prepare_cb(GnomeDruidPage * page,
                                        gpointer arg1,
                                        gpointer user_data) {
  QIFImportWindow * wind = 
    gtk_object_get_data(GTK_OBJECT(user_data), "qif_window_struct");

  update_categories_page(wind);
}


/********************************************************************
 * gnc_ui_qif_import_categories_next_cb
 ********************************************************************/

gboolean
gnc_ui_qif_import_categories_next_cb(GnomeDruidPage * page,
                                     gpointer arg1,
                                     gpointer user_data) {
  QIFImportWindow * wind = 
    gtk_object_get_data(GTK_OBJECT(user_data), "qif_window_struct");

  SCM any_new   = gh_eval_str("qif-import:any-new-accts?");
  SCM any_stock = gh_eval_str("qif-import:any-new-stock-accts?");

  /* if any accounts are new, ask about the currency; else,
   * just skip that page */
  if((gh_call1(any_new, wind->acct_map_info) == SCM_BOOL_T) ||
     (gh_call1(any_new, wind->cat_map_info) == SCM_BOOL_T)) {
    return FALSE;
  }
  else {
    /* if we need to look at stocks, do that, otherwise go 
     * to the end page */
    if(gh_call1(any_stock, wind->acct_map_info) == SCM_BOOL_T) {
      gnome_druid_set_page(GNOME_DRUID(wind->druid),
                           GNOME_DRUID_PAGE(wind->commodity_page));
      return TRUE;
    }
    else {
      gnome_druid_set_page(GNOME_DRUID(wind->druid),
                           GNOME_DRUID_PAGE(wind->end_page));
      return TRUE;
    }
  }
}


/********************************************************************
 * gnc_ui_qif_import_currency_next_cb
 ********************************************************************/

gboolean
gnc_ui_qif_import_currency_next_cb(GnomeDruidPage * page,
                                   gpointer arg1,
                                   gpointer user_data) {
  QIFImportWindow * wind = 
    gtk_object_get_data(GTK_OBJECT(user_data), "qif_window_struct");
  SCM any_stock = gh_eval_str("qif-import:any-new-stock-accts?");
  
  if(gh_call1(any_stock, wind->acct_map_info) == SCM_BOOL_T) {
    return FALSE;
  }
  else {
    gnome_druid_set_page(GNOME_DRUID(wind->druid),
                         GNOME_DRUID_PAGE(wind->end_page));
    return TRUE;
  }
}

static gboolean
gnc_ui_qif_import_comm_check_cb(GnomeDruidPage * page,
                                gpointer arg1,
                                gpointer user_data) {
  QIFDruidPage    * qpage = 
    gtk_object_get_data(GTK_OBJECT(page), "page_struct");
  
  char * namespace = gtk_entry_get_text(GTK_ENTRY(qpage->new_type_entry));
  char * name      = gtk_entry_get_text(GTK_ENTRY(qpage->new_name_entry));
  char * mnemonic  = gtk_entry_get_text(GTK_ENTRY(qpage->new_mnemonic_entry));

  if(!namespace || (namespace[0] == 0)) {
    gnc_warning_dialog(_("You must enter a Type for the commodity."));
    return TRUE;
  }
  else if(!name || (name[0] == 0)) {
    gnc_warning_dialog(_("You must enter a name for the commodity."));
    return TRUE;
  }
  else if(!mnemonic || (mnemonic[0] == 0)) {
    gnc_warning_dialog(_("You must enter an abbreviation for the commodity."));
    return TRUE;
  }
  
  return FALSE;
}


/********************************************************************
 * gnc_ui_qif_import_commodity_prepare_cb
 * build a mapping of QIF stock name to a gnc_commodity 
 ********************************************************************/

void
gnc_ui_qif_import_commodity_prepare_cb(GnomeDruidPage * page,
                                       gpointer arg1,
                                       gpointer user_data) {
  QIFImportWindow * wind = 
    gtk_object_get_data(GTK_OBJECT(user_data), "qif_window_struct");

  SCM   hash_ref          = gh_eval_str("hash-ref");
  SCM   setup_stock_hash  = gh_eval_str("qif-import:setup-stock-hash");
  SCM   setup_info;
  SCM   stock_names;
  SCM   comm_ptr_token;

  gnc_commodity  * commodity;
  GnomeDruidPage * back_page = GNOME_DRUID_PAGE(wind->commodity_page);  
  QIFDruidPage   * new_page;
  
  /* only set up once */
  if(wind->pages) return;

  /* make a list of the new stocks that we need info about */
  setup_info       = gh_call1(setup_stock_hash, wind->acct_map_info);  
  stock_names      = gh_cadr(setup_info);

  scm_unprotect_object(wind->stock_hash);
  wind->stock_hash = gh_car(setup_info);
  scm_protect_object(wind->stock_hash);
  
  /* insert new pages, one for each stock */
  while(!gh_null_p(stock_names)) {
    comm_ptr_token = gh_call2(hash_ref, wind->stock_hash, gh_car(stock_names));
    commodity      = gw_wcp_get_ptr(comm_ptr_token);
    
    new_page = make_qif_druid_page(commodity);

    gtk_signal_connect(GTK_OBJECT(new_page->page), "next",
                       GTK_SIGNAL_FUNC(gnc_ui_qif_import_comm_check_cb),
                       wind->window);
    
    wind->pages = g_list_append(wind->pages, new_page);

    gnome_druid_insert_page(GNOME_DRUID(wind->druid),
                            back_page, 
                            GNOME_DRUID_PAGE(new_page->page));
    back_page = GNOME_DRUID_PAGE(new_page->page);
    
    stock_names = gh_cdr(stock_names);
    gtk_widget_show_all(new_page->page);
  }
}

static QIFDruidPage *
make_qif_druid_page(gnc_commodity * comm) {
  
  QIFDruidPage * retval = g_new0(QIFDruidPage, 1);
  GtkWidget * top_vbox;
  GtkWidget * info_label;
  GtkWidget * next_label;
  GtkWidget * temp;
  char      * title = NULL;
  GnomeDruidPageStandard * page;

  /* make the page widget */
  retval->page = gnome_druid_page_standard_new_with_vals("", NULL);
  retval->commodity = comm;
  gtk_object_set_data(GTK_OBJECT(retval->page),
                      "page_struct", (gpointer)retval);

  page   = GNOME_DRUID_PAGE_STANDARD(retval->page);
 
  /* save the old commodity name */
  title = g_strdup_printf(_("Enter information about \"%s\""),
                          gnc_commodity_get_mnemonic(comm));
  
  gnome_druid_page_standard_set_bg_color(page, & std_bg_color);  
  gnome_druid_page_standard_set_logo_bg_color(page, & std_logo_bg_color);
  gnome_druid_page_standard_set_title_color(page, & std_title_color);
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

  retval->new_type_combo = gtk_combo_new(); 
  gtk_box_pack_start(GTK_BOX(temp),
                     retval->new_type_combo, TRUE, TRUE, 0);

  info_label = gtk_label_new("");
  gtk_box_pack_start(GTK_BOX(temp), info_label, TRUE, TRUE, 0);

  retval->new_type_entry = (GTK_COMBO(retval->new_type_combo))->entry;

  gnc_ui_update_namespace_picker(retval->new_type_combo, 
                                 gnc_commodity_get_namespace(comm));
  
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

  next_label = gtk_label_new(_("Click \"Next\" to accept the information "
                               "and move on."));
  gtk_label_set_justify (GTK_LABEL(next_label), GTK_JUSTIFY_LEFT);
  gtk_box_pack_end(GTK_BOX(top_vbox), next_label, TRUE, TRUE, 0);

  
  return retval;
}


/****************************************************************
 * qif_import_finish_cb
 * do the work of actually translating QIF xtns to GNC xtns.
 ****************************************************************/

void
gnc_ui_qif_import_finish_cb(GnomeDruidPage * gpage, 
                            gpointer arg1, 
                            gpointer user_data) {
  
  SCM   save_map_prefs = gh_eval_str("qif-import:save-map-prefs");
  SCM   qif_to_gnc = gh_eval_str("qif-import:qif-to-gnc");
  QIFImportWindow * wind = 
    gtk_object_get_data(GTK_OBJECT(user_data), "qif_window_struct");
  QIFDruidPage * page;
  GList        * pageptr;
  char * mnemonic = NULL; 
  char * namespace = NULL;
  char * fullname = NULL;

  /* get the default currency */
  char * currname = gtk_entry_get_text(GTK_ENTRY(wind->currency_entry));
  
  /* busy cursor */
  gnc_set_busy_cursor(NULL);

  /* get any changes to the imported stocks */
  for(pageptr = wind->pages; pageptr; pageptr=pageptr->next) {
    page      = (QIFDruidPage *)(pageptr->data); 

    mnemonic  = gtk_entry_get_text(GTK_ENTRY(page->new_mnemonic_entry));
    namespace = gtk_entry_get_text(GTK_ENTRY(page->new_type_entry));
    fullname  = gtk_entry_get_text(GTK_ENTRY(page->new_name_entry));
    
    gnc_commodity_set_namespace(page->commodity, namespace);
    gnc_commodity_set_fullname(page->commodity, fullname);
    gnc_commodity_set_mnemonic(page->commodity, mnemonic);

    gnc_commodity_table_insert(gnc_engine_commodities(), page->commodity);
  }
  
  /* call a scheme function to do the work */
  gh_apply(qif_to_gnc, 
           SCM_LIST5(wind->imported_files,
                     wind->acct_map_info, 
                     wind->cat_map_info,
                     wind->stock_hash,
                     gh_str02scm(currname)));
  
  /* write out mapping info before destroying the window */
  gh_call2(save_map_prefs, wind->acct_map_info, wind->cat_map_info); 
  
  gnc_unset_busy_cursor(NULL);
  
  gnc_ui_qif_import_druid_destroy(wind);  
}


void
gnc_ui_qif_import_cancel_cb (GnomeDruid * druid, 
                             gpointer user_data) {
  
  GtkWidget       * d = GTK_WIDGET(user_data);
  QIFImportWindow * wind = 
    gtk_object_get_data(GTK_OBJECT(d), "qif_window_struct");
  
  gnc_ui_qif_import_druid_destroy(wind);
}

/* ======================================================== */

void
gncFileQIFImport (void) 
{
  /* pop up the QIF File Import dialog box */
  gnc_ui_qif_import_druid_make();
}
