/********************************************************************\
 * dialog-qif-import.c -- window for importing QIF files            *
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

#include "top-level.h"

#include <gnome.h>
#include <stdio.h>
#include <sys/time.h>
#include <unistd.h>

#include <guile/gh.h>

#include "dialog-qif-import.h"
#include "dialog-account-picker.h"
#include "window-help.h"
#include "messages.h"
#include "messages_i18n.h"
#include "gnome-top-level.h"
#include "ui-callbacks.h"

#include "Account.h"
#include "AccInfo.h"
#include "FileDialog.h"
#include "FileBox.h"
#include "dialog-utils.h"
#include "query-user.h"
#include "util.h"

struct _qifimportwindow
{
  /* on the Files tab */
  GtkWidget * dialog;
  GtkWidget * currency_entry;
  GtkWidget * filename_entry;
  GtkWidget * acct_auto_button;
  GtkWidget * acct_entry;
  GtkWidget * selected_file_list;

  /* on the Accounts tab */
  GtkWidget * acct_list;
  
  /* on the Categories tab */
  GtkWidget * cat_list;

  SCM       imported_files;
  SCM       selected_file;
  SCM       mapping_info; 
  SCM       cat_display_info;
  SCM       acct_display_info;
};


static void update_file_info(QIFImportWindow * win, SCM qiffile);
static void update_file_page(QIFImportWindow * win);
static void update_accounts_page(QIFImportWindow * win);
static void update_categories_page(QIFImportWindow * win);


/********************************************************************\
 * gnc_ui_qif_import_dialog_make(GtkWidget * parent) * build the
 * dialog.  For now, there can be only one (obhighlanderref)
\********************************************************************/

QIFImportWindow *
gnc_ui_qif_import_dialog_make() 
{
  QIFImportWindow * retval;
  
  SCM  load_map_prefs;
  SCM  mapping_info;
  SCM  lookup_option;
  SCM  lookup_value;
  SCM  default_currency;
  int  scm_strlen;

  retval = g_new0(QIFImportWindow, 1);
  
  retval->dialog = create_QIF_File_Import_Dialog();
  retval->imported_files = 
    SCM_EOL;
  retval->selected_file = SCM_BOOL_F;

  retval->currency_entry = 
    gtk_object_get_data(GTK_OBJECT(retval->dialog), "qif_currency_entry");
  retval->filename_entry =
    gtk_object_get_data(GTK_OBJECT(retval->dialog), "qif_filename_entry");
  retval->acct_auto_button = 
    gtk_object_get_data(GTK_OBJECT(retval->dialog), "qif_account_auto_check");
  retval->acct_entry =   
    gtk_object_get_data(GTK_OBJECT(retval->dialog), "qif_account_entry");
  retval->selected_file_list = 
    gtk_object_get_data(GTK_OBJECT(retval->dialog), "selected_file_list");

  retval->acct_list = 
    gtk_object_get_data(GTK_OBJECT(retval->dialog), "account_page_list");
  retval->cat_list = 
    gtk_object_get_data(GTK_OBJECT(retval->dialog), "category_page_list");
  

  gtk_object_set_data(GTK_OBJECT(retval->dialog),
                      "qif_window_struct", retval);

  /* load the saved-state of the mappings from Quicken accounts and
   * categories to gnucash accounts */
  load_map_prefs = gh_eval_str("qif-import:load-map-prefs");
  lookup_option = gh_eval_str("gnc:lookup-global-option");
  lookup_value  = gh_eval_str("gnc:option-value");

  mapping_info = gh_call0(load_map_prefs);
  retval->mapping_info = mapping_info;
  
  default_currency = gh_call1(lookup_value,
                              gh_call2(lookup_option,
                                       gh_str02scm("International"),
                                       gh_str02scm("Default Currency")));
  
  scm_protect_object(retval->imported_files);
  scm_protect_object(retval->mapping_info);

  /* set the currency entry to the GNC default currency */
  gtk_entry_set_text(GTK_ENTRY(retval->currency_entry), 
                     gh_scm2newstr(default_currency, &scm_strlen));

  gtk_widget_show_all(retval->dialog);
  
  return retval;
}


/********************************************************************\
 * gnc_ui_qif_import_dialog_destroy
 * close the QIF Import dialog window
\********************************************************************/

void
gnc_ui_qif_import_dialog_destroy (QIFImportWindow * window)
{
  if(window) {
    gnome_dialog_close(GNOME_DIALOG(window->dialog));
  }

  scm_unprotect_object(window->imported_files);
  scm_unprotect_object(window->selected_file);
  scm_unprotect_object(window->mapping_info);
  scm_unprotect_object(window->cat_display_info);
  scm_unprotect_object(window->acct_display_info);

  g_free(window);
}


/********************************************************************\
 * gnc_ui_qif_import_select_file_cb
 * invoked when the "select file" button is clicked
 * this is just to pick a file name and reset-to-defaults all the 
 * fields describing how to parse the file.
\********************************************************************/

void
gnc_ui_qif_import_select_file_cb(GtkButton * button,
                                 gpointer user_data) {
  GtkWidget       * dialog = GTK_WIDGET(user_data);
  QIFImportWindow * wind = 
    gtk_object_get_data(GTK_OBJECT(dialog), "qif_window_struct");
  
  const char * new_file_name;

  new_file_name = fileBox(_("Select QIF File"), "*.qif");

  if(new_file_name && (access(new_file_name, R_OK) == 0)) {

    /* set the filename entry for what was selected */
    if(wind->filename_entry) {
      gtk_entry_set_text(GTK_ENTRY(wind->filename_entry),
                         new_file_name);
    }

    /* the account should be auto-determined by default 
     * if the "opening balance" trick doesn't work "auto" will
     * use the file name as a guess */
    if(wind->acct_auto_button) {
      gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(wind->acct_auto_button),
                                   TRUE);
    }
    if(wind->acct_entry) {
      gtk_entry_set_text(GTK_ENTRY(wind->acct_entry),
                         "");
    }    
  }
}


/********************************************************************\
 * gnc_ui_qif_import_load_file_cb
 * 
 * Invoked when the "load file" button is clicked on the first page of
 * the QIF Import notebook.  Filename, currency, and
 * date format are read from the UI and passed to the Scheme side.
\********************************************************************/

void
gnc_ui_qif_import_load_file_cb(GtkButton * button, gpointer user_data) {

  GtkWidget       * dialog = GTK_WIDGET(user_data);
  QIFImportWindow * wind = 
    gtk_object_get_data(GTK_OBJECT(dialog), "qif_window_struct");
  
  char * path_to_load;
  char * qif_account;
  char * currency;
  char * error_string = NULL;
  
  struct timeval start, end;
  
  SCM make_qif_file, qif_file_load, qif_file_loaded, unload_qif_file;
  SCM qif_file_parse;
  SCM scm_filename, scm_currency, scm_qif_account;
  SCM scm_qiffile;
  SCM imported_files = SCM_EOL;
  SCM load_return, parse_return;

  /* get the UI elements */
  path_to_load = gtk_entry_get_text(GTK_ENTRY(wind->filename_entry));
  currency     = gtk_entry_get_text(GTK_ENTRY(wind->currency_entry));
  qif_account  = gtk_entry_get_text(GTK_ENTRY(wind->acct_entry));

  if(strlen(path_to_load) == 0) {
    gnc_error_dialog_parented(GTK_WINDOW(wind->dialog), 
                              _("You must specify a file to load."));
  }
  else if(strlen(currency) == 0) {
    gnc_error_dialog_parented(GTK_WINDOW(wind->dialog), 
                              _("You must specify a currency."));
  }
  else {
    /* find the make and load functions. */
    make_qif_file   = gh_eval_str("make-qif-file");
    qif_file_load   = gh_eval_str("qif-file:read-file");
    qif_file_parse  = gh_eval_str("qif-file:parse-fields");
    qif_file_loaded = gh_eval_str("qif-dialog:qif-file-loaded?");
    unload_qif_file = gh_eval_str("qif-dialog:unload-qif-file");
    
    if((!gh_procedure_p(make_qif_file)) ||
       (!gh_procedure_p(qif_file_load)) ||
       (!gh_procedure_p(qif_file_loaded))) {
      gnc_error_dialog_parented
        (GTK_WINDOW(wind->dialog),
         _("QIF File scheme code not loaded properly."));
    }
    else {
      /* convert args */
      scm_filename = gh_str02scm(path_to_load);
      scm_currency = gh_str02scm(currency);

      if(gtk_toggle_button_get_active
         (GTK_TOGGLE_BUTTON(wind->acct_auto_button))) {
        scm_qif_account = gh_symbol2scm("unknown");
      }
      else {
        scm_qif_account = gh_str02scm(qif_account);
      }

      imported_files = wind->imported_files;

      if(gh_call2(qif_file_loaded, scm_filename, wind->imported_files)
         == SCM_BOOL_T) {
        if(gnc_verify_dialog_parented
           (GTK_WINDOW(wind->dialog),
            _("QIF File already loaded. Reload with current settings?"), 
            TRUE)) {
          imported_files = 
            gh_call2(unload_qif_file, scm_filename, wind->imported_files);
        }
        else {
          return;
        }
      }

      /* turn on the busy cursor */
      gnc_set_busy_cursor(NULL);

      gettimeofday(&start, NULL);

      /* create the <qif-file> object */
      scm_qiffile = gh_apply(make_qif_file, 
                             SCM_LIST2(scm_qif_account, scm_currency));
      
      imported_files = 
        gh_cons(scm_qiffile, imported_files);

      wind->selected_file = scm_qiffile;

      scm_protect_object(wind->selected_file);      
      
      load_return = gh_call2(qif_file_load,  gh_car(imported_files),
                             scm_filename);
      
      /* a list returned is (#f error-message) for an error, 
       * (#t error-message) for a warning */
      if(gh_list_p(load_return) &&
         (gh_car(load_return) == SCM_BOOL_T)) {
        asprintf(&error_string,
                 QIF_LOAD_WARNING_FORMAT_MSG,
                 gh_scm2newstr(gh_cadr(load_return), NULL));
        gnc_warning_dialog_parented(GTK_WIDGET(wind->dialog), error_string);
      }      
        
      if((load_return != SCM_BOOL_T) &&
         (!gh_list_p(load_return) || 
          (gh_car(load_return) != SCM_BOOL_T))) {
        asprintf(&error_string,
                 QIF_LOAD_FAILED_FORMAT_MSG,
                 gh_scm2newstr(gh_cadr(load_return), NULL));
        gnc_error_dialog_parented(GTK_WINDOW(wind->dialog), error_string);
        
        imported_files = 
          gh_call2(unload_qif_file, scm_filename, imported_files);
      }
      else {
        parse_return = gh_call1(qif_file_parse, gh_car(imported_files));
        
        if(gh_list_p(parse_return) && 
           (gh_car(parse_return) == SCM_BOOL_T)) {
          asprintf(&error_string,
                   QIF_PARSE_WARNING_FORMAT_MSG,
                   gh_scm2newstr(gh_cadr(parse_return), NULL));
          gnc_warning_dialog_parented(GTK_WIDGET(wind->dialog), error_string);
        }
        if((parse_return != SCM_BOOL_T) &&
           (!gh_list_p(parse_return) ||
            (gh_car(parse_return) != SCM_BOOL_T))) {
          asprintf(&error_string,
                   QIF_PARSE_FAILED_FORMAT_MSG,
                   gh_scm2newstr(gh_cadr(parse_return), NULL));
          gnc_error_dialog_parented(GTK_WINDOW(wind->dialog), error_string);
          imported_files = 
            gh_call2(unload_qif_file, scm_filename, imported_files);
        }
      }
      
      wind->imported_files = imported_files;
      scm_protect_object(wind->imported_files);
      
      gettimeofday(&end, NULL);
#if 0
      printf("QIF file load took %f ms total.\n", 
             1000.0*(end.tv_sec - start.tv_sec) +
             .001*(end.tv_usec - start.tv_usec));
#endif      
      gettimeofday(&start, NULL);
      
      /* now update the Accounts and Categories pages in the notebook */
      update_file_page(wind);
      update_accounts_page(wind); 
      update_categories_page(wind);

      gettimeofday(&end, NULL);

#if 0
      printf("QIF Category/account tab update took %f ms.\n", 
             1000.0*(end.tv_sec - start.tv_sec) +
             .001*(end.tv_usec - start.tv_usec));
#endif

      gettimeofday(&end, NULL);

      /* turn back the cursor */
      gnc_unset_busy_cursor(NULL);
    }
  }
}


void
gnc_ui_qif_import_select_loaded_file_cb(GtkList   * list,
                                        GtkWidget * widget,
                                        gpointer  user_data) {
  GtkWidget       * dialog = GTK_WIDGET(user_data);
  QIFImportWindow * wind = 
    gtk_object_get_data(GTK_OBJECT(dialog), "qif_window_struct");  
  int file_index = 
    GPOINTER_TO_INT(gtk_object_get_data(GTK_OBJECT(widget), "file_index"));
  
  wind->selected_file = gh_list_ref(wind->imported_files, 
                                    gh_int2scm(file_index));
  
  scm_protect_object(wind->selected_file);
  update_file_info(wind, wind->selected_file);
}


/****************************************************************\
 * qif_import_ok_cb
 * do the work of actually translating QIF xtns to GNC xtns.
\****************************************************************/

void
gnc_ui_qif_import_ok_cb(GtkButton * button, gpointer user_data) {

  SCM  save_map_prefs = gh_eval_str("qif-import:save-map-prefs");
  SCM  qif_to_gnc = gh_eval_str("qif-import:qif-to-gnc");

  QIFImportWindow * wind = 
    gtk_object_get_data(GTK_OBJECT(user_data), "qif_window_struct");
 
  /* busy cursor */
  gnc_set_busy_cursor(NULL);
  
  /* call a scheme function to do the work */
  gh_call2(qif_to_gnc, wind->imported_files, 
           wind->mapping_info);
  
  /* write out mapping info before destroying the window */
  gh_call1(save_map_prefs, wind->mapping_info);
  
  gnc_unset_busy_cursor(NULL);
  
  gnc_ui_qif_import_dialog_destroy(wind);
  wind = NULL;
}


void
gnc_ui_qif_import_cancel_cb (GtkButton * button, gpointer user_data) {

  GtkWidget       * dialog = GTK_WIDGET(user_data);
  QIFImportWindow * wind = 
    gtk_object_get_data(GTK_OBJECT(dialog), "qif_window_struct");
 
  gnc_ui_qif_import_dialog_destroy(wind);
}


void
gnc_ui_qif_import_help_cb (GtkButton * button, gpointer user_data) {
  
  helpWindow(NULL, HELP_STR, HH_QIFIMPORT);
}

void
gnc_ui_qif_import_account_line_select_cb(GtkCList * clist, gint row,
                                         gint column, GdkEvent * event,
                                         gpointer user_data) {
  QIFImportWindow * wind = 
    gtk_object_get_data(GTK_OBJECT(user_data), "qif_window_struct");
  char  * gnc_acct_text = NULL;
  char  * qif_acct_text = NULL;
 
  int   initial_type;
  SCM   scm_acct;
  SCM   mapping_info;
  SCM   scm_hash_ref = gh_eval_str("hash-ref");

  gtk_clist_get_text(GTK_CLIST(clist), row, 0, &qif_acct_text);
  gtk_clist_get_text(GTK_CLIST(clist), row, 2, &gnc_acct_text);

  mapping_info = gh_call2(scm_hash_ref, gh_cadr(wind->mapping_info),
                          gh_str02scm(qif_acct_text));
  initial_type = gh_scm2int(gh_list_ref(mapping_info, gh_int2scm(2)));

  scm_acct = accountPickerBox(gnc_acct_text, initial_type);
  
  if(gh_list_p(scm_acct)) {
    scm_list_set_x(mapping_info, gh_int2scm(1), gh_car(scm_acct));
    scm_list_set_x(mapping_info, gh_int2scm(2), gh_cadr(scm_acct));
    scm_list_set_x(mapping_info, gh_int2scm(5), gh_caddr(scm_acct));
    
    gtk_clist_set_text(GTK_CLIST(clist), row, 2, 
                       gh_scm2newstr(gh_car(scm_acct), NULL));
    gtk_clist_set_text(GTK_CLIST(clist), row, 3,
                       xaccAccountTypeEnumAsString
                       (gh_scm2int(gh_cadr(scm_acct))));
  }
}

void
gnc_ui_qif_import_category_line_select_cb(GtkCList * clist, gint row,
                                          gint column, GdkEvent * event,
                                          gpointer user_data) {
  QIFImportWindow * wind = 
    gtk_object_get_data(GTK_OBJECT(user_data), "qif_window_struct");
  char  * gnc_acct_text = NULL;
  char  * qif_acct_text = NULL;
  
  int   initial_type;
  SCM   scm_acct;
  SCM   mapping_info;
  SCM   scm_hash_ref = gh_eval_str("hash-ref");
  
  gtk_clist_get_text(GTK_CLIST(clist), row, 0, &qif_acct_text);
  gtk_clist_get_text(GTK_CLIST(clist), row, 2, &gnc_acct_text);

  mapping_info = gh_call2(scm_hash_ref, gh_caddr(wind->mapping_info),
                          gh_str02scm(qif_acct_text));
  initial_type = gh_scm2int(gh_list_ref(mapping_info, gh_int2scm(2)));
  
  scm_acct = accountPickerBox(gnc_acct_text, initial_type);
  
  if(gh_list_p(scm_acct)) {
    scm_list_set_x(mapping_info, gh_int2scm(1), gh_car(scm_acct));
    scm_list_set_x(mapping_info, gh_int2scm(2), gh_cadr(scm_acct));
    scm_list_set_x(mapping_info, gh_int2scm(5), gh_caddr(scm_acct));
    
    gtk_clist_set_text(GTK_CLIST(clist), row, 2, 
                       gh_scm2newstr(gh_car(scm_acct), NULL));
    gtk_clist_set_text(GTK_CLIST(clist), row, 3,
                       xaccAccountTypeEnumAsString
                       (gh_scm2int(gh_cadr(scm_acct))));
  }
}


/********************************************************************\
 * update_file_page
 * update the left-side list and the right-side info. 
\********************************************************************/

static void
update_file_page(QIFImportWindow * wind) {
  
  GtkWidget * new_list_item;
  GList     * new_loaded_file;
  SCM       loaded_file_list = wind->imported_files;
  SCM       scm_qiffile;
  SCM       qif_file_path;
  int       path_strlen;
  int       scm_file_index = 0;

  /* clear the list */
  gtk_list_remove_items(GTK_LIST(wind->selected_file_list),
                        gtk_container_children
                        (GTK_CONTAINER(wind->selected_file_list)));
  qif_file_path = gh_eval_str("qif-file:path");
  
  /* iterate over all the imported files */
  while(!gh_null_p(loaded_file_list)) {  
    scm_qiffile = gh_car(loaded_file_list);
    
    /* make a list item with the SCM object attached as data */
    new_list_item = 
      gtk_list_item_new_with_label(gh_scm2newstr(gh_call1(qif_file_path,
                                                          scm_qiffile),
                                                 &path_strlen));  
    gtk_object_set_data(GTK_OBJECT(new_list_item),
                        "file_index", GINT_TO_POINTER(scm_file_index));
    scm_file_index++;

    /* tack it on to the displayed list */
    new_loaded_file = g_list_alloc();
    new_loaded_file->next = NULL;
    new_loaded_file->prev = NULL;
    gtk_widget_show(new_list_item);
    new_loaded_file->data = new_list_item;
    
    /* now add the file to the loaded-files list */
    gtk_list_append_items(GTK_LIST(wind->selected_file_list), 
                          new_loaded_file);      
    
    /* select_child will update the file info */
    if(scm_qiffile == wind->selected_file) {
      gtk_list_select_child(GTK_LIST(wind->selected_file_list), new_list_item);
    }
    
    loaded_file_list = gh_cdr(loaded_file_list);
  }
}


/********************************************************************\
 * update_file_info
 * 
 * Invoked when a file is loaded or the name of a loaded file is
 * clicked in the loaded files list.  This causes the pickers and text
 * boxes on the right side to be updated to reflect the actual values
 * used or detected in loading the files.
\********************************************************************/

static void
update_file_info(QIFImportWindow * wind, SCM qif_file) {

  SCM   qif_file_currency;
  SCM   qif_file_path;
  SCM   qif_file_account;
  SCM   scm_currency;
  SCM   scm_qif_account;
  SCM   scm_qif_path;

  int scm_strlen;

  /* look up the <qif-file> methods */
  qif_file_currency     = gh_eval_str("qif-file:currency");
  qif_file_path         = gh_eval_str("qif-file:path");
  qif_file_account      = gh_eval_str("qif-file:default-account");
  
  /* make sure the methods are loaded */
  if((!gh_procedure_p(qif_file_currency)) ||
     (!gh_procedure_p(qif_file_account)) ||
     (!gh_procedure_p(qif_file_path))) {
    gnc_error_dialog_parented(GTK_WINDOW(wind->dialog),
                              _("QIF File scheme code not loaded properly."));
    return;
  }
  else {
    /* get the currency etc from the Scheme side */
    scm_currency      = gh_call1(qif_file_currency,
                                 qif_file);
    scm_qif_path      = gh_call1(qif_file_path,
                                 qif_file);
    scm_qif_account   = gh_call1(qif_file_account,
                                 qif_file);
    
    /* put the data in the info fields */
    gtk_entry_set_text(GTK_ENTRY(wind->filename_entry),
                       gh_scm2newstr(scm_qif_path, &scm_strlen));
    gtk_entry_set_text(GTK_ENTRY(wind->currency_entry),
                       gh_scm2newstr(scm_currency, &scm_strlen));
    
    /* account is weird. after loading, either we know it or we don't 
     * but in either case the auto should be off. */
    gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(wind->acct_auto_button), 
                                 FALSE);
    gtk_entry_set_text(GTK_ENTRY(wind->acct_entry),
                       gh_scm2newstr(scm_qif_account, &scm_strlen));    
  }
}



/****************************************************************\
 * update_accounts_page 
 * Ask the Scheme side to guess some account translations , then 
 * show the filename, account name, and suggested translation in 
 * the Accounts page clist. 
\****************************************************************/

static void
update_accounts_page(QIFImportWindow * wind) {

  SCM        make_account_display;
  SCM        strings_left;
  SCM        display_info;
  SCM        hash_data;
  SCM        hash_set;
  int        xtn_count;
  char       * xtn_count_string;
  char       * qif_acct_name;
  int        row;
  int        scheme_strlen;
  char       * row_text[4];

  make_account_display = gh_eval_str("qif-dialog:make-account-display");
  hash_set             = gh_eval_str("hash-set!");
  
  /* make sure we found the procedure */
  if(!gh_procedure_p(make_account_display)) {
    gnc_error_dialog_parented(GTK_WINDOW(wind->dialog),
                              _("QIF File scheme code not loaded properly."));
    return;
  }

  /* transfer the existing info from the account picker to 
   * the mapping info hash table  */
  for(row=0; row < GTK_CLIST(wind->acct_list)->rows; row++) {
    gtk_clist_get_text(GTK_CLIST(wind->acct_list), row, 0, &qif_acct_name);
    
    hash_data = (SCM)gtk_clist_get_row_data(GTK_CLIST(wind->acct_list), row);
    gh_call3(hash_set, gh_cadr(wind->mapping_info), 
             gh_str02scm(qif_acct_name), 
             hash_data);
  }
  
  /* now get the list of strings to display in the clist widget */
  /*  gnc_unprotect_object(wind->acct_display_info); */
  display_info = gh_call2(make_account_display, 
                          wind->imported_files,
                          wind->mapping_info);
  wind->acct_display_info = display_info;

  scm_protect_object(wind->acct_display_info);

  strings_left = wind->acct_display_info;
  if(!gh_list_p(strings_left)) {
    gnc_error_dialog_parented
      (GTK_WINDOW(wind->dialog),
       _("Something is very wrong with QIF Importing."));
    return;
  }

  /* clear the list */
  gtk_clist_clear(GTK_CLIST(wind->acct_list));

  /* update the text in the boxes */
  gtk_clist_freeze(GTK_CLIST(wind->acct_list));

  gtk_clist_set_column_justification(GTK_CLIST(wind->acct_list),
                                     0,
                                     GTK_JUSTIFY_RIGHT);
  row = 0;
  while(!gh_null_p(strings_left)) {
    row_text[0] = gh_scm2newstr(gh_caar(strings_left), &scheme_strlen);
    xtn_count   = gh_scm2int(gh_list_ref(gh_car(strings_left),
                                         gh_int2scm(4)));
    asprintf(&xtn_count_string, "%d", xtn_count);
    row_text[1] = xtn_count_string;
    row_text[2] = gh_scm2newstr(gh_cadr(gh_car(strings_left)), 
                                &scheme_strlen);    
    row_text[3] = 
      xaccAccountTypeEnumAsString(gh_scm2int
                                  (gh_caddr(gh_car(strings_left))));
    
    gtk_clist_append(GTK_CLIST(wind->acct_list), row_text);

    gtk_clist_set_row_data(GTK_CLIST(wind->acct_list), row,
                           GINT_TO_POINTER((gh_car(strings_left))));
    
    scm_protect_object(gh_car(strings_left));
    
    strings_left = gh_cdr(strings_left);
    row++;
  }
  

  gtk_clist_thaw(GTK_CLIST(wind->acct_list));
}


/****************************************************************\
 * update_categories_page 
 * Ask the Scheme side to guess some account translations , then 
 * show the filename, account name, and suggested translation in 
 * the Accounts page clist. 
\****************************************************************/

static void
update_categories_page(QIFImportWindow * wind) {

  SCM        make_category_display;
  SCM        strings_left;
  SCM        display_info;
  SCM        hash_data;
  SCM        hash_set;
  int        xtn_count;
  char       * xtn_count_string;
  char       * qif_cat_name;
  int        row;
  int        scheme_strlen;
  char       * row_text[4];

  make_category_display = gh_eval_str("qif-dialog:make-category-display");
  hash_set              = gh_eval_str("hash-set!");

  /* make sure we found the procedure */
  if(!gh_procedure_p(make_category_display)) {
    gnc_error_dialog_parented(GTK_WINDOW(wind->dialog),
                              _("QIF File scheme code not loaded properly."));
    return;
  }

  /* get the existing mappings from the display */
  for(row=0; row < GTK_CLIST(wind->cat_list)->rows; row++) {
    gtk_clist_get_text(GTK_CLIST(wind->cat_list), row, 0, &qif_cat_name);
    
    hash_data = (SCM)gtk_clist_get_row_data(GTK_CLIST(wind->cat_list), row);
    gh_call3(hash_set, gh_caddr(wind->mapping_info), 
             gh_str02scm(qif_cat_name), 
             hash_data);
  }
  
  /* now get the list of strings to display in the clist widget */
  /*   gnc_unprotect_object(wind->cat_display_info); */
  display_info = gh_call2(make_category_display, 
                          wind->imported_files,
                          wind->mapping_info);
  wind->cat_display_info = display_info;

  scm_protect_object(wind->cat_display_info);

  strings_left = wind->cat_display_info;
  if(!gh_list_p(strings_left)) {
    gnc_error_dialog_parented
      (GTK_WINDOW(wind->dialog),
       _("Something is very wrong with QIF Importing."));
    return;
  }

  /* clear the list */
  gtk_clist_clear(GTK_CLIST(wind->cat_list));

  /* update the text in the boxes */
  gtk_clist_freeze(GTK_CLIST(wind->cat_list));

  gtk_clist_set_column_justification(GTK_CLIST(wind->cat_list),
                                     0,
                                     GTK_JUSTIFY_RIGHT);
  row = 0;
  while(!gh_null_p(strings_left)) {
    row_text[0] = gh_scm2newstr(gh_caar(strings_left), &scheme_strlen);
    xtn_count   = gh_scm2int(gh_list_ref(gh_car(strings_left), 
                                         gh_int2scm(4)));
    asprintf(&xtn_count_string, "%d", xtn_count);
    row_text[1] = xtn_count_string;
    row_text[2] = gh_scm2newstr(gh_cadr(gh_car(strings_left)), 
                                &scheme_strlen);
    row_text[3] = xaccAccountTypeEnumAsString(gh_scm2int
                                          (gh_caddr(gh_car(strings_left))));

    gtk_clist_append(GTK_CLIST(wind->cat_list), row_text);
    gtk_clist_set_row_data(GTK_CLIST(wind->cat_list), row,
                           GINT_TO_POINTER(gh_car(strings_left)));
    scm_protect_object(gh_car(strings_left));
    strings_left = gh_cdr(strings_left);
    row++;
  }
  
  gtk_clist_thaw(GTK_CLIST(wind->cat_list));
}
