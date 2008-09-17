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
#include "dialog-progress.h"
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
#include "gnc-main-window.h"
#include "gnc-plugin-page-account-tree.h"
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
  ACCOUNT_COL_ELLIPSIZE,
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

  /* Widgets on the file selection page. */
  GtkWidget * filename_entry;

  /* File loading progress page. */
  GtkWidget * load_pause;
  GtkWidget * load_log;
  GNCProgressDialog *load_progress;

  /* Widgets on the default account page. */
  GtkWidget * acct_entry;

  /* Widgets on the date format page. */
  GtkWidget * date_format_combo;

  /* Widgets on the files loaded page. */
  GtkWidget * selected_file_view;

  /* Widgets on the account matching page. */
  GtkWidget * acct_view;
  GtkWidget * acct_view_count;
  GtkWidget * acct_view_btn;

  /* Widgets on the category matching page. */
  GtkWidget * cat_view;
  GtkWidget * cat_view_count;
  GtkWidget * cat_view_btn;

  /* Widgets on the memo matching page. */
  GtkWidget * memo_view;
  GtkWidget * memo_view_count;
  GtkWidget * memo_view_btn;

  /* Widgets on the currency page. */
  GtkWidget * currency_picker;

  /* Conversion progress page. */
  GtkWidget * convert_pause;
  GtkWidget * convert_log;
  GNCProgressDialog *convert_progress;

  /* Widgets on the duplicates page. */
  GtkWidget * new_transaction_view;
  GtkWidget * old_transaction_view;

  GList     * pre_comm_pages;
  GList     * commodity_pages;
  GList     * post_comm_pages;
  GList     * doc_pages;

  gboolean  show_doc_pages;
  gboolean  ask_date_format;
  gboolean  busy;

  SCM       imported_files;
  SCM       selected_file;

  SCM       acct_map_info;
  SCM       acct_display_info;

  SCM       cat_map_info;
  SCM       cat_display_info;

  SCM       memo_map_info;
  SCM       memo_display_info;

  SCM       gnc_acct_info;
  SCM       security_hash;
  SCM       security_prefs;
  SCM       new_securities;
  GList   * new_namespaces;
  SCM       ticker_map;

  SCM       imported_account_tree;
  SCM       match_transactions;
  int       selected_transaction;
};

struct _qifdruidpage {
  GtkWidget     *page;
  GtkWidget     *namespace_combo;
  GtkWidget     *name_entry;
  GtkWidget     *mnemonic_entry;
  gnc_commodity *commodity;
  SCM            hash_key;
};

typedef struct _qifdruidpage QIFDruidPage;

static GdkColor std_bg_color = { 0, 39835, 49087, 40092 };
static GdkColor std_logo_bg_color = { 0, 65535, 65535, 65535 };
static GdkColor std_title_color =  { 0, 65535, 65535, 65535 };

#define NUM_PRE_PAGES 14
#define NUM_POST_PAGES 4
#define NUM_DOC_PAGES  6

static GnomeDruidPage *
get_named_page(QIFImportWindow * w, const char * name)
{
  return GNOME_DRUID_PAGE(gnc_glade_lookup_widget(w->window, name));
}


/****************************************************************
 * update_account_picker_page
 *
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
                       ACCOUNT_COL_INDEX,     row++,
                       ACCOUNT_COL_QIF_NAME,  qif_name,
                       ACCOUNT_COL_GNC_NAME,  gnc_name,
                       ACCOUNT_COL_NEW,       checked,
                       ACCOUNT_COL_ELLIPSIZE, PANGO_ELLIPSIZE_START,
                       -1);
    accts_left = SCM_CDR(accts_left);
  }

  /* move to the old selected row */
  prev_row = GPOINTER_TO_INT(g_object_get_data(G_OBJECT(store), PREV_ROW));
  if (prev_row != -1) {
    selection = gtk_tree_view_get_selection(GTK_TREE_VIEW(view));
    path = gtk_tree_path_new_from_indices(prev_row, -1);
    gtk_tree_selection_select_path(selection, path);
    gtk_tree_path_free(path);
  }
}


/****************************************************************
 * update_account_page
 *
 * update the QIF account -> GNC Account picker
 ****************************************************************/

static void
update_account_page(QIFImportWindow * wind)
{

  SCM  make_account_display = scm_c_eval_string("qif-dialog:make-account-display");

  update_account_picker_page(wind, make_account_display, wind->acct_view,
                             wind->acct_map_info, &(wind->acct_display_info));
}


/****************************************************************
 * update_category_page
 *
 * update the QIF category -> GNC Account picker
 ****************************************************************/

static void
update_category_page(QIFImportWindow * wind)
{
  SCM  make_category_display = scm_c_eval_string("qif-dialog:make-category-display");

  update_account_picker_page(wind, make_category_display, wind->cat_view,
                             wind->cat_map_info, &(wind->cat_display_info));
}


/****************************************************************
 * update_memo_page
 *
 * update the QIF memo -> GNC Account picker
 ****************************************************************/

static void
update_memo_page(QIFImportWindow * wind)
{
  SCM  make_memo_display = scm_c_eval_string("qif-dialog:make-memo-display");

  update_account_picker_page(wind, make_memo_display, wind->memo_view,
                             wind->memo_map_info, &(wind->memo_display_info));
}


/****************************************************************
 * gnc_ui_qif_import_commodity_destroy
 *
 * This function destroys any commodity pages.
 ****************************************************************/

static void
gnc_ui_qif_import_commodity_destroy(QIFImportWindow * wind)
{
  GList          *pageptr;
  GnomeDruidPage *gtkpage;
  QIFDruidPage   *page;

  for (pageptr = wind->commodity_pages; pageptr; pageptr=pageptr->next)
  {
    gtkpage   = GNOME_DRUID_PAGE(pageptr->data);
    page      = g_object_get_data(G_OBJECT(gtkpage), "page_struct");

    /* Unprotect the Scheme hash key. */
    scm_gc_unprotect_object(page->hash_key);

    /* Free the memory allocated for the page's struct. */
    g_free(page);
  }

  /* Free the list of pages. */
  g_list_free(wind->commodity_pages);
  wind->commodity_pages = NULL;
}


/********************************************************************\
 * gnc_ui_qif_import_druid_destroy
 * close the QIF Import druid window
\********************************************************************/

void
gnc_ui_qif_import_druid_destroy(QIFImportWindow * wind)
{
  if (!wind)
    return;

  /* Destroy the progress dialog helpers. */
  gnc_progress_dialog_destroy(wind->load_progress);

  /* Destroy any commodity pages. */
  gnc_ui_qif_import_commodity_destroy(wind);

  gnc_unregister_gui_component_by_data(DRUID_QIF_IMPORT_CM_CLASS, wind);

  gtk_widget_destroy(wind->window);

  scm_gc_unprotect_object(wind->imported_files);
  scm_gc_unprotect_object(wind->selected_file);
  scm_gc_unprotect_object(wind->gnc_acct_info);
  scm_gc_unprotect_object(wind->cat_display_info);
  scm_gc_unprotect_object(wind->cat_map_info);
  scm_gc_unprotect_object(wind->memo_display_info);
  scm_gc_unprotect_object(wind->memo_map_info);
  scm_gc_unprotect_object(wind->acct_display_info);
  scm_gc_unprotect_object(wind->acct_map_info);
  scm_gc_unprotect_object(wind->security_hash);
  scm_gc_unprotect_object(wind->security_prefs);
  scm_gc_unprotect_object(wind->new_securities);
  scm_gc_unprotect_object(wind->ticker_map);
  scm_gc_unprotect_object(wind->imported_account_tree);
  scm_gc_unprotect_object(wind->match_transactions);

  g_free(wind);
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
  if ((current = g_list_find(wind->pre_comm_pages, page)) == NULL) {
    if ((current = g_list_find(wind->commodity_pages, page)) == NULL) {
      if ((current = g_list_find(wind->post_comm_pages, page)) == NULL) {
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
         (wind->new_securities == SCM_BOOL_F &&
          GNOME_DRUID_PAGE(next->data) == get_named_page(wind, "commodity_doc_page"))) {
    if (next && next->next) {
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

  if (next)
    return (GtkWidget *)next->data;

  return NULL;
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
  if ((current = g_list_find(wind->pre_comm_pages, page)) == NULL) {
    if ((current = g_list_find(wind->commodity_pages, page)) == NULL) {
      if ((current = g_list_find(wind->post_comm_pages, page)) == NULL) {
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
         (wind->new_securities == SCM_BOOL_F &&
          GNOME_DRUID_PAGE(prev->data) == get_named_page(wind, "commodity_doc_page"))) {
    /* We're either out of pages for this stage, or we've reached
     * an optional doc page that shouldn't be shown. */

    if (prev && prev->prev) {
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
        if (wind->new_securities != SCM_BOOL_F) {
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

  if (prev)
    return (GtkWidget *)prev->data;

  return NULL;
}


/********************************************************************
 * gnc_ui_qif_import_generic_next_cb
 *
 * Display the next druid page.
 ********************************************************************/

static gboolean
gnc_ui_qif_import_generic_next_cb(GnomeDruidPage * page, gpointer arg1,
                                  gpointer user_data)
{
  QIFImportWindow * wind = user_data;
  GtkWidget * next_page = get_next_druid_page(wind, page);

  if (next_page)
  {
    gnome_druid_set_page(GNOME_DRUID(wind->druid),
                         GNOME_DRUID_PAGE(next_page));
    return TRUE;
  }

  return FALSE;
}


/********************************************************************
 * gnc_ui_qif_import_generic_back_cb
 *
 * Display the previous druid page.
 ********************************************************************/

static gboolean
gnc_ui_qif_import_generic_back_cb(GnomeDruidPage * page, gpointer arg1,
                                  gpointer user_data)
{
  QIFImportWindow * wind = user_data;
  GtkWidget * back_page = get_prev_druid_page(wind, page);

  if (back_page)
  {
    gnome_druid_set_page(GNOME_DRUID(wind->druid),
                         GNOME_DRUID_PAGE(back_page));
    return TRUE;
  }

  return FALSE;
}


/********************************************************************
 * gnc_ui_qif_import_select_file_cb
 *
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

  filter = gtk_file_filter_new();
  gtk_file_filter_set_name(filter, "*.qif");
  gtk_file_filter_add_pattern(filter, "*.[Qq][Ii][Ff]");
  new_file_name = gnc_file_dialog(_("Select QIF File"),
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
  const gchar * path_to_load;

  /* Get the file name. */
  path_to_load = gtk_entry_get_text(GTK_ENTRY(wind->filename_entry));

  /* Validate the chosen filename. */
  if (strlen(path_to_load) == 0)
    gnc_error_dialog(wind->window, _("Please select a file to load."));
  else if (g_access(path_to_load, R_OK) < 0)
    gnc_error_dialog(wind->window,
                     _("File not found or read permission denied. "
                       "Please select another file."));
  else
  {
    SCM qif_file_loaded = scm_c_eval_string("qif-dialog:qif-file-loaded?");

    /* See if the file is already loaded. */
    if (scm_call_2(qif_file_loaded,
                   scm_makfrom0str(path_to_load),
                   wind->imported_files) == SCM_BOOL_T)
      gnc_error_dialog(wind->window,
                       _("That QIF file is already loaded. "
                         "Please select another file."));
    else
    {
      /* Passed all checks; proceed to the next page. */
      return gnc_ui_qif_import_generic_next_cb(page, arg1, wind);
    }
  }

  /* Stay on this page. */
  return TRUE;
}


/********************************************************************
 * gnc_ui_qif_import_load_progress_prepare_cb
 *
 * Prepare the file loading progress page for display.
 ********************************************************************/

static void
gnc_ui_qif_import_load_progress_prepare_cb(GnomeDruidPage * page,
                                           gpointer arg1,
                                           gpointer user_data)
{
  QIFImportWindow   *wind = user_data;

  /* Reset the progress display. */
  gnc_progress_dialog_set_primary(wind->load_progress, "");
  gnc_progress_dialog_set_secondary(wind->load_progress,
    _("GnuCash will now load your QIF file. If there are no errors or warnings, you will automatically proceed to the next step. Otherwise, the details will be shown below for your review."));
  gnc_progress_dialog_set_sub(wind->load_progress, " ");
  gnc_progress_dialog_reset_value(wind->load_progress);
  gnc_progress_dialog_reset_log(wind->load_progress);

  /* Disable the "Forward" button for now.
   *
   * NOTE: Due to bug 91001 in GnomeDruid, gnome_druid_set_buttons_sensitive()
   *       will not work in prepare callbacks unless they are run AFTER the
   *       standard one. Make sure the Glade line has the callback set up with
   *       after=yes. For example:
   *         <signal name="prepare" handler="my_prepare_cb" after="yes"/>   */
  gnome_druid_set_buttons_sensitive(GNOME_DRUID(wind->druid),
                                    TRUE, FALSE, TRUE, TRUE);

  /* Generate a show signal once the page is displayed. This
   * will kick off our (potentially) long-running operations. */
  gtk_widget_hide(GTK_WIDGET(page));
  gtk_widget_set_sensitive(GTK_WIDGET(page), TRUE);
  gtk_widget_show(GTK_WIDGET(page));
}


/********************************************************************
 * gnc_ui_qif_import_load_progress_show_cb
 *
 * Start the long-running operation.
 ********************************************************************/

static void
gnc_ui_qif_import_load_progress_show_cb(GtkWidget *widget,
                                        gpointer user_data)
{
  QIFImportWindow   *wind = user_data;
  const gchar * path_to_load;

  SCM make_qif_file   = scm_c_eval_string("make-qif-file");
  SCM qif_file_load   = scm_c_eval_string("qif-file:read-file");
  SCM qif_file_parse  = scm_c_eval_string("qif-file:parse-fields");
  SCM unload_qif_file = scm_c_eval_string("qif-dialog:unload-qif-file");
  SCM parse_results   = scm_c_eval_string("qif-file:parse-fields-results");
  SCM scm_qiffile;
  SCM imported_files = SCM_EOL;
  SCM load_return, parse_return;
  SCM progress;

  /* Raise the busy flag so the druid can't be canceled unexpectedly. */
  wind->busy = TRUE;
  gtk_widget_set_sensitive(wind->load_pause, TRUE);

  /* Get the file name. */
  path_to_load = gtk_entry_get_text(GTK_ENTRY(wind->filename_entry));

  /* Create the <qif-file> object. */
  scm_qiffile          = scm_call_0(make_qif_file);
  scm_gc_unprotect_object(wind->selected_file);
  wind->selected_file  = scm_qiffile;
  scm_gc_protect_object(wind->selected_file);
  imported_files       = scm_cons(scm_qiffile, wind->imported_files);

  /* Create SCM for the progress helper. */
  progress = SWIG_NewPointerObj(wind->load_progress,
                                SWIG_TypeQuery("_p__GNCProgressDialog"),
                                0);

  /* Clear any previous pause or cancel state. */
  scm_c_eval_string("(qif-import:reset-cancel-pause)");


  /*
   * Load the file.
   *
   * The loader returns:
   *  success:   ()
   *  failure:   (#f error-message)
   *  warning:   (#t error-message)
   *  cancel:    #t
   *  exception: #f
   */

  /* This step will fill 70% of the bar. */
  gnc_progress_dialog_push(wind->load_progress, 0.7);
  load_return = scm_call_4(qif_file_load,
                           SCM_CAR(imported_files),
                           scm_makfrom0str(path_to_load),
                           wind->ticker_map,
                           progress);
  gnc_progress_dialog_pop(wind->load_progress);
  if (load_return == SCM_BOOL_T)
  {
    /* Canceled by the user. */

    /* Disable the pause button. */
    gtk_widget_set_sensitive(wind->load_pause, FALSE);

    /* Inform the user. */
    gnc_progress_dialog_set_sub(wind->load_progress, _("Canceled"));

    wind->busy = FALSE;
    return;
  }
  else if (load_return == SCM_BOOL_F || !SCM_LISTP(load_return))
  {
    /* A bug was detected. */

    /* Disable the pause button. */
    gtk_widget_set_sensitive(wind->load_pause, FALSE);

    /* Inform the user. */
    gnc_progress_dialog_append_log(wind->load_progress,
                     _( "A bug was detected while reading the QIF file."));
    gnc_progress_dialog_set_sub(wind->load_progress, _("Failed"));
    gnc_progress_dialog_reset_value(wind->load_progress);
    gnc_error_dialog(wind->window,
                     _( "A bug was detected while reading the QIF file."));
    /* FIXME: How should we request that the user report this problem? */

    wind->busy = FALSE;
    return;
  }
  else if (!SCM_NULLP(load_return))
  {
    const gchar *str = SCM_STRING_CHARS(SCM_CADR(load_return));

    if (SCM_CAR(load_return) == SCM_BOOL_F)
    {
      imported_files = scm_call_2(unload_qif_file, scm_qiffile, imported_files);
      scm_gc_unprotect_object(wind->imported_files);
      wind->imported_files = imported_files;
      scm_gc_protect_object(wind->imported_files);

      gnc_progress_dialog_set_sub(wind->load_progress, _("Failed"));
      gnc_progress_dialog_reset_value(wind->load_progress);

      gtk_widget_set_sensitive(wind->load_pause, FALSE);
      wind->busy = FALSE;
      return;
    }
  }


  /*
   * Parse the fields.
   *
   * The parser returns:
   *   success:   ()
   *   failure:   (#f . ((type . error) ...))
   *   warning:   (#t . ((type . error) ...))
   *   cancel:    #t
   *   exception: #f
   */

  /* This step will fill the remainder of the bar. */
  gnc_progress_dialog_push(wind->load_progress, 1);
  parse_return = scm_call_2(qif_file_parse, SCM_CAR(imported_files), progress);
  gnc_progress_dialog_pop(wind->load_progress);
  wind->ask_date_format = FALSE;
  if (parse_return == SCM_BOOL_T)
  {
    /* Canceled by the user. */

    /* Disable the pause button. */
    gtk_widget_set_sensitive(wind->load_pause, FALSE);

    /* Unload the file. */
    gnc_progress_dialog_set_sub(wind->load_progress, _("Cleaning up"));
    imported_files = scm_call_2(unload_qif_file, scm_qiffile, imported_files);

    /* Inform the user. */
    gnc_progress_dialog_set_sub(wind->load_progress, _("Canceled"));

    wind->busy = FALSE;
    return;
  }
  else if (parse_return == SCM_BOOL_F || !SCM_LISTP(parse_return))
  {
    /* A bug was detected. */

    /* Disable the pause button. */
    gtk_widget_set_sensitive(wind->load_pause, FALSE);

    /* Unload the file. */
    gnc_progress_dialog_set_sub(wind->load_progress, _("Cleaning up"));
    imported_files = scm_call_2(unload_qif_file, scm_qiffile, imported_files);

    /* Inform the user. */
    gnc_progress_dialog_append_log(wind->load_progress,
                     _( "A bug was detected while parsing the QIF file."));
    gnc_progress_dialog_set_sub(wind->load_progress, _("Failed"));
    gnc_progress_dialog_reset_value(wind->load_progress);
    gnc_error_dialog(wind->window,
                     _( "A bug was detected while parsing the QIF file."));
    /* FIXME: How should we request that the user report this problem? */

    wind->busy = FALSE;
    return;
  }
  else if (!SCM_NULLP(parse_return))
  {
    /* Are there only warnings? */
    if (SCM_CAR(parse_return) == SCM_BOOL_T)
    {
      SCM date_formats;

      /* A warning means that (potentially) the date format is
       * ambiguous.  So search the results for the "date" type and if
       * it's found, set up the format selector page. */
      if ((date_formats = scm_call_2(parse_results,
                                     SCM_CDR(parse_return),
                                     scm_str2symbol("date"))) != SCM_BOOL_F)
      {
        gint n_items;

        /* Clear the date format combo box. */
        gtk_combo_box_set_active(GTK_COMBO_BOX(wind->date_format_combo), -1);
        n_items = gtk_tree_model_iter_n_children(
          gtk_combo_box_get_model(GTK_COMBO_BOX(wind->date_format_combo)),
                                  NULL);
        while (n_items-- > 0)
          gtk_combo_box_remove_text(GTK_COMBO_BOX(wind->date_format_combo), 0);

        /* Add the formats for the user to select from. */
        while(SCM_LISTP(date_formats) && !SCM_NULLP(date_formats))
        {
          gtk_combo_box_append_text(GTK_COMBO_BOX(wind->date_format_combo),
                                    SCM_SYMBOL_CHARS(SCM_CAR(date_formats)));
          date_formats = SCM_CDR(date_formats);
        }
        gtk_combo_box_set_active(GTK_COMBO_BOX(wind->date_format_combo), 0);

        wind->ask_date_format = TRUE;
      }
    }
    else
    {
      /* Parsing failed. */
      imported_files = scm_call_2(unload_qif_file, scm_qiffile, imported_files);
      gnc_progress_dialog_set_sub(wind->load_progress, _("Failed"));
      gnc_progress_dialog_reset_value(wind->load_progress);

      gtk_widget_set_sensitive(wind->load_pause, FALSE);
      wind->busy = FALSE;
      return;
    }
  }

  /* The file was loaded successfully. */
  gnc_progress_dialog_set_sub(wind->load_progress, _("Loading completed"));
  gnc_progress_dialog_set_value(wind->load_progress, 1);

  scm_gc_unprotect_object(wind->imported_files);
  wind->imported_files = imported_files;
  scm_gc_protect_object(wind->imported_files);

  /* Enable all buttons. */
  gnome_druid_set_buttons_sensitive(GNOME_DRUID(wind->druid),
                                    TRUE, TRUE, TRUE, TRUE);

  /* If the log is empty, move on to the next page automatically. */
  if (gtk_text_buffer_get_char_count(gtk_text_view_get_buffer(GTK_TEXT_VIEW(wind->load_log))) == 0)
    gnome_druid_page_next(GNOME_DRUID_PAGE(widget));

  gtk_widget_set_sensitive(wind->load_pause, FALSE);
  wind->busy = FALSE;
  return;
}


/********************************************************************
 * gnc_ui_qif_import_load_progress_pause_cb
 *
 * Invoked when the "Pause" button is clicked.
 ********************************************************************/

static void
gnc_ui_qif_import_load_progress_pause_cb(GtkButton * button,
                                         gpointer user_data)
{
  QIFImportWindow *wind = user_data;
  SCM toggle_pause      = scm_c_eval_string("qif-import:toggle-pause");
  SCM progress;

  if (!wind->busy)
    return;

  /* Create SCM for the progress helper. */
  progress = SWIG_NewPointerObj(wind->load_progress,
                                SWIG_TypeQuery("_p__GNCProgressDialog"),
                                0);

  /* Pause (or resume) the currently running operation. */
  scm_call_1(toggle_pause, progress);

  /* Swap the button label between pause and resume. */
  if (strcmp(gtk_button_get_label(button), _("_Resume")))
  {
    gtk_button_set_use_stock(button, FALSE);
    gtk_button_set_use_underline(button, TRUE);
    gtk_button_set_label(button, _("_Resume"));
  }
  else
  {
    gtk_button_set_use_stock(button, TRUE);
    gtk_button_set_use_underline(button, FALSE);
    gtk_button_set_label(button, "gtk-media-pause");
  }
}


/********************************************************************
 * gnc_ui_qif_import_load_progress_next_cb
 *
 * Determine the next page after loading a file successfully.
 ********************************************************************/

static gboolean
gnc_ui_qif_import_load_progress_next_cb(GnomeDruidPage * page,
                                        gpointer arg1,
                                        gpointer user_data)
{
  QIFImportWindow *wind = user_data;
  SCM check_from_acct   = scm_c_eval_string("qif-file:check-from-acct");

  if (wind->ask_date_format)
  {
    /* We need to get a date format, so go to the next page. */
    return gnc_ui_qif_import_generic_next_cb(page, arg1, user_data);
  }
  else if (scm_call_1(check_from_acct, wind->selected_file) != SCM_BOOL_T)
  {
    SCM default_acct = scm_c_eval_string("qif-file:path-to-accountname");
    const gchar * default_acctname;

    /* Go to the "ask account name" page. */
    default_acctname = SCM_STRING_CHARS(scm_call_1(default_acct,
                                                   wind->selected_file));
    gtk_entry_set_text(GTK_ENTRY(wind->acct_entry), default_acctname);

    gnome_druid_set_page(GNOME_DRUID(wind->druid),
                         get_named_page(wind, "account_name_page"));
    return TRUE;
  }

  /* Skip ahead to the "loaded files" page. */
  gnome_druid_set_page(GNOME_DRUID(wind->druid),
                       get_named_page(wind, "loaded_files_page"));

  return TRUE;
}


/****************************************************************
 * load_progress_back_timeout_cb
 *
 * This timer callback function waits until the busy flag
 * has been cleared before going back to the previous page.
 ****************************************************************/

static gboolean
load_progress_back_timeout_cb(gpointer data)
{
  QIFImportWindow *wind = data;

  if (wind->busy)
    /* Wait for timer to go off again. */
    return TRUE;

  /* The busy flag was lowered. Go back to the previous page. */
  gnome_druid_page_back(get_named_page(wind, "load_progress_page"));

  /* Cancel the timer. */
  return FALSE;
}


/********************************************************************
 * gnc_ui_qif_import_load_progress_back_cb
 *
 * Return to the previous page, waiting if necessary.
 ********************************************************************/

static gboolean
gnc_ui_qif_import_load_progress_back_cb(GnomeDruidPage * page,
                                        gpointer arg1,
                                        gpointer user_data)
{
  QIFImportWindow *wind = user_data;

  if (wind->busy)
  {
    /* Cancel any long-running Scheme operation. */
    scm_c_eval_string("(qif-import:cancel)");

    /* Wait for the busy flag to be lowered. */
    g_timeout_add(200, load_progress_back_timeout_cb, user_data);

    return TRUE;
  }

  return gnc_ui_qif_import_generic_back_cb(page, arg1, user_data);
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

  /* Get the selected date format. */
  text = gtk_combo_box_get_active_text(GTK_COMBO_BOX(wind->date_format_combo));
  if (!text)
  {
    g_critical("QIF import: BUG DETECTED in gnc_ui_qif_import_date_format_next_cb. Format is NULL.");
    return TRUE;
  }
  format_sym = scm_str2symbol(text);
  g_free(text);

  /* Reparse the dates using the selected format. */
  scm_call_2(reparse_dates, wind->selected_file, format_sym);

  /* Determine the next page to display. */
  if (scm_call_1(check_from_acct, wind->selected_file) != SCM_BOOL_T)
  {
    /* There is an account name missing. Ask the user to provide one. */
    SCM default_acct = scm_c_eval_string("qif-file:path-to-accountname");
    const gchar * default_acctname;

    default_acctname = SCM_STRING_CHARS(scm_call_1(default_acct,
                                                   wind->selected_file));
    gtk_entry_set_text(GTK_ENTRY(wind->acct_entry), default_acctname);

    return FALSE;
  }
  else
  {
    /* Skip ahead to the "loaded files" page. */
    gnome_druid_set_page(GNOME_DRUID(wind->druid),
                         get_named_page(wind, "loaded_files_page"));

    return TRUE;
  }
}


/********************************************************************
 * update_file_page
 *
 * Update the list of loaded files.
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
    if (scm_qiffile == wind->selected_file) {
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


/****************************************************************
 * gnc_ui_qif_import_select_loaded_file_cb
 * callback when a file is clicked in the "loaded files" page
 ****************************************************************/

static void
gnc_ui_qif_import_select_loaded_file_cb(GtkTreeSelection *selection,
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
    if (SCM_LISTP(wind->imported_files) &&
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

  if (wind->selected_file != SCM_BOOL_F) {
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
 * gnc_ui_qif_import_loaded_files_next_cb
 *
 * Get the matching pages ready for viewing.
 ********************************************************************/

static gboolean
gnc_ui_qif_import_loaded_files_next_cb(GnomeDruidPage * page,
                                       gpointer arg1,
                                       gpointer user_data)
{
  QIFImportWindow * wind = user_data;

  /* Prepare the matching pages. */
  gnc_set_busy_cursor(NULL, TRUE);
  update_account_page(wind);
  update_category_page(wind);
  update_memo_page(wind);
  gnc_unset_busy_cursor(NULL);

  return gnc_ui_qif_import_generic_next_cb(page, arg1, user_data);
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
  const gchar * acct_name = gtk_entry_get_text(GTK_ENTRY(wind->acct_entry));
  SCM    fix_default = scm_c_eval_string("qif-import:fix-from-acct");
  SCM    scm_name;

  g_return_val_if_fail(wind->selected_file != SCM_BOOL_F, FALSE);
  if (!acct_name || acct_name[0] == 0) {
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


/********************************************************************
 ********************************************************************/

static void
create_account_picker_view(GtkWidget *widget,
                           const gchar *col_name,
                           GCallback activate_cb,
                           GCallback select_cb,
                           gpointer user_data)
{
  GtkTreeView *view = GTK_TREE_VIEW(widget);
  GtkTreeSelection *selection = gtk_tree_view_get_selection(view);
  GtkListStore *store;
  GtkCellRenderer *renderer;
  GtkTreeViewColumn *column;

  store = gtk_list_store_new(NUM_ACCOUNT_COLS, G_TYPE_INT, G_TYPE_STRING,
                             G_TYPE_STRING, G_TYPE_BOOLEAN,
                             PANGO_TYPE_ELLIPSIZE_MODE);
  gtk_tree_view_set_model(view, GTK_TREE_MODEL(store));
  g_object_unref(store);

  renderer = gtk_cell_renderer_text_new();
  column = gtk_tree_view_column_new_with_attributes(col_name,
                                                    renderer,
                                                    "text",
                                                    ACCOUNT_COL_QIF_NAME,
                                                    "ellipsize",
                                                    ACCOUNT_COL_ELLIPSIZE,
                                                    NULL);
  g_object_set(column, "expand", TRUE, NULL);
  gtk_tree_view_column_set_resizable(column, TRUE);
  gtk_tree_view_append_column(view, column);

  renderer = gtk_cell_renderer_text_new();
  column = gtk_tree_view_column_new_with_attributes(_("GnuCash account name"),
                                                    renderer,
                                                    "text",
                                                    ACCOUNT_COL_GNC_NAME,
                                                    "ellipsize",
                                                    ACCOUNT_COL_ELLIPSIZE,
                                                    NULL);
  g_object_set(column, "expand", TRUE, NULL);
  gtk_tree_view_column_set_resizable(column, TRUE);
  gtk_tree_view_append_column(view, column);

  renderer = gtk_cell_renderer_toggle_new();
  g_object_set(renderer, "activatable", FALSE, NULL);
  column = gtk_tree_view_column_new_with_attributes(_("New?"),
                                                    renderer,
                                                    "active",
                                                    ACCOUNT_COL_NEW,
                                                    NULL);
  gtk_tree_view_append_column(view, column);

  g_object_set_data(G_OBJECT(store), PREV_ROW, GINT_TO_POINTER(-1));

  /* Connect the signal handlers. */
  g_signal_connect(view, "row-activated", G_CALLBACK(activate_cb), user_data);
  g_signal_connect(selection, "changed", G_CALLBACK(select_cb), user_data);

  /* Allow multiple rows to be selected. */
  gtk_tree_selection_set_mode(selection, GTK_SELECTION_MULTIPLE);
}


/********************************************************************
 * rematch_line
 *
 * This is a helper function for tree controls used by some druid
 * pages for mapping QIF values to GnuCash accounts. It processes
 * the selected rows when a user tries to edit the account mappings.
 * The account picker is displayed, and the chosen GnuCash account
 * becomes the new mapping for each row.  Finally, the update_page
 * function is called.
 ********************************************************************/

static void
rematch_line(QIFImportWindow *wind, GtkTreeSelection *selection,
             SCM display_info, SCM map_info,
            void (*update_page)(QIFImportWindow *))
{
  SCM           get_qif_name = scm_c_eval_string("qif-map-entry:qif-name");
  SCM           get_gnc_name = scm_c_eval_string("qif-map-entry:gnc-name");
  SCM           set_gnc_name = scm_c_eval_string("qif-map-entry:set-gnc-name!");
  SCM           map_entry;
  SCM           gnc_name;
  GList        *pathlist;
  GList        *current;
  GtkTreeModel *model;
  GtkTreeIter   iter;
  gint          row;

  /* Get a list of selected rows. */
  pathlist = gtk_tree_selection_get_selected_rows(selection, &model);
  if (!pathlist)
    return;


  /*
   * Update the first selected row.
   */

  /* Get the row number of the first selected row. */
  if (!gtk_tree_model_get_iter(model, &iter, (GtkTreePath *) pathlist->data))
    return;
  gtk_tree_model_get(model, &iter, ACCOUNT_COL_INDEX, &row, -1);

  /* Save the row number. */
  g_object_set_data(G_OBJECT(model), PREV_ROW, GINT_TO_POINTER(row));
  if (row == -1)
    return;

  /* Find the <qif-map-entry> corresponding to the selected row. */
  map_entry = scm_list_ref(display_info, scm_int2num(row));

  /* Call the account picker to update it. */
  if (!qif_account_picker_dialog(wind, map_entry))
    return;
  gnc_name = scm_call_1(get_gnc_name, map_entry);

  /* Update the mapping hash table. */
  scm_hash_set_x(map_info, scm_call_1(get_qif_name, map_entry), map_entry);


  /*
   * Map all other selected rows to the same GnuCash account.
   */
  for (current = pathlist->next; current; current = current->next)
  {
      /* Get the row number. */
      gtk_tree_model_get_iter(model, &iter, (GtkTreePath *) current->data);
      gtk_tree_model_get(model, &iter, ACCOUNT_COL_INDEX, &row, -1);

      /* Update the <qif-map-entry> for the selected row. */
      map_entry = scm_list_ref(display_info, scm_int2num(row));
      scm_call_2(set_gnc_name, map_entry, gnc_name);

      /* Update the mapping hash table. */
      scm_hash_set_x(map_info, scm_call_1(get_qif_name, map_entry), map_entry);
  }

  /* Free the path list. */
  g_list_foreach(pathlist, (GFunc) gtk_tree_path_free, NULL);
  g_list_free(pathlist);

  /* Update the display. */
  update_page(wind);
}


/********************************************************************
 * gnc_ui_qif_import_account_activate_cb
 *
 * This handler is invoked when a row is double-clicked in the "Match
 * QIF accounts to GnuCash accounts" page.
 ********************************************************************/

static void
gnc_ui_qif_import_account_activate_cb(GtkTreeView *view, GtkTreePath *path,
                                      GtkTreeViewColumn *column,
                                      gpointer user_data)
{
  QIFImportWindow  *wind = user_data;

  g_return_if_fail(wind);

  rematch_line(wind, gtk_tree_view_get_selection(view),
               wind->acct_display_info, wind->acct_map_info,
               update_account_page);
}


/********************************************************************
 * gnc_ui_qif_import_account_select_cb
 *
 * This handler is invoked when the selection of account matchings
 * has changed.  It updates the selection count and enables/disables
 * the "Change" button.
 ********************************************************************/

static void
gnc_ui_qif_import_account_select_cb(GtkTreeSelection *selection,
                                    gpointer user_data)
{
  QIFImportWindow  *wind = user_data;
  gint              count = gtk_tree_selection_count_selected_rows(selection);
  gchar            *count_str;

  g_return_if_fail(wind);

  /* Update the "items selected" count. */
  if (wind->acct_view_count)
  {
    count_str = g_strdup_printf("%d", count);
    gtk_label_set_text(GTK_LABEL(wind->acct_view_count), count_str);
    g_free(count_str);
  }

  /* Enable/disable the Change button. */
  if (wind->acct_view_btn)
  {
    if (count)
      gtk_widget_set_sensitive(wind->acct_view_btn, TRUE);
    else
      gtk_widget_set_sensitive(wind->acct_view_btn, FALSE);
  }
}


/********************************************************************
 * gnc_ui_qif_import_category_activate_cb
 *
 * This handler is invoked when a row is double-clicked in the "Match
 * QIF categories to GnuCash accounts" page.
 ********************************************************************/

static void
gnc_ui_qif_import_category_activate_cb(GtkTreeView *view, GtkTreePath *path,
                                       GtkTreeViewColumn *column,
                                       gpointer user_data)
{
  QIFImportWindow *wind = user_data;
  GtkTreeSelection *selection;

  g_return_if_fail(view && wind);
  selection = gtk_tree_view_get_selection(view);

  rematch_line(wind, selection, wind->cat_display_info, wind->cat_map_info,
               update_category_page);
}


/********************************************************************
 * gnc_ui_qif_import_category_select_cb
 *
 * This handler is invoked when the selection of category matchings
 * has changed.  It updates the selection count and enables/disables
 * the "Change" button.
 ********************************************************************/

static void
gnc_ui_qif_import_category_select_cb(GtkTreeSelection *selection,
                                     gpointer user_data)
{
  QIFImportWindow  *wind = user_data;
  gint              count = gtk_tree_selection_count_selected_rows(selection);
  gchar            *count_str;

  g_return_if_fail(wind);

  /* Update the "items selected" count. */
  if (wind->cat_view_count)
  {
    count_str = g_strdup_printf("%d", count);
    gtk_label_set_text(GTK_LABEL(wind->cat_view_count), count_str);
    g_free(count_str);
  }

  /* Enable/disable the Change button. */
  if (wind->cat_view_btn)
  {
    if (count)
      gtk_widget_set_sensitive(wind->cat_view_btn, TRUE);
    else
      gtk_widget_set_sensitive(wind->cat_view_btn, FALSE);
  }
}


/********************************************************************
 *  gnc_ui_qif_import_memo_activate_cb
 *
 * This handler is invoked when a row is double-clicked in the "Match
 * QIF payee/memo to GnuCash accounts" page.
 ********************************************************************/

static void
gnc_ui_qif_import_memo_activate_cb(GtkTreeView *view, GtkTreePath *path,
                                   GtkTreeViewColumn *column,
                                   gpointer user_data)
{
  QIFImportWindow *wind = user_data;
  GtkTreeSelection *selection;

  g_return_if_fail(view && wind);
  selection = gtk_tree_view_get_selection(view);

  rematch_line(wind, selection, wind->memo_display_info, wind->memo_map_info,
               update_memo_page);
}


/********************************************************************
 * gnc_ui_qif_import_memo_select_cb
 *
 * This handler is invoked when the selection of memo matchings
 * has changed.  It updates the selection count and enables/disables
 * the "Change" button.
 ********************************************************************/

static void
gnc_ui_qif_import_memo_select_cb(GtkTreeSelection *selection,
                                 gpointer user_data)
{
  QIFImportWindow  *wind = user_data;
  gint              count = gtk_tree_selection_count_selected_rows(selection);
  gchar            *count_str;

  g_return_if_fail(wind);

  /* Update the "items selected" count. */
  if (wind->memo_view_count)
  {
    count_str = g_strdup_printf("%d", count);
    gtk_label_set_text(GTK_LABEL(wind->memo_view_count), count_str);
    g_free(count_str);
  }

  /* Enable/disable the Change button. */
  if (wind->memo_view_btn)
  {
    if (count)
      gtk_widget_set_sensitive(wind->memo_view_btn, TRUE);
    else
      gtk_widget_set_sensitive(wind->memo_view_btn, FALSE);
  }
}


/****************************************************************
 * gnc_ui_qif_import_account_rematch_cb
 *
 * This handler is invoked when the user clicks the "Change
 * GnuCash account" button on the account mapping page. This
 * button is an alternative to double-clicking a row.
 ****************************************************************/

static void
gnc_ui_qif_import_account_rematch_cb(GtkButton *button, gpointer user_data)
{
  QIFImportWindow  *wind = user_data;

  g_return_if_fail(wind);

  rematch_line(wind,
               gtk_tree_view_get_selection(GTK_TREE_VIEW(wind->acct_view)),
               wind->acct_display_info,
               wind->acct_map_info,
               update_account_page);
}


/****************************************************************
 * gnc_ui_qif_import_account_next_cb
 *
 * Find the next page to show, depending on whether there are
 * category or payee/memo mappings to be dealt with.
 ****************************************************************/

static gboolean
gnc_ui_qif_import_account_next_cb(GnomeDruidPage * page,
                                  gpointer arg1,
                                  gpointer user_data)
{
  QIFImportWindow * wind = user_data;

  /* If there are category mappings then proceed as usual. */
  if (SCM_LISTP(wind->cat_display_info) && !SCM_NULLP(wind->cat_display_info))
    return gnc_ui_qif_import_generic_next_cb(page, arg1, user_data);

  /* If there are memo mappings then skip to that step. */
  if (SCM_LISTP(wind->memo_display_info) && !SCM_NULLP(wind->memo_display_info))
  {
    if (wind->show_doc_pages)
      gnome_druid_set_page(GNOME_DRUID(wind->druid),
                           get_named_page(wind, "memo_doc_page"));
    else
      gnome_druid_set_page(GNOME_DRUID(wind->druid),
                           get_named_page(wind, "memo_match_page"));
    return TRUE;
  }

  /* Skip ahead to the currency page. */
  gnome_druid_set_page(GNOME_DRUID(wind->druid),
                       get_named_page(wind, "currency_page"));
  return TRUE;
}


/****************************************************************
 * gnc_ui_qif_import_category_rematch_cb
 *
 * This handler is invoked when the user clicks the "Change
 * GnuCash account" button on the category mapping page. This
 * button is an alternative to double-clicking a row.
 ****************************************************************/

static void
gnc_ui_qif_import_category_rematch_cb(GtkButton *button, gpointer user_data)
{
  QIFImportWindow  *wind = user_data;

  g_return_if_fail(wind);

  rematch_line(wind,
               gtk_tree_view_get_selection(GTK_TREE_VIEW(wind->cat_view)),
               wind->cat_display_info,
               wind->cat_map_info,
               update_category_page);
}


/****************************************************************
 * gnc_ui_qif_import_category_next_cb
 *
 * Check to see if there are any memo or payee mappings to show.
 * If not, skip that step.
 ****************************************************************/

static gboolean
gnc_ui_qif_import_category_next_cb(GnomeDruidPage * page,
                                   gpointer arg1,
                                   gpointer user_data)
{
  QIFImportWindow * wind = user_data;

  /* If there aren't any payee/memo mappings then skip that step. */
  if (!SCM_LISTP(wind->memo_display_info) || SCM_NULLP(wind->memo_display_info))
  {
    gnome_druid_set_page(GNOME_DRUID(wind->druid),
                         get_named_page(wind, "currency_page"));
    return TRUE;
  }

  return gnc_ui_qif_import_generic_next_cb(page, arg1, user_data);
}


/****************************************************************
 * gnc_ui_qif_import_memo_rematch_cb
 *
 * This handler is invoked when the user clicks the "Change
 * GnuCash account" button on the memo mapping page. This
 * button is an alternative to double-clicking a row.
 ****************************************************************/

static void
gnc_ui_qif_import_memo_rematch_cb(GtkButton *button, gpointer user_data)
{
  QIFImportWindow  *wind = user_data;

  g_return_if_fail(wind);

  rematch_line(wind,
               gtk_tree_view_get_selection(GTK_TREE_VIEW(wind->memo_view)),
               wind->memo_display_info,
               wind->memo_map_info,
               update_memo_page);
}


/****************************************************************
 * gnc_ui_qif_import_memo_doc_back_cb
 *
 * Figure out which page went before payee/memo documentation.
 ****************************************************************/

static gboolean
gnc_ui_qif_import_memo_doc_back_cb(GnomeDruidPage * page, gpointer arg1,
                                   gpointer user_data)
{
  QIFImportWindow * wind = user_data;

  /* If there are no categories to show, go to account matching. */
  if (!SCM_LISTP(wind->cat_display_info) || SCM_NULLP(wind->cat_display_info))
  {

    gnome_druid_set_page(GNOME_DRUID(wind->druid),
                         get_named_page(wind, "account_match_page"));
    return TRUE;
  }

  return gnc_ui_qif_import_generic_back_cb(page, arg1, user_data);
}


/****************************************************************
 * gnc_ui_qif_import_memo_back_cb
 *
 * Figure out which page went before payee/memo mapping.
 ****************************************************************/

static gboolean
gnc_ui_qif_import_memo_back_cb(GnomeDruidPage * page, gpointer arg1,
                               gpointer user_data)
{
  QIFImportWindow * wind = user_data;

  /* If documentation is off and there are no categories to show,
   * skip directly to account matching. */
  if (!wind->show_doc_pages &&
      (!SCM_LISTP(wind->cat_display_info) || SCM_NULLP(wind->cat_display_info)))
  {

    gnome_druid_set_page(GNOME_DRUID(wind->druid),
                         get_named_page(wind, "account_match_page"));
    return TRUE;
  }

  return gnc_ui_qif_import_generic_back_cb(page, arg1, user_data);
}


/****************************************************************
 * gnc_ui_qif_import_new_securities
 *
 * This function creates or updates the list of QIF securities
 * for which no corresponding GnuCash commodity existed prior to
 * import. If there are any such securities, TRUE is returned.
 * Otherwise, FALSE is returned.
 ****************************************************************/

static gboolean
gnc_ui_qif_import_new_securities(QIFImportWindow * wind)
{
  SCM updates;
  SCM update_securities = scm_c_eval_string("qif-import:update-security-hash");

  /* Get a list of any new QIF securities since the previous call. */
  updates = scm_call_4(update_securities,
                       wind->security_hash,
                       wind->ticker_map,
                       wind->acct_map_info,
                       wind->security_prefs);
  if (updates != SCM_BOOL_F)
  {
    /* A list of new QIF securities was returned. Save it. */
    scm_gc_unprotect_object(wind->new_securities);
    if (wind->new_securities != SCM_BOOL_F)
      /* There is an existing list, so append the new list. */
      wind->new_securities = scm_append(scm_list_2(wind->new_securities,
                                                   updates));
    else
      wind->new_securities = updates;
    scm_gc_protect_object(wind->new_securities);

    return TRUE;
  }

  if (wind->new_securities != SCM_BOOL_F)
    return TRUE;

  return FALSE;
}


/****************************************************************
 * gnc_ui_qif_import_currency_back_cb
 *
 * Set the next page depending on whether there are payee/memo
 * or category mappings to show.
 ****************************************************************/

static gboolean
gnc_ui_qif_import_currency_back_cb(GnomeDruidPage * page,
                                   gpointer arg1,
                                   gpointer user_data)
{
  QIFImportWindow * wind = user_data;

  /* If there are payee/memo mappings to display, go there. */
  if (SCM_LISTP(wind->memo_display_info) && !SCM_NULLP(wind->memo_display_info))
  {
    gnome_druid_set_page(GNOME_DRUID(wind->druid),
                         get_named_page(wind, "memo_match_page"));
    return TRUE;
  }

  /* If there are category mappings to display, go there. */
  if (SCM_LISTP(wind->cat_display_info) && !SCM_NULLP(wind->cat_display_info))
  {
    gnome_druid_set_page(GNOME_DRUID(wind->druid),
                         get_named_page(wind, "category_match_page"));
    return TRUE;
  }

  /* Go to account matching. */
  gnome_druid_set_page(GNOME_DRUID(wind->druid),
                       get_named_page(wind, "account_match_page"));
  return TRUE;
}


static void
gnc_ui_qif_import_comm_prepare_cb(GnomeDruidPage * page,
                                  gpointer arg1,
                                  gpointer user_data)
{
  QIFImportWindow *wind = user_data;
  QIFDruidPage    *qpage = g_object_get_data(G_OBJECT(page), "page_struct");
  gchar           *ns;

  /* Get any entered namespace. */
  ns = gtk_combo_box_get_active_text(GTK_COMBO_BOX(qpage->namespace_combo));

  /* Update the namespaces available to select. */
  if (!ns || !ns[0])
    gnc_ui_update_namespace_picker(
      qpage->namespace_combo,
      gnc_commodity_get_namespace(qpage->commodity),
      DIAG_COMM_ALL);
  else
    gnc_ui_update_namespace_picker(qpage->namespace_combo, ns, DIAG_COMM_ALL);

  g_free(ns);
}


static gboolean
gnc_ui_qif_import_comm_next_cb(GnomeDruidPage * page,
                               gpointer arg1,
                               gpointer user_data)
{
  QIFImportWindow *wind = user_data;
  QIFDruidPage    *qpage = g_object_get_data(G_OBJECT(page), "page_struct");

  QofBook                 *book;
  gnc_commodity_table     *table;
  gnc_commodity_namespace *newns;

  gchar       *namespace = gnc_ui_namespace_picker_ns(qpage->namespace_combo);
  const gchar *name      = gtk_entry_get_text(GTK_ENTRY(qpage->name_entry));
  const gchar *mnemonic  = gtk_entry_get_text(GTK_ENTRY(qpage->mnemonic_entry));

  if (!name || (name[0] == 0)) {
    gnc_warning_dialog(wind->window,
     _("Enter a name or short description, such as \"Red Hat Stock\"."));
    g_free(namespace);
    return TRUE;
  }
  else if (!mnemonic || (mnemonic[0] == 0)) {
    gnc_warning_dialog(wind->window,
     _("Enter the ticker symbol or other well known abbreviation, such as"
       " \"RHT\". If there isn't one, or you don't know it, create your own."));
    g_free(namespace);
    return TRUE;
  }
  else if (!namespace || (namespace[0] == 0)) {
    gnc_warning_dialog(wind->window,
     _("Select the exchange on which the symbol is traded, or select the"
       " type of investment (such as FUND for mutual funds.) If you don't"
       " see your exchange or an appropriate investment type, you can"
       " enter a new one."));
    if (namespace)
      g_free(namespace);
    return TRUE;
  }

  /* FIXME: Should check whether a commodity with this namespace and
   *        mnemonic already exists. If so, ask the user whether to use
   *        the existing one, or go back and change what they've entered.
   */

  book = gnc_get_current_book();
  table = gnc_commodity_table_get_table(book);
  if (gnc_commodity_namespace_is_iso(namespace) &&
      !gnc_commodity_table_lookup(table, namespace, mnemonic))
  {
    gnc_warning_dialog(wind->window,
                       _("You must enter an existing national "
                         "currency or enter a different type."));

    g_free(namespace);
    return TRUE;
  }

  /* Is the namespace a new one? */
  if (!gnc_commodity_table_has_namespace(table, namespace))
  {
    /* Register it so that it will appear as an option on other pages. */
    newns = gnc_commodity_table_add_namespace(table, namespace, book);

    /* Remember it so it can be removed if the import gets canceled. */
    if (newns)
      wind->new_namespaces = g_list_prepend(wind->new_namespaces, namespace);
    else
    {
      g_warning("QIF import: Couldn't create namespace %s", namespace);
      g_free(namespace);
    }
  }
  else
    g_free(namespace);

  return FALSE;
}


static QIFDruidPage *
new_security_page(SCM security_hash_key, gnc_commodity *comm)
{

  QIFDruidPage *retval = g_new0(QIFDruidPage, 1);
  GtkWidget    *table;
  GtkWidget    *label;
  gchar        *title = NULL;
  const char   *str;
  GnomeDruidPageStandard *page;
  char         *name_tooltip =
   _("Enter a name or short description, such as \"Red Hat Stock\".");
  char         *mnemonic_tooltip =
   _("Enter the ticker symbol or other well known abbreviation, such as"
     " \"RHT\". If there isn't one, or you don't know it, create your own.");
  char         *namespace_tooltip =
   _("Select the exchange on which the symbol is traded, or select the"
     " type of investment (such as FUND for mutual funds.) If you don't"
     " see your exchange or an appropriate investment type, you can"
     " enter a new one.");
#if GTK_CHECK_VERSION(2, 12, 0)
  /* Use the GtkTooltip API */
#elif GTK_CHECK_VERSION(2, 6, 0)
  /* Use the deprecated GtkTooltips API */
  GtkTooltips  *tooltips = gtk_tooltips_new();
#endif

  /* Make the page widget. */
  retval->page = gnome_druid_page_standard_new_with_vals("", NULL, NULL);
  g_object_set_data(G_OBJECT(retval->page), "page_struct", retval);
  page = GNOME_DRUID_PAGE_STANDARD(retval->page);

  /* Save the commodity and the hash table key. */
  retval->commodity = comm;
  retval->hash_key = security_hash_key;
  scm_gc_protect_object(retval->hash_key);

  /* Set the page title. */
  str = gnc_commodity_get_mnemonic(comm);
  str = str ? str : "";
  title = g_markup_printf_escaped(_("Enter information about \"%s\""), str);
  gnome_druid_page_standard_set_title(page, title);
  g_free(title);

  /* Set the page colors. */
  gnome_druid_page_standard_set_background(page, &std_bg_color);
  gnome_druid_page_standard_set_logo_background(page, &std_logo_bg_color);
  gnome_druid_page_standard_set_title_foreground(page, &std_title_color);

  /*
   * Add all the widgets to the page.
   */
  table = gtk_table_new(3, 2, FALSE);
  gtk_table_set_row_spacings(GTK_TABLE(table), 6);
  gtk_table_set_col_spacings(GTK_TABLE(table), 12);

  /* Name entry */
  retval->name_entry = gtk_entry_new();
  gtk_entry_set_text(GTK_ENTRY(retval->name_entry),
                     gnc_commodity_get_fullname(comm));
  label = gtk_label_new_with_mnemonic(_("_Name or description:"));
  gtk_label_set_mnemonic_widget(GTK_LABEL(label), retval->name_entry);
  gtk_misc_set_alignment(GTK_MISC(label), 0, 0.5);
#if GTK_CHECK_VERSION(2, 12, 0)
  gtk_widget_set_tooltip_text(label, name_tooltip);
  gtk_widget_set_tooltip_text(retval->name_entry, name_tooltip);
#elif GTK_CHECK_VERSION(2, 6, 0)
  gtk_tooltips_set_tip(tooltips, label,
                       name_tooltip, NULL);
  gtk_tooltips_set_tip(tooltips, retval->name_entry,
                       name_tooltip, NULL);
#endif
  gtk_table_attach(GTK_TABLE(table), label, 0, 1, 0, 1,
                   GTK_FILL, GTK_EXPAND | GTK_FILL, 0, 0);
  gtk_table_attach_defaults(GTK_TABLE(table), retval->name_entry,
                            1, 2, 0, 1);
  /* Mnemonic entry */
  retval->mnemonic_entry = gtk_entry_new();
  gtk_entry_set_text(GTK_ENTRY(retval->mnemonic_entry),
                     gnc_commodity_get_mnemonic(comm));
  label = gtk_label_new_with_mnemonic(
    _("_Ticker symbol or other abbreviation:"));
  gtk_label_set_mnemonic_widget(GTK_LABEL(label), retval->mnemonic_entry);
  gtk_misc_set_alignment(GTK_MISC(label), 0, 0.5);
#if GTK_CHECK_VERSION(2, 12, 0)
  gtk_widget_set_tooltip_text(label, mnemonic_tooltip);
  gtk_widget_set_tooltip_text(retval->mnemonic_entry, mnemonic_tooltip);
#elif GTK_CHECK_VERSION(2, 6, 0)
  gtk_tooltips_set_tip(tooltips, label,
                       mnemonic_tooltip, NULL);
  gtk_tooltips_set_tip(tooltips, retval->mnemonic_entry,
                       mnemonic_tooltip, NULL);
#endif
  gtk_table_attach(GTK_TABLE(table), label, 0, 1, 1, 2,
                   GTK_FILL, GTK_EXPAND | GTK_FILL, 0, 0);
  gtk_table_attach_defaults(GTK_TABLE(table), retval->mnemonic_entry,
                            1, 2, 1, 2);

  /* Namespace entry */
  retval->namespace_combo = gtk_combo_box_entry_new_text();
  gnc_cbe_add_completion(GTK_COMBO_BOX_ENTRY(retval->namespace_combo));
  label = gtk_label_new_with_mnemonic(
    _("_Exchange or abbreviation type:"));
  gtk_label_set_mnemonic_widget(GTK_LABEL(label), retval->namespace_combo);
  gtk_misc_set_alignment(GTK_MISC(label), 0, 0.5);
#if GTK_CHECK_VERSION(2, 12, 0)
  gtk_widget_set_tooltip_text(label, namespace_tooltip);
  gtk_widget_set_tooltip_text(retval->namespace_combo, namespace_tooltip);
#elif GTK_CHECK_VERSION(2, 6, 0)
  gtk_tooltips_set_tip(tooltips, label,
                       namespace_tooltip, NULL);
  gtk_tooltips_set_tip(tooltips, retval->namespace_combo,
                       namespace_tooltip, NULL);
#endif
  gtk_table_attach(GTK_TABLE(table), label, 0, 1, 2, 3,
                   GTK_FILL, GTK_EXPAND | GTK_FILL, 0, 0);
  gtk_table_attach_defaults(GTK_TABLE(table), retval->namespace_combo,
                            1, 2, 2, 3);

  gtk_box_pack_start(GTK_BOX(page->vbox), table, FALSE, FALSE, 0);

  return retval;
}


/********************************************************************
 * prepare_security_pages
 *
 * Prepare the druid page for each security.
 ********************************************************************/

static void
prepare_security_pages(QIFImportWindow * wind)
{
  SCM   hash_ref  = scm_c_eval_string("hash-ref");
  SCM   securities;
  SCM   comm_ptr_token;

  GList          * current;
  gnc_commodity  * commodity;
  GnomeDruidPage * back_page = get_named_page(wind, "commodity_doc_page");
  QIFDruidPage   * new_page;

  /*
   * Make druid pages for each new QIF security.
   */
  gnc_set_busy_cursor(NULL, TRUE);
  securities = wind->new_securities;
  current = wind->commodity_pages;
  while (!SCM_NULLP(securities) && (securities != SCM_BOOL_F))
  {
    if (current)
    {
      /* The page has already been made. */
      back_page = GNOME_DRUID_PAGE(current->data);
      current = current->next;
    }
    else
    {
      /* Get the GnuCash commodity corresponding to the new QIF security. */
      comm_ptr_token = scm_call_2(hash_ref,
                                  wind->security_hash,
                                  SCM_CAR(securities));
      #define FUNC_NAME "new_security_page"
      commodity = SWIG_MustGetPtr(comm_ptr_token,
                                  SWIG_TypeQuery("_p_gnc_commodity"), 1, 0);
      #undef FUNC_NAME

      /* Build a new security page. */
      new_page = new_security_page(SCM_CAR(securities), commodity);

      /* Connect the signals. */
      g_signal_connect(new_page->page, "prepare",
                       G_CALLBACK(gnc_ui_qif_import_comm_prepare_cb),
                       wind);

      g_signal_connect(new_page->page, "back",
                       G_CALLBACK(gnc_ui_qif_import_generic_back_cb),
                       wind);

      g_signal_connect(new_page->page, "next",
                       G_CALLBACK(gnc_ui_qif_import_comm_next_cb),
                       wind);

      /* Add it to the list of security pages. */
      wind->commodity_pages = g_list_append(wind->commodity_pages,
                                            new_page->page);

      /* Add the new page to the druid. */
      gnome_druid_insert_page(GNOME_DRUID(wind->druid),
                              back_page,
                              GNOME_DRUID_PAGE(new_page->page));

      back_page = GNOME_DRUID_PAGE(new_page->page);
      gtk_widget_show_all(new_page->page);
    }

    securities = SCM_CDR(securities);
  }
  gnc_unset_busy_cursor(NULL);

  gnc_druid_set_colors(GNOME_DRUID(wind->druid));
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

  /* If there are new securities, prepare the security pages. */
  if (gnc_ui_qif_import_new_securities(wind))
    prepare_security_pages(wind);

  return gnc_ui_qif_import_generic_next_cb(page, arg1, wind);
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
  const gchar    *mnemonic = NULL;
  gchar          *namespace = NULL;
  const gchar    *fullname = NULL;
  gnc_commodity  *tab_commodity;

  for (pageptr = wind->commodity_pages; pageptr; pageptr=pageptr->next)
  {
    gtkpage   = GNOME_DRUID_PAGE(pageptr->data);
    page      = g_object_get_data(G_OBJECT(gtkpage), "page_struct");

    /* Get any changes from the commodity page. */
    mnemonic  = gtk_entry_get_text(GTK_ENTRY(page->mnemonic_entry));
    namespace = gnc_ui_namespace_picker_ns(page->namespace_combo);
    fullname  = gtk_entry_get_text(GTK_ENTRY(page->name_entry));

    /* Update the commodity with the new values. */
    gnc_commodity_set_namespace(page->commodity, namespace);
    gnc_commodity_set_fullname(page->commodity, fullname);
    gnc_commodity_set_mnemonic(page->commodity, mnemonic);

    /* Add the commodity to the commodity table (if it isn't a duplicate). */
    tab_commodity = gnc_commodity_table_lookup(gnc_get_current_commodities(),
                                               namespace, mnemonic);
    if (!tab_commodity || tab_commodity == page->commodity)
      tab_commodity = gnc_commodity_table_insert(gnc_get_current_commodities(),
                                                 page->commodity);

    /* Update the security hash table. */
    scm_hash_set_x(wind->security_hash,
                   page->hash_key,
                   SWIG_NewPointerObj(tab_commodity,
                                      SWIG_TypeQuery("_p_gnc_commodity"), 0));

    g_free(namespace);
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

  if (!SCM_LISTP(wind->match_transactions))
    return;

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
  path = gtk_tree_path_new_from_indices(0, -1);
  gtk_tree_selection_select_path(selection, path);
  gtk_tree_path_free(path);
}


/****************************************************************
 * gnc_ui_qif_import_convert_undo
 *
 * This function launches the Scheme procedure that un-imports
 * any imported accounts and transactions.
 ****************************************************************/

static void
gnc_ui_qif_import_convert_undo(QIFImportWindow * wind)
{
  SCM undo = scm_c_eval_string("qif-import:qif-to-gnc-undo");

  gnc_set_busy_cursor(NULL, TRUE);

  /* Undo the conversion. */
  scm_call_1(undo, wind->imported_account_tree);

  /* There's no imported account tree any more. */
  scm_gc_unprotect_object(wind->imported_account_tree);
  wind->imported_account_tree = SCM_BOOL_F;
  scm_gc_protect_object(wind->imported_account_tree);


  /* Get rid of the list of matched transactions. */
  scm_gc_unprotect_object(wind->match_transactions);
  wind->match_transactions = SCM_BOOL_F;
  scm_gc_protect_object(wind->match_transactions);

  gnc_unset_busy_cursor(NULL);
}


/********************************************************************
 * gnc_ui_qif_import_convert_progress_prepare_cb
 *
 * Prepare the data conversion progress page for display.
 ********************************************************************/

static void
gnc_ui_qif_import_convert_progress_prepare_cb(GnomeDruidPage * page,
                                              gpointer arg1,
                                              gpointer user_data)
{
  QIFImportWindow   *wind = user_data;

  /* Reset the progress display. */
  gnc_progress_dialog_set_primary(wind->convert_progress, "");
  gnc_progress_dialog_set_secondary(wind->convert_progress,
    _("GnuCash is now importing your QIF data. If there are no errors or warnings, you will automatically proceed to the next step. Otherwise, the details will be shown below for your review."));
  gnc_progress_dialog_set_sub(wind->convert_progress, " ");
  gnc_progress_dialog_reset_value(wind->convert_progress);
  gnc_progress_dialog_reset_log(wind->convert_progress);

  /* Disable the "Forward" button for now.
   *
   * NOTE: Due to bug 91001 in GnomeDruid, gnome_druid_set_buttons_sensitive()
   *       will not work in prepare callbacks unless they are run AFTER the
   *       standard one. Make sure the Glade line has the callback set up with
   *       after=yes. For example:
   *         <signal name="prepare" handler="my_prepare_cb" after="yes"/>   */
  gnome_druid_set_buttons_sensitive(GNOME_DRUID(wind->druid),
                                    TRUE, FALSE, TRUE, TRUE);

  /* Generate a show signal once the page is displayed. This
   * will kick off our (potentially) long-running operations. */
  gtk_widget_hide(GTK_WIDGET(page));
  gtk_widget_set_sensitive(GTK_WIDGET(page), TRUE);
  gtk_widget_show(GTK_WIDGET(page));
}


/********************************************************************
 * gnc_ui_qif_import_convert_progress_show_cb
 *
 * Perform the conversion.
 ********************************************************************/

static void
gnc_ui_qif_import_convert_progress_show_cb(GtkWidget *widget,
                                           gpointer user_data)
{
  QIFImportWindow   *wind = user_data;
  SCM qif_to_gnc      = scm_c_eval_string("qif-import:qif-to-gnc");
  SCM find_duplicates = scm_c_eval_string("gnc:account-tree-find-duplicates");
  SCM retval;

  /* SCM for the progress dialog. */
  SCM progress = SWIG_NewPointerObj(wind->convert_progress,
                                    SWIG_TypeQuery("_p__GNCProgressDialog"),
                                    0);

  /* The default currency. */
  gchar *currname =
    gtk_combo_box_get_active_text(GTK_COMBO_BOX(wind->currency_picker));

  /* Raise the busy flag so the druid can't be canceled unexpectedly. */
  wind->busy = TRUE;
  gtk_widget_set_sensitive(wind->convert_pause, TRUE);

  /* Clear any previous pause or cancel state. */
  scm_c_eval_string("(qif-import:reset-cancel-pause)");

  /* Update the commodities. */
  gnc_ui_qif_import_commodity_update(wind);


  /*
   * Convert the QIF data into GnuCash data.
   *
   * A Scheme function does all the work.  The return value is the
   * root account of an account tree containing all the new accounts
   * and transactions. Upon failure, #f is returned. If the user
   * cancels, #t is returned.
   */

  /* This step will fill 70% of the bar. */
  gnc_progress_dialog_push(wind->convert_progress, 0.7);
  retval = scm_apply(qif_to_gnc,
                     SCM_LIST7(wind->imported_files,
                               wind->acct_map_info,
                               wind->cat_map_info,
                               wind->memo_map_info,
                               wind->security_hash,
                               scm_makfrom0str(currname),
                               progress),
                     SCM_EOL);
  gnc_progress_dialog_pop(wind->convert_progress);
  g_free(currname);

  if (retval == SCM_BOOL_T)
  {
    /* Canceled by the user. */

    /* Disable the pause button. */
    gtk_widget_set_sensitive(wind->convert_pause, FALSE);

    /* Remove any converted data. */
    gnc_progress_dialog_set_sub(wind->convert_progress, _("Cleaning up"));
    gnc_ui_qif_import_convert_undo(wind);

    /* Inform the user. */
    gnc_progress_dialog_set_sub(wind->convert_progress, _("Canceled"));
    gnc_progress_dialog_reset_value(wind->convert_progress);

    wind->busy = FALSE;
    return;
  }
  else if (retval == SCM_BOOL_F)
  {
    /* An bug was encountered during conversion. */

    /* Disable the pause button. */
    gtk_widget_set_sensitive(wind->convert_pause, FALSE);

    /* Remove any converted data. */
    gnc_progress_dialog_set_sub(wind->convert_progress, _("Cleaning up"));
    gnc_ui_qif_import_convert_undo(wind);

    /* Inform the user. */
    gnc_progress_dialog_append_log(wind->convert_progress,
                     _( "A bug was detected while converting the QIF data."));
    gnc_progress_dialog_set_sub(wind->convert_progress, _("Failed"));
    gnc_progress_dialog_reset_value(wind->convert_progress);
    gnc_error_dialog(wind->window,
                     _( "A bug was detected while converting the QIF data."));
    /* FIXME: How should we request that the user report this problem? */

    wind->busy = FALSE;
    return;
  }
  else if (SCM_SYMBOLP(retval))
  {
    /* An error was encountered during conversion. */

    /* Disable the pause button. */
    gtk_widget_set_sensitive(wind->convert_pause, FALSE);

    /* Remove any converted data. */
    gnc_progress_dialog_set_sub(wind->convert_progress, _("Cleaning up"));
    gnc_ui_qif_import_convert_undo(wind);

    /* Inform the user. */
    gnc_progress_dialog_set_sub(wind->convert_progress, _("Failed"));
    gnc_progress_dialog_reset_value(wind->convert_progress);

    wind->busy = FALSE;
    return;
  }


  /* Save the imported account tree. */
  scm_gc_unprotect_object(wind->imported_account_tree);
  wind->imported_account_tree = retval;
  scm_gc_protect_object(wind->imported_account_tree);


  /*
   * Detect potentially duplicated transactions.
   */

  /* This step will fill the remainder of the bar. */
  gnc_progress_dialog_push(wind->convert_progress, 1);
  retval = scm_call_3(find_duplicates,
                      scm_c_eval_string("(gnc-get-current-root-account)"),
                      wind->imported_account_tree, progress);
  gnc_progress_dialog_pop(wind->convert_progress);

  /* Save the results. */
  scm_gc_unprotect_object(wind->match_transactions);
  wind->match_transactions = retval;
  scm_gc_protect_object(wind->match_transactions);

  if (retval == SCM_BOOL_T)
  {
    /* Canceled by the user. */
    gtk_widget_set_sensitive(wind->convert_pause, FALSE);
    gnc_progress_dialog_set_sub(wind->convert_progress, _("Canceling"));
    wind->busy = FALSE;
    return;
  }
  else if (retval == SCM_BOOL_F)
  {
    /* An error occurred during duplicate checking. */

    /* Remove any converted data. */
    gnc_progress_dialog_set_sub(wind->convert_progress, _("Cleaning up"));
    gnc_ui_qif_import_convert_undo(wind);

    /* Inform the user. */
    gnc_progress_dialog_append_log(wind->convert_progress,
                     _( "A bug was detected while detecting duplicates."));
    gnc_progress_dialog_set_sub(wind->convert_progress, _("Failed"));
    gnc_progress_dialog_reset_value(wind->convert_progress);
    gnc_error_dialog(wind->window,
                     _( "A bug was detected while detecting duplicates."));
    /* FIXME: How should we request that the user report this problem? */

    gtk_widget_set_sensitive(wind->convert_pause, FALSE);
    wind->busy = FALSE;
    return;
  }


  /* The conversion completed successfully. */
  gnc_progress_dialog_set_sub(wind->convert_progress,
                              _("Conversion completed"));
  gnc_progress_dialog_set_value(wind->convert_progress, 1);

  /* Enable all buttons. */
  gnome_druid_set_buttons_sensitive(GNOME_DRUID(wind->druid),
                                    TRUE, TRUE, TRUE, TRUE);

  /* If the log is empty, move on to the next page automatically. */
  if (gtk_text_buffer_get_char_count(gtk_text_view_get_buffer(GTK_TEXT_VIEW(wind->convert_log))) == 0)
    gnome_druid_page_next(GNOME_DRUID_PAGE(widget));

  gtk_widget_set_sensitive(wind->convert_pause, FALSE);
  wind->busy = FALSE;
  return;
}


/********************************************************************
 * gnc_ui_qif_import_convert_progress_pause_cb
 *
 * Invoked when the "Pause" button is clicked.
 ********************************************************************/

static void
gnc_ui_qif_import_convert_progress_pause_cb(GtkButton * button,
                                            gpointer user_data)
{
  QIFImportWindow *wind = user_data;
  SCM toggle_pause      = scm_c_eval_string("qif-import:toggle-pause");
  SCM progress;

  if (!wind->busy)
    return;

  /* Create SCM for the progress helper. */
  progress = SWIG_NewPointerObj(wind->convert_progress,
                                SWIG_TypeQuery("_p__GNCProgressDialog"),
                                0);

  /* Pause (or resume) the currently running operation. */
  scm_call_1(toggle_pause, progress);

  /* Swap the button label between pause and resume. */
  if (strcmp(gtk_button_get_label(button), _("_Resume")))
  {
    gtk_button_set_use_stock(button, FALSE);
    gtk_button_set_use_underline(button, TRUE);
    gtk_button_set_label(button, _("_Resume"));
  }
  else
  {
    gtk_button_set_use_stock(button, TRUE);
    gtk_button_set_use_underline(button, FALSE);
    gtk_button_set_label(button, "gtk-media-pause");
  }
}


/********************************************************************
 * gnc_ui_qif_import_convert_progress_next_cb
 *
 * Determine the next page after converting successfully.
 ********************************************************************/

static gboolean
gnc_ui_qif_import_convert_progress_next_cb(GnomeDruidPage * page,
                                           gpointer arg1,
                                           gpointer user_data)
{
  QIFImportWindow *wind = user_data;

  if (SCM_NULLP(wind->match_transactions))
  {
    /* No potential duplicates, so skip to the last page. */
    gnome_druid_set_page(GNOME_DRUID(wind->druid),
                         get_named_page(wind, "end_page"));
    return TRUE;
  }

  /* Prepare the duplicates page. */
  gnc_ui_qif_import_prepare_duplicates(wind);

  /* Display the next page. */
  return gnc_ui_qif_import_generic_next_cb(page, arg1, wind);
}


/****************************************************************
 * convert_progress_back_timeout_cb
 *
 * This timer callback function waits until the busy flag
 * has been cleared before going back to the previous page.
 ****************************************************************/

static gboolean
convert_progress_back_timeout_cb(gpointer data)
{
  QIFImportWindow *wind = data;

  if (wind->busy)
    /* Wait for timer to go off again. */
    return TRUE;

  /* The busy flag was lowered. Go back to the previous page. */
  gnome_druid_page_back(get_named_page(wind, "convert_progress_page"));

  /* Cancel the timer. */
  return FALSE;
}


/********************************************************************
 * gnc_ui_qif_import_convert_progress_back_cb
 *
 * Return to the previous page, waiting if necessary.
 ********************************************************************/

static gboolean
gnc_ui_qif_import_convert_progress_back_cb(GnomeDruidPage * page,
                                           gpointer arg1,
                                           gpointer user_data)
{
  QIFImportWindow *wind = user_data;

  if (wind->busy)
  {
    /* Cancel any long-running Scheme operation. */
    scm_c_eval_string("(qif-import:cancel)");

    /* Wait for the busy flag to be lowered. */
    g_timeout_add(200, convert_progress_back_timeout_cb, user_data);

    return TRUE;
  }

  return gnc_ui_qif_import_generic_back_cb(page, arg1, user_data);
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

  if (wind->match_transactions != SCM_BOOL_F) {
    possible_matches = SCM_CDR(scm_list_ref(wind->match_transactions,
                                  scm_int2num(wind->selected_transaction)));
    scm_call_2(scm_c_eval_string("qif-import:refresh-match-selection"),
               possible_matches, scm_int2num(selection));

    while(!SCM_NULLP(possible_matches)) {
      current_xtn = SCM_CAR(possible_matches);
      #define FUNC_NAME "xaccTransCountSplits"
      gnc_xtn     = SWIG_MustGetPtr(SCM_CAR(current_xtn),
                                    SWIG_TypeQuery("_p_Transaction"), 1, 0);
      #undef FUNC_NAME
      selected    = SCM_CDR(current_xtn);

      if (xaccTransCountSplits(gnc_xtn) > 2) {
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
gnc_ui_qif_import_duplicate_new_select_cb(GtkTreeSelection *selection,
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
gnc_ui_qif_import_duplicate_old_select_cb(GtkTreeSelection *selection,
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


/********************************************************************
 * gnc_ui_qif_import_check_acct_tree
 *
 * Designed for use with gnc_main_window_foreach_page(), this
 * function determines whether an account tab is open in the main
 * window. The parameter user_data must point to a gboolean.
 ********************************************************************/

static void
gnc_ui_qif_import_check_acct_tree(GncPluginPage *page, gpointer user_data)
{
  gboolean *found = user_data;

  if (GNC_IS_PLUGIN_PAGE_ACCOUNT_TREE(page) && found)
    *found = TRUE;
}


/********************************************************************
 * gnc_ui_qif_import_finish_cb
 *
 * Invoked when the "Apply" button is clicked on the final page.
 ********************************************************************/

static void
gnc_ui_qif_import_finish_cb(GnomeDruidPage * gpage,
                            gpointer arg1,
                            gpointer user_data)
{
  SCM save_map_prefs = scm_c_eval_string("qif-import:save-map-prefs");
  SCM cat_and_merge = scm_c_eval_string("gnc:account-tree-catenate-and-merge");
  SCM prune_xtns = scm_c_eval_string("gnc:prune-matching-transactions");
  SCM scm_result;

  QIFImportWindow * wind = user_data;
  GncPluginPage *page;
  gboolean acct_tree_found = FALSE;

  gnc_suspend_gui_refresh();

  /* Prune any imported transactions that were determined to be duplicates. */
  if (wind->match_transactions != SCM_BOOL_F)
    scm_call_1(prune_xtns, wind->match_transactions);

  /* Merge the imported account tree with the existing one. */
  if (wind->imported_account_tree != SCM_BOOL_F)
    scm_call_2(cat_and_merge,
               scm_c_eval_string("(gnc-get-current-root-account)"),
               wind->imported_account_tree);

  gnc_resume_gui_refresh();

  /* Save the user's mapping preferences. */
  scm_result = scm_apply(save_map_prefs,
                         SCM_LIST5(wind->acct_map_info, wind->cat_map_info,
                                   wind->memo_map_info, wind->security_hash,
                                   wind->security_prefs),
                         SCM_EOL);
  if (scm_result == SCM_BOOL_F)
    gnc_warning_dialog(wind->window,
            _("GnuCash was unable to save your mapping preferences."));

  /* Open an account tab in the main window if one doesn't exist already. */
  gnc_main_window_foreach_page(gnc_ui_qif_import_check_acct_tree,
                               &acct_tree_found);
  if (!acct_tree_found)
  {
    page = gnc_plugin_page_account_tree_new();
    gnc_main_window_open_page(NULL, page);
  }

  gnc_ui_qif_import_druid_destroy(wind);
}


/****************************************************************
 * do_cancel
 *
 * Clears out any imported data and shuts down the importer.
 ****************************************************************/

static void
do_cancel(QIFImportWindow * wind)
{
  GList               *pageptr;
  GnomeDruidPage      *gtkpage;
  QIFDruidPage        *page;
  gnc_commodity_table *table;

  gnc_set_busy_cursor(NULL, TRUE);

  /* Remove any converted data. */
  gnc_ui_qif_import_convert_undo(wind);

  /* Remove any commodities created for druid pages. */
  for (pageptr = wind->commodity_pages; pageptr; pageptr=pageptr->next)
  {
    gtkpage   = GNOME_DRUID_PAGE(pageptr->data);
    page      = g_object_get_data(G_OBJECT(gtkpage), "page_struct");
    gnc_commodity_destroy(page->commodity);
  }

  /* Remove any namespaces created by the user. */
  table = gnc_get_current_commodities();
  while (wind->new_namespaces)
  {
    gnc_commodity_table_delete_namespace(table, (gchar *) wind->new_namespaces->data);

    /* Free the data and the list element. */
    g_free(wind->new_namespaces->data);
    wind->new_namespaces = g_list_delete_link(wind->new_namespaces,
                                              wind->new_namespaces);
  }

  gnc_unset_busy_cursor(NULL);

  /* Destroy the druid. */
  gnc_ui_qif_import_druid_destroy(wind);
}


/****************************************************************
 * cancel_timeout_cb
 *
 * This timer callback function waits until the busy flag
 * has been cleared before acting to cancel the import.
 ****************************************************************/

static gboolean
cancel_timeout_cb(gpointer data)
{
  QIFImportWindow *wind = data;

  if (wind->busy)
    /* Wait for timer to go off again. */
    return TRUE;

  /* The busy flag was lowered. Perform the cancel. */
  do_cancel(wind);

  /* Cancel the timer. */
  return FALSE;
}


/****************************************************************
 * gnc_ui_qif_import_cancel_cb
 *
 * Invoked when the "Cancel" button is clicked.
 ****************************************************************/

static void
gnc_ui_qif_import_cancel_cb(GnomeDruid * druid, gpointer user_data)
{
  QIFImportWindow     *wind = user_data;

  if (wind->busy)
  {
    /* Cancel any long-running Scheme operation. */
    scm_c_eval_string("(qif-import:cancel)");

    /* Wait for the busy flag to be lowered. */
    g_timeout_add(200, cancel_timeout_cb, user_data);
  }
  else
    do_cancel(wind);
}


SCM
gnc_ui_qif_import_druid_get_mappings(QIFImportWindow * w)
{
  return SCM_LIST3(w->acct_map_info,
                   w->cat_map_info,
                   w->memo_map_info);
}



/* ================================================================== */
/*                                                                    */
/*                         IMPORTER CREATION                          */
/*                                                                    */
/* ================================================================== */


/********************************************************************
 * get_preferences
 *
 * Get all user preferences related to QIF import.
 ********************************************************************/

static void
get_preferences(QIFImportWindow *wind)
{
  GError * err = NULL;

  g_return_if_fail(wind);

  /* Get the user's preference for showing documentation pages. */
  wind->show_doc_pages =
    gnc_gconf_get_bool(GCONF_SECTION, GCONF_NAME_SHOW_DOC, &err);
  if (err != NULL) {
    g_warning("QIF import: gnc_gconf_get_bool error: %s", err->message);
    g_error_free(err);

    /* Show documentation pages by default. */
    g_warning("QIF import: Couldn't get %s setting from gconf.",
              GCONF_NAME_SHOW_DOC);
    g_warning("QIF import: Documentation pages will be shown by default.");
    wind->show_doc_pages = TRUE;
  }
}


/********************************************************************
 * initialize_scheme
 *
 * Initialize all Scheme-controlled objects.
 ********************************************************************/

static void
initialize_scheme(QIFImportWindow *wind)
{
  SCM  load_map_prefs;
  SCM  mapping_info;
  SCM  create_ticker_map;

  g_return_if_fail(wind);

  /* Initialize Scheme variables. */
  wind->imported_files        = SCM_EOL;
  wind->selected_file         = SCM_BOOL_F;
  wind->gnc_acct_info         = SCM_BOOL_F;
  wind->cat_display_info      = SCM_BOOL_F;
  wind->cat_map_info          = SCM_BOOL_F;
  wind->acct_display_info     = SCM_BOOL_F;
  wind->acct_map_info         = SCM_BOOL_F;
  wind->memo_display_info     = SCM_BOOL_F;
  wind->memo_map_info         = SCM_BOOL_F;
  wind->security_hash         = SCM_BOOL_F;
  wind->security_prefs        = SCM_BOOL_F;
  wind->new_securities        = SCM_BOOL_F;
  wind->ticker_map            = SCM_BOOL_F;
  wind->imported_account_tree = SCM_BOOL_F;
  wind->match_transactions    = SCM_BOOL_F;

  /* Get the saved state of mappings from Quicken accounts and
   * categories to GnuCash accounts. */
  load_map_prefs = scm_c_eval_string("qif-import:load-map-prefs");
  mapping_info = scm_call_0(load_map_prefs);
  wind->gnc_acct_info         = scm_list_ref(mapping_info, scm_int2num(0));
  wind->acct_map_info         = scm_list_ref(mapping_info, scm_int2num(1));
  wind->cat_map_info          = scm_list_ref(mapping_info, scm_int2num(2));
  wind->memo_map_info         = scm_list_ref(mapping_info, scm_int2num(3));
  wind->security_hash         = scm_list_ref(mapping_info, scm_int2num(4));
  wind->security_prefs        = scm_list_ref(mapping_info, scm_int2num(5));

  /* Get the initial ticker map. */
  create_ticker_map = scm_c_eval_string("make-ticker-map");
  wind->ticker_map            = scm_call_0(create_ticker_map);

  /* Protect our data from garbage collection. */
  scm_gc_protect_object(wind->imported_files);
  scm_gc_protect_object(wind->selected_file);
  scm_gc_protect_object(wind->gnc_acct_info);
  scm_gc_protect_object(wind->cat_display_info);
  scm_gc_protect_object(wind->cat_map_info);
  scm_gc_protect_object(wind->memo_display_info);
  scm_gc_protect_object(wind->memo_map_info);
  scm_gc_protect_object(wind->acct_display_info);
  scm_gc_protect_object(wind->acct_map_info);
  scm_gc_protect_object(wind->security_hash);
  scm_gc_protect_object(wind->security_prefs);
  scm_gc_protect_object(wind->new_securities);
  scm_gc_protect_object(wind->ticker_map);
  scm_gc_protect_object(wind->imported_account_tree);
  scm_gc_protect_object(wind->match_transactions);
}


/********************************************************************
 * build_page_lists
 *
 * Build the lists of druid pages.
 ********************************************************************/

static void
build_page_lists(QIFImportWindow *wind, GladeXML *xml)
{
  int  i;

  /* Pages that may appear prior to security-related pages. */
  char * pre_page_names[NUM_PRE_PAGES] = {
    "start_page", "load_file_page", "load_progress_page",
    "date_format_page", "account_name_page",
    "loaded_files_page", "account_doc_page", "account_match_page",
    "category_doc_page", "category_match_page", "memo_doc_page",
    "memo_match_page", "currency_page", "commodity_doc_page"
  };

  /* Pages that may appear after security-related pages. */
  char * post_page_names[NUM_POST_PAGES] = {
    "convert_progress_page", "match_doc_page", "match_duplicates_page",
    "end_page"
  };

  /* Optional pages that only show documention. */
  char * doc_page_names[NUM_DOC_PAGES] = {
    "start_page", "account_doc_page", "category_doc_page",
    "commodity_doc_page", "memo_doc_page", "match_doc_page"
  };

  g_return_if_fail(wind);

  wind->pre_comm_pages   = NULL;
  wind->post_comm_pages  = NULL;
  wind->doc_pages        = NULL;
  wind->commodity_pages = NULL;

  g_return_if_fail(xml);

  /* Build the page lists. */
  for(i = 0; i < NUM_PRE_PAGES; i++) {
    wind->pre_comm_pages =
      g_list_append(wind->pre_comm_pages,
                    glade_xml_get_widget(xml, pre_page_names[i]));
  }
  for(i = 0; i < NUM_POST_PAGES; i++) {
    wind->post_comm_pages =
      g_list_append(wind->post_comm_pages,
                    glade_xml_get_widget(xml, post_page_names[i]));
  }
  for(i = 0; i < NUM_DOC_PAGES; i++) {
    wind->doc_pages =
      g_list_append(wind->doc_pages,
                    glade_xml_get_widget(xml, doc_page_names[i]));
  }
}


/********************************************************************
 * get_glade_widgets
 *
 * Get all glade-defined widgets that need to be actively managed.
 ********************************************************************/

static void
get_glade_widgets(QIFImportWindow *wind, GladeXML *xml)
{
  g_return_if_fail(wind);
  g_return_if_fail(xml);

  wind->window            = glade_xml_get_widget(xml, "QIF Import Druid");
  wind->druid             = glade_xml_get_widget(xml, "qif_import_druid");
  wind->filename_entry    = glade_xml_get_widget(xml, "qif_filename_entry");
  wind->load_pause        = glade_xml_get_widget(xml, "load_progress_pause");
  wind->load_log          = glade_xml_get_widget(xml, "load_progress_log");
  wind->load_progress     = gnc_progress_dialog_custom(
    GTK_LABEL(glade_xml_get_widget(xml, "load_progress_primary")),
    GTK_LABEL(glade_xml_get_widget(xml, "load_progress_secondary")),
    GTK_PROGRESS_BAR(glade_xml_get_widget(xml, "load_progress_bar")),
    GTK_LABEL(glade_xml_get_widget(xml, "load_progress_sub")),
    GTK_TEXT_VIEW(wind->load_log));
  wind->acct_entry        = glade_xml_get_widget(xml, "qif_account_entry");
  wind->date_format_combo = glade_xml_get_widget(xml, "date_format_combobox");
  wind->selected_file_view = glade_xml_get_widget(xml, "selected_file_view");
  wind->currency_picker   = glade_xml_get_widget(xml, "currency_comboboxentry");
  wind->acct_view         = glade_xml_get_widget(xml, "account_page_view");
  wind->acct_view_count   = glade_xml_get_widget(xml, "account_page_count");
  wind->acct_view_btn     = glade_xml_get_widget(xml, "account_page_change");
  wind->cat_view          = glade_xml_get_widget(xml, "category_page_view");
  wind->cat_view_count    = glade_xml_get_widget(xml, "category_page_count");
  wind->cat_view_btn      = glade_xml_get_widget(xml, "category_page_change");
  wind->memo_view         = glade_xml_get_widget(xml, "memo_page_view");
  wind->memo_view_count   = glade_xml_get_widget(xml, "memo_page_count");
  wind->memo_view_btn     = glade_xml_get_widget(xml, "memo_page_change");
  wind->convert_pause     = glade_xml_get_widget(xml, "convert_progress_pause");
  wind->convert_log       = glade_xml_get_widget(xml, "convert_progress_log");
  wind->convert_progress  = gnc_progress_dialog_custom(
    GTK_LABEL(glade_xml_get_widget(xml, "convert_progress_primary")),
    GTK_LABEL(glade_xml_get_widget(xml, "convert_progress_secondary")),
    GTK_PROGRESS_BAR(glade_xml_get_widget(xml, "convert_progress_bar")),
    GTK_LABEL(glade_xml_get_widget(xml, "convert_progress_sub")),
    GTK_TEXT_VIEW(wind->convert_log));
  wind->new_transaction_view =
    glade_xml_get_widget(xml, "new_transaction_view");
  wind->old_transaction_view =
    glade_xml_get_widget(xml, "old_transaction_view");
}


/********************************************************************
 * connect_glade_signals
 *
 * Connect all glade-defined signals to their handlers.
 ********************************************************************/

static void
connect_glade_signals(QIFImportWindow *wind, GladeXML *xml)
{
  g_return_if_fail(wind);
  g_return_if_fail(xml);

  /*
   * Connect all glade-defined signals to their handlers.
   */
  glade_xml_signal_connect_data
    (xml, "gnc_ui_qif_import_cancel_cb",
     G_CALLBACK(gnc_ui_qif_import_cancel_cb), wind);

  glade_xml_signal_connect_data
    (xml, "gnc_ui_qif_import_generic_next_cb",
     G_CALLBACK(gnc_ui_qif_import_generic_next_cb), wind);

  glade_xml_signal_connect_data
    (xml, "gnc_ui_qif_import_generic_back_cb",
     G_CALLBACK(gnc_ui_qif_import_generic_back_cb), wind);

  glade_xml_signal_connect_data
    (xml, "gnc_ui_qif_import_select_file_cb",
     G_CALLBACK(gnc_ui_qif_import_select_file_cb), wind);

  glade_xml_signal_connect_data
    (xml, "gnc_ui_qif_import_load_file_back_cb",
     G_CALLBACK(gnc_ui_qif_import_load_file_back_cb), wind);

  glade_xml_signal_connect_data
    (xml, "gnc_ui_qif_import_load_file_next_cb",
     G_CALLBACK(gnc_ui_qif_import_load_file_next_cb), wind);

  glade_xml_signal_connect_data
    (xml, "gnc_ui_qif_import_load_progress_prepare_cb",
     G_CALLBACK(gnc_ui_qif_import_load_progress_prepare_cb), wind);

  glade_xml_signal_connect_data
    (xml, "gnc_ui_qif_import_load_progress_show_cb",
     G_CALLBACK(gnc_ui_qif_import_load_progress_show_cb), wind);

  glade_xml_signal_connect_data
    (xml, "gnc_ui_qif_import_load_progress_pause_cb",
     G_CALLBACK(gnc_ui_qif_import_load_progress_pause_cb), wind);

  glade_xml_signal_connect_data
    (xml, "gnc_ui_qif_import_load_progress_next_cb",
     G_CALLBACK(gnc_ui_qif_import_load_progress_next_cb), wind);

  glade_xml_signal_connect_data
    (xml, "gnc_ui_qif_import_load_progress_back_cb",
     G_CALLBACK(gnc_ui_qif_import_load_progress_back_cb), wind);

  glade_xml_signal_connect_data
    (xml, "gnc_ui_qif_import_date_format_next_cb",
     G_CALLBACK(gnc_ui_qif_import_date_format_next_cb), wind);

  glade_xml_signal_connect_data
    (xml, "gnc_ui_qif_import_loaded_files_prepare_cb",
     G_CALLBACK(gnc_ui_qif_import_loaded_files_prepare_cb), wind);

  glade_xml_signal_connect_data
    (xml, "gnc_ui_qif_import_load_another_cb",
     G_CALLBACK(gnc_ui_qif_import_load_another_cb), wind);

  glade_xml_signal_connect_data
    (xml, "gnc_ui_qif_import_unload_file_cb",
     G_CALLBACK(gnc_ui_qif_import_unload_file_cb), wind);

  glade_xml_signal_connect_data
    (xml, "gnc_ui_qif_import_loaded_files_next_cb",
     G_CALLBACK(gnc_ui_qif_import_loaded_files_next_cb), wind);

  glade_xml_signal_connect_data
    (xml, "gnc_ui_qif_import_default_acct_next_cb",
     G_CALLBACK(gnc_ui_qif_import_default_acct_next_cb), wind);

  glade_xml_signal_connect_data
    (xml, "gnc_ui_qif_import_default_acct_back_cb",
     G_CALLBACK(gnc_ui_qif_import_default_acct_back_cb), wind);

  glade_xml_signal_connect_data
    (xml, "gnc_ui_qif_import_account_rematch_cb",
     G_CALLBACK(gnc_ui_qif_import_account_rematch_cb), wind);

  glade_xml_signal_connect_data
    (xml, "gnc_ui_qif_import_account_next_cb",
     G_CALLBACK(gnc_ui_qif_import_account_next_cb), wind);

  glade_xml_signal_connect_data
    (xml, "gnc_ui_qif_import_category_rematch_cb",
     G_CALLBACK(gnc_ui_qif_import_category_rematch_cb), wind);

  glade_xml_signal_connect_data
    (xml, "gnc_ui_qif_import_category_next_cb",
     G_CALLBACK(gnc_ui_qif_import_category_next_cb), wind);

  glade_xml_signal_connect_data
    (xml, "gnc_ui_qif_import_memo_doc_back_cb",
     G_CALLBACK(gnc_ui_qif_import_memo_doc_back_cb), wind);

  glade_xml_signal_connect_data
    (xml, "gnc_ui_qif_import_memo_rematch_cb",
     G_CALLBACK(gnc_ui_qif_import_memo_rematch_cb), wind);

  glade_xml_signal_connect_data
    (xml, "gnc_ui_qif_import_memo_back_cb",
     G_CALLBACK(gnc_ui_qif_import_memo_back_cb), wind);

  glade_xml_signal_connect_data
    (xml, "gnc_ui_qif_import_currency_back_cb",
     G_CALLBACK(gnc_ui_qif_import_currency_back_cb), wind);

  glade_xml_signal_connect_data
    (xml, "gnc_ui_qif_import_currency_next_cb",
     G_CALLBACK(gnc_ui_qif_import_currency_next_cb), wind);

  glade_xml_signal_connect_data
    (xml, "gnc_ui_qif_import_convert_progress_prepare_cb",
     G_CALLBACK(gnc_ui_qif_import_convert_progress_prepare_cb), wind);

  glade_xml_signal_connect_data
    (xml, "gnc_ui_qif_import_convert_progress_show_cb",
     G_CALLBACK(gnc_ui_qif_import_convert_progress_show_cb), wind);

  glade_xml_signal_connect_data
    (xml, "gnc_ui_qif_import_convert_progress_pause_cb",
     G_CALLBACK(gnc_ui_qif_import_convert_progress_pause_cb), wind);

  glade_xml_signal_connect_data
    (xml, "gnc_ui_qif_import_convert_progress_next_cb",
     G_CALLBACK(gnc_ui_qif_import_convert_progress_next_cb), wind);

  glade_xml_signal_connect_data
    (xml, "gnc_ui_qif_import_convert_progress_back_cb",
     G_CALLBACK(gnc_ui_qif_import_convert_progress_back_cb), wind);

  glade_xml_signal_connect_data
    (xml, "gnc_ui_qif_import_finish_cb",
     G_CALLBACK(gnc_ui_qif_import_finish_cb), wind);
}


/********************************************************************
 * build_views
 *
 * Build the details of all GtkTreeView widgets.
 ********************************************************************/

static void
build_views(QIFImportWindow *wind)
{
  GtkTreeView *view;
  GtkListStore *store;
  GtkCellRenderer *renderer;
  GtkTreeViewColumn *column;
  GtkTreeSelection *selection;

  g_return_if_fail(wind);

  /* Set up the selected file view */
  view = GTK_TREE_VIEW(wind->selected_file_view);
  store = gtk_list_store_new(NUM_FILENAME_COLS, G_TYPE_INT, G_TYPE_STRING);
  gtk_tree_view_set_model(view, GTK_TREE_MODEL(store));
  g_object_unref(store);

  renderer = gtk_cell_renderer_text_new();
  column = gtk_tree_view_column_new_with_attributes("unused",
                                                    renderer,
                                                    "text",
                                                    FILENAME_COL_NAME,
                                                    NULL);
  gtk_tree_view_append_column(view, column);

  selection = gtk_tree_view_get_selection(view);
  g_signal_connect(selection, "changed",
                   G_CALLBACK(gnc_ui_qif_import_select_loaded_file_cb),
                   wind);

  /* Set up the QIF account to GnuCash account matcher. */
  create_account_picker_view(wind->acct_view, _("QIF account name"),
                             G_CALLBACK(gnc_ui_qif_import_account_activate_cb),
                             G_CALLBACK(gnc_ui_qif_import_account_select_cb),
                             wind);

  /* Set up the QIF category to GnuCash account matcher. */
  create_account_picker_view(wind->cat_view,  _("QIF category name"),
                             G_CALLBACK(gnc_ui_qif_import_category_activate_cb),
                             G_CALLBACK(gnc_ui_qif_import_category_select_cb),
                             wind);

  /* Set up the QIF payee/memo to GnuCash account matcher. */
  create_account_picker_view(wind->memo_view, _("QIF payee/memo"),
                             G_CALLBACK(gnc_ui_qif_import_memo_activate_cb),
                             G_CALLBACK(gnc_ui_qif_import_memo_select_cb),
                             wind);

  /* Set up the new transaction view */
  view = GTK_TREE_VIEW(wind->new_transaction_view);
  store = gtk_list_store_new(NUM_QIF_TRANS_COLS, G_TYPE_INT, G_TYPE_STRING,
                             G_TYPE_STRING, G_TYPE_STRING, G_TYPE_BOOLEAN);
  gtk_tree_view_set_model(view, GTK_TREE_MODEL(store));
  g_object_unref(store);

  renderer = gtk_cell_renderer_text_new();
  column = gtk_tree_view_column_new_with_attributes(_("Date"),
                                                    renderer,
                                                    "text",
                                                    QIF_TRANS_COL_DATE,
                                                    NULL);
  gtk_tree_view_append_column(view, column);

  renderer = gtk_cell_renderer_text_new();
  column = gtk_tree_view_column_new_with_attributes(_("Description"),
                                                    renderer,
                                                    "text",
                                                    QIF_TRANS_COL_DESCRIPTION,
                                                    NULL);
  gtk_tree_view_append_column(view, column);
  gtk_tree_view_column_set_expand(column, TRUE);

  renderer = gtk_cell_renderer_text_new();
  column = gtk_tree_view_column_new_with_attributes(_("Amount"),
                                                    renderer,
                                                    "text",
                                                    QIF_TRANS_COL_AMOUNT,
                                                    NULL);
  gtk_tree_view_append_column(view, column);

  selection = gtk_tree_view_get_selection(view);
  g_signal_connect(selection, "changed",
                   G_CALLBACK(gnc_ui_qif_import_duplicate_new_select_cb),
                   wind);


  /* Set up the old transaction view */
  view = GTK_TREE_VIEW(wind->old_transaction_view);
  store = gtk_list_store_new(NUM_QIF_TRANS_COLS, G_TYPE_INT, G_TYPE_STRING,
                             G_TYPE_STRING, G_TYPE_STRING, G_TYPE_BOOLEAN);
  gtk_tree_view_set_model(view, GTK_TREE_MODEL(store));
  g_object_unref(store);

  renderer = gtk_cell_renderer_text_new();
  column = gtk_tree_view_column_new_with_attributes(_("Date"),
                                                    renderer,
                                                    "text",
                                                    QIF_TRANS_COL_DATE,
                                                    NULL);
  gtk_tree_view_append_column(view, column);

  renderer = gtk_cell_renderer_text_new();
  column = gtk_tree_view_column_new_with_attributes(_("Description"),
                                                    renderer,
                                                    "text",
                                                    QIF_TRANS_COL_DESCRIPTION,
                                                    NULL);
  gtk_tree_view_append_column(view, column);
  gtk_tree_view_column_set_expand(column, TRUE);

  renderer = gtk_cell_renderer_text_new();
  column = gtk_tree_view_column_new_with_attributes(_("Amount"),
                                                    renderer,
                                                    "text",
                                                    QIF_TRANS_COL_AMOUNT,
                                                    NULL);
  gtk_tree_view_append_column(view, column);

  renderer = gtk_cell_renderer_toggle_new();
  column = gtk_tree_view_column_new_with_attributes(_("Match?"),
                                                    renderer,
                                                    "active",
                                                    QIF_TRANS_COL_CHECKED,
                                                    NULL);
  gtk_tree_view_append_column(view, column);

  selection = gtk_tree_view_get_selection(view);
  g_signal_connect(selection, "changed",
                   G_CALLBACK(gnc_ui_qif_import_duplicate_old_select_cb),
                   wind);
}


/********************************************************************
 * gnc_ui_qif_import_druid_make
 *
 * Build a new QIF import druid.
 ********************************************************************/

QIFImportWindow *
gnc_ui_qif_import_druid_make(void)
{
  QIFImportWindow * wind;
  GladeXML        * xml;

  /* Allocate space for the druid and load its design. */
  wind = g_new0(QIFImportWindow, 1);
  xml = gnc_glade_xml_new("qif.glade", "QIF Import Druid");

  wind->new_namespaces       = NULL;
  wind->selected_transaction = 0;
  wind->busy                 = FALSE;

  /* Get all user preferences related to QIF importing. */
  get_preferences(wind);

  /* Set up the Scheme side of things. */
  initialize_scheme(wind);

  /* Build lists of the druid pages. */
  build_page_lists(wind, xml);

  /* Get all interesting glade-defined widgets. */
  get_glade_widgets(wind, xml);

  /* Connect all glade-defined signals to their handlers. */
  connect_glade_signals(wind, xml);

  /* Build the details of all GtkTreeView widgets. */
  build_views(wind);

  /* Set a default currency for new accounts */
  gnc_cbe_require_list_item(GTK_COMBO_BOX_ENTRY(wind->currency_picker));
  gnc_ui_update_commodity_picker(wind->currency_picker,
                                 GNC_COMMODITY_NS_CURRENCY,
                                 gnc_commodity_get_printname(
                                   gnc_default_currency()));

  /* If desired, skip the initial documentation pages. */
  if (!wind->show_doc_pages)
    gnome_druid_set_page(GNOME_DRUID(wind->druid),
                         get_named_page(wind, "load_file_page"));

  gnc_druid_set_colors(GNOME_DRUID(wind->druid));

  gnc_register_gui_component(DRUID_QIF_IMPORT_CM_CLASS, NULL, NULL, wind);

  gtk_widget_show_all(wind->window);
  gtk_window_present(GTK_WINDOW(wind->window));

  return wind;
}


static gboolean
show_handler(const char *class, gint component_id,
             gpointer user_data, gpointer iter_data)
{
  QIFImportWindow *qif_win = user_data;

  if (!qif_win)
    return(FALSE);
  gtk_window_present(GTK_WINDOW(qif_win->window));
  return(TRUE);
}

void
gnc_file_qif_import(void)
{
  if (gnc_forall_gui_components(DRUID_QIF_IMPORT_CM_CLASS,
                                show_handler, NULL))
    return;

  /* pop up the QIF File Import dialog box */
  gnc_ui_qif_import_druid_make();
}
