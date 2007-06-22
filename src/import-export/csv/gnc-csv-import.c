/*******************************************************************\
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
    @file gnc-csv-import.c
    @brief Csv import module code
    @author Copyright (c) 2007 Benny Sperisen <lasindi@gmail.com>
*/
#include "config.h"

#include <gtk/gtk.h>
#include <glade/glade.h>
#include <glib/gi18n.h>
#include <goffice/gtk/go-charmap-sel.h>

#include "import-account-matcher.h"
#include "import-main-matcher.h"

#include "gnc-file.h"
#include "gnc-book.h"
#include "gnc-ui-util.h"
#include "gnc-glib-utils.h"
#include "dialog-utils.h"

#include "gnc-csv-import.h"
#include "gnc-csv-model.h"

#include <stdio.h> /* Get rid of this */
#include <stdlib.h> /* Get rid of this */

#define GCONF_SECTION "dialogs/import/csv"

static QofLogModule log_module = GNC_MOD_IMPORT;

/* TODO Comment */

enum SEP_BUTTON_TYPES {SEP_SPACE, SEP_TAB, SEP_COMMA, SEP_COLON, SEP_SEMICOLON, SEP_HYPHEN,
                       SEP_NUM_OF_TYPES};


typedef struct
{
  GncCsvParseData* parse_data;
  GladeXML* xml;
  GtkDialog* dialog;
  GtkTreeView* treeview;
  GtkTreeView* ctreeview;
  gboolean approved;
  GtkCheckButton* sep_buttons[SEP_NUM_OF_TYPES];
  GtkCheckButton* custom_cbutton;
  GtkEntry* custom_entry;
} GncCsvPreview;

static void gnc_csv_preview_treeview(GncCsvPreview* preview, gboolean notEmpty);

static void sep_button_clicked(GtkCheckButton* widget, GncCsvPreview* preview)
{
  char* sep_chars[] = {" ", "\t", ",", ":", ";", "-"};
  GSList* separators = NULL;
  int i;
  for(i = 0; i < SEP_NUM_OF_TYPES; i++)
  {
    if(gtk_toggle_button_get_active(GTK_TOGGLE_BUTTON(preview->sep_buttons[i])))
      separators = g_slist_append(separators, sep_chars[i]);
  }
  if(gtk_toggle_button_get_active(GTK_TOGGLE_BUTTON(preview->custom_cbutton)))
  {
    char* custom_sep = (char*)gtk_entry_get_text(preview->custom_entry);
    if(custom_sep[0] != '\0')
      separators = g_slist_append(separators, custom_sep);
  }
  
  stf_parse_options_csv_set_separators(preview->parse_data->options, NULL, separators);
  g_slist_free(separators);
  /* TODO Handle error */
  gnc_csv_parse(preview->parse_data, FALSE, NULL);
  gnc_csv_preview_treeview(preview, TRUE);
}

static void encoding_selected(GOCharmapSel* selector, const char* enc,
                              GncCsvPreview* preview)
{
  static gboolean second_call = FALSE;
  if(second_call)
  {
    GError* error = NULL;
    g_debug("Encoding selected! %p %p %s\n", selector, preview, enc);
    /* TODO Handle errors */
    gnc_csv_convert_enc(preview->parse_data, enc);
    gnc_csv_parse(preview->parse_data, FALSE, &error);
    gnc_csv_preview_treeview(preview, TRUE);
    second_call = FALSE;
  }
  else
  {
    second_call = TRUE;
  }
}

static void ok_button_clicked(GtkWidget* widget, GncCsvPreview* preview)
{
  preview->approved = TRUE;
  gtk_widget_hide((GtkWidget*)(preview->dialog));
}

static void cancel_button_clicked(GtkWidget* widget, GncCsvPreview* preview)
{
  gtk_widget_hide((GtkWidget*)(preview->dialog));
}

static void treeview_resized(GtkWidget* widget, GtkAllocation* allocation, GncCsvPreview* preview)
{
  int i, ncols = preview->parse_data->column_types->len;
  g_debug("tree resized!\n");
  for(i = 0; i < ncols - 1; i++)
  {
    gint col_width;
    GtkTreeViewColumn* ccol;
    col_width = gtk_tree_view_column_get_width(gtk_tree_view_get_column(preview->treeview, i));
    ccol = gtk_tree_view_get_column(preview->ctreeview, i);
    gtk_tree_view_column_set_min_width(ccol, col_width);
    gtk_tree_view_column_set_max_width(ccol, col_width);
    g_debug("%d got width %d\n", i, col_width);
  }
}

static void column_type_edited(GtkCellRenderer *renderer, gchar *path,
                               gchar *new_text, GncCsvPreview* preview)
{
  int i, ncols = preview->parse_data->column_types->len;
  GtkTreeModel* store = (gtk_tree_view_get_model(preview->ctreeview));
  GtkTreeIter iter;
  gtk_tree_model_get_iter_first(store, &iter);
  for(i = 0; i < ncols; i++)
  {
    GtkCellRenderer* col_renderer;
    GtkTreeViewColumn* col = gtk_tree_view_get_column(preview->ctreeview, i);
    GList* rend_list = gtk_tree_view_column_get_cell_renderers(col);
    col_renderer = rend_list->data;
    g_list_free(rend_list);

    if(col_renderer != renderer)
    {
      ;
    }
    else
    {
      gtk_list_store_set(GTK_LIST_STORE(store), &iter, 2*i+1, new_text, -1);
      g_debug("editing col %d\n", i);
    }
  }
  g_debug("Got here %s %s!\n", path, new_text);
}


static GncCsvPreview* gnc_csv_new_preview()
{
  GncCsvPreview* preview = g_malloc(sizeof(GncCsvPreview));
  GtkWidget *ok_button, *cancel_button;
  char* sep_button_names[] = {"space_cbutton",
                              "tab_cbutton",
                              "comma_cbutton",
                              "colon_cbutton",
                              "semicolon_cbutton",
                              "hyphen_cbutton"};
  int i;
  GtkWidget *encselector = go_charmap_sel_new(GO_CHARMAP_SEL_TO_UTF8);
  GtkTable* enctable;
  preview->xml = gnc_glade_xml_new("gnc-csv-preview-dialog.glade", "dialog");
  preview->dialog = (GtkDialog*)(glade_xml_get_widget(preview->xml, "dialog"));

  for(i = 0; i < SEP_NUM_OF_TYPES; i++)
  {
    preview->sep_buttons[i]
      = (GtkCheckButton*)glade_xml_get_widget(preview->xml, sep_button_names[i]);
    g_signal_connect(G_OBJECT(preview->sep_buttons[i]), "toggled",
                     G_CALLBACK(sep_button_clicked), (gpointer)preview);
  }

  preview->custom_cbutton
    = (GtkCheckButton*)glade_xml_get_widget(preview->xml, "custom_cbutton");
  g_signal_connect(G_OBJECT(preview->custom_cbutton), "clicked",
                   G_CALLBACK(sep_button_clicked), (gpointer)preview);

  preview->custom_entry = (GtkEntry*)glade_xml_get_widget(preview->xml, "custom_entry");
  g_signal_connect(G_OBJECT(preview->custom_entry), "changed",
                   G_CALLBACK(sep_button_clicked), (gpointer)preview);

  enctable = GTK_TABLE(glade_xml_get_widget(preview->xml, "enctable"));
  gtk_table_attach_defaults(enctable, encselector, 1, 2, 0, 1);
  gtk_widget_show_all(GTK_WIDGET(enctable));
  g_signal_connect(G_OBJECT(encselector), "charmap_changed",
                   G_CALLBACK(encoding_selected), (gpointer)preview);

  ok_button = glade_xml_get_widget(preview->xml, "ok_button");
  g_signal_connect(G_OBJECT(ok_button), "clicked",
                   G_CALLBACK(ok_button_clicked), (gpointer)preview);

  cancel_button = glade_xml_get_widget(preview->xml, "cancel_button");
  g_signal_connect(G_OBJECT(cancel_button), "clicked",
                   G_CALLBACK(cancel_button_clicked), (gpointer)preview);

  preview->treeview = (GtkTreeView*)(glade_xml_get_widget(preview->xml, "treeview"));
  preview->ctreeview = (GtkTreeView*)(glade_xml_get_widget(preview->xml, "ctreeview"));
  g_signal_connect(G_OBJECT(preview->treeview), "size-allocate",
                   G_CALLBACK(treeview_resized), (gpointer)preview);

  /* TODO Free stuff */
  preview->approved = FALSE;
  return preview;
}

static void gnc_csv_preview_free(GncCsvPreview* preview)
{
  g_object_unref(preview->xml);
  g_free(preview);
}

static void gnc_csv_preview_treeview(GncCsvPreview* preview, gboolean notEmpty)
{
  GtkListStore *store, **cstores, *ctstore;
  GtkTreeIter iter;
  int i, j, ncols = preview->parse_data->column_types->len;
  GType* types = g_malloc(2 * ncols * sizeof(GType));
  for(i = 0; i < ncols; i++)
    types[i] = G_TYPE_STRING;
  store = gtk_list_store_newv(ncols, types);
  for(i = 0; i < 2*ncols; i += 2)
  {
    types[i] = GTK_TYPE_TREE_MODEL;
    types[i+1] = G_TYPE_STRING;
  }
  ctstore = gtk_list_store_newv(2*ncols, types);
  cstores = g_malloc(ncols * sizeof(GtkListStore*));
  for(i = 0; i < ncols; i++)
  {
    cstores[i] = gtk_list_store_new(1, G_TYPE_STRING);
    gtk_list_store_append(cstores[i], &iter);
    gtk_list_store_set(cstores[i], &iter, 0, "Hello", -1);
    gtk_list_store_append(cstores[i], &iter);
    gtk_list_store_set(cstores[i], &iter, 0, "Good bye", -1);
  }

  /* Clear out any exisiting columns. */
  if(notEmpty)
  {
    GList *children, *children_begin;
    int size;
    do
    {
      GtkTreeViewColumn* col = gtk_tree_view_get_column(preview->treeview, 0);
      size = gtk_tree_view_remove_column(preview->treeview, col);
    } while(size);
    do
    {
      GtkTreeViewColumn* col = gtk_tree_view_get_column(preview->ctreeview, 0);
      size = gtk_tree_view_remove_column(preview->treeview, col);
    } while(size);
  }
  
  /* TODO free types */

  for(i = 0; i < preview->parse_data->orig_lines->len; i++)
  {
    gtk_list_store_append(store, &iter);
    for(j = 0; j < ((GPtrArray*)(preview->parse_data->orig_lines->pdata[i]))->len; j++)
    {
      gtk_list_store_set(store, &iter, j,
                         ((GPtrArray*)(preview->parse_data->orig_lines->pdata[i]))->pdata[j],
                         -1);
    }
  }
  gtk_list_store_append(ctstore, &iter);
  for(i = 0; i < ncols; i++)
  {
    gtk_list_store_set(ctstore, &iter, 2*i, cstores[i], 2*i+1, "Hello", -1);
  }

  for(i = 0; i < ncols; i++)
  {
    GtkCellRenderer *renderer = gtk_cell_renderer_text_new(),
      *crenderer = gtk_cell_renderer_combo_new();
    g_object_set(G_OBJECT(crenderer), "model", cstores[i], "text-column", 0,
                 "editable", TRUE, "has-entry", FALSE, NULL);
    g_signal_connect(G_OBJECT(crenderer), "edited",
                     G_CALLBACK(column_type_edited), (gpointer)preview);
    
    gtk_tree_view_insert_column_with_attributes(preview->treeview,
                                                -1, "", renderer, "text", i, NULL);
    gtk_tree_view_insert_column_with_attributes(preview->ctreeview,
                                                -1, "", crenderer, "model", 2*i,
                                                "text", 2*i+1, NULL);
  }

  gtk_tree_view_set_model(preview->treeview, GTK_TREE_MODEL(store));
  g_object_unref(GTK_TREE_MODEL(store));
  gtk_tree_view_set_model(preview->ctreeview, GTK_TREE_MODEL(ctstore));
  g_object_unref(GTK_TREE_MODEL(ctstore));

  gtk_widget_show_all(GTK_WIDGET(preview->treeview));
  gtk_widget_show_all(GTK_WIDGET(preview->ctreeview));
  g_debug("ctreeview is %p\n", preview->ctreeview);
  /* TODO free cstore and ctstore */
}

static int gnc_csv_preview(GncCsvPreview* preview, GncCsvParseData* parse_data)
{
  preview->parse_data = parse_data;
  gnc_csv_preview_treeview(preview, FALSE);
  gtk_dialog_run(GTK_DIALOG(preview->dialog));

  if(preview->approved)
    return 0;
  else
    return 1;
}

typedef struct
{
  GncCsvParseData* parse_data;
  char approved;
  char* encoding;
} EncodingDialog;

static void encoding_dialog_selected(GOCharmapSel* selector, const char* enc,
                                     EncodingDialog* encdialog)
{
  static gboolean second_call = FALSE;
  if(second_call)
  {
    g_debug("set encoding %s\n", enc);
    encdialog->encoding = (char*)enc;
    second_call = FALSE;
  }
  else
  {
    second_call = TRUE;
  }
}

static void enc_ok_button_clicked(GtkWidget* button, EncodingDialog* encdialog)
{
  g_debug("OK!\n");
  g_debug("using dialog encoding %s\n", encdialog->encoding);
  if(!gnc_csv_convert_enc(encdialog->parse_data, encdialog->encoding))
    encdialog->approved = 1;
}

static void enc_cancel_button_clicked(GtkWidget* button, EncodingDialog* encdialog)
{
  g_debug("cancel!\n");
  encdialog->approved = -1;
}

static int gnc_csv_encoding_dialog(GncCsvParseData* parse_data)
{
  EncodingDialog* encdialog = g_malloc(sizeof(encdialog));
  GladeXML* xml;
  GtkWidget *ok_button, *cancel_button;
  GOCharmapSel* encselector = (GOCharmapSel*)go_charmap_sel_new(GO_CHARMAP_SEL_TO_UTF8);
  GtkVBox* vbox;
  GtkDialog* dialog;
  encdialog->parse_data = parse_data;
  encdialog->encoding = (char*)go_charmap_sel_get_encoding(encselector);
  encdialog->approved = 0;
  xml = gnc_glade_xml_new("preview-dialog.glade", "encdialog");
  dialog = (GtkDialog*)(glade_xml_get_widget(xml, "encdialog"));

  vbox = GTK_VBOX(glade_xml_get_widget(xml, "encvbox"));
  gtk_box_pack_start(GTK_BOX(vbox), GTK_WIDGET(encselector), TRUE, TRUE, 5);
  gtk_widget_show_all(GTK_WIDGET(vbox));
  g_signal_connect(G_OBJECT(encselector), "charmap_changed",
                   G_CALLBACK(encoding_dialog_selected), (gpointer)encdialog);

  ok_button = glade_xml_get_widget(xml, "enc_ok_button");
  g_signal_connect(G_OBJECT(ok_button), "clicked",
                   G_CALLBACK(enc_ok_button_clicked), (gpointer)encdialog);

  cancel_button = glade_xml_get_widget(xml, "enc_cancel_button");
  g_signal_connect(G_OBJECT(cancel_button), "clicked",
                   G_CALLBACK(enc_cancel_button_clicked), (gpointer)encdialog);

  gtk_dialog_run(GTK_DIALOG(dialog));

  g_object_unref(xml);
  /* TODO Free stuff */

  return encdialog->approved;
}

/* The function that actually imports a CSV/Fixed-Width file. */
/* TODO Comment this function. */
void gnc_file_csv_import(void)
{
  const char *selected_filename;
  char *default_dir;
  GNCImportMainMatcher* gnc_csv_importer_gui = NULL;

  default_dir = gnc_get_default_directory(GCONF_SECTION);

  selected_filename = gnc_file_dialog(_("Select an CSV/Fixed-Width file to import"),
				      NULL,
				      default_dir,
				      GNC_FILE_DIALOG_IMPORT);
  g_free(default_dir);

  if(selected_filename!=NULL)
  {
    Account* account;
    int i;
    GncCsvParseData* parse_data;
    GError* error = NULL;
    GList *transactions;
    GncCsvPreview* preview;
    
    /* Remember the directory as the default. */
    default_dir = g_path_get_dirname(selected_filename);
    gnc_set_default_directory(GCONF_SECTION, default_dir);
    g_free(default_dir);

    /* TODO Check for errors */

    parse_data = gnc_csv_new_parse_data();
    if(gnc_csv_load_file(parse_data, selected_filename, &error))
    {
      if(error->code == GNC_CSV_ENCODING_ERR)
      {
        int result;
        do
        {
          result = gnc_csv_encoding_dialog(parse_data);
        } while(result == 0);
        if(result == -1)
        {
          g_debug("Canceled!\n");
        }
        else
        {
          g_debug("Success!\n");
        }
      }
      else /* GNC_CSV_FILE_OPEN_ERR */
      {
        /* TODO Do real error handling */
        g_debug("Couldn't open file\n");
      }
    }
    if(gnc_csv_parse(parse_data, TRUE, &error))
    {
      /* TODO real error handling */
      g_debug("Error in parsing: %s\n", error->message);
    }

    parse_data->column_types->data[0] = GNC_CSV_DATE;
    parse_data->column_types->data[1] = GNC_CSV_DESCRIPTION;
    parse_data->column_types->data[2] = GNC_CSV_AMOUNT;

    preview = gnc_csv_new_preview();
    if(gnc_csv_preview(preview, parse_data))
    {
      gnc_csv_preview_free(preview);
      gnc_csv_parse_data_free(parse_data);
      return;
    }

    /* TODO Move this after the parsing code. */
    account = gnc_import_select_account(NULL, NULL, 1, NULL, NULL, 0, NULL, NULL);
    g_debug("2ctype 1: %d\n", parse_data->column_types->data[0]);

    gnc_parse_to_trans(parse_data, account);
    g_debug("3ctype 1: %d\n", parse_data->column_types->data[0]);

    /* Create the Generic transaction importer GUI. */
    gnc_csv_importer_gui = gnc_gen_trans_list_new(NULL, NULL, FALSE, 42);
    g_debug("4ctype 1: %d\n", parse_data->column_types->data[0]);

    transactions = parse_data->transactions;
    while(transactions != NULL)
    {
      gnc_gen_trans_list_add_trans(gnc_csv_importer_gui,
                                   (Transaction*)(transactions->data));
      transactions = g_list_next(transactions);
    }
    gnc_gen_trans_list_run(gnc_csv_importer_gui);
    
    gnc_csv_preview_free(preview);
    gnc_csv_parse_data_free(parse_data);
  }
}



/** @} */
