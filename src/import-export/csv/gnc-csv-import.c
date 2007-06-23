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

#include <stdlib.h> /* TODO Get rid of this */

#define GCONF_SECTION "dialogs/import/csv"

static QofLogModule log_module = GNC_MOD_IMPORT;

/* These are the different types of checkbuttons that the user can
 * click to configure separators in a delimited file. */
enum SEP_BUTTON_TYPES {SEP_SPACE, SEP_TAB, SEP_COMMA, SEP_COLON, SEP_SEMICOLON, SEP_HYPHEN,
                       SEP_NUM_OF_TYPES};

/* This struct contains all of the data relevant to the preview dialog
 * that lets the user configure an import. */
typedef struct
{
  GncCsvParseData* parse_data; /* The actual data */
  GladeXML* xml; /* The Glade file that contains the dialog. */
  GtkDialog* dialog; /* The dialog */
  GtkTreeView* treeview; /* The treeview containing the data */
  GtkTreeView* ctreeview; /* The treeview containing the column types */
  GtkCheckButton* sep_buttons[SEP_NUM_OF_TYPES]; /* Checkbuttons for common separators */
  GtkCheckButton* custom_cbutton; /* The checkbutton for a custom separator */
  GtkEntry* custom_entry; /* The entry for custom separators */
  gboolean approved; /* This is FALSE until the user clicks "OK". */
} GncCsvPreview;

static void gnc_csv_preview_treeview(GncCsvPreview* preview, gboolean notEmpty);

/* Event handler for when one of the separator checkbuttons is clicked. */
static void sep_button_clicked(GtkCheckButton* widget, GncCsvPreview* preview)
{
  /* The stock separator charactors */
  char* sep_chars[] = {" ", "\t", ",", ":", ";", "-"};
  /* A list of all the separators that have been checked. */
  GSList* separators = NULL;
  int i;

  /* Go through each of the separator buttons. */
  for(i = 0; i < SEP_NUM_OF_TYPES; i++)
  {
    /* If this button is checked, add the corresponding character to
     * the separators list. */
    if(gtk_toggle_button_get_active(GTK_TOGGLE_BUTTON(preview->sep_buttons[i])))
      separators = g_slist_append(separators, sep_chars[i]);
  }

  /* If the custom button is checked ... */
  if(gtk_toggle_button_get_active(GTK_TOGGLE_BUTTON(preview->custom_cbutton)))
  {
    /* ... get the separator string from the custom entry ... */
    char* custom_sep = (char*)gtk_entry_get_text(preview->custom_entry);
    if(custom_sep[0] != '\0') /* ... and if it isn't blank add it to the separators list. */
      separators = g_slist_append(separators, custom_sep);
  }

  /* Set the parse options using the separators list. */
  stf_parse_options_csv_set_separators(preview->parse_data->options, NULL, separators);
  g_slist_free(separators); /* Free the separators list. */

  /* TODO Handle error */
  gnc_csv_parse(preview->parse_data, FALSE, NULL);
  gnc_csv_preview_treeview(preview, TRUE);
}

/* Event handler for a new encoding being selected. */
static void encoding_selected(GOCharmapSel* selector, const char* encoding,
                              GncCsvPreview* preview)
{
  /* This gets called twice everytime a new encoding is selected. The
   * second call actually passes the correct data; thus, we only do
   * something the second time this is called. */
  static gboolean second_call = FALSE;

  /* If this is the second time the function is called ... */
  if(second_call)
  {
    GError* error = NULL;
    /* TODO Handle errors and comment */
    gnc_csv_convert_enc(preview->parse_data, encoding);
    gnc_csv_parse(preview->parse_data, FALSE, &error);
    gnc_csv_preview_treeview(preview, TRUE);
    second_call = FALSE;
  }
  else /* If this is the first call of the function ... */
  {
    second_call = TRUE; /* ... set the flag and wait for the next call. */
  }
}

/* This array contains all of the different strings for different column types. */
static char* column_type_strs[GNC_CSV_NUM_COL_TYPES] = {"None",
                                                        "Date",
                                                        "Description",
                                                        "Amount"};

/* Event handler for the "OK" button being clicked on the dialog. */
static void ok_button_clicked(GtkWidget* widget, GncCsvPreview* preview)
{
  /* Shorten the column_types identifier. */
  GArray* column_types = preview->parse_data->column_types;
  int i, ncols = column_types->len; /* ncols is the number of columns in the data. */
  /* store contains the actual strings appearing in the column types treeview. */
  GtkTreeModel* store = gtk_tree_view_get_model(preview->ctreeview);
  GtkTreeIter iter;
  /* Get an iterator for the first (and only) row. */
  gtk_tree_model_get_iter_first(store, &iter);

  /* Go through each of the columns. */
  for(i = 0; i < ncols; i++)
  {
    int type; /* The column type contained in this column. */
    gchar* contents; /* The column type string in this column. */
    /* Get the type string first. (store is arranged so that every two
     * columns is a pair of the model used for the combobox and the
     * string that appears, so that store looks like:
     * model 0, string 0, model 1, string 1, ..., model ncols, string ncols. */
    gtk_tree_model_get(store, &iter, 2*i+1, &contents, -1);

    /* Go through each column type until ... */
    for(type = 0; type < GNC_CSV_NUM_COL_TYPES; type++)
    {
      /* ... we find one that matches with what's in the column. */
      if(!strcmp(contents, column_type_strs[type]))
      {
        /* Set the column_types array appropriately and quit. */
        column_types->data[i] = type;
        break;
      }
    }
  }

  /* Close the dialog. */
  gtk_widget_hide((GtkWidget*)(preview->dialog));
  preview->approved = TRUE; /* The user has wants to do the import. */
}

/* Event handler for the "Cancel" dialog being clicked on the dialog. */
static void cancel_button_clicked(GtkWidget* widget, GncCsvPreview* preview)
{
  /* Simply close the dialog. */
  gtk_widget_hide((GtkWidget*)(preview->dialog));
}

/* Event handler for the treeview being resized. */
static void treeview_resized(GtkWidget* widget, GtkAllocation* allocation, GncCsvPreview* preview)
{
  /* ncols is the number of columns in the data. */
  int i, ncols = preview->parse_data->column_types->len;

  /* Go through each column except for the last. (We don't want to set
   * the width of the last column because the user won't be able to
   * shrink the dialog back if it's expanded.) */
  for(i = 0; i < ncols - 1; i++)
  {
    gint col_width; /* The width of the column in preview->treeview. */
    GtkTreeViewColumn* ccol; /* The corresponding column in preview->ctreeview. */

    /* Get the width. */
    col_width = gtk_tree_view_column_get_width(gtk_tree_view_get_column(preview->treeview, i));

    /* Set ccol's width the same. */
    ccol = gtk_tree_view_get_column(preview->ctreeview, i);
    gtk_tree_view_column_set_min_width(ccol, col_width);
    gtk_tree_view_column_set_max_width(ccol, col_width);
  }
}

/* Event handler for the user selecting a new column type. */
static void column_type_edited(GtkCellRenderer *renderer, gchar *path,
                               gchar *new_text, GncCsvPreview* preview)
{
  /* ncols is the number of columns in the data. */
  int i, ncols = preview->parse_data->column_types->len;
  /* store has the actual strings that appear in preview->ctreeview. */
  GtkTreeModel* store = gtk_tree_view_get_model(preview->ctreeview);
  GtkTreeIter iter;
  /* Get an iterator for the first (and only) row. */
  gtk_tree_model_get_iter_first(store, &iter);

  /* Go through each column. */
  for(i = 0; i < ncols; i++)
  {
    /* We need all this stuff so that we can find out whether or not
     * this was the column that was edited. */
    GtkCellRenderer* col_renderer; /* The renderer for this column. */
    /* The column in the treeview we are looking at */
    GtkTreeViewColumn* col = gtk_tree_view_get_column(preview->ctreeview, i);
    /* The list of renderers for col */
    GList* rend_list = gtk_tree_view_column_get_cell_renderers(col);
    /* rend_list has only one entry, which we put in col_renderer. */
    col_renderer = rend_list->data;
    g_list_free(rend_list); /* Free rend_list since we don't need it anymore. */

    /* If this is not the column that was edited ... */
    if(col_renderer != renderer)
    {
      /* The string that appears in the column */
      gchar* contents;
      /* Get the type string. (store is arranged so that every two
       * columns is a pair of the model used for the combobox and the
       * string that appears, so that store looks like:
       * model 0, string 0, model 1, string 1, ..., model ncols, string ncols. */
      gtk_tree_model_get(store, &iter, 2*i+1, &contents, -1);
      /* If this column has the same string that the user selected ... */
      if(!strcmp(contents, new_text))
      {
        /* ... set this column to the "None" type. (We can't allow duplicate types.) */
        gtk_list_store_set(GTK_LIST_STORE(store), &iter, 2*i+1,
                           column_type_strs[GNC_CSV_NONE], -1);
      }
    }
    else /* If this is the column that was edited ... */
    {
      /* Set the text for this column to what the user selected. (See
       * comment above "Get the type string. ..." for why we set
       * column 2*i+1 in store.) */
      gtk_list_store_set(GTK_LIST_STORE(store), &iter, 2*i+1, new_text, -1);
    }
  }
}

/* Constructor for GncCsvPreview. */
static GncCsvPreview* gnc_csv_preview_new()
{
  int i;
  GncCsvPreview* preview = g_malloc(sizeof(GncCsvPreview));
  GtkWidget *ok_button, *cancel_button;
  /* The names in the glade file for the sep buttons. */
  char* sep_button_names[] = {"space_cbutton",
                              "tab_cbutton",
                              "comma_cbutton",
                              "colon_cbutton",
                              "semicolon_cbutton",
                              "hyphen_cbutton"};
  /* The table containing encselector and the separator configuration widgets */
  GtkTable* enctable;
  /* The widget for selecting the encoding. (The default is UTF-8.) */
  GtkWidget* encselector = go_charmap_sel_new(GO_CHARMAP_SEL_TO_UTF8);
  /* Connect it to the encoding_selected event handler. */
  g_signal_connect(G_OBJECT(encselector), "charmap_changed",
                   G_CALLBACK(encoding_selected), (gpointer)preview);

  /* Load the Glade file. */
  preview->xml = gnc_glade_xml_new("gnc-csv-preview-dialog.glade", "dialog");
  /* Load the dialog. */
  preview->dialog = (GtkDialog*)(glade_xml_get_widget(preview->xml, "dialog"));

  /* Load the separator buttons from the glade file into the
   * preview->sep_buttons array. */
  for(i = 0; i < SEP_NUM_OF_TYPES; i++)
  {
    preview->sep_buttons[i]
      = (GtkCheckButton*)glade_xml_get_widget(preview->xml, sep_button_names[i]);
    /* Connect them to the sep_button_clicked event handler. */
    g_signal_connect(G_OBJECT(preview->sep_buttons[i]), "toggled",
                     G_CALLBACK(sep_button_clicked), (gpointer)preview);
  }

  /* Load and connect the custom separator checkbutton in the same way
   * as the other separator buttons. */
  preview->custom_cbutton
    = (GtkCheckButton*)glade_xml_get_widget(preview->xml, "custom_cbutton");
  g_signal_connect(G_OBJECT(preview->custom_cbutton), "clicked",
                   G_CALLBACK(sep_button_clicked), (gpointer)preview);

  /* Load the entry for the custom separator entry. Connect it to the
   * sep_button_clicked event handler as well. */
  preview->custom_entry = (GtkEntry*)glade_xml_get_widget(preview->xml, "custom_entry");
  g_signal_connect(G_OBJECT(preview->custom_entry), "changed",
                   G_CALLBACK(sep_button_clicked), (gpointer)preview);

  /* Get the table from the Glade file. */
  enctable = GTK_TABLE(glade_xml_get_widget(preview->xml, "enctable"));
  /* Put the selector in at the top. */
  gtk_table_attach_defaults(enctable, encselector, 1, 2, 0, 1);
  /* Show the table in all its glory. */
  gtk_widget_show_all(GTK_WIDGET(enctable));

  /* Connect the "OK" and "Cancel" buttons to their event handlers. */
  ok_button = glade_xml_get_widget(preview->xml, "ok_button");
  g_signal_connect(G_OBJECT(ok_button), "clicked",
                   G_CALLBACK(ok_button_clicked), (gpointer)preview);

  cancel_button = glade_xml_get_widget(preview->xml, "cancel_button");
  g_signal_connect(G_OBJECT(cancel_button), "clicked",
                   G_CALLBACK(cancel_button_clicked), (gpointer)preview);

  /* Load the data treeview and connect it to its resizing event handler. */
  preview->treeview = (GtkTreeView*)(glade_xml_get_widget(preview->xml, "treeview"));
  g_signal_connect(G_OBJECT(preview->treeview), "size-allocate",
                   G_CALLBACK(treeview_resized), (gpointer)preview);

  /* Load the column type treeview. */
  preview->ctreeview = (GtkTreeView*)(glade_xml_get_widget(preview->xml, "ctreeview"));

  /* TODO Free stuff */
  preview->approved = FALSE; /* This is FALSE until the user clicks "OK". */

  return preview;
}

/* Function for destroying a preview when we're done with it. */
static void gnc_csv_preview_free(GncCsvPreview* preview)
{
  g_object_unref(preview->xml);
  g_free(preview);
}

/* This function loads the preview's data (preview->parse_data) into
 * its data treeview. notEmpty is TRUE when the data treeview already
 * contains data, FALSE otherwise (e.g. the first time this function
 * is called on a preview). */
/* TODO Something's probably screwing up with this function when
 * selecting a new encoding. */
/* TODO Comment this function. */
/* TODO Look at getting rid of notEmpty */
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
    for(j = 0; j < GNC_CSV_NUM_COL_TYPES; j++)
    {
      gtk_list_store_append(cstores[i], &iter);
      gtk_list_store_set(cstores[i], &iter, 0, column_type_strs[j], -1);
    }
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
      size = gtk_tree_view_remove_column(preview->ctreeview, col);
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
    gtk_list_store_set(ctstore, &iter, 2*i, cstores[i], 2*i+1, "None", -1);
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

/* This function is used to let the user preview and configure the
 * data parsed from the file. It doesn't return until the user clicks
 * "OK" or "Cancel" on the dialog. It returns 0 if the user approved
 * the import and 1 if the user didn't. */
static int gnc_csv_preview(GncCsvPreview* preview, GncCsvParseData* parse_data)
{
  /* Set the preview's parse_data to the one we're getting passed. */
  preview->parse_data = parse_data;
  /* Load the data into the treeview. (This is the first time we've
   * called gnc_csv_preview_treeview on this preview, so we use
   * FALSE. */
  gnc_csv_preview_treeview(preview, FALSE);
  /* Wait until the user clicks "OK" or "Cancel". */
  gtk_dialog_run(GTK_DIALOG(preview->dialog));

  /* Return 0 or 1 if preview->approved is TRUE or FALSE, respectively. */
  if(preview->approved)
    return 0;
  else
    return 1;
}

/* The function that actually imports a CSV/Fixed-Width file. */
/* TODO Comment this function. */
void gnc_file_csv_import(void)
{
  /* The name of the file the user selected. */
  char* selected_filename;
   /* The default directory for the user to select files. */
  char* default_dir= gnc_get_default_directory(GCONF_SECTION);
  /* The generic GUI for importing transactions. */
  GNCImportMainMatcher* gnc_csv_importer_gui = NULL;

  /* Let the user select a file. */
  selected_filename = gnc_file_dialog(_("Select an CSV/Fixed-Width file to import"),
				      NULL,
				      default_dir,
				      GNC_FILE_DIALOG_IMPORT);
  g_free(default_dir); /* We don't need default_dir anymore. */

  /* If the user actually selected a file ... */
  if(selected_filename!=NULL)
  {
    int i;
    Account* account; /* The account the user will select */
    GError* error = NULL;
    GList* transactions; /* A list of the transactions we create. */
    GncCsvParseData* parse_data;
    GncCsvPreview* preview;
    
    /* Remember the directory of the selected file as the default. */
    default_dir = g_path_get_dirname(selected_filename);
    gnc_set_default_directory(GCONF_SECTION, default_dir);
    g_free(default_dir);

    /* TODO Check for errors */

    /* Load the file into parse_data. */
    parse_data = gnc_csv_new_parse_data();
    if(gnc_csv_load_file(parse_data, selected_filename, &error))
    {
      /* If we couldn't load the file ... */
      /* TODO Do real error handling */
      g_debug("Couldn't open file\n");
    }
    /* Parse the data. */
    if(gnc_csv_parse(parse_data, TRUE, &error))
    {
      /* If we couldn't parse the data ... */
      /* TODO real error handling */
      g_debug("Error in parsing: %s\n", error->message);
    }

    /* Preview the data. */
    preview = gnc_csv_preview_new();
    if(gnc_csv_preview(preview, parse_data))
    {
      /* If the user clicked "Cancel", free everything and quit. */
      gnc_csv_preview_free(preview);
      gnc_csv_parse_data_free(parse_data);
      g_free(selected_filename);
      return;
    }

    /* Let the user select an account to put the transactions in. */
    account = gnc_import_select_account(NULL, NULL, 1, NULL, NULL, 0, NULL, NULL);

    /* Create transactions from the parsed data. */
    /* TODO Handle errors here. */
    gnc_parse_to_trans(parse_data, account);

    /* Create the genereic transaction importer GUI. */
    gnc_csv_importer_gui = gnc_gen_trans_list_new(NULL, NULL, FALSE, 42);

    /* Get the list of the transactions that were created. */
    transactions = parse_data->transactions;
    /* Copy all of the transactions to the importer GUI. */
    while(transactions != NULL)
    {
      gnc_gen_trans_list_add_trans(gnc_csv_importer_gui,
                                   (Transaction*)(transactions->data));
      transactions = g_list_next(transactions);
    }
    /* Let the user load those transactions into the account. */
    gnc_gen_trans_list_run(gnc_csv_importer_gui);

    /* Free the memory we allocated. */
    gnc_csv_preview_free(preview);
    gnc_csv_parse_data_free(parse_data);
    g_free(selected_filename);
  }
}

/** @} */
