/*
 * dialog-bi-import-gui.c -- Invoice Importer GUI
 *
 * This program is free software; you can redistribute it and/or
 * modify it under the terms of the GNU General Public License as
 * published by the Free Software Foundation; either version 2 of
 * the License, or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, contact:
 *
 * Free Software Foundation           Voice:  +1-617-542-5942
 * 51 Franklin Street, Fifth Floor    Fax:    +1-617-542-2652
 * Boston, MA  02110-1301,  USA       gnu@gnu.org
 */

/**
 * @internal
 * @file dialog-bi-import-gui.c
 * @brief GUI handling for bi-import plugin
 * @author Copyright (C) 2009 Sebastian Held <sebastian.held@gmx.de>
 * @author Mike Evans <mikee@saxicola.co.uk>
 * @author Rob Laan <rob.laan@chello.nl>
 */

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include <glib/gi18n.h>

#include "gnc-ui.h"
#include "gnc-ui-util.h"
#include "gnc-component-manager.h"
#include "dialog-utils.h"
#include "gnc-gui-query.h"
#include "gnc-file.h"
#include "dialog-bi-import.h"
#include "dialog-bi-import-gui.h"

typedef struct _BiImportNoticeSequence BiImportNoticeSequence;

struct _bi_import_gui
{
    GtkWindow    *parent;
    GtkWidget    *dialog;
    GtkWidget    *tree_view;
    GtkWidget    *entryFilename;
    GtkListStore *store;
    gint          component_id;
    GString      *regexp;
    QofBook      *book;
    gchar        *type;
    gchar        *open_mode;
    BiImportNoticeSequence *notice_sequence;
};

struct _BiImportNoticeSequence
{
    BillImportGui *gui;
    guint count;
    guint next;
    gchar *titles[3];
    gchar *messages[3];
};

typedef struct
{
    BillImportGui *gui;
    bi_import_stats stats;
    GString *info;
    guint n_fixed, n_deleted;
} BiImportPending;

static void
bi_import_pending_free (BiImportPending *pending)
{
    if (pending->stats.ignored_lines)
        g_string_free (pending->stats.ignored_lines, TRUE);
    if (pending->info)
        g_string_free (pending->info, TRUE);
    g_free (pending);
}

static void bi_import_notice_dismissed (GtkWindow *parent, gint response,
                                        gpointer user_data);

static void
bi_import_notice_sequence_free (BiImportNoticeSequence *sequence)
{
    for (guint i = 0; i < sequence->count; ++i)
    {
        g_free (sequence->titles[i]);
        g_free (sequence->messages[i]);
    }
    g_free (sequence);
}

static void
bi_import_notice_show_next (GtkWindow *parent, BiImportNoticeSequence *sequence)
{
    BillImportGui *gui = sequence->gui;
    if (!parent || !gui || GTK_WIDGET (parent) != gui->dialog)
    {
        if (gui)
            gui->notice_sequence = NULL;
        bi_import_notice_sequence_free (sequence);
        return;
    }

    if (sequence->next < sequence->count)
    {
        guint index = sequence->next++;
        if (sequence->titles[index])
            gnc_info2_dialog_async (GTK_WIDGET (parent), sequence->titles[index],
                                    sequence->messages[index],
                                    bi_import_notice_dismissed, sequence);
        else
            gnc_info_dialog_async_response (parent, bi_import_notice_dismissed,
                                            sequence, "%s",
                                            sequence->messages[index]);
        return;
    }

    gui->notice_sequence = NULL;
    gint component_id = gui->component_id;
    bi_import_notice_sequence_free (sequence);
    gnc_close_gui_component (component_id);
}

static void
bi_import_notice_dismissed (GtkWindow *parent, [[maybe_unused]] gint response,
                            gpointer user_data)
{
    bi_import_notice_show_next (parent, user_data);
}

static void
bi_import_notice_add (BiImportNoticeSequence *sequence, const gchar *title,
                      const gchar *message)
{
    guint index = sequence->count++;
    sequence->titles[index] = g_strdup (title);
    sequence->messages[index] = g_strdup (message);
}


// callback routines
void gnc_bi_import_gui_ok_cb (GtkWidget *widget, gpointer data);
void gnc_bi_import_gui_cancel_cb (GtkWidget *widget, gpointer data);
void gnc_bi_import_gui_help_cb (GtkWidget *widget, gpointer data);
void gnc_bi_import_gui_destroy_cb (GtkWidget *widget, gpointer data);
static void gnc_bi_import_gui_close_handler (gpointer user_data);
void gnc_bi_import_gui_buttonOpen_cb (GtkWidget *widget, gpointer data);
void gnc_bi_import_gui_filenameChanged_cb (GtkWidget *widget, gpointer data);
void gnc_bi_import_gui_option1_cb (GtkWidget *widget, gpointer data);
void gnc_bi_import_gui_option2_cb (GtkWidget *widget, gpointer data);
void gnc_bi_import_gui_option3_cb (GtkWidget *widget, gpointer data);
void gnc_bi_import_gui_option4_cb (GtkWidget *widget, gpointer data);
void gnc_bi_import_gui_option5_cb (GtkWidget *widget, gpointer data);
void gnc_bi_import_gui_open_mode_cb (GtkWidget *widget, gpointer data);
void gnc_import_gui_type_cb (GtkWidget *widget, gpointer data);

#define UNUSED_VAR     __attribute__ ((unused))

static QofLogModule UNUSED_VAR log_module = G_LOG_DOMAIN; //G_LOG_BUSINESS;

BillImportGui *
gnc_plugin_bi_import_showGUI (GtkWindow *parent)
{
    BillImportGui *gui;
    GtkBuilder *builder;
    GList *glist;
    GtkCellRenderer *renderer;
    GtkTreeViewColumn *column;

    // if window exists already, activate it
    glist = gnc_find_gui_components ("dialog-bi-import-gui", NULL, NULL);
    if (glist)
    {
        // window found
        gui = g_list_nth_data (glist, 0);
        g_list_free (glist);

        gtk_window_set_transient_for(GTK_WINDOW(gui->dialog), GTK_WINDOW(parent));
        gui->parent = parent;
        gtk_window_present (GTK_WINDOW(gui->dialog));
        return gui;
    }

    // create new window
    gui = g_new0 (BillImportGui, 1);
    gui->type = "BILL"; // Set default type to match gui.  really shouldn't be here TODO change me
    gui->open_mode = "ALL";

    builder = gtk_builder_new();
    gnc_builder_add_from_file (builder, "dialog-bi-import-gui.glade", "bi_import_dialog");
    gui->dialog = GTK_WIDGET(gtk_builder_get_object (builder, "bi_import_dialog"));
    gtk_window_set_transient_for(GTK_WINDOW(gui->dialog), GTK_WINDOW(parent));
    gui->parent = parent;
    gui->tree_view = GTK_WIDGET(gtk_builder_get_object (builder, "treeview1"));
    gui->entryFilename = GTK_WIDGET(gtk_builder_get_object (builder, "entryFilename"));

    // Set the name for this dialog so it can be easily manipulated with css
    gtk_widget_set_name (GTK_WIDGET(gui->dialog), "gnc-id-bill-import");
    gnc_widget_style_context_add_class (GTK_WIDGET(gui->dialog), "gnc-class-imports");

    gtk_window_set_transient_for (GTK_WINDOW (gui->dialog), parent);

    gui->book = gnc_get_current_book();

    gui->regexp = g_string_new ( "^(\\x{FEFF})?(?<id>[^;]*);(?<date_opened>[^;]*);(?<owner_id>[^;]*);(?<billing_id>[^;]*);(?<notes>[^;]*);(?<date>[^;]*);(?<desc>[^;]*);(?<action>[^;]*);(?<account>[^;]*);(?<quantity>[^;]*);(?<price>[^;]*);(?<disc_type>[^;]*);(?<disc_how>[^;]*);(?<discount>[^;]*);(?<taxable>[^;]*);(?<taxincluded>[^;]*);(?<tax_table>[^;]*);(?<date_posted>[^;]*);(?<due_date>[^;]*);(?<account_posted>[^;]*);(?<memo_posted>[^;]*);(?<accu_splits>[^;]*)$");

    // create model and bind to view
    gui->store = gtk_list_store_new (N_COLUMNS,
                                     G_TYPE_STRING, G_TYPE_STRING, G_TYPE_STRING, G_TYPE_STRING, // invoice settings
                                     G_TYPE_STRING, G_TYPE_STRING, G_TYPE_STRING, G_TYPE_STRING, G_TYPE_STRING, G_TYPE_STRING, G_TYPE_STRING, G_TYPE_STRING, G_TYPE_STRING, G_TYPE_STRING, G_TYPE_STRING, G_TYPE_STRING, G_TYPE_STRING, // entry settings
                                     G_TYPE_STRING, G_TYPE_STRING, G_TYPE_STRING, G_TYPE_STRING, G_TYPE_STRING); // autopost settings
    gtk_tree_view_set_model( GTK_TREE_VIEW(gui->tree_view), GTK_TREE_MODEL(gui->store) );
#define CREATE_COLUMN(description,column_id) \
  renderer = gtk_cell_renderer_text_new (); \
  column = gtk_tree_view_column_new_with_attributes (description, renderer, "text", column_id, NULL); \
  gtk_tree_view_column_set_resizable (column, TRUE); \
  gtk_tree_view_append_column (GTK_TREE_VIEW (gui->tree_view), column);
    CREATE_COLUMN (_("ID"), ID);
    CREATE_COLUMN (_("Date Opened"), DATE_OPENED);
    CREATE_COLUMN (_("Owner-ID"), OWNER_ID);
    CREATE_COLUMN (_("Billing-ID"), BILLING_ID);
    CREATE_COLUMN (_("Notes"), NOTES);

    CREATE_COLUMN (_("Date"), DATE);
    CREATE_COLUMN (_("Description"), DESC);
    CREATE_COLUMN (_("Action"), ACTION);
    CREATE_COLUMN (_("Account"), ACCOUNT);
    CREATE_COLUMN (_("Quantity"), QUANTITY);
    CREATE_COLUMN (_("Price"), PRICE);
    CREATE_COLUMN (_("Disc-type"), DISC_TYPE);
    CREATE_COLUMN (_("Disc-how"), DISC_HOW);
    CREATE_COLUMN (_("Discount"), DISCOUNT);
    CREATE_COLUMN (_("Taxable"), TAXABLE);
    CREATE_COLUMN (_("Taxincluded"), TAXINCLUDED);
    CREATE_COLUMN (_("Tax-table"), TAX_TABLE);

    CREATE_COLUMN (_("Date Posted"), DATE_POSTED);
    CREATE_COLUMN (_("Due Date"), DUE_DATE);
    CREATE_COLUMN (_("Account-posted"), ACCOUNT_POSTED);
    CREATE_COLUMN (_("Memo-posted"), MEMO_POSTED);
    CREATE_COLUMN (_("Accu-splits"), ACCU_SPLITS);

    gui->component_id = gnc_register_gui_component ("dialog-bi-import-gui",
                        NULL,
                        gnc_bi_import_gui_close_handler,
                        gui);

    /* Setup signals */
    gtk_builder_connect_signals_full (builder, gnc_builder_connect_full_func, gui);

    gtk_widget_show_all ( gui->dialog );

    g_object_unref(G_OBJECT(builder));

    return gui;
}

typedef struct
{
    GtkWidget *entry;
} BiImportFileRequest;

static void
bi_import_file_selected (GSList *filenames, gpointer user_data)
{
    BiImportFileRequest *request = user_data;
    if (request->entry && filenames)
        gtk_entry_set_text (GTK_ENTRY (request->entry), filenames->data);
    g_slist_free_full (filenames, g_free);
}

static void
bi_import_file_request_free (gpointer user_data)
{
    BiImportFileRequest *request = user_data;
    if (request->entry)
        g_object_remove_weak_pointer (G_OBJECT (request->entry),
                                      (gpointer *)&request->entry);
    g_free (request);
}

static void
gnc_plugin_bi_import_getFilename(GtkWindow *parent, GtkWidget *entry)
{
    GList *filters = NULL;
    GtkFileFilter *filter = gtk_file_filter_new ();
    gtk_file_filter_set_name (filter, "comma separated values (*.csv)");
    gtk_file_filter_add_pattern (filter, "*.csv");
    filters = g_list_append( filters, filter );
    filter = gtk_file_filter_new ();
    gtk_file_filter_set_name (filter, "text files (*.txt)");
    gtk_file_filter_add_pattern (filter, "*.txt");
    filters = g_list_append( filters, filter );
    BiImportFileRequest *request = g_new0 (BiImportFileRequest, 1);
    request->entry = entry;
    g_object_add_weak_pointer (G_OBJECT (entry), (gpointer *)&request->entry);
    gnc_file_dialog_async (parent, _("Import Bills or Invoices from CSV"),
                           filters, NULL, GNC_FILE_DIALOG_IMPORT, FALSE,
                           bi_import_file_selected, request,
                           bi_import_file_request_free);
}

static void
bi_import_finish_rows (GtkWindow *parent, gint response, gpointer user_data)
{
    BiImportPending *pending = user_data;
    BillImportGui *gui = pending->gui;

    if (!parent || !gui || GTK_WIDGET (parent) != gui->dialog)
    {
        bi_import_pending_free (pending);
        return;
    }

    guint n_invoices_created = 0, n_invoices_updated = 0;
    BiImportNoticeSequence *sequence = g_new0 (BiImportNoticeSequence, 1);
    gnc_bi_import_create_bis (gui->store, gui->book, &n_invoices_created,
                              &n_invoices_updated, &pending->n_deleted,
                              gui->type, gui->open_mode, pending->info,
                              gui->parent, response == GTK_RESPONSE_YES);
    if (pending->info->len > 0)
        bi_import_notice_add (sequence, NULL, pending->info->str);
    gchar *summary = g_strdup_printf (_("Import:\n- rows ignored: %i\n- rows imported: %i\n\nValidation & processing:\n- rows fixed: %u\n- rows ignored: %u\n- invoices created: %u\n- invoices updated: %u"),
                                      pending->stats.n_ignored,
                                      pending->stats.n_imported,
                                      pending->n_fixed, pending->n_deleted,
                                      n_invoices_created, n_invoices_updated);
    bi_import_notice_add (sequence, NULL, summary);
    g_free (summary);
    if (pending->stats.n_ignored > 0)
        bi_import_notice_add (sequence,
                              _("These lines were ignored during import"),
                              pending->stats.ignored_lines->str);
    bi_import_pending_free (pending);
    sequence->gui = gui;
    gui->notice_sequence = sequence;
    bi_import_notice_show_next (GTK_WINDOW (gui->dialog), sequence);
}

void
gnc_bi_import_gui_ok_cb (GtkWidget *widget, gpointer data)
{
    BillImportGui *gui = data;
    if (!gui || gui->notice_sequence)
        return;
    gchar *filename = g_strdup( gtk_entry_get_text( GTK_ENTRY(gui->entryFilename) ) );
    BiImportPending *pending = g_new0 (BiImportPending, 1);
    pending->gui = gui;
    bi_import_result res;
    GString *info = g_string_new ("");

    // import
    gtk_list_store_clear (gui->store);
    res = gnc_bi_import_read_file (filename, gui->regexp->str, gui->store, 0,
                                   &pending->stats);
    g_free (filename);
    if (res == RESULT_OK)
    {
        pending->info = info;
        gnc_bi_import_fix_bis (gui->store, &pending->n_fixed,
                               &pending->n_deleted, pending->info, gui->type);
        if (gnc_bi_import_has_existing_bis (gui->store, gui->book, gui->type))
            gnc_verify_dialog_async (GTK_WINDOW (gui->dialog), TRUE,
                                     bi_import_finish_rows, pending,
                                     "%s", _("Do you want to update existing bills/invoices?"));
        else
            bi_import_finish_rows (GTK_WINDOW (gui->dialog), GTK_RESPONSE_YES,
                                  pending);
    }
    else if (res ==  RESULT_OPEN_FAILED)
    {
        gnc_error_dialog_async (GTK_WINDOW (gui->dialog), "%s",
                                _("The input file can not be opened."));
        pending->info = info;
        bi_import_pending_free (pending);
    }
    else if (res ==  RESULT_ERROR_IN_REGEXP)
    {
        //gnc_error_dialog (GTK_WINDOW (gui->dialog), "The regular expression is faulty:\n\n%s", stats.err->str);
        pending->info = info;
        bi_import_pending_free (pending);
    }
}

void
gnc_bi_import_gui_cancel_cb (GtkWidget *widget, gpointer data)
{
    BillImportGui *gui = data;

    gnc_close_gui_component (gui->component_id);
}

void
gnc_bi_import_gui_help_cb (GtkWidget *widget, gpointer data)
{
    BillImportGui *gui = data;
    gnc_gnome_help (GTK_WINDOW(gui->dialog), DF_GUIDE, DL_IMPORT_BC);
}

static void
gnc_bi_import_gui_close_handler (gpointer user_data)
{
    BillImportGui *gui = user_data;

    gtk_widget_destroy (gui->dialog);
    // gui has already been freed by this point.
    // gui->dialog = NULL;
}

void
gnc_bi_import_gui_destroy_cb (GtkWidget *widget, gpointer data)
{
    BillImportGui *gui = data;

    if (gui->notice_sequence)
    {
        gui->notice_sequence->gui = NULL;
        gui->notice_sequence = NULL;
    }

    gnc_suspend_gui_refresh ();
    gnc_unregister_gui_component (gui->component_id);
    gnc_resume_gui_refresh ();

    g_object_unref (gui->store);
    g_string_free (gui->regexp, TRUE);
    g_free (gui);
}

void gnc_bi_import_gui_buttonOpen_cb (GtkWidget *widget, gpointer data)
{
    BillImportGui *gui = data;
    gnc_plugin_bi_import_getFilename (gnc_ui_get_gtk_window (widget),
                                      gui->entryFilename);
}

void gnc_bi_import_gui_filenameChanged_cb (GtkWidget *widget, gpointer data)
{
    BillImportGui *gui = data;
    gchar *filename = g_strdup( gtk_entry_get_text( GTK_ENTRY(gui->entryFilename) ) );

    // generate preview
    gtk_list_store_clear (gui->store);
    gnc_bi_import_read_file (filename, gui->regexp->str, gui->store, 100, NULL);

    g_free( filename );
}

// Semicolon separated
void gnc_bi_import_gui_option1_cb (GtkWidget *widget, gpointer data)
{
    BillImportGui *gui = data;
    if (!gtk_toggle_button_get_active( GTK_TOGGLE_BUTTON(widget) ))
        return;
    g_string_assign (gui->regexp, "^(\\x{FEFF})?(?<id>[^;]*);(?<date_opened>[^;]*);(?<owner_id>[^;]*);(?<billing_id>[^;]*);(?<notes>[^;]*);(?<date>[^;]*);(?<desc>[^;]*);(?<action>[^;]*);(?<account>[^;]*);(?<quantity>[^;]*);(?<price>[^;]*);(?<disc_type>[^;]*);(?<disc_how>[^;]*);(?<discount>[^;]*);(?<taxable>[^;]*);(?<taxincluded>[^;]*);(?<tax_table>[^;]*);(?<date_posted>[^;]*);(?<due_date>[^;]*);(?<account_posted>[^;]*);(?<memo_posted>[^;]*);(?<accu_splits>[^;]*)$");
    gnc_bi_import_gui_filenameChanged_cb (gui->entryFilename, gui);
}

// Comma separated
void gnc_bi_import_gui_option2_cb (GtkWidget *widget, gpointer data)
{
    BillImportGui *gui = data;
    if (!gtk_toggle_button_get_active( GTK_TOGGLE_BUTTON(widget) ))
        return;
    g_string_assign (gui->regexp, "^(\\x{FEFF})?(?<id>[^,]*),(?<date_opened>[^,]*),(?<owner_id>[^,]*),(?<billing_id>[^,]*),(?<notes>[^,]*),(?<date>[^,]*),(?<desc>[^,]*),(?<action>[^,]*),(?<account>[^,]*),(?<quantity>[^,]*),(?<price>[^,]*),(?<disc_type>[^,]*),(?<disc_how>[^,]*),(?<discount>[^,]*),(?<taxable>[^,]*),(?<taxincluded>[^,]*),(?<tax_table>[^,]*),(?<date_posted>[^,]*),(?<due_date>[^,]*),(?<account_posted>[^,]*),(?<memo_posted>[^,]*),(?<accu_splits>[^,]*)$");
    gnc_bi_import_gui_filenameChanged_cb (gui->entryFilename, gui);
}

// Semicolon separated with quotes
void gnc_bi_import_gui_option3_cb (GtkWidget *widget, gpointer data)
{
    BillImportGui *gui = data;
    if (!gtk_toggle_button_get_active( GTK_TOGGLE_BUTTON(widget) ))
        return;
    g_string_assign (gui->regexp, "^(\\x{FEFF})?((?<id>[^\";]*)|\"(?<id>[^\"]*)\");((?<date_opened>[^\";]*)|\"(?<date_opened>[^\"]*)\");((?<owner_id>[^\";]*)|\"(?<owner_id>[^\"]*)\");((?<billing_id>[^\";]*)|\"(?<billing_id>[^\"]*)\");((?<notes>[^\";]*)|\"(?<notes>([^\"]|\"\")*)\");((?<date>[^\";]*)|\"(?<date>[^\"]*)\");((?<desc>[^\";]*)|\"(?<desc>([^\"]|\"\")*)\");((?<action>[^\";]*)|\"(?<action>[^\"]*)\");((?<account>[^\";]*)|\"(?<account>[^\"]*)\");((?<quantity>[^\";]*)|\"(?<quantity>[^\"]*)\");((?<price>[^\";]*)|\"(?<price>[^\"]*)\");((?<disc_type>[^\";]*)|\"(?<disc_type>[^\"]*)\");((?<disc_how>[^\";]*)|\"(?<disc_how>[^\"]*)\");((?<discount>[^\";]*)|\"(?<discount>[^\"]*)\");((?<taxable>[^\";]*)|\"(?<taxable>[^\"]*)\");((?<taxincluded>[^\";]*)|\"(?<taxincluded>[^\"]*)\");((?<tax_table>[^\";]*)|\"(?<tax_table>[^\"]*)\");((?<date_posted>[^\";]*)|\"(?<date_posted>[^\"]*)\");((?<due_date>[^\";]*)|\"(?<due_date>[^\"]*)\");((?<account_posted>[^\";]*)|\"(?<account_posted>[^\"]*)\");((?<memo_posted>[^\";]*)|\"(?<memo_posted>[^\"]*)\");((?<accu_splits>[^\";]*)|\"(?<accu_splits>[^\"]*)\")$");
    gnc_bi_import_gui_filenameChanged_cb (gui->entryFilename, gui);
}

// Comma separated with quote
void gnc_bi_import_gui_option4_cb (GtkWidget *widget, gpointer data)
{
    BillImportGui *gui = data;
    if (!gtk_toggle_button_get_active( GTK_TOGGLE_BUTTON(widget) ))
        return;
    g_string_assign (gui->regexp, "^(\\x{FEFF})?((?<id>[^\",]*)|\"(?<id>[^\"]*)\"),((?<date_opened>[^\",]*)|\"(?<date_opened>[^\"]*)\"),((?<owner_id>[^\",]*)|\"(?<owner_id>[^\"]*)\"),((?<billing_id>[^\",]*)|\"(?<billing_id>[^\"]*)\"),((?<notes>[^\",]*)|\"(?<notes>([^\"]|\"\")*)\"),((?<date>[^\",]*)|\"(?<date>[^\"]*)\"),((?<desc>[^\",]*)|\"(?<desc>([^\"]|\"\")*)\"),((?<action>[^\",]*)|\"(?<action>[^\"]*)\"),((?<account>[^\",]*)|\"(?<account>[^\"]*)\"),((?<quantity>[^\",]*)|\"(?<quantity>[^\"]*)\"),((?<price>[^\",]*)|\"(?<price>[^\"]*)\"),((?<disc_type>[^\",]*)|\"(?<disc_type>[^\"]*)\"),((?<disc_how>[^\",]*)|\"(?<disc_how>[^\"]*)\"),((?<discount>[^\",]*)|\"(?<discount>[^\"]*)\"),((?<taxable>[^\",]*)|\"(?<taxable>[^\"]*)\"),((?<taxincluded>[^\",]*)|\"(?<taxincluded>[^\"]*)\"),((?<tax_table>[^\",]*)|\"(?<tax_table>[^\"]*)\"),((?<date_posted>[^\",]*)|\"(?<date_posted>[^\"]*)\"),((?<due_date>[^\",]*)|\"(?<due_date>[^\"]*)\"),((?<account_posted>[^\",]*)|\"(?<account_posted>[^\"]*)\"),((?<memo_posted>[^\",]*)|\"(?<memo_posted>[^\"]*)\"),((?<accu_splits>[^\",]*)|\"(?<accu_splits>[^\"]*)\")$");
    gnc_bi_import_gui_filenameChanged_cb (gui->entryFilename, gui);
}

// DIY regex.
typedef struct
{
    BillImportGui *gui;
    GtkWidget *window;
} BiImportRegexpRequest;

static void
bi_import_regexp_received (GtkWindow *parent, gchar *input, gpointer user_data)
{
    BiImportRegexpRequest *request = user_data;
    if (request->window && input)
    {
        g_string_assign (request->gui->regexp, input);
        gnc_bi_import_gui_filenameChanged_cb (request->gui->entryFilename,
                                               request->gui);
    }
    if (request->window)
        g_object_remove_weak_pointer (G_OBJECT (request->window),
                                      (gpointer *)&request->window);
    g_free (input);
    g_free (request);
}

void gnc_bi_import_gui_option5_cb (GtkWidget *widget, gpointer data)
{
    BillImportGui *gui = data;
    if (!gtk_toggle_button_get_active( GTK_TOGGLE_BUTTON(widget) ))
        return;
    BiImportRegexpRequest *request = g_new0 (BiImportRegexpRequest, 1);
    request->gui = gui;
    request->window = GTK_WIDGET (gui->dialog);
    g_object_add_weak_pointer (G_OBJECT (request->window),
                               (gpointer *)&request->window);
    gnc_input_dialog_async (request->window,
                            _("Adjust regular expression used for import"),
                            _("This regular expression is used to parse the import file. Modify according to your needs.\n"),
                            gui->regexp->str, bi_import_regexp_received,
                            request);
}

void gnc_bi_import_gui_open_mode_cb (GtkWidget *widget, gpointer data)
{
    BillImportGui *gui = data;
    const gchar *name = NULL;
    name = gtk_buildable_get_name(GTK_BUILDABLE(widget));
    if (!gtk_toggle_button_get_active( GTK_TOGGLE_BUTTON(widget) ))
        return;
    if  (g_ascii_strcasecmp(name, "radiobuttonOpenAll") == 0)gui->open_mode = "ALL";
    else if (g_ascii_strcasecmp(name, "radiobuttonOpenNotPosted") == 0)gui->open_mode = "NOT_POSTED";
    else if (g_ascii_strcasecmp(name, "radiobuttonOpenNone") == 0)gui->open_mode = "NONE";
}


/*****************************************************************
 * Set whether we are importing a bill, invoice, Customer or Vendor
 * ****************************************************************/
void gnc_import_gui_type_cb (GtkWidget *widget, gpointer data)
{
    BillImportGui *gui = data;
    const gchar *name = NULL;
    name = gtk_buildable_get_name(GTK_BUILDABLE(widget));
    if (!gtk_toggle_button_get_active( GTK_TOGGLE_BUTTON(widget) ))
        return;
    if  (g_ascii_strcasecmp(name, "radiobuttonInvoice") == 0)gui->type = "INVOICE";
    else if (g_ascii_strcasecmp(name, "radiobuttonBill") == 0)gui->type = "BILL";
    //printf ("TYPE set to, %s\n",gui->type);

}
