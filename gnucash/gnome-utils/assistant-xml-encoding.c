/**********************************************************************
 * assistant-xml-encoding.c -- Conversion of old XML file
 * Copyright (C) 2006 Andreas Koehler <andi5.py@gmx.net>
 * Copyright (C) 2011 Robert Fewell
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
 *
 **********************************************************************/

#include <config.h>

#include <gio/gio.h>
#include <glib/gi18n.h>
#include <gmodule.h>

#include "TransLog.h"
#include "assistant-xml-encoding.h"
#include "dialog-utils.h"
#include "gnc-backend-xml.h"
#include "gnc-component-manager.h"
#include "gnc-gtk-utils.h"
#include "gnc-uri-utils.h"
#include "gnc-ui.h"

/* The following are copied from src/backend/xml/io-gncxml2-v2.h as a temporary
 * measure to enable this to compile in the face of making changing struct
 * FileBackend into C++ class XmlBackend, which can't be exposed to this C
 * file. A future commit will separate the session code from the UI code in this
 * file.
 */
typedef struct
{
    GQuark encoding;
    gchar* utf8_string;
} conv_type;

extern gint gnc_xml2_find_ambiguous (const gchar* filename,
                                     GList* encodings,
                                     GHashTable** unique,
                                     GHashTable** ambiguous,
                                     GList** impossible);

extern gboolean gnc_xml2_parse_with_subst (QofBackend* xml_be, QofBook* book,
                                           GHashTable* subst);
/* NOTE: This file uses the term "encoding" even in places where it is not
 * accurate. Please ignore that. Encodings occur in different forms:
 * - as descriptive string, as in the list of system encodings
 * - as string used for g_iconv_open
 * - as GQuark, representing above string
 * - as pointer, containing above gquark, used in lists
 */

typedef struct
{
    GtkWindow *window;
    GtkStack *stack;
    GtkLabel *message_label;
    GtkLabel *start_label;
    GtkBox *default_encoding_box;
    GtkWidget *default_encoding_dropdown;
    GtkLabel *summary_label;
    GtkBox *string_box;
    GtkBox *string_box_container;
    GtkLabel *end_label;
    GtkButton *back_button;
    GtkButton *next_button;
    GtkButton *cancel_button;

    GtkWindow *encodings_window;
    GtkLabel *encodings_message_label;
    GtkEntry *custom_enc_entry;
    GtkSingleSelection *available_selection;
    GtkSingleSelection *selected_selection;
    GListStore *selected_encodings;
    GtkButton *add_encoding_button;
    GtkButton *remove_encoding_button;
    GList *encodings_backup;

    GTask *task;
    gulong cancellable_handler;
    gboolean completed;
    gboolean parsing_complete;
    guint page;

    GList *encodings;                   /* list of GQuarks for encodings */
    GQuark default_encoding;            /* default GQuark, may be zero */

    /* hash table that maps byte sequences to conversions, i.e. in the current
       encodings setting, there is only one possible conversion */
    GHashTable *unique;

    /* hash table that maps byte sequences to a list of conversions, i.e. in the
       current encodings setting, there exactly these conversions are possible */
    GHashTable *ambiguous_ht;

    /* sorted list of ambiguous words, used for the construction of the combos */
    GList *ambiguous_list;

    /* hash table that maps byte sequences to conversions. these reflect the
       choices the user made, accumulated and updated in the whole conversion.
       Note: this may contain conversions that are not available in the current
       encodings setting, just imagine, user accidentally removed an important
       encoding from the list */
    GHashTable *choices;

    /* number of byte sequences that have multiple possible conversions, but not in
       the default encoding. and the user has not decided yet, of course. */
    gint n_unassigned;

    /* number of byte sequences without any reasonable interpretation */
    gint n_impossible;

    /* hash table that maps byte sequences to other byte sequences to be replaced
       by them. */
    GHashTable *subst;

    gchar *filename;
    gchar *error_message;
    QofSession *session;
} GncXmlImportData;

/* used for the string combos, see ambiguous_free */
typedef struct
{
    gchar *byte_sequence;
    GList *conv_list;
} ambiguous_type;

typedef struct
{
    ambiguous_type *ambiguous;
    GArray *encodings;
} GncXmlEncodingChoices;

enum
{
    GNC_XML_PAGE_START,
    GNC_XML_PAGE_CONVERSION,
    GNC_XML_PAGE_FINISH
};

static void gxi_data_destroy (GncXmlImportData *data);
static void gxi_ambiguous_info_destroy (GncXmlImportData *data);
static gboolean gxi_session_destroy (GncXmlImportData *data);
static gboolean gxi_check_file (GncXmlImportData *data);
static void gxi_sort_ambiguous_list (GncXmlImportData *data);
static gboolean gxi_parse_file (GncXmlImportData *data);
static gboolean gxi_save_file (GncXmlImportData *data);
static void gxi_update_progress_bar (const gchar *message, double percentage);
static void gxi_update_default_enc_combo (GncXmlImportData *data);
static void gxi_update_summary_label (GncXmlImportData *data);
static void gxi_update_string_box (GncXmlImportData *data);
static void gxi_update_conversion_forward (GncXmlImportData *data);
static void gxi_default_enc_dropdown_changed_cb (GtkDropDown *dropdown,
                                                  GParamSpec *param_spec,
                                                  GncXmlImportData *data);
static void gxi_string_dropdown_changed_cb (GtkDropDown *dropdown,
                                            GParamSpec *param_spec,
                                            GncXmlImportData *data);
/* Translators: Run the assistant in your language to see GTK's translation of the button labels. */
static const gchar *encodings_doc_string = N_(
            "\nThe file you are trying to load is from an older version of "
            "GnuCash. The file format in the older versions was missing the "
            "detailed specification of the character encoding being used. This "
            "means the text in your data file could be read in multiple ambiguous "
            "ways. This ambiguity cannot be resolved automatically, but the new "
            "GnuCash 2.0.0 file format will include all necessary specifications so "
            "that you do not have to go through this step again."
            "\n\n"
            "GnuCash will try to guess the correct character encoding for your data "
            "file. On the next page GnuCash will show the resulting texts when "
            "using this guess. You have to check whether the words look as "
            "expected. Either everything looks fine and you can simply press "
            "\"Next\". Or the words contain unexpected characters, in which "
            "case you should select different character encodings to see "
            "different results. You may have to edit the list of character "
            "encodings by clicking on the respective button."
            "\n\n"
            "Press \"Next\" now to select the correct character encoding for "
            "your data file.\n");

static const gchar *encodings_doc_page_title = N_("Ambiguous character encoding");

static const gchar *finish_convert_string = N_(
            "The file has been loaded successfully. If you click \"Apply\" it will be saved "
            "and reloaded into the main application. That way you will have a working "
            "file as backup in the same directory.\n\n"
            "You can also go back and verify your selections by clicking on \"Back\".");

/* The debugging module that this .o belongs to. */
static QofLogModule log_module = GNC_MOD_ASSISTANT;

/* window containing a progress bar */
static GtkWidget *progress_window = NULL;
static GtkProgressBar *progress_bar = NULL;

/* this is used for a static tree of system encodings. encoding may be NULL.
   parent declares how often to go up in the path of the previous element and use
   that as parent, e.g. 0 -> child of previous, 1 -> same level as previous */
typedef struct
{
    gchar *text;
    gchar *encoding;
    gint parent;
} system_encoding_type;
static system_encoding_type system_encodings [] =
{
    { N_("Unicode"),                                NULL,          2 },
    {    "UTF-8",                                   "UTF-8",       0 },
    { N_("European"),                               NULL,          2 },
    { N_("ISO-8859-1 (West European)"),             "ISO-8859-1",  0 },
    { N_("ISO-8859-2 (East European)"),             "ISO-8859-2",  1 },
    { N_("ISO-8859-3 (South European)"),            "ISO-8859-3",  1 },
    { N_("ISO-8859-4 (North European)"),            "ISO-8859-4",  1 },
    { N_("ISO-8859-5 (Cyrillic)"),                  "ISO-8859-5",  1 },
    { N_("ISO-8859-6 (Arabic)"),                    "ISO-8859-6",  1 },
    { N_("ISO-8859-7 (Greek)"),                     "ISO-8859-7",  1 },
    { N_("ISO-8859-8 (Hebrew)"),                    "ISO-8859-8",  1 },
    { N_("ISO-8859-9 (Turkish)"),                   "ISO-8859-9",  1 },
    { N_("ISO-8859-10 (Nordic)"),                   "ISO-8859-10", 1 },
    { N_("ISO-8859-11 (Thai)"),                     "ISO-8859-11", 1 },
    { N_("ISO-8859-13 (Baltic)"),                   "ISO-8859-13", 1 },
    { N_("ISO-8859-14 (Celtic)"),                   "ISO-8859-14", 1 },
    { N_("ISO-8859-15 (West European, Euro sign)"), "ISO-8859-15", 1 },
    { N_("ISO-8859-16 (South-East European)"),      "ISO-8859-16", 1 },
    { N_("Cyrillic"),                               NULL,          2 },
    { N_("KOI8-R (Russian)"),                       "KOI8-R",      0 },
    { N_("KOI8-U (Ukrainian)"),                     "KOI8-U",      1 },
};
static guint n_system_encodings = G_N_ELEMENTS (system_encodings);


static void gxi_close_encoding_editor (GncXmlImportData *data, gboolean apply);
static void gxi_edit_encodings_clicked_cb (GtkButton *button,
                                            GncXmlImportData *data);

static void
gxi_set_error (GncXmlImportData *data, const gchar *message)
{
    g_free (data->error_message);
    data->error_message = g_strdup (message);

    if (data->message_label)
    {
        gtk_label_set_text (data->message_label, message);
        gtk_widget_set_visible (GTK_WIDGET (data->message_label), TRUE);
    }
}

static void
gxi_clear_error (GncXmlImportData *data)
{
    g_clear_pointer (&data->error_message, g_free);

    if (data->message_label)
        gtk_widget_set_visible (GTK_WIDGET (data->message_label), FALSE);
}

static void
gxi_complete (GncXmlImportData *data, gboolean success, gboolean cancelled)
{
    GCancellable *cancellable;
    gchar *message;

    if (!data || data->completed)
        return;

    data->completed = TRUE;
    cancellable = g_task_get_cancellable (data->task);
    if (cancellable && data->cancellable_handler)
    {
        g_cancellable_disconnect (cancellable, data->cancellable_handler);
        data->cancellable_handler = 0;
    }

    gxi_close_encoding_editor (data, FALSE);
    if (data->window)
    {
        GtkWindow *window = data->window;

        data->window = NULL;
        gtk_window_destroy (window);
        g_object_unref (window);
    }

    if (success)
        g_task_return_boolean (data->task, TRUE);
    else if (cancelled)
        g_task_return_new_error (data->task, G_IO_ERROR, G_IO_ERROR_CANCELLED,
                                 "%s", _("Character encoding conversion was cancelled."));
    else
    {
        message = g_steal_pointer (&data->error_message);
        g_task_return_new_error (data->task, G_IO_ERROR, G_IO_ERROR_FAILED,
                                 "%s", message ? message :
                                 _("Character encoding conversion failed."));
        g_free (message);
    }

    gxi_data_destroy (data);
    g_free (data);
}

static gboolean
gxi_window_close_request_cb (GtkWindow *window, GncXmlImportData *data)
{
    (void)window;
    gxi_complete (data, FALSE, TRUE);
    return TRUE;
}

static void
gxi_window_destroy_cb (GtkWidget *widget, GncXmlImportData *data)
{
    (void)widget;
    gxi_complete (data, FALSE, TRUE);
}

static void
gxi_cancel_clicked_cb (GtkButton *button, GncXmlImportData *data)
{
    (void)button;
    gxi_complete (data, FALSE, TRUE);
}

static void
gxi_show_page (GncXmlImportData *data, guint page)
{
    const gchar *name;

    data->page = page;
    switch (page)
    {
    case GNC_XML_PAGE_START:
        name = "start";
        gtk_widget_set_visible (GTK_WIDGET (data->back_button), FALSE);
        gtk_button_set_label (data->next_button, _("_Next"));
        gtk_widget_set_sensitive (GTK_WIDGET (data->next_button), TRUE);
        break;
    case GNC_XML_PAGE_CONVERSION:
        name = "conversion";
        gtk_widget_set_visible (GTK_WIDGET (data->back_button), TRUE);
        gtk_button_set_label (data->next_button, _("_Next"));
        gxi_update_string_box (data);
        gxi_update_conversion_forward (data);
        break;
    default:
        name = "finish";
        gtk_widget_set_visible (GTK_WIDGET (data->back_button), TRUE);
        gtk_button_set_label (data->next_button, _("_Apply"));
        gtk_widget_set_sensitive (GTK_WIDGET (data->next_button), TRUE);
        break;
    }
    gtk_stack_set_visible_child_name (data->stack, name);
}

static void
gxi_back_clicked_cb (GtkButton *button, GncXmlImportData *data)
{
    (void)button;
    if (data->page == GNC_XML_PAGE_FINISH)
    {
        data->parsing_complete = FALSE;
        gxi_session_destroy (data);
        gxi_show_page (data, GNC_XML_PAGE_CONVERSION);
    }
    else if (data->page == GNC_XML_PAGE_CONVERSION)
        gxi_show_page (data, GNC_XML_PAGE_START);
}

static void
gxi_next_clicked_cb (GtkButton *button, GncXmlImportData *data)
{
    gboolean success;

    (void)button;
    if (data->page == GNC_XML_PAGE_START)
    {
        gxi_show_page (data, GNC_XML_PAGE_CONVERSION);
        return;
    }

    if (data->page == GNC_XML_PAGE_CONVERSION)
    {
        gxi_clear_error (data);
        success = gxi_parse_file (data);
        if (success)
        {
            data->parsing_complete = TRUE;
            gxi_show_page (data, GNC_XML_PAGE_FINISH);
        }
        return;
    }

    if (!data->parsing_complete)
        return;

    gxi_clear_error (data);
    success = gxi_save_file (data);
    if (success)
        gxi_complete (data, TRUE, FALSE);
}

static gboolean
gxi_create_window (GncXmlImportData *data, GtkWindow *parent)
{
    GtkBuilder *builder;
    GtkWindow *window;
    GtkButton *edit_button;

    builder = gtk_builder_new ();
    gnc_builder_add_from_file (builder, "assistant-xml-encoding.glade",
                               "xml_encoding_window");
    window = GTK_WINDOW (gtk_builder_get_object (builder,
                                                  "xml_encoding_window"));
    if (!window)
    {
        g_object_unref (builder);
        gxi_set_error (data, _("The character encoding assistant could not be created."));
        return FALSE;
    }

    gnc_window_bind_to_application (window);
    data->window = g_object_ref (window);
    data->stack = GTK_STACK (gtk_builder_get_object (builder,
                                                      "xml_encoding_stack"));
    data->message_label = GTK_LABEL (gtk_builder_get_object (builder,
                                                               "message_label"));
    data->start_label = GTK_LABEL (gtk_builder_get_object (builder,
                                                            "start_page_label"));
    data->default_encoding_box = GTK_BOX (gtk_builder_get_object (builder,
                                                                   "default_enc_box"));
    data->summary_label = GTK_LABEL (gtk_builder_get_object (builder,
                                                              "impossible_label"));
    data->string_box_container = GTK_BOX (gtk_builder_get_object (builder,
                                                                    "string_box_container"));
    data->end_label = GTK_LABEL (gtk_builder_get_object (builder,
                                                          "end_page_label"));
    data->back_button = GTK_BUTTON (gtk_builder_get_object (builder,
                                                             "assistant_back"));
    data->next_button = GTK_BUTTON (gtk_builder_get_object (builder,
                                                             "assistant_next"));
    data->cancel_button = GTK_BUTTON (gtk_builder_get_object (builder,
                                                               "assistant_cancel"));
    edit_button = GTK_BUTTON (gtk_builder_get_object (builder, "edit_encs_button"));
    g_object_unref (builder);

    if (!data->stack || !data->message_label || !data->start_label ||
        !data->default_encoding_box || !data->summary_label ||
        !data->string_box_container || !data->end_label ||
        !data->back_button || !data->next_button || !data->cancel_button ||
        !edit_button)
    {
        gxi_set_error (data, _("The character encoding assistant is incomplete."));
        return FALSE;
    }

    if (parent)
        gtk_window_set_transient_for (data->window, parent);
    gtk_window_set_modal (data->window, TRUE);
    gtk_widget_set_name (GTK_WIDGET (data->window),
                         "gnc-id-assistant-xml-encoding");
    gtk_window_set_title (data->window, gettext (encodings_doc_page_title));
    gtk_label_set_text (data->start_label, gettext (encodings_doc_string));
    gtk_label_set_text (data->end_label, gettext (finish_convert_string));
    gtk_widget_set_visible (GTK_WIDGET (data->message_label), FALSE);
    gtk_widget_set_visible (GTK_WIDGET (data->summary_label), FALSE);

    g_signal_connect (data->window, "close-request",
                      G_CALLBACK (gxi_window_close_request_cb), data);
    g_signal_connect (data->window, "destroy",
                      G_CALLBACK (gxi_window_destroy_cb), data);
    g_signal_connect (data->back_button, "clicked",
                      G_CALLBACK (gxi_back_clicked_cb), data);
    g_signal_connect (data->next_button, "clicked",
                      G_CALLBACK (gxi_next_clicked_cb), data);
    g_signal_connect (data->cancel_button, "clicked",
                      G_CALLBACK (gxi_cancel_clicked_cb), data);
    g_signal_connect (edit_button, "clicked",
                      G_CALLBACK (gxi_edit_encodings_clicked_cb), data);

    gxi_update_default_enc_combo (data);
    gxi_show_page (data, GNC_XML_PAGE_START);
    gtk_window_present (data->window);
    return TRUE;
}

static void
gxi_cancellable_cancelled_cb (GCancellable *cancellable,
                               GncXmlImportData *data)
{
    (void)cancellable;
    data->cancellable_handler = 0;
    gxi_complete (data, FALSE, TRUE);
}

void
gnc_xml_convert_single_file_async (const gchar *filename,
                                   GtkWindow *parent,
                                   GCancellable *cancellable,
                                   GAsyncReadyCallback callback,
                                   gpointer user_data)
{
    GncXmlImportData *data;
    gulong handler;
    gboolean success;

    g_return_if_fail (filename);

    data = g_new0 (GncXmlImportData, 1);
    data->task = g_task_new (NULL, cancellable, callback, user_data);
    data->filename = gnc_uri_get_path (filename);
    if (!data->filename)
    {
        gxi_set_error (data, _("The selected file has no local path."));
        gxi_complete (data, FALSE, FALSE);
        return;
    }

    if (cancellable)
    {
        handler = g_cancellable_connect (cancellable,
                                         G_CALLBACK (gxi_cancellable_cancelled_cb),
                                         data, NULL);
        if (handler == 0)
            return;
        data->cancellable_handler = handler;
    }

    if (!gxi_check_file (data))
    {
        gxi_complete (data, FALSE, FALSE);
        return;
    }

    if (!g_hash_table_size (data->ambiguous_ht))
    {
        success = gxi_parse_file (data) && gxi_save_file (data);
        gxi_complete (data, success, FALSE);
        return;
    }

    if (!gxi_create_window (data, parent))
        gxi_complete (data, FALSE, FALSE);
}

gboolean
gnc_xml_convert_single_file_finish (GAsyncResult *result, GError **error)
{
    g_return_val_if_fail (g_task_is_valid (result, NULL), FALSE);
    return g_task_propagate_boolean (G_TASK (result), error);
}

/* This compatibility entry point can complete files that need no interaction.
 * Callers that may show the encoding chooser must use the asynchronous API. */
gboolean
gnc_xml_convert_single_file (const gchar *filename)
{
    GncXmlImportData *data;
    gboolean success;

    g_return_val_if_fail (filename, FALSE);
    data = g_new0 (GncXmlImportData, 1);
    data->filename = gnc_uri_get_path (filename);
    if (!data->filename || !gxi_check_file (data) ||
        g_hash_table_size (data->ambiguous_ht))
    {
        gxi_data_destroy (data);
        g_free (data);
        return FALSE;
    }

    success = gxi_parse_file (data) && gxi_save_file (data);
    gxi_data_destroy (data);
    g_free (data);
    return success;
}

static void
gxi_data_destroy (GncXmlImportData *data)
{
    if (!data)
        return;

    gxi_close_encoding_editor (data, FALSE);
    if (data->window)
    {
        gtk_window_destroy (data->window);
        g_clear_object (&data->window);
    }
    if (data->string_box)
    {
        gtk_widget_unparent (GTK_WIDGET (data->string_box));
        data->string_box = NULL;
    }

    g_clear_pointer (&data->filename, g_free);
    g_clear_pointer (&data->error_message, g_free);
    gxi_session_destroy (data);
    gxi_ambiguous_info_destroy (data);
    g_clear_pointer (&data->choices, g_hash_table_destroy);
    g_clear_pointer (&data->encodings, g_list_free);
    g_clear_object (&data->task);
}
static void
conv_free (conv_type *conv)
{
    if (conv)
    {
        g_free(conv->utf8_string);
        g_free(conv);
    }
}

static conv_type *
conv_copy (const conv_type *conv)
{
    conv_type *new_type = NULL;
    if (conv)
    {
        new_type = g_new(conv_type, 1);
        new_type->encoding = conv->encoding;
        new_type->utf8_string = g_strdup (conv->utf8_string);
    }
    return new_type;
}

static gint
conv_enc_cmp (const conv_type *conv, const GQuark *enc)
{
    return conv->encoding - *enc;
}

static const gchar *
get_decoded_string (const ambiguous_type *amb, const GQuark enc)
{
    GList *found = g_list_find_custom (amb->conv_list, &enc,
                                       (GCompareFunc) conv_enc_cmp);

    if (found)
    {
        return ((conv_type*) found->data)->utf8_string;
    }
    else
    {
        return NULL;
    }
}

static gint
ambiguous_cmp (const ambiguous_type *a, const ambiguous_type *b,
               GncXmlImportData *data)
{
    const gchar *string_a = get_decoded_string (a, data->default_encoding);
    const gchar *string_b = get_decoded_string (b, data->default_encoding);

    if (string_a)
    {
        if (string_b)
        {
            /* both look good, usual compare */
            return strcmp (string_a, string_b);
        }
        else
        {
            /* a look good, b not. put b to the top */
            return 1;
        }
    }
    else
    {
        if (string_b)
        {
            /* b looks good, a not. put a to the top */
            return -1;
        }
        else
        {
            /* both look suboptimal, see whether one has a decision attached to it */
            conv_type *conv_a = g_hash_table_lookup (data->choices, a->byte_sequence);
            conv_type *conv_b = g_hash_table_lookup (data->choices, b->byte_sequence);
            if (conv_a && !conv_b) return 1;
            if (conv_b && !conv_a) return -1;
            return strcmp (a->byte_sequence, b->byte_sequence);
        }
    }
}

static void
ambiguous_list_insert (gchar *byte_sequence, GList *conv_list,
                       GncXmlImportData *data)
{
    GList *iter;

    ambiguous_type *amb = g_new (ambiguous_type, 1);
    amb->byte_sequence = g_strdup (byte_sequence);
    amb->conv_list = NULL;
    for (iter = g_list_last (conv_list); iter; iter = iter->prev)
        amb->conv_list = g_list_prepend (amb->conv_list, conv_copy (iter->data));

    data->ambiguous_list = g_list_prepend (data->ambiguous_list, amb);
}

static void
ambiguous_free (ambiguous_type *amb)
{
    if (amb)
    {
        g_free (amb->byte_sequence);
        g_list_foreach (amb->conv_list, (GFunc) conv_free, NULL);
        g_list_free (amb->conv_list);
        g_free (amb);
    }
}

static void
gxi_ambiguous_info_destroy (GncXmlImportData *data)
{
    if (data->unique)
    {
        g_hash_table_destroy (data->unique);
        data->unique = NULL;
    }
    if (data->ambiguous_ht)
    {
        g_hash_table_destroy (data->ambiguous_ht);
        data->ambiguous_ht = NULL;
    }
    if (data->ambiguous_list)
    {
        g_list_foreach (data->ambiguous_list, (GFunc) ambiguous_free, NULL);
        g_list_free (data->ambiguous_list);
        data->ambiguous_list = NULL;
    }
}

static gboolean
gxi_session_destroy (GncXmlImportData *data)
{
    QofSessionOperationLease *lease;
    gboolean destroyed;

    if (!data->session)
        return TRUE;

    lease = qof_session_operation_lease_acquire_for (
        data->session, QOF_SESSION_OPERATION_CLOSE);
    xaccLogDisable ();
    destroyed = lease && qof_session_destroy_with_lease (data->session, lease);
    xaccLogEnable ();
    qof_session_operation_lease_release (lease);
    if (!destroyed)
        return FALSE;

    data->session = NULL;
    return TRUE;
}

static gboolean
gxi_session_begin (QofSession *session, const gchar *filename)
{
    QofSessionOperationLease *lease;
    gboolean begun;

    lease = qof_session_operation_lease_acquire_for (
        session, QOF_SESSION_OPERATION_OPEN);
    begun = lease && qof_session_begin_with_lease (
        session, lease, filename, SESSION_READ_ONLY);
    qof_session_operation_lease_release (lease);
    return begun;
}

static gboolean
gxi_session_load (QofSession *session)
{
    QofSessionOperationLease *lease;
    gboolean loaded;

    lease = qof_session_operation_lease_acquire_for (
        session, QOF_SESSION_OPERATION_OPEN);
    loaded = lease && qof_session_load_with_lease (
        session, lease, gxi_update_progress_bar);
    qof_session_operation_lease_release (lease);
    return loaded;
}

static gboolean
gxi_session_parse_with_subst (QofSession *session, QofBackend *backend,
                              QofBook *book, GHashTable *subst)
{
    QofSessionOperationLease *lease;
    gboolean parsed;

    lease = qof_session_operation_lease_acquire_for (
        session, QOF_SESSION_OPERATION_OPEN);
    parsed = lease && gnc_xml2_parse_with_subst (backend, book, subst);
    qof_session_operation_lease_release (lease);
    return parsed;
}

static gboolean
gxi_session_save (QofSession *session)
{
    QofSessionOperationLease *lease;
    gboolean saved;

    lease = qof_session_operation_lease_acquire_for (
        session, QOF_SESSION_OPERATION_SAVE);
    saved = lease && qof_session_save_with_lease (
        session, lease, gxi_update_progress_bar);
    qof_session_operation_lease_release (lease);
    return saved;
}

static void
gxi_sort_ambiguous_list (GncXmlImportData *data)
{
    data->ambiguous_list = g_list_sort_with_data (
                               data->ambiguous_list, (GCompareDataFunc) ambiguous_cmp, data);

}

static void
subst_insert_amb (gchar *byte_sequence, GList *conv_list, GncXmlImportData *data)
{
    conv_type *choice;
    GList *default_conv;
    gchar *default_utf8;

    if (!data->subst)
        return;
    choice = g_hash_table_lookup (data->choices, byte_sequence);
    if (choice)
    {
        /* user choice */
        g_hash_table_insert (data->subst, g_strdup (byte_sequence),
                             g_strdup (choice->utf8_string));
    }
    else
    {
        default_conv = g_list_find_custom (conv_list, &data->default_encoding,
                                           (GCompareFunc) conv_enc_cmp);
        if (default_conv)
        {
            /* default conversion */
            default_utf8 = ((conv_type*) default_conv->data)->utf8_string;
            g_hash_table_insert (data->subst, g_strdup (byte_sequence),
                                 g_strdup (default_utf8));
        }
        else
        {
            /* no conversion available, stop filling of subst */
            g_hash_table_destroy (data->subst);
            data->subst = NULL;
        }
    }
}

static void
subst_insert_unique (gchar *byte_sequence, conv_type *conv,
                     GncXmlImportData *data)
{
    if (!data->subst)
        return;
    g_hash_table_insert (data->subst, g_strdup (byte_sequence),
                         g_strdup (conv->utf8_string));
}

static void
gxi_update_progress_bar (const gchar *message, double percentage)
{
    if (!progress_window)
    {
        progress_window = gtk_window_new ();
        gnc_window_bind_to_application (GTK_WINDOW (progress_window));
        progress_bar = GTK_PROGRESS_BAR (gtk_progress_bar_new ());
        gtk_window_set_title (GTK_WINDOW (progress_window), _("Converting file"));
        gtk_progress_bar_set_show_text (progress_bar, TRUE);
        gtk_widget_set_margin_start (GTK_WIDGET (progress_bar), 12);
        gtk_widget_set_margin_end (GTK_WIDGET (progress_bar), 12);
        gtk_widget_set_margin_top (GTK_WIDGET (progress_bar), 12);
        gtk_widget_set_margin_bottom (GTK_WIDGET (progress_bar), 12);
        gtk_window_set_child (GTK_WINDOW (progress_window), GTK_WIDGET (progress_bar));
    }

    if (percentage < 0)
    {
        gtk_progress_bar_set_text (progress_bar, NULL);
        gtk_progress_bar_set_fraction (progress_bar, 0.0);
        gtk_widget_set_visible (progress_window, FALSE);
    }
    else
    {
        gtk_progress_bar_set_text (progress_bar, message);
        if (percentage <= 100)
            gtk_progress_bar_set_fraction (progress_bar, percentage / 100);
        else
            gtk_progress_bar_pulse (progress_bar);
        gtk_widget_set_visible (progress_window, TRUE);
    }
}

static void
gxi_encoding_choices_free (GncXmlEncodingChoices *choices)
{
    if (!choices)
        return;
    g_clear_pointer (&choices->encodings, g_array_unref);
    g_free (choices);
}

static void
gxi_update_conversion_forward (GncXmlImportData *data)
{
    if (data->page == GNC_XML_PAGE_CONVERSION)
        gtk_widget_set_sensitive (GTK_WIDGET (data->next_button),
                                  data->n_unassigned == 0 &&
                                  data->n_impossible == 0);
}

static void
gxi_update_default_enc_combo (GncXmlImportData *data)
{
    GtkStringList *model;
    GtkDropDown *dropdown;
    GList *iter;
    guint selected = GTK_INVALID_LIST_POSITION;
    guint position = 0;

    if (data->default_encoding_dropdown)
    {
        gtk_widget_unparent (data->default_encoding_dropdown);
        data->default_encoding_dropdown = NULL;
    }

    model = gtk_string_list_new (NULL);
    for (iter = data->encodings; iter; iter = iter->next, position++)
    {
        GQuark encoding = GPOINTER_TO_UINT (iter->data);

        gtk_string_list_append (model, g_quark_to_string (encoding));
        if (encoding == data->default_encoding)
            selected = position;
    }

    dropdown = gnc_gtk_drop_down_new (G_LIST_MODEL (model), NULL);
    if (selected != GTK_INVALID_LIST_POSITION)
        gtk_drop_down_set_selected (dropdown, selected);
    g_signal_connect (dropdown, "notify::selected",
                      G_CALLBACK (gxi_default_enc_dropdown_changed_cb), data);
    gtk_box_append (data->default_encoding_box, GTK_WIDGET (dropdown));
    data->default_encoding_dropdown = GTK_WIDGET (dropdown);
    g_object_unref (model);
}

static void
gxi_update_summary_label (GncXmlImportData *data)
{
    gchar *string = NULL;

    if (data->n_unassigned && data->n_impossible)
        string = g_strdup_printf (_("There are %d unassigned and %d undecodable "
                                   "words. Please add encodings."),
                                  data->n_unassigned, data->n_impossible);
    else if (data->n_unassigned)
        string = g_strdup_printf (_("There are %d unassigned words. Please decide "
                                   "on them or add encodings."), data->n_unassigned);
    else if (data->n_impossible)
        string = g_strdup_printf (_("There are %d undecodable words. Please add "
                                   "encodings."), data->n_impossible);

    if (string)
    {
        gtk_label_set_text (data->summary_label, string);
        gtk_widget_set_visible (GTK_WIDGET (data->summary_label), TRUE);
        g_free (string);
    }
    else
        gtk_widget_set_visible (GTK_WIDGET (data->summary_label), FALSE);
}

static void
gxi_update_string_box (GncXmlImportData *data)
{
    GList *word_iter;

    if (data->string_box)
    {
        gtk_widget_unparent (GTK_WIDGET (data->string_box));
        data->string_box = NULL;
    }

    data->string_box = GTK_BOX (gtk_box_new (GTK_ORIENTATION_VERTICAL, 6));
    data->n_unassigned = 0;
    for (word_iter = data->ambiguous_list; word_iter; word_iter = word_iter->next)
    {
        ambiguous_type *ambiguous = word_iter->data;
        GtkStringList *model = gtk_string_list_new (NULL);
        GtkDropDown *dropdown;
        GncXmlEncodingChoices *choices = g_new0 (GncXmlEncodingChoices, 1);
        conv_type *chosen;
        const gchar *utf8;
        GList *conversion;
        guint selected = GTK_INVALID_LIST_POSITION;
        guint position = 0;

        choices->ambiguous = ambiguous;
        choices->encodings = g_array_new (FALSE, FALSE, sizeof (GQuark));
        utf8 = get_decoded_string (ambiguous, data->default_encoding);
        if (utf8)
        {
            gchar *display = g_strdup_printf ("%s (default)", utf8);
            GQuark encoding = data->default_encoding;

            gtk_string_list_append (model, display);
            g_array_append_val (choices->encodings, encoding);
            g_free (display);
            selected = position++;
        }

        chosen = g_hash_table_lookup (data->choices, ambiguous->byte_sequence);
        for (conversion = ambiguous->conv_list; conversion;
             conversion = conversion->next, position++)
        {
            conv_type *conv = conversion->data;
            gchar *display = g_strdup_printf ("%s (%s)", conv->utf8_string,
                                              g_quark_to_string (conv->encoding));
            GQuark encoding = conv->encoding;

            gtk_string_list_append (model, display);
            g_array_append_val (choices->encodings, encoding);
            g_free (display);
            if (chosen && chosen->encoding == encoding)
                selected = position;
        }

        if (selected == GTK_INVALID_LIST_POSITION)
            data->n_unassigned++;

        dropdown = gnc_gtk_drop_down_new (G_LIST_MODEL (model), NULL);
        if (selected != GTK_INVALID_LIST_POSITION)
            gtk_drop_down_set_selected (dropdown, selected);
        g_object_set_data_full (G_OBJECT (dropdown), "gnc-xml-encoding-choices",
                                choices, (GDestroyNotify)gxi_encoding_choices_free);
        g_signal_connect (dropdown, "notify::selected",
                          G_CALLBACK (gxi_string_dropdown_changed_cb), data);
        gtk_box_append (data->string_box, GTK_WIDGET (dropdown));
        g_object_unref (model);
    }

    gtk_box_append (data->string_box_container, GTK_WIDGET (data->string_box));
    gxi_update_summary_label (data);
}

static void
gxi_default_enc_dropdown_changed_cb (GtkDropDown *dropdown,
                                     GParamSpec *param_spec,
                                     GncXmlImportData *data)
{
    GtkStringObject *item;
    const gchar *encoding;
    GQuark current;

    (void)param_spec;
    item = GTK_STRING_OBJECT (gtk_drop_down_get_selected_item (dropdown));
    if (!item)
        return;
    encoding = gtk_string_object_get_string (item);
    current = g_quark_from_string (encoding);
    if (data->default_encoding == current)
        return;
    if (!g_list_find (data->encodings, GUINT_TO_POINTER (current)))
    {
        PERR ("invalid encoding selection");
        return;
    }

    data->default_encoding = current;
    gxi_sort_ambiguous_list (data);
    gxi_update_string_box (data);
    gxi_update_conversion_forward (data);
}

static void
gxi_string_dropdown_changed_cb (GtkDropDown *dropdown,
                                GParamSpec *param_spec,
                                GncXmlImportData *data)
{
    GncXmlEncodingChoices *choices;
    ambiguous_type *ambiguous;
    GList *found;
    GList *default_conv;
    conv_type *previous;
    conv_type *current = NULL;
    GQuark previous_encoding = 0;
    GQuark current_encoding = 0;
    guint selected;

    (void)param_spec;
    choices = g_object_get_data (G_OBJECT (dropdown), "gnc-xml-encoding-choices");
    if (!choices)
        return;
    ambiguous = choices->ambiguous;
    previous = g_hash_table_lookup (data->choices, ambiguous->byte_sequence);
    if (previous)
        previous_encoding = previous->encoding;

    default_conv = g_list_find_custom (ambiguous->conv_list,
                                       &data->default_encoding,
                                       (GCompareFunc)conv_enc_cmp);
    selected = gtk_drop_down_get_selected (dropdown);
    if (selected != GTK_INVALID_LIST_POSITION && selected < choices->encodings->len)
    {
        current_encoding = g_array_index (choices->encodings, GQuark, selected);
        found = g_list_find_custom (ambiguous->conv_list, &current_encoding,
                                    (GCompareFunc)conv_enc_cmp);
        if (found)
            current = found->data;
        else
            PERR ("invalid string selection");
    }

    if (current)
    {
        if (previous)
        {
            if (current_encoding == previous_encoding)
                return;
            g_hash_table_replace (data->choices, g_strdup (ambiguous->byte_sequence),
                                  conv_copy (current));
            found = g_list_find_custom (ambiguous->conv_list, &previous_encoding,
                                        (GCompareFunc)conv_enc_cmp);
            if (!found && !default_conv)
                data->n_unassigned--;
        }
        else
        {
            g_hash_table_insert (data->choices, g_strdup (ambiguous->byte_sequence),
                                 conv_copy (current));
            if (!default_conv)
                data->n_unassigned--;
        }
    }
    else if (previous)
    {
        g_hash_table_remove (data->choices, ambiguous->byte_sequence);
        if (!default_conv)
            data->n_unassigned++;
    }

    gxi_update_summary_label (data);
    gxi_update_conversion_forward (data);
}
static gboolean
gxi_check_file (GncXmlImportData *data)
{
    if (!data->encodings)
    {
        gboolean is_utf8;
        const gchar *locale_enc;
        gchar *enc_string, **enc_array, **enc_cursor;
        gpointer enc_ptr;
        GIConv iconv;

        /* first locale encoding */
        is_utf8 = g_get_charset (&locale_enc);
        enc_string = g_ascii_strup (locale_enc, -1);
        enc_ptr = GUINT_TO_POINTER (g_quark_from_string (enc_string));
        g_free (enc_string);
        data->encodings = g_list_append (NULL, enc_ptr);

        /* add utf-8 */
        if (!is_utf8)
        {
            enc_ptr = GUINT_TO_POINTER (g_quark_from_string ("UTF-8"));
            data->encodings = g_list_append (data->encodings, enc_ptr);
        }

        /* Translators: Please insert encodings here that are typically used in your
           locale, separated by spaces. No need for ASCII or UTF-8, check 'locale -m'
           for assistance with spelling. */
        enc_array = g_strsplit (_("ISO-8859-1 KOI8-U"), " ", 0);

        /* loop through typical encodings */
        for (enc_cursor = enc_array; *enc_cursor; enc_cursor++)
        {
            if (!**enc_cursor) continue;
            enc_string = g_ascii_strup (*enc_cursor, -1);
            enc_ptr = GUINT_TO_POINTER (g_quark_from_string (enc_string));

            if (!g_list_find (data->encodings, enc_ptr))
            {
                /* test whether we like this encoding */
                iconv = g_iconv_open ("UTF-8", enc_string);
                if (iconv != (GIConv) - 1)
                {
                    /* we like it */
                    data->encodings = g_list_append (data->encodings, enc_ptr);
                    g_iconv_close (iconv);
                }
            }
            g_free (enc_string);
        }
        g_strfreev (enc_array);
    }

    if (!data->default_encoding)
    {
        /* choose top one */
        data->default_encoding = GPOINTER_TO_UINT (data->encodings->data);
    }

    if (!data->choices)
    {
        data->choices = g_hash_table_new_full (g_str_hash, g_str_equal,
                                               g_free, (GDestroyNotify) conv_free);
    }

    gxi_ambiguous_info_destroy (data);

    /* analyze file */
    data->n_impossible = gnc_xml2_find_ambiguous (
                             data->filename, data->encodings, &data->unique, &data->ambiguous_ht, NULL);

    if (data->n_impossible != -1)
    {
        /* sort ambiguous words */
        g_hash_table_foreach (data->ambiguous_ht, (GHFunc)ambiguous_list_insert,
                              data);
        gxi_sort_ambiguous_list (data);
    }
    else
        gxi_set_error (data, _("The file could not be analyzed for character encodings."));

    return data->n_impossible != -1;
}

static gboolean
gxi_parse_file (GncXmlImportData *data)
{
    QofSession *session = NULL;
    QofBook *book;
    QofBackend *backend;
    QofBackendError io_err = ERR_BACKEND_NO_ERR;
    gchar *message = NULL;
    gboolean success = FALSE;

    if (data->n_unassigned || data->n_impossible)
        goto cleanup_parse_file;

    /* fill subst hash table with byte sequence substitutions */
    data->subst = g_hash_table_new_full (g_str_hash, g_str_equal, g_free, g_free);
    g_hash_table_foreach (data->ambiguous_ht, (GHFunc) subst_insert_amb, data);
    g_hash_table_foreach (data->unique, (GHFunc) subst_insert_unique, data);

    if (!data->subst)
        goto cleanup_parse_file;

    /* create a temporary QofSession */
    gxi_session_destroy (data);
    session = qof_session_new (NULL);
    data->session = session;
    if (!gxi_session_begin (session, data->filename))
    {
        message = _("The file could not be reopened.");
        goto cleanup_parse_file;
    }
    io_err = qof_session_get_error (session);
    if (io_err != ERR_BACKEND_NO_ERR)
    {
        message = _("The file could not be reopened.");
        goto cleanup_parse_file;
    }

    xaccLogDisable ();
    gxi_update_progress_bar (_("Reading file…"), 0.0);
    if (!gxi_session_load (session))
    {
        gxi_update_progress_bar (NULL, -1.0);
        xaccLogEnable ();
        message = _("The file could not be reopened.");
        goto cleanup_parse_file;
    }
    gxi_update_progress_bar (NULL, -1.0);
    xaccLogEnable ();

    io_err = qof_session_get_error (session);
    if (io_err == ERR_BACKEND_NO_ERR)
    {
        /* loaded successfully now. strange, but ok */
        success = TRUE;
        goto cleanup_parse_file;
    }
    else if (io_err != ERR_FILEIO_NO_ENCODING)
    {
        /* another error, cannot handle this here */
        message = _("The file could not be reopened.");
        goto cleanup_parse_file;
    }

    qof_session_pop_error (session);
    book = qof_session_get_book (session);
    backend = qof_book_get_backend (book);

    gxi_update_progress_bar (_("Parsing file…"), 0.0);
    success = gxi_session_parse_with_subst (session, backend, book, data->subst);
    gxi_update_progress_bar (NULL, -1.0);

    if (success)
        data->session = session;
    else
        message = _("There was an error parsing the file.");

cleanup_parse_file:

    if (data->subst)
    {
        g_hash_table_destroy (data->subst);
        data->subst = NULL;
    }
    if (message)
    {
        gxi_set_error (data, message);
    }
    if (!success)
        gxi_session_destroy (data);

    return success;
}

static gboolean
gxi_save_file (GncXmlImportData *data)
{
    QofBackendError io_err;
    g_return_val_if_fail (data && data->session, FALSE);

    gxi_update_progress_bar (_("Writing file…"), 0.0);
    if (!gxi_session_save (data->session))
    {
        gxi_update_progress_bar (NULL, -1.0);
        gxi_set_error (data, _("The converted file could not be saved."));
        gxi_session_destroy (data);
        return FALSE;
    }
    gxi_update_progress_bar (NULL, -1.0);

    io_err = qof_session_get_error (data->session);

    if (io_err == ERR_BACKEND_NO_ERR)
    {
        return TRUE;
    }
    else
    {
        gxi_set_error (data, _("The converted file could not be saved."));
        gxi_session_destroy (data);
        return FALSE;
    }
}


/***************************
 *                         *
 * Encodings editor window *
 *                         *
 **************************/

static GListModel *
gxi_system_encoding_children_cb (gpointer item, gpointer user_data)
{
    GListModel *children;

    (void)user_data;
    children = g_object_get_data (G_OBJECT (item),
                                  "gnc-xml-encoding-children");
    return children ? G_LIST_MODEL (g_object_ref (children)) : NULL;
}

static GListStore *
gxi_system_encoding_model_new (void)
{
    GListStore *roots;
    GtkStringObject *previous = NULL;
    guint i;

    roots = g_list_store_new (GTK_TYPE_STRING_OBJECT);
    for (i = 0; i < n_system_encodings; i++)
    {
        system_encoding_type *system_encoding = &system_encodings[i];
        GtkStringObject *parent = previous;
        GtkStringObject *node;
        GListStore *children;
        GListStore *store;
        GQuark encoding = 0;
        gint level;

        for (level = 0; level < system_encoding->parent && parent; level++)
            parent = g_object_get_data (G_OBJECT (parent),
                                        "gnc-xml-encoding-parent");
        node = gtk_string_object_new (gettext (system_encoding->text));
        if (system_encoding->encoding)
            encoding = g_quark_from_string (system_encoding->encoding);
        children = g_list_store_new (GTK_TYPE_STRING_OBJECT);
        g_object_set_data_full (G_OBJECT (node), "gnc-xml-encoding-children",
                                children, g_object_unref);
        g_object_set_data (G_OBJECT (node), "gnc-xml-encoding-parent", parent);
        g_object_set_data (G_OBJECT (node), "gnc-xml-encoding-quark",
                           GUINT_TO_POINTER (encoding));
        store = parent ? g_object_get_data (G_OBJECT (parent),
                                            "gnc-xml-encoding-children") : roots;
        g_list_store_append (store, node);
        g_clear_object (&previous);
        previous = g_object_ref (node);
        g_object_unref (node);
    }
    g_clear_object (&previous);
    return roots;
}

static void
gxi_string_item_setup_cb (GtkSignalListItemFactory *factory,
                          GtkListItem *list_item, gpointer user_data)
{
    GtkWidget *label;

    (void)factory;
    (void)user_data;
    label = gtk_label_new (NULL);
    gtk_label_set_xalign (GTK_LABEL (label), 0.0);
    gtk_label_set_ellipsize (GTK_LABEL (label), PANGO_ELLIPSIZE_END);
    gtk_list_item_set_child (list_item, label);
}

static void
gxi_string_item_bind_cb (GtkSignalListItemFactory *factory,
                         GtkListItem *list_item, gpointer user_data)
{
    GtkStringObject *item;

    (void)factory;
    (void)user_data;
    item = GTK_STRING_OBJECT (gtk_list_item_get_item (list_item));
    gtk_label_set_text (GTK_LABEL (gtk_list_item_get_child (list_item)),
                        gtk_string_object_get_string (item));
}

static void
gxi_tree_item_setup_cb (GtkSignalListItemFactory *factory,
                        GtkListItem *list_item, gpointer user_data)
{
    GtkTreeExpander *expander;
    GtkWidget *label;

    (void)factory;
    (void)user_data;
    expander = GTK_TREE_EXPANDER (gtk_tree_expander_new ());
    label = gtk_label_new (NULL);
    gtk_label_set_xalign (GTK_LABEL (label), 0.0);
    gtk_label_set_ellipsize (GTK_LABEL (label), PANGO_ELLIPSIZE_END);
    gtk_tree_expander_set_child (expander, label);
    gtk_list_item_set_child (list_item, GTK_WIDGET (expander));
}

static void
gxi_tree_item_bind_cb (GtkSignalListItemFactory *factory,
                       GtkListItem *list_item, gpointer user_data)
{
    GtkTreeListRow *row;
    GtkStringObject *item;
    GtkTreeExpander *expander;
    GtkLabel *label;

    (void)factory;
    (void)user_data;
    row = GTK_TREE_LIST_ROW (gtk_list_item_get_item (list_item));
    item = GTK_STRING_OBJECT (gtk_tree_list_row_get_item (row));
    expander = GTK_TREE_EXPANDER (gtk_list_item_get_child (list_item));
    label = GTK_LABEL (gtk_tree_expander_get_child (expander));
    gtk_label_set_text (label, gtk_string_object_get_string (item));
    gtk_tree_expander_set_list_row (expander, row);
    g_object_unref (item);
}

static GQuark
gxi_available_encoding (GncXmlImportData *data)
{
    GObject *selected;
    GtkTreeListRow *row;
    GObject *item;

    selected = gtk_single_selection_get_selected_item (data->available_selection);
    if (!selected)
        return 0;
    row = GTK_TREE_LIST_ROW (selected);
    item = gtk_tree_list_row_get_item (row);
    GQuark encoding = GPOINTER_TO_UINT (g_object_get_data (item,
                                                            "gnc-xml-encoding-quark"));
    g_object_unref (item);
    return encoding;
}

static GQuark
gxi_selected_encoding (GncXmlImportData *data)
{
    GObject *selected;

    selected = gtk_single_selection_get_selected_item (data->selected_selection);
    return selected ? GPOINTER_TO_UINT (g_object_get_data (selected,
                                                            "gnc-xml-encoding-quark")) : 0;
}

static void
gxi_update_encoding_editor_buttons (GncXmlImportData *data)
{
    gboolean can_add = gxi_available_encoding (data) != 0;
    gboolean can_remove = gxi_selected_encoding (data) != 0 &&
                          g_list_model_get_n_items (G_LIST_MODEL (data->selected_encodings)) > 1;

    gtk_widget_set_sensitive (GTK_WIDGET (data->add_encoding_button), can_add);
    gtk_widget_set_sensitive (GTK_WIDGET (data->remove_encoding_button), can_remove);
}

static void
gxi_encoding_editor_message (GncXmlImportData *data, const gchar *message)
{
    gtk_label_set_text (data->encodings_message_label, message);
    gtk_widget_set_visible (GTK_WIDGET (data->encodings_message_label), TRUE);
}

static void
gxi_append_selected_encoding (GncXmlImportData *data, GQuark encoding)
{
    GtkStringObject *item;

    item = gtk_string_object_new (g_quark_to_string (encoding));
    g_object_set_data (G_OBJECT (item), "gnc-xml-encoding-quark",
                       GUINT_TO_POINTER (encoding));
    g_list_store_append (data->selected_encodings, item);
    g_object_unref (item);
}

static void
gxi_add_encoding (GncXmlImportData *data, GQuark encoding)
{
    GIConv iconv;
    const gchar *message;
    gchar *string;

    if (!encoding)
        return;
    string = g_ascii_strup (g_quark_to_string (encoding), -1);
    encoding = g_quark_from_string (string);
    if (g_list_find (data->encodings, GUINT_TO_POINTER (encoding)))
    {
        message = _("This encoding has been added to the list already.");
        gxi_encoding_editor_message (data, message);
        g_free (string);
        return;
    }

    iconv = g_iconv_open ("UTF-8", string);
    if (iconv == (GIConv)-1)
    {
        message = _("This is an invalid encoding.");
        gxi_encoding_editor_message (data, message);
        g_free (string);
        return;
    }
    g_iconv_close (iconv);

    data->encodings = g_list_append (data->encodings, GUINT_TO_POINTER (encoding));
    gxi_append_selected_encoding (data, encoding);
    gtk_widget_set_visible (GTK_WIDGET (data->encodings_message_label), FALSE);
    gtk_editable_set_text (GTK_EDITABLE (data->custom_enc_entry), "");
    gxi_update_encoding_editor_buttons (data);
    g_free (string);
}

static void
gxi_add_selected_encoding_cb (GtkButton *button, GncXmlImportData *data)
{
    (void)button;
    gxi_add_encoding (data, gxi_available_encoding (data));
}

static void
gxi_add_custom_encoding_cb (GtkButton *button, GncXmlImportData *data)
{
    const gchar *encoding;

    (void)button;
    encoding = gtk_editable_get_text (GTK_EDITABLE (data->custom_enc_entry));
    if (encoding && *encoding)
        gxi_add_encoding (data, g_quark_from_string (encoding));
}

static void
gxi_custom_encoding_activate_cb (GtkEntry *entry, GncXmlImportData *data)
{
    (void)entry;
    gxi_add_custom_encoding_cb (NULL, data);
}

static void
gxi_remove_selected_encoding_cb (GtkButton *button, GncXmlImportData *data)
{
    guint position;
    GQuark encoding;

    (void)button;
    position = gtk_single_selection_get_selected (data->selected_selection);
    encoding = gxi_selected_encoding (data);
    if (position == GTK_INVALID_LIST_POSITION || !encoding)
        return;
    data->encodings = g_list_remove (data->encodings, GUINT_TO_POINTER (encoding));
    g_list_store_remove (data->selected_encodings, position);
    gtk_widget_set_visible (GTK_WIDGET (data->encodings_message_label), FALSE);
    gxi_update_encoding_editor_buttons (data);
}

static void
gxi_encoding_selection_changed_cb (GtkSelectionModel *model, guint position,
                                   guint n_items, GncXmlImportData *data)
{
    (void)model;
    (void)position;
    (void)n_items;
    gxi_update_encoding_editor_buttons (data);
}

static void
gxi_available_encoding_activated_cb (GtkListView *view, guint position,
                                     GncXmlImportData *data)
{
    (void)view;
    gtk_single_selection_set_selected (data->available_selection, position);
    gxi_add_encoding (data, gxi_available_encoding (data));
}

static void
gxi_selected_encoding_activated_cb (GtkListView *view, guint position,
                                    GncXmlImportData *data)
{
    (void)view;
    gtk_single_selection_set_selected (data->selected_selection, position);
    gxi_remove_selected_encoding_cb (NULL, data);
}

static void
gxi_close_encoding_editor (GncXmlImportData *data, gboolean apply)
{
    GtkWindow *window;

    if (!data->encodings_window)
        return;
    if (!apply && data->encodings_backup)
    {
        g_list_free (data->encodings);
        data->encodings = data->encodings_backup;
        data->encodings_backup = NULL;
    }
    g_clear_pointer (&data->encodings_backup, g_list_free);
    window = data->encodings_window;
    data->encodings_window = NULL;
    g_clear_object (&data->available_selection);
    g_clear_object (&data->selected_selection);
    g_clear_object (&data->selected_encodings);
    data->add_encoding_button = NULL;
    data->remove_encoding_button = NULL;
    data->encodings_message_label = NULL;
    data->custom_enc_entry = NULL;
    gtk_window_destroy (window);
    g_object_unref (window);
}

static gboolean
gxi_encoding_editor_close_request_cb (GtkWindow *window,
                                       GncXmlImportData *data)
{
    (void)window;
    gxi_close_encoding_editor (data, FALSE);
    return TRUE;
}

static void
gxi_encoding_editor_cancel_cb (GtkButton *button, GncXmlImportData *data)
{
    (void)button;
    gxi_close_encoding_editor (data, FALSE);
}

static void
gxi_encoding_editor_apply_cb (GtkButton *button, GncXmlImportData *data)
{
    (void)button;
    if (!data->encodings)
    {
        gxi_encoding_editor_message (data, _("Select at least one encoding."));
        return;
    }
    if (!g_list_find (data->encodings, GUINT_TO_POINTER (data->default_encoding)))
        data->default_encoding = GPOINTER_TO_UINT (data->encodings->data);
    if (!gxi_check_file (data))
    {
        gxi_encoding_editor_message (data, data->error_message);
        return;
    }

    gxi_update_default_enc_combo (data);
    gxi_update_string_box (data);
    gxi_update_conversion_forward (data);
    gxi_close_encoding_editor (data, TRUE);
}

static void
gxi_edit_encodings_clicked_cb (GtkButton *button, GncXmlImportData *data)
{
    GtkWindow *window;
    GtkWidget *content;
    GtkWidget *columns;
    GtkWidget *available_box;
    GtkWidget *selected_box;
    GtkWidget *buttons;
    GtkWidget *entry_box;
    GtkWidget *view;
    GListStore *roots;
    GtkTreeListModel *tree;
    GtkSignalListItemFactory *tree_factory;
    GtkSignalListItemFactory *string_factory;
    GList *iter;
    guint position;

    (void)button;
    if (data->encodings_window)
    {
        gtk_window_present (data->encodings_window);
        return;
    }

    window = GTK_WINDOW (gtk_window_new ());
    gnc_window_bind_to_application (window);
    data->encodings_window = g_object_ref (window);
    data->encodings_backup = g_list_copy (data->encodings);
    gtk_window_set_title (window, _("Edit the list of encodings"));
    gtk_window_set_default_size (window, 720, 420);
    gtk_window_set_modal (window, TRUE);
    gtk_window_set_transient_for (window, data->window);
    gtk_widget_set_name (GTK_WIDGET (window), "gnc-id-assistant-xml-encoding");

    content = gtk_box_new (GTK_ORIENTATION_VERTICAL, 12);
    gtk_widget_set_margin_start (content, 12);
    gtk_widget_set_margin_end (content, 12);
    gtk_widget_set_margin_top (content, 12);
    gtk_widget_set_margin_bottom (content, 12);
    gtk_window_set_child (window, content);
    data->encodings_message_label = GTK_LABEL (gtk_label_new (NULL));
    gtk_label_set_wrap (data->encodings_message_label, TRUE);
    gtk_label_set_xalign (data->encodings_message_label, 0.0);
    gtk_widget_add_css_class (GTK_WIDGET (data->encodings_message_label), "error");
    gtk_widget_set_visible (GTK_WIDGET (data->encodings_message_label), FALSE);
    gtk_box_append (GTK_BOX (content), GTK_WIDGET (data->encodings_message_label));

    columns = gtk_box_new (GTK_ORIENTATION_HORIZONTAL, 12);
    gtk_widget_set_vexpand (columns, TRUE);
    gtk_box_append (GTK_BOX (content), columns);
    available_box = gtk_box_new (GTK_ORIENTATION_VERTICAL, 6);
    selected_box = gtk_box_new (GTK_ORIENTATION_VERTICAL, 6);
    gtk_widget_set_hexpand (available_box, TRUE);
    gtk_widget_set_hexpand (selected_box, TRUE);
    gtk_box_append (GTK_BOX (columns), available_box);
    gtk_box_append (GTK_BOX (columns), selected_box);
    gtk_box_append (GTK_BOX (available_box), gtk_label_new (_("System input encodings")));
    gtk_box_append (GTK_BOX (selected_box), gtk_label_new (_("Selected encodings")));

    roots = gxi_system_encoding_model_new ();
    tree = gtk_tree_list_model_new (G_LIST_MODEL (g_object_ref (roots)), FALSE, FALSE,
                                    gxi_system_encoding_children_cb, NULL, NULL);
    data->available_selection = gtk_single_selection_new (G_LIST_MODEL (g_object_ref (tree)));
    tree_factory = GTK_SIGNAL_LIST_ITEM_FACTORY (gtk_signal_list_item_factory_new ());
    g_signal_connect (tree_factory, "setup", G_CALLBACK (gxi_tree_item_setup_cb), NULL);
    g_signal_connect (tree_factory, "bind", G_CALLBACK (gxi_tree_item_bind_cb), NULL);
    view = gtk_list_view_new (GTK_SELECTION_MODEL (g_object_ref (data->available_selection)),
                              GTK_LIST_ITEM_FACTORY (tree_factory));
    g_signal_connect (view, "activate", G_CALLBACK (gxi_available_encoding_activated_cb), data);
    gtk_widget_set_vexpand (view, TRUE);
    {
        GtkWidget *scrolled = gtk_scrolled_window_new ();
        gtk_widget_set_vexpand (scrolled, TRUE);
        gtk_scrolled_window_set_child (GTK_SCROLLED_WINDOW (scrolled), view);
        gtk_box_append (GTK_BOX (available_box), scrolled);
    }
    for (position = 0; position < g_list_model_get_n_items (G_LIST_MODEL (tree)); position++)
    {
        GtkTreeListRow *row = g_list_model_get_item (G_LIST_MODEL (tree), position);
        if (gtk_tree_list_row_get_depth (row) == 0)
            gtk_tree_list_row_set_expanded (row, TRUE);
        g_object_unref (row);
    }
    g_object_unref (tree);
    g_object_unref (roots);

    data->selected_encodings = g_list_store_new (GTK_TYPE_STRING_OBJECT);
    for (iter = data->encodings; iter; iter = iter->next)
        gxi_append_selected_encoding (data, GPOINTER_TO_UINT (iter->data));
    data->selected_selection = gtk_single_selection_new (
        G_LIST_MODEL (g_object_ref (data->selected_encodings)));
    string_factory = GTK_SIGNAL_LIST_ITEM_FACTORY (gtk_signal_list_item_factory_new ());
    g_signal_connect (string_factory, "setup", G_CALLBACK (gxi_string_item_setup_cb), NULL);
    g_signal_connect (string_factory, "bind", G_CALLBACK (gxi_string_item_bind_cb), NULL);
    view = gtk_list_view_new (GTK_SELECTION_MODEL (g_object_ref (data->selected_selection)),
                              GTK_LIST_ITEM_FACTORY (string_factory));
    g_signal_connect (view, "activate", G_CALLBACK (gxi_selected_encoding_activated_cb), data);
    gtk_widget_set_vexpand (view, TRUE);
    {
        GtkWidget *scrolled = gtk_scrolled_window_new ();
        gtk_widget_set_vexpand (scrolled, TRUE);
        gtk_scrolled_window_set_child (GTK_SCROLLED_WINDOW (scrolled), view);
        gtk_box_append (GTK_BOX (selected_box), scrolled);
    }
    entry_box = gtk_box_new (GTK_ORIENTATION_HORIZONTAL, 6);
    data->custom_enc_entry = GTK_ENTRY (gtk_entry_new ());
    gtk_widget_set_hexpand (GTK_WIDGET (data->custom_enc_entry), TRUE);
    gtk_box_append (GTK_BOX (entry_box), GTK_WIDGET (data->custom_enc_entry));
    data->add_encoding_button = GTK_BUTTON (gtk_button_new_with_mnemonic (_("_Add")));
    gtk_box_append (GTK_BOX (entry_box), GTK_WIDGET (data->add_encoding_button));
    gtk_box_append (GTK_BOX (available_box), gtk_label_new (_("Custom encoding")));
    gtk_box_append (GTK_BOX (available_box), entry_box);
    data->remove_encoding_button = GTK_BUTTON (gtk_button_new_with_mnemonic (_("_Remove")));
    gtk_box_append (GTK_BOX (selected_box), GTK_WIDGET (data->remove_encoding_button));

    buttons = gtk_box_new (GTK_ORIENTATION_HORIZONTAL, 6);
    gtk_widget_set_halign (buttons, GTK_ALIGN_END);
    {
        GtkWidget *cancel = gtk_button_new_with_mnemonic (_("_Cancel"));
        GtkWidget *apply = gtk_button_new_with_mnemonic (_("_Apply"));
        gtk_box_append (GTK_BOX (buttons), cancel);
        gtk_box_append (GTK_BOX (buttons), apply);
        g_signal_connect (cancel, "clicked", G_CALLBACK (gxi_encoding_editor_cancel_cb), data);
        g_signal_connect (apply, "clicked", G_CALLBACK (gxi_encoding_editor_apply_cb), data);
        gtk_window_set_default_widget (window, apply);
    }
    gtk_box_append (GTK_BOX (content), buttons);

    g_signal_connect (data->encodings_window, "close-request",
                      G_CALLBACK (gxi_encoding_editor_close_request_cb), data);
    g_signal_connect (data->add_encoding_button, "clicked",
                      G_CALLBACK (gxi_add_selected_encoding_cb), data);
    g_signal_connect (data->remove_encoding_button, "clicked",
                      G_CALLBACK (gxi_remove_selected_encoding_cb), data);
    g_signal_connect (data->custom_enc_entry, "activate",
                      G_CALLBACK (gxi_custom_encoding_activate_cb), data);
    g_signal_connect (data->available_selection, "selection-changed",
                      G_CALLBACK (gxi_encoding_selection_changed_cb), data);
    g_signal_connect (data->selected_selection, "selection-changed",
                      G_CALLBACK (gxi_encoding_selection_changed_cb), data);
    gxi_update_encoding_editor_buttons (data);
    gtk_window_present (window);
}
