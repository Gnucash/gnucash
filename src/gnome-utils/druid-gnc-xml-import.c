/*
 * druid-gnc-xml-import.c --
 * Copyright (C) 2006 Andreas Koehler <andi5.py@gmx.net>
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

#include "config.h"

#include <gnome.h>
#include <glib/gi18n.h>
#include <gmodule.h>

#include "TransLog.h"
#include "druid-gnc-xml-import.h"
#include "dialog-utils.h"
#include "druid-utils.h"
#include "gnc-backend-file.h"
#include "gnc-filepath-utils.h"
#include "gnc-module.h"
#include "gnc-ui.h"
#include "io-gncxml-v2.h"

#define XML_GLADE_FILE "druid-gnc-xml-import.glade"

/* NOTE: Merge stuff is not implemented, only the result of a file parse is
 * merged into a given session.
 */

/* NOTE: This file uses the term "encoding" even in places where it is not
 * accurate. Please ignore that. Encodings occur in different forms:
 * - as descriptive string, as in the list of system encodings
 * - as string used for g_iconv_open
 * - as GQuark, representing above string
 * - as pointer, containing above gquark, used in lists
 */

typedef enum {
  XML_CONVERT_SINGLE_FILE,
  XML_MERGE_FILES
} GncXmlImportType;

typedef struct {
  GncXmlImportType import_type;

  GtkWidget *dialog;                  /* global window */
  GtkWidget *druid;                   /* druid */
  GtkWidget *file_chooser;            /* file chooser widget on load file page */
  GtkWidget *default_encoding_combo;  /* top combo on conversion page */
  GtkWidget *summary_label;           /* label on conversion page */
  GtkWidget *string_box;              /* vbox of combos on conversion page */
  GtkWidget *encodings_dialog;        /* dialog for selection of encodings */
  GtkTreeView *available_encs_view;   /* list view of standard encodings */
  GtkTreeView *selected_encs_view;    /* list view of selected encodings */
  GtkListStore *file_list_store;      /* list store for loaded files */
  GtkTreeView *file_list_view;        /* list view for loaded files */

  GList *files;                       /* list of loaded files */

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
  QofSession *session;
} GncXmlImportData;

typedef struct {
  gchar *filename;
  GtkTreeIter *file_list_iter;
} GncXmlImportFile;

/* used for the string combos, see ambiguous_free */
typedef struct {
  gchar *byte_sequence;
  GList *conv_list;
} ambiguous_type;

enum {
  FILE_COL_NAME = 0,
  FILE_COL_INFO,
  FILE_NUM_COLS
};

enum {
  WORD_COL_STRING = 0,
  WORD_COL_ENCODING,
  WORD_NUM_COLS
};

enum {
  ENC_COL_STRING = 0,
  ENC_COL_QUARK,
  ENC_NUM_COLS
};

static void gxi_data_destroy (GncXmlImportData *data);
static void gxi_ambiguous_info_destroy (GncXmlImportData *data);
static void gxi_session_destroy (GncXmlImportData *data);
static void gxi_check_file (GncXmlImportData *data);
static void gxi_sort_ambiguous_list (GncXmlImportData *data);
static gboolean gxi_parse_file (GncXmlImportData *data);
static gboolean gxi_save_file (GncXmlImportData *data);
static void gxi_update_progress_bar (const gchar *message, double percentage);
static void gxi_update_default_enc_combo (GncXmlImportData *data);
static void gxi_update_summary_label (GncXmlImportData *data);
static void gxi_update_string_box (GncXmlImportData *data);
static void gxi_update_conversion_forward (GncXmlImportData *data);
static GnomeDruidPage *gxi_get_named_page (GncXmlImportData *data,
                                           const gchar *name);
void gxi_dialog_destroy_cb (GtkObject *object, GncXmlImportData *data);
void gxi_cancel_cb (GnomeDruid *druid, GncXmlImportData *data);
void gxi_chooser_file_activated_cb (GtkFileChooser *chooser,
                                    GncXmlImportData *data);
gboolean gxi_load_file_next_cb (GnomeDruidPage *page, GtkWidget *widget,
                                GncXmlImportData *data);
void gxi_conversion_prepare_cb (GnomeDruidPage *page, GtkWidget *widget,
                                GncXmlImportData *data);
static void gxi_default_enc_combo_changed_cb (GtkComboBox *combo,
                                              GncXmlImportData *data);
static void gxi_string_combo_changed_cb (GtkComboBox *combo,
                                         GncXmlImportData *data);
void gxi_edit_encodings_clicked_cb (GtkButton *button, GncXmlImportData *data);
void gxi_available_enc_activated_cb (GtkTreeView *view, GtkTreePath *path,
                                     GtkTreeViewColumn *column,
                                     GncXmlImportData *data);
void gxi_add_enc_clicked_cb (GtkButton *button, GncXmlImportData *data);
void gxi_custom_enc_activate_cb (GtkEntry *entry, GncXmlImportData *data);
void gxi_add_custom_enc_clicked_cb (GtkButton *button, GncXmlImportData *data);
void gxi_selected_enc_activated_cb (GtkTreeView *view, GtkTreePath *path,
                                    GtkTreeViewColumn *column,
                                    GncXmlImportData *data);
void gxi_remove_enc_clicked_cb (GtkButton *button, GncXmlImportData *data);
gboolean gxi_conversion_next_cb  (GnomeDruidPage *page, GtkWidget *widget,
                                  GncXmlImportData *data);
void gxi_loaded_files_prepare_cb (GnomeDruidPage *page, GtkWidget *widget,
                                  GncXmlImportData *data);
gboolean gxi_loaded_files_next_cb (GnomeDruidPage *page, GtkWidget *widget,
                                   GncXmlImportData *data);
void gxi_unload_file_clicked_cb (GtkButton *button, GncXmlImportData *data);
void gxi_load_file_clicked_cb (GtkButton *button, GncXmlImportData *data);
void gxi_end_finish_cb (GnomeDruidPage *page, GtkWidget *widget,
                        GncXmlImportData *data);

static const gchar *encodings_doc_string = N_(
  "The file you are trying to load is from an older version of "
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
  "'Forward'. Or the words contain unexpected characters, in which "
  "case you should select different character encodings to see "
  "different results. You may have to edit the list of character "
  "encodings by clicking on the respective button."
  "\n\n"
  "Press 'Forward' now to select the correct character encoding for "
  "your data file.");

static const gchar *encodings_doc_page_title = N_("Ambiguous character encoding");

static const gchar *finish_convert_string = N_(
  "The file has been loaded successfully. If you click 'Apply' it will be saved "
  "and reloaded into the main application. That way you will have a working "
  "file as backup in the same directory.\n\n"
  "You can also go back and verify your selections by clicking on 'Back'.");

/* The debugging module that this .o belongs to. */
static QofLogModule log_module = GNC_MOD_GUI;

/* window containing a progress bar */
static GtkWidget *progress_window = NULL;
static GtkProgressBar *progress_bar = NULL;

/* this is used for a static tree of system encodings. encoding may be NULL.
   parent declares how often to go up in the path of the previous element and use
   that as parent, e.g. 0 -> child of previous, 1 -> same level as previous */
typedef struct {
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

gboolean
gnc_xml_convert_single_file (const gchar *filename)
{
  GncXmlImportData *data;
  GtkWidget *dialog, *widget;
  GladeXML *xml;
  gboolean success;

  data = g_new0 (GncXmlImportData, 1);
  data->import_type = XML_CONVERT_SINGLE_FILE;
  data->filename = g_strdup (filename);

  /* gather ambiguous info */
  gxi_check_file (data);
  if (data->n_impossible == -1)
    return FALSE;

  if (!g_hash_table_size (data->ambiguous_ht)) {

    /* no ambiguous strings */
    success =
      gxi_parse_file (data) &&
      gxi_save_file (data);

    gxi_data_destroy (data);

  } else {

    /* common druid initialization */
    xml = gnc_glade_xml_new (XML_GLADE_FILE, "GnuCash XML Import Dialog");

    dialog = glade_xml_get_widget (xml, "GnuCash XML Import Dialog");
    gtk_widget_hide ((GTK_DIALOG (dialog))->action_area);
    data->dialog = dialog;
    g_object_set_data_full (G_OBJECT (dialog), "xml", xml, g_object_unref);
    glade_xml_signal_autoconnect_full (xml, gnc_glade_autoconnect_full_func,
                                       data);

    data->druid = glade_xml_get_widget (xml, "gnc_xml_import_druid");
    gnc_druid_set_colors (GNOME_DRUID (data->druid));

    /* start page, explanations */
    widget = glade_xml_get_widget (xml, "start_page");
    gnome_druid_page_edge_set_text (GNOME_DRUID_PAGE_EDGE (widget),
                                    gettext (encodings_doc_string));
    gnome_druid_page_edge_set_title (GNOME_DRUID_PAGE_EDGE (widget),
                                     gettext (encodings_doc_page_title));
    gtk_widget_show (widget);

    gtk_widget_hide (glade_xml_get_widget (xml, "encodings_doc_page"));
    gtk_widget_hide (glade_xml_get_widget (xml, "load_file_page"));
    gtk_widget_hide (glade_xml_get_widget (xml, "loaded_files_page"));
    gtk_widget_hide (glade_xml_get_widget (xml, "merge_page"));

    /* finish page */
    widget = glade_xml_get_widget (xml, "end_page");
    gnome_druid_page_edge_set_text (GNOME_DRUID_PAGE_EDGE (widget),
                                    gettext (finish_convert_string));
    gtk_widget_show (widget);

    gxi_update_default_enc_combo (data);
    gxi_update_string_box (data);

    success =
      gtk_dialog_run (GTK_DIALOG (dialog)) == GTK_RESPONSE_APPLY &&
      gxi_save_file (data);

    gtk_widget_destroy (data->dialog);
  }

  return success;
}

/* this is NOT fully implemented */
void
gnc_xml_merge_files (void)
{
  GncXmlImportData *data;
  GtkWidget *dialog, *widget, *box;
  GladeXML *xml;

  data = g_new0 (GncXmlImportData, 1);
  data->import_type = XML_MERGE_FILES;

  /* common druid initialization */
  xml = gnc_glade_xml_new (XML_GLADE_FILE, "GnuCash XML Import Dialog");

  dialog = glade_xml_get_widget (xml, "GnuCash XML Import Dialog");
  gtk_widget_hide ((GTK_DIALOG (dialog))->action_area);
  data->dialog = dialog;
  g_object_set_data_full (G_OBJECT (dialog), "xml", xml, g_object_unref);
  glade_xml_signal_autoconnect_full (xml, gnc_glade_autoconnect_full_func,
                                     data);

  data->druid = glade_xml_get_widget (xml, "gnc_xml_import_druid");
  gnc_druid_set_colors (GNOME_DRUID (data->druid));

  /* encodings explanations */
  widget = glade_xml_get_widget (xml, "encodings_doc_label");
  gtk_label_set_text (GTK_LABEL (widget), gettext (encodings_doc_string));
  widget = glade_xml_get_widget (xml, "encodings_doc_page");
  gnome_druid_page_standard_set_title (GNOME_DRUID_PAGE_STANDARD (widget),
                                       gettext (encodings_doc_page_title));

  gtk_widget_show (glade_xml_get_widget (xml, "start_page"));
  gtk_widget_show (glade_xml_get_widget (xml, "end_page"));

  /* file chooser */
  data->file_chooser = gtk_file_chooser_widget_new (
    GTK_FILE_CHOOSER_ACTION_OPEN);
  box = glade_xml_get_widget (xml, "file_chooser_box");
  gtk_box_pack_start (GTK_BOX (box), data->file_chooser, TRUE, TRUE, 0);
  g_signal_connect (G_OBJECT (data->file_chooser), "file-activated",
                    G_CALLBACK (gxi_chooser_file_activated_cb), data);
  gtk_widget_show (data->file_chooser);

  /* selected file list */
  data->file_list_store = gtk_list_store_new (FILE_NUM_COLS,
                                              G_TYPE_STRING, G_TYPE_POINTER);
  data->file_list_view = GTK_TREE_VIEW (glade_xml_get_widget (
                                          xml, "selected_file_list"));
  gtk_tree_view_insert_column_with_attributes (
    data->file_list_view, -1, NULL,
    gtk_cell_renderer_text_new (), "text", FILE_COL_NAME, NULL);
  gtk_tree_view_set_model (data->file_list_view,
                           GTK_TREE_MODEL (data->file_list_store));
  g_object_unref (data->file_list_store);

  gtk_widget_show (dialog);
}

static void
gxi_data_destroy (GncXmlImportData *data)
{
  if (!data)
    return;

  if (data->dialog)
    gtk_widget_hide (data->dialog);

  if (data->file_chooser) {
    gtk_widget_destroy (data->file_chooser);
    data->file_chooser = NULL;
  }

  if (data->filename) {
    g_free (data->filename);
    data->filename = NULL;
  }

  gxi_session_destroy (data);
  gxi_ambiguous_info_destroy (data);

  if (data->choices) {
    g_hash_table_destroy (data->choices);
    data->choices = NULL;
  }

  if (data->string_box) {
    gtk_widget_destroy (data->string_box);
    data->string_box = NULL;
  }

  if (data->dialog) {
    gtk_widget_destroy (data->dialog);
    data->dialog = NULL;
  }

  g_free (data);
}

static void
conv_free (conv_type *conv) {
  if (conv) {
    g_free(conv->utf8_string);
    g_free(conv);
  }
}

static conv_type *
conv_copy (const conv_type *conv) {
  conv_type *new = NULL;
  if (conv) {
    new = g_new(conv_type, 1);
    new->encoding = conv->encoding;
    new->utf8_string = g_strdup (conv->utf8_string);
  }
  return new;
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

  if (found) {
    return ((conv_type*) found->data)->utf8_string;
  } else {
    return NULL;
  }
}

static gint
ambiguous_cmp (const ambiguous_type *a, const ambiguous_type *b,
               GncXmlImportData *data)
{
  const gchar *string_a = get_decoded_string (a, data->default_encoding);
  const gchar *string_b = get_decoded_string (b, data->default_encoding);

  if (string_a) {
    if (string_b) {
      /* both look good, usual compare */
      return strcmp (string_a, string_b);
    } else {
      /* a look good, b not. put b to the top */
      return 1;
    }
  } else {
    if (string_b) {
      /* b looks good, a not. put a to the top */
      return -1;
    } else {
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
ambiguous_free (ambiguous_type *amb) {
  if (amb) {
    g_free (amb->byte_sequence);
    g_list_foreach (amb->conv_list, (GFunc) conv_free, NULL);
    g_list_free (amb->conv_list);
    g_free (amb);
  }
}

static void
gxi_ambiguous_info_destroy (GncXmlImportData *data)
{
  if (data->unique) {
    g_hash_table_destroy (data->unique);
    data->unique = NULL;
  }
  if (data->ambiguous_ht) {
    g_hash_table_destroy (data->ambiguous_ht);
    data->unique = NULL;
  }
  if (data->ambiguous_list) {
    g_list_foreach (data->ambiguous_list, (GFunc) ambiguous_free, NULL);
    g_list_free (data->ambiguous_list);
    data->ambiguous_list = NULL;
  }
}

static void
gxi_session_destroy (GncXmlImportData *data)
{
  if (data->session) {
    xaccLogDisable ();
    qof_session_destroy (data->session);
    xaccLogEnable ();
    data->session = NULL;
  }
}

static void
gxi_check_file (GncXmlImportData *data)
{
  if (!data->encodings) {
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
    if (!is_utf8) {
      enc_ptr = GUINT_TO_POINTER (g_quark_from_string ("UTF-8"));
      data->encodings = g_list_append (data->encodings, enc_ptr);
    }

    /* Translators: Please insert encodings here that are typically used in your
     * locale, separated by spaces. No need for ASCII or UTF-8, check `locale -m`
     * for assistance with spelling. */
    enc_array = g_strsplit (_("ISO-8859-1 KOI8-U"), " ", 0);

    /* loop through typical encodings */
    for (enc_cursor = enc_array; *enc_cursor; enc_cursor++) {
      if (!**enc_cursor) continue;
      enc_string = g_ascii_strup (*enc_cursor, -1);
      enc_ptr = GUINT_TO_POINTER (g_quark_from_string (enc_string));

      if (!g_list_find (data->encodings, enc_ptr)) {
        /* test whether we like this encoding */
        iconv = g_iconv_open ("UTF-8", enc_string);
        if (iconv != (GIConv) -1)
          /* we like it */
          data->encodings = g_list_append (data->encodings, enc_ptr);
        g_iconv_close (iconv);
      }
      g_free (enc_string);
    }
    g_strfreev (enc_array);
  }

  if (!data->default_encoding) {
    /* choose top one */
    data->default_encoding = GPOINTER_TO_UINT (data->encodings->data);
  }

  if (!data->choices) {
    data->choices = g_hash_table_new_full (g_str_hash, g_str_equal,
                                           g_free, (GDestroyNotify) conv_free);
  }

  gxi_ambiguous_info_destroy (data);

  /* analyze file */
  data->n_impossible = gnc_xml2_find_ambiguous (
    data->filename, data->encodings, &data->unique, &data->ambiguous_ht, NULL);

  if (data->n_impossible != -1) {
    /* sort ambiguous words */
    g_hash_table_foreach (data->ambiguous_ht, (GHFunc)ambiguous_list_insert,
                          data);
    gxi_sort_ambiguous_list (data);
  }
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
  if (choice) {
    /* user choice */
    g_hash_table_insert (data->subst, g_strdup (byte_sequence),
                         g_strdup (choice->utf8_string));
  } else {
    default_conv = g_list_find_custom (conv_list, &data->default_encoding,
                                       (GCompareFunc) conv_enc_cmp);
    if (default_conv) {
      /* default conversion */
      default_utf8 = ((conv_type*) default_conv->data)->utf8_string;
      g_hash_table_insert (data->subst, g_strdup (byte_sequence),
                           g_strdup (default_utf8));
    } else {
      /* no conversion avaiable, stop filling of subst */
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

static gboolean
gxi_parse_file (GncXmlImportData *data)
{
  QofSession *session=NULL;
  QofBook *book;
  FileBackend *backend;
  QofBackendError io_err = ERR_BACKEND_NO_ERR;
  gchar *logpath, *message=NULL;
  gboolean success=FALSE;

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
  session = qof_session_new ();
  data->session = session;
  qof_session_begin (session, data->filename, TRUE, FALSE);
  io_err = qof_session_get_error (session);
  if (io_err != ERR_BACKEND_NO_ERR) {
    message = _("The file could not be reopened.");
    goto cleanup_parse_file;
  }

  logpath = xaccResolveFilePath (data->filename);
  xaccLogSetBaseName (logpath);
  xaccLogDisable ();
  gxi_update_progress_bar (_("Reading file..."), 0.0);
  qof_session_load (session, gxi_update_progress_bar);
  gxi_update_progress_bar (NULL, -1.0);
  xaccLogEnable ();

  io_err = qof_session_get_error (session);
  if (io_err == ERR_BACKEND_NO_ERR) {
    /* loaded sucessfully now. strange, but ok */
    success = TRUE;
    goto cleanup_parse_file;
  } else if (io_err != ERR_FILEIO_NO_ENCODING) {
    /* another error, cannot handle this here */
    message = _("The file could not be reopened.");
    goto cleanup_parse_file;
  }

  qof_session_pop_error (session);
  book = qof_session_get_book (session);
  backend = (FileBackend*) qof_book_get_backend (book);

  gxi_update_progress_bar (_("Parsing file..."), 0.0);
  success = gnc_xml2_parse_with_subst (backend, book, data->subst);
  gxi_update_progress_bar (NULL, -1.0);

  if (success)
    data->session = session;
  else
    message = _("There was an error parsing the file.");

 cleanup_parse_file:

  if (data->subst) {
    g_hash_table_destroy (data->subst);
    data->subst = NULL;
  }
  if (message) {
    gnc_error_dialog (data->dialog, "%s", message);
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

  gxi_update_progress_bar (_("Writing file..."), 0.0);
  qof_session_save (data->session, gxi_update_progress_bar);
  gxi_update_progress_bar (NULL, -1.0);

  io_err = qof_session_get_error (data->session);

  if (io_err == ERR_BACKEND_NO_ERR) {
    return TRUE;
  } else {
    gxi_session_destroy (data);
    return FALSE;
  }
}

static void
gxi_update_progress_bar (const gchar *message, double percentage)
{
  if (!progress_window) {
    progress_window = gtk_window_new (GTK_WINDOW_POPUP);
    progress_bar = GTK_PROGRESS_BAR (gtk_progress_bar_new ());
    gtk_container_set_border_width (GTK_CONTAINER (progress_window), 12);
    gtk_container_add (GTK_CONTAINER (progress_window),
                       GTK_WIDGET (progress_bar));
    gtk_widget_show (GTK_WIDGET (progress_bar));
  }

  if (percentage < 0) {
    gtk_progress_bar_set_text (progress_bar, NULL);
    gtk_progress_bar_set_fraction (progress_bar, 0.0);
    gtk_widget_hide (progress_window);
  } else {
    gtk_progress_bar_set_text (progress_bar, message);
    if (percentage <= 100)
      gtk_progress_bar_set_fraction (progress_bar, percentage/100);
    else
      gtk_progress_bar_pulse (progress_bar);
    gtk_widget_show (progress_window);
  }
}

static void
gxi_update_default_enc_combo (GncXmlImportData *data)
{
  GtkComboBox *combo;
  GList *enc_iter;

  /* add encodings list */
  if (data->default_encoding_combo)
    gtk_widget_destroy (data->default_encoding_combo);
  data->default_encoding_combo = gtk_combo_box_new_text ();
  combo = GTK_COMBO_BOX (data->default_encoding_combo);

  for (enc_iter = data->encodings; enc_iter; enc_iter = enc_iter->next) {
    gtk_combo_box_append_text (
      combo, g_quark_to_string (GPOINTER_TO_UINT (enc_iter->data)));
  }
  gtk_combo_box_set_active (
    combo,
    g_list_index (data->encodings, GUINT_TO_POINTER (data->default_encoding)));

  /* show encodings */
  g_signal_connect (G_OBJECT (combo), "changed",
                    G_CALLBACK (gxi_default_enc_combo_changed_cb), data);
  gtk_container_add (GTK_CONTAINER (gnc_glade_lookup_widget (
                                      data->druid, "default_enc_box")),
                     GTK_WIDGET (combo));
  gtk_widget_show (GTK_WIDGET (combo));
}

static void
gxi_update_summary_label (GncXmlImportData *data)
{
  gchar *string=NULL;
  gboolean show=FALSE;

  if (data->n_unassigned) {
    if (data->n_impossible) {
      string = g_strdup_printf (
        _("There are %d unassigned and %d undecodable words. "
          "Please add encodings."),
        data->n_unassigned, data->n_impossible);
      show = TRUE;
    } else {
      string = g_strdup_printf (
        _("There are %d unassigned words. "
          "Please decide on them or add encodings."),
        data->n_unassigned);
      show = TRUE;
    }
  } else {
    if (data->n_impossible) {
      string = g_strdup_printf (
        _("There are %d undecodable words. "
          "Please add encodings."),
        data->n_impossible);
      show = TRUE;
    } else {
      show = FALSE;
    }
  }

  if (show) {
    gtk_label_set_text (GTK_LABEL (data->summary_label), string);
    g_free (string);
    gtk_widget_show (data->summary_label);
  } else {
    gtk_widget_hide (data->summary_label);
  }
}

static void
gxi_update_string_box (GncXmlImportData *data)
{
  gchar *string;
  const gchar *utf8;
  GtkBox *vbox;
  GtkComboBox *combo;
  GtkListStore *store;
  GList *word_iter, *conv_iter;
  GtkCellRenderer *renderer;
  GtkTreeIter iter;
  GQuark chosen_encoding;
  GtkTreeIter *chosen_iter, *default_iter;
  ambiguous_type *amb;
  conv_type *conv;

  if (data->string_box)
    gtk_widget_destroy (data->string_box);

  data->string_box = gtk_vbox_new (FALSE, 6);
  vbox = GTK_BOX (data->string_box);

  data->n_unassigned = 0;

  /* loop through words */
  for (word_iter=data->ambiguous_list; word_iter; word_iter=word_iter->next) {

    store = gtk_list_store_new (WORD_NUM_COLS, G_TYPE_STRING, G_TYPE_POINTER);
    combo = GTK_COMBO_BOX (gtk_combo_box_new_with_model (
                             GTK_TREE_MODEL (store)));
    g_object_unref (store);
    renderer = gtk_cell_renderer_text_new ();
    gtk_cell_layout_pack_start (GTK_CELL_LAYOUT (combo), renderer, TRUE);
    gtk_cell_layout_set_attributes (GTK_CELL_LAYOUT (combo), renderer,
                                    "text", WORD_COL_STRING, NULL);

    /* add default string, if possible */
    amb = (ambiguous_type*) word_iter->data;
    utf8 = get_decoded_string (amb, data->default_encoding);
    default_iter = NULL;
    if (utf8) {
      string = g_strdup_printf ("%s (default)", utf8);
      gtk_list_store_append (store, &iter);
      gtk_list_store_set (store, &iter, WORD_COL_STRING, string,
                          WORD_COL_ENCODING,
                          GUINT_TO_POINTER (data->default_encoding), -1);
      g_free (string);
      default_iter = gtk_tree_iter_copy (&iter);
    }

    /* user has selected this previously */
    conv = (conv_type*) g_hash_table_lookup (data->choices, amb->byte_sequence);
    chosen_encoding = (conv) ? conv->encoding : 0;
    chosen_iter = NULL;

    /* loop through conversions */
    for (conv_iter = amb->conv_list; conv_iter; conv_iter = conv_iter->next) {
      conv = (conv_type*) conv_iter->data;
      string = g_strdup_printf ("%s (%s)", conv->utf8_string,
                                g_quark_to_string (conv->encoding));
      gtk_list_store_append (store, &iter);
      gtk_list_store_set (store, &iter, WORD_COL_STRING, string,
                          WORD_COL_ENCODING,
                          GUINT_TO_POINTER (conv->encoding), -1);
      g_free (string);

      if (chosen_encoding && conv->encoding == chosen_encoding) {
        chosen_iter = gtk_tree_iter_copy (&iter);
      }
    } /* next conversion */

    if (chosen_iter) {
      /* select previous selection again, are not we cute */
      gtk_combo_box_set_active_iter (combo, chosen_iter);
      gtk_tree_iter_free (chosen_iter);
    } else {
      if (default_iter) {
        /* select default entry */
        gtk_combo_box_set_active_iter (combo, default_iter);
      } else {
        /* count it */
        data->n_unassigned++;
      }
    }

    /* wire up combo */
    g_object_set_data (G_OBJECT (combo), "ambiguous", amb);
    g_signal_connect (G_OBJECT (combo), "changed",
                      G_CALLBACK (gxi_string_combo_changed_cb), data);
    gtk_box_pack_start (vbox, GTK_WIDGET (combo), FALSE, FALSE, 0);
    gtk_widget_show (GTK_WIDGET (combo));

  } /* next word */

  /* wire up whole string vbox */
  gtk_container_add (GTK_CONTAINER (gnc_glade_lookup_widget (
                                      data->druid, "string_box_container")),
                     GTK_WIDGET (vbox));
  gtk_widget_show (GTK_WIDGET (vbox));

  /* update label now, n_unassigned is calculated */
  if (!data->summary_label)
    data->summary_label = gnc_glade_lookup_widget (data->druid,
                                                   "impossible_label");
  gxi_update_summary_label (data);
}

static void
gxi_update_conversion_forward (GncXmlImportData *data)
{
  if (data->n_unassigned || data->n_impossible)
    gnome_druid_set_buttons_sensitive (GNOME_DRUID (data->druid),
                                       TRUE, FALSE, TRUE, TRUE);
  else
    gnome_druid_set_buttons_sensitive (GNOME_DRUID (data->druid),
                                       TRUE, TRUE, TRUE, TRUE);
}

static GnomeDruidPage *
gxi_get_named_page (GncXmlImportData *data, const gchar *name)
{
  return GNOME_DRUID_PAGE (gnc_glade_lookup_widget (data->dialog, name));
}

void
gxi_dialog_destroy_cb (GtkObject *object, GncXmlImportData *data)
{
  data->dialog = NULL;
  gxi_data_destroy (data);
}

void
gxi_cancel_cb (GnomeDruid *druid, GncXmlImportData *data)
{
  if (data->import_type == XML_CONVERT_SINGLE_FILE) {
    gtk_dialog_response (GTK_DIALOG (data->dialog), GTK_RESPONSE_CANCEL);
  } else {
    gtk_widget_destroy (data->dialog);
  }
}

static gint
file_filename_cmp (const GncXmlImportFile *file, const gchar *filename)
{
  return strcmp (file->filename, filename);
}

static void
gxi_load_file (GncXmlImportData *data)
{
  GncXmlImportFile *file;
  gchar *filename;
  GtkTreeIter iter;

  g_return_if_fail (data != NULL);

  filename  = gtk_file_chooser_get_filename (
    GTK_FILE_CHOOSER (data->file_chooser));

  if (filename == NULL) {
    return;
  }
  if (!g_file_test (filename, G_FILE_TEST_IS_REGULAR)) {
    g_free (filename);
    return;
  }

  if (g_list_find_custom (data->files, filename,
                          (GCompareFunc) file_filename_cmp)) {
    const gchar *message = _(
      "That GnuCash XML file is already loaded. Please select another file.");
    gnc_error_dialog (data->dialog, "%s", message);
    g_free (filename);
    return;
  }

  file = g_new0 (GncXmlImportFile, 1);
  file->filename = filename;

  data->files = g_list_append (data->files, file);

  gtk_list_store_append (data->file_list_store, &iter);
  gtk_list_store_set (data->file_list_store, &iter,
                      FILE_COL_NAME, filename,
                      FILE_COL_INFO, file,
                      -1);
  file->file_list_iter = gtk_tree_iter_copy (&iter);

  gnome_druid_set_page (
    GNOME_DRUID (data->druid),
/*     gxi_get_named_page (data, "loaded_files_page")); */
    gxi_get_named_page (data, "encodings_doc_page"));
}

static void
gxi_unload_file (GncXmlImportData *data, GncXmlImportFile *file)
{
  g_return_if_fail (data != NULL && file != NULL);

  data->files = g_list_remove (data->files, file);
  gtk_list_store_remove (data->file_list_store, file->file_list_iter);
  gtk_tree_iter_free (file->file_list_iter);

  g_free (file->filename);
}

void
gxi_chooser_file_activated_cb (GtkFileChooser *chooser, GncXmlImportData *data)
{
  gxi_load_file (data);
}

gboolean
gxi_load_file_next_cb (GnomeDruidPage *page, GtkWidget *widget,
                       GncXmlImportData *data)
{
  GtkFileChooser *chooser = GTK_FILE_CHOOSER (data->file_chooser);
  gchar *filename = gtk_file_chooser_get_filename (chooser);

  if (filename != NULL) {
    if (g_file_test (filename, G_FILE_TEST_IS_DIR)) {
      gtk_file_chooser_set_current_folder (chooser, filename);
    } else {
      gxi_load_file (data);
    }
    g_free (filename);
  }

  return TRUE;
}

void
gxi_conversion_prepare_cb (GnomeDruidPage *page, GtkWidget *widget,
                           GncXmlImportData *data)
{
  gxi_update_conversion_forward (data);
}

static void
gxi_default_enc_combo_changed_cb (GtkComboBox *combo, GncXmlImportData *data)
{
  GtkTreeIter iter;
  gchar *enc_string;
  GQuark curr_enc;

  if (!gtk_combo_box_get_active_iter (combo, &iter))
    return;

  gtk_tree_model_get (gtk_combo_box_get_model (combo), &iter,
                      0, &enc_string, -1);
  curr_enc = g_quark_from_string (enc_string);
  g_free (enc_string);

  if (data->default_encoding == curr_enc)
    return;
  if (!g_list_find (data->encodings, GUINT_TO_POINTER (curr_enc))) {
    /* should not happen */
    PERR("invalid encoding selection");
    return;
  }

  data->default_encoding = curr_enc;
  gxi_sort_ambiguous_list (data);
  gxi_update_string_box (data);
  gxi_update_conversion_forward (data);
}

static void
gxi_string_combo_changed_cb (GtkComboBox *combo, GncXmlImportData *data)
{
  GtkTreeIter iter;
  GList *found, *default_conv;
  gboolean is_active;
  ambiguous_type *amb;
  conv_type *prev_conv, *curr_conv=NULL;
  gpointer ptr;
  GQuark prev_enc, curr_enc;

  amb = (ambiguous_type*) g_object_get_data (G_OBJECT (combo), "ambiguous");
  prev_conv = (conv_type*) g_hash_table_lookup (data->choices,
                                                amb->byte_sequence);
  if (prev_conv)
    prev_enc = prev_conv->encoding;

  default_conv = g_list_find_custom (amb->conv_list, &data->default_encoding,
                                     (GCompareFunc) conv_enc_cmp);

  is_active = gtk_combo_box_get_active_iter (combo, &iter);
  if (is_active) {
    gtk_tree_model_get (gtk_combo_box_get_model (combo), &iter,
                        WORD_COL_ENCODING, &ptr, -1);
    curr_enc = GPOINTER_TO_UINT (ptr);
    found = g_list_find_custom (amb->conv_list, &curr_enc,
                                (GCompareFunc) conv_enc_cmp);
    if (found) {
      curr_conv = (conv_type*) found->data;
    } else {
      /* should not happen */
      PERR("invalid string selection");
      is_active = FALSE;
    }
  }

  if (is_active) {
    if (prev_conv) {
      if (curr_enc == prev_enc)
        return;

      /* remember new choice */
      g_hash_table_replace (data->choices, g_strdup (amb->byte_sequence),
                            conv_copy (curr_conv));

      found = g_list_find_custom (amb->conv_list, &prev_enc,
                                  (GCompareFunc) conv_enc_cmp);
      if (!found && !default_conv) {
        /* user selected encoding for a byte sequence undecodable in the default
           encoding, for the first time. previous selection is invalid now */
        data->n_unassigned--;
        gxi_update_summary_label (data);
        gxi_update_conversion_forward (data);
      }
    }
    else {
      /* first choice ever */
      g_hash_table_insert (data->choices, g_strdup (amb->byte_sequence),
                           conv_copy (curr_conv));

      if (!default_conv) {
        /* user selected encoding for a byte sequence undecodable in the default
           encoding, for the first time. no previous selection */
        data->n_unassigned--;
        gxi_update_summary_label (data);
        gxi_update_conversion_forward (data);
      }
    }
  }
  else {
    if (prev_conv) {
      /* user decided not to decide... however he did that */
      g_hash_table_remove (data->choices, amb->byte_sequence);

      if (!default_conv) {
        /* user deselected encoding for a byte sequence undecodable in the
           default encoding */
        data->n_unassigned++;
        gxi_update_summary_label (data);
        gxi_update_conversion_forward (data);
      }
    }
    /* the missing else clause means pure ignorance of this dialog ;-) */
  }
}

void
gxi_edit_encodings_clicked_cb (GtkButton *button, GncXmlImportData *data)
{
  GladeXML *xml;
  GtkWidget *dialog;
  GtkListStore *list_store;
  GtkTreeStore *tree_store;
  GtkTreeIter iter, parent, *parent_ptr;
  GList *encodings_bak, *enc_iter;
  const gchar *encoding;
  gchar *string;
  system_encoding_type *system_enc;
  gpointer enc_ptr;
  gint i, j;

  xml = gnc_glade_xml_new (XML_GLADE_FILE, "Encodings Dialog");
  dialog = glade_xml_get_widget (xml, "Encodings Dialog");
  data->encodings_dialog = dialog;
  g_object_set_data_full (G_OBJECT (dialog), "xml", xml, g_object_unref);
  glade_xml_signal_autoconnect_full (xml, gnc_glade_autoconnect_full_func, data);
  gtk_window_set_transient_for (GTK_WINDOW (dialog), GTK_WINDOW (data->dialog));

  data->available_encs_view = GTK_TREE_VIEW (glade_xml_get_widget (
                                               xml, "available_encs_view"));

  /* set up selected encodings list */
  data->selected_encs_view = GTK_TREE_VIEW (glade_xml_get_widget (
                                              xml, "selected_encs_view"));
  list_store = gtk_list_store_new (ENC_NUM_COLS, G_TYPE_STRING, G_TYPE_POINTER);
  for (enc_iter = data->encodings; enc_iter; enc_iter = enc_iter->next) {
    encoding = g_quark_to_string (GPOINTER_TO_UINT (enc_iter->data));
    gtk_list_store_append (list_store, &iter);
    gtk_list_store_set (list_store, &iter, ENC_COL_STRING, encoding,
                        ENC_COL_QUARK, enc_iter->data, -1);
  }
  gtk_tree_view_insert_column_with_attributes (
    data->selected_encs_view, -1, NULL,
    gtk_cell_renderer_text_new (), "text", ENC_COL_STRING, NULL);
  gtk_tree_view_set_model (data->selected_encs_view,
                           GTK_TREE_MODEL (list_store));
  g_object_unref (list_store);

  /* set up system encodings list */
  data->available_encs_view = GTK_TREE_VIEW (glade_xml_get_widget (
                                               xml, "available_encs_view"));
  tree_store = gtk_tree_store_new (ENC_NUM_COLS, G_TYPE_STRING, G_TYPE_POINTER);
  for (i = 0, system_enc = system_encodings;
       i < n_system_encodings;
       i++, system_enc++) {
    if (i == 0) {
      /* first system encoding */
      parent_ptr = NULL;
    } else {
      parent_ptr = &iter;
      for (j = 0; j < system_enc->parent; j++)
        if (gtk_tree_model_iter_parent (GTK_TREE_MODEL (tree_store),
                                        &parent, &iter)) {
          /* go up one level */
          iter = parent;
        } else {
          /* no parent to toplevel element */
          parent_ptr = NULL;
        }
    }
    if (system_enc->encoding)
      enc_ptr = GUINT_TO_POINTER (g_quark_from_string (system_enc->encoding));
    else
      enc_ptr = NULL;
    string = gettext (system_enc->text);
    gtk_tree_store_append (tree_store, &iter, parent_ptr);
    gtk_tree_store_set (tree_store, &iter, ENC_COL_STRING,
                        gettext (system_enc->text), ENC_COL_QUARK, enc_ptr, -1);
  }
  gtk_tree_view_insert_column_with_attributes (
    data->available_encs_view, -1, NULL,
    gtk_cell_renderer_text_new (), "text", ENC_COL_STRING, NULL);
  gtk_tree_view_set_model (data->available_encs_view,
                           GTK_TREE_MODEL (tree_store));
  g_object_unref (tree_store);

  /* run the dialog */
  encodings_bak = g_list_copy (data->encodings);
  if (gtk_dialog_run (GTK_DIALOG (dialog)) == GTK_RESPONSE_OK) {
    g_list_free (encodings_bak);
    if (!g_list_find (data->encodings,
                      GUINT_TO_POINTER (data->default_encoding))) {
      /* choose top level encoding then */
      data->default_encoding = GPOINTER_TO_UINT (data->encodings->data);
    }

    /* update whole page */
    gxi_check_file (data);
    gxi_update_default_enc_combo (data);
    gxi_update_string_box (data);
    gxi_update_conversion_forward (data);
  }
  else {
    g_list_free (data->encodings);
    data->encodings = encodings_bak;
  }

  gtk_widget_destroy (dialog);
  data->encodings_dialog = NULL;
}

static void
gxi_add_encoding (GncXmlImportData *data, gpointer encoding_ptr)
{
  GIConv iconv;
  const gchar *message;
  gchar *enc_string;
  GtkListStore *store;
  GtkTreeIter iter;

  enc_string = g_ascii_strup (
    g_quark_to_string (GPOINTER_TO_UINT (encoding_ptr)), -1);
  encoding_ptr = GUINT_TO_POINTER (g_quark_from_string (enc_string));

  if (g_list_find (data->encodings, encoding_ptr)) {
    message = _("This encoding has been added to the list already.");
    gnc_error_dialog (data->encodings_dialog, "%s", message);
    return;
  }

  /* test whether we like this encoding */
  iconv = g_iconv_open ("UTF-8", enc_string);
  if (iconv == (GIConv) -1) {
    g_iconv_close (iconv);
    g_free (enc_string);
    message = _("This is an invalid encoding.");
    gnc_error_dialog (data->encodings_dialog, "%s", message);
    return;
  }
  g_iconv_close (iconv);

  /* add to the list */
  data->encodings = g_list_append (data->encodings, encoding_ptr);
  store = GTK_LIST_STORE (gtk_tree_view_get_model (data->selected_encs_view));
  gtk_list_store_append (store, &iter);
  gtk_list_store_set (store, &iter, ENC_COL_STRING, enc_string,
                      ENC_COL_QUARK, encoding_ptr, -1);

  g_free (enc_string);

  if (!data->encodings->next)
    gtk_dialog_set_response_sensitive (GTK_DIALOG (data->encodings_dialog),
                                       GTK_RESPONSE_OK, TRUE);
}

void
gxi_available_enc_activated_cb (GtkTreeView *view, GtkTreePath *path,
                                GtkTreeViewColumn *column,
                                GncXmlImportData *data)
{
  GtkTreeModel *model;
  GtkTreeIter iter;
  gpointer enc_ptr;

  model = gtk_tree_view_get_model (data->available_encs_view);
  if (!gtk_tree_model_get_iter (model, &iter, path))
    return;
  gtk_tree_model_get (model, &iter, ENC_COL_QUARK, &enc_ptr, -1);
  if (!enc_ptr)
    return;
  gxi_add_encoding (data, enc_ptr);
}

void
gxi_add_enc_clicked_cb (GtkButton *button, GncXmlImportData *data)
{
  GtkTreeSelection *selection;
  GtkTreeModel *model;
  GtkTreeIter iter;
  gpointer enc_ptr;

  selection = gtk_tree_view_get_selection (data->available_encs_view);
  if (!gtk_tree_selection_get_selected (selection, &model, &iter))
    return;
  gtk_tree_model_get (model, &iter, ENC_COL_QUARK, &enc_ptr, -1);
  if (!enc_ptr)
    return;
  gxi_add_encoding (data, enc_ptr);
}

void
gxi_custom_enc_activate_cb (GtkEntry *entry, GncXmlImportData *data)
{
  const gchar *enc_string;

  enc_string = gtk_entry_get_text (entry);
  if (!enc_string)
    return;
  gxi_add_encoding (data, GUINT_TO_POINTER (g_quark_from_string (enc_string)));
}

void
gxi_add_custom_enc_clicked_cb (GtkButton *button, GncXmlImportData *data)
{
  GtkWidget *entry = gnc_glade_lookup_widget (data->encodings_dialog,
                                              "custom_enc_entry");
  gxi_custom_enc_activate_cb (GTK_ENTRY (entry), data);
}

static void
gxi_remove_encoding (GncXmlImportData *data, GtkTreeModel *model,
                         GtkTreeIter *iter)
{
  gpointer enc_ptr;

  gtk_tree_model_get (model, iter, ENC_COL_QUARK, &enc_ptr, -1);
  data->encodings = g_list_remove (data->encodings, enc_ptr);
  gtk_list_store_remove (GTK_LIST_STORE (model), iter);
  if (!data->encodings)
    gtk_dialog_set_response_sensitive (GTK_DIALOG (data->encodings_dialog),
                                       GTK_RESPONSE_OK, FALSE);
}

void
gxi_selected_enc_activated_cb (GtkTreeView *view, GtkTreePath *path,
                               GtkTreeViewColumn *column, GncXmlImportData *data)
{
  GtkTreeModel *model;
  GtkTreeIter iter;

  model = gtk_tree_view_get_model (data->selected_encs_view);
  if (!gtk_tree_model_get_iter (model, &iter, path))
    return;
  gxi_remove_encoding (data, model, &iter);
}

void
gxi_remove_enc_clicked_cb (GtkButton *button, GncXmlImportData *data)
{
  GtkTreeSelection *selection;
  GtkTreeModel *model;
  GtkTreeIter iter;

  selection = gtk_tree_view_get_selection (data->selected_encs_view);
  if (!gtk_tree_selection_get_selected (selection, &model, &iter))
    return;
  gxi_remove_encoding (data, model, &iter);
}

gboolean
gxi_conversion_next_cb  (GnomeDruidPage *page, GtkWidget *widget,
                         GncXmlImportData *data)
{
  return !gxi_parse_file (data);
}

void
gxi_loaded_files_prepare_cb (GnomeDruidPage *page, GtkWidget *widget,
                             GncXmlImportData *data)
{
  gnome_druid_set_buttons_sensitive (GNOME_DRUID (data->druid),
                                     FALSE, TRUE, TRUE, TRUE);
}

gboolean
gxi_loaded_files_next_cb (GnomeDruidPage *page, GtkWidget *widget,
                          GncXmlImportData *data)
{
  if (!g_list_first (data->files)) {
    const gchar *message = _(
      "No files to merge. Please add ones by clicking on 'Load another file'.");
    gnc_error_dialog (data->dialog, "%s", message);
    return TRUE;
  }

  return FALSE;
}

void
gxi_unload_file_clicked_cb (GtkButton *button, GncXmlImportData *data)
{
  GtkTreeSelection *selection;
  GtkTreeModel *model;
  GtkTreeIter iter;
  GncXmlImportFile *file;

  selection = gtk_tree_view_get_selection (GTK_TREE_VIEW (data->file_list_view));
  if (!gtk_tree_selection_get_selected (selection, &model, &iter))
    return;

  gtk_tree_model_get (model, &iter, FILE_COL_INFO, &file, -1);



  gxi_unload_file (data, file);
}

void
gxi_load_file_clicked_cb (GtkButton *button, GncXmlImportData *data)
{
  gnome_druid_set_page (GNOME_DRUID (data->druid),
                        gxi_get_named_page (data, "load_file_page"));
}

void
gxi_end_finish_cb (GnomeDruidPage *page, GtkWidget *widget,
                   GncXmlImportData *data)
{
  if (data->import_type == XML_CONVERT_SINGLE_FILE) {
    gtk_dialog_response (GTK_DIALOG (data->dialog), GTK_RESPONSE_APPLY);
  } else {
    gtk_widget_destroy (data->dialog);
  }
}
