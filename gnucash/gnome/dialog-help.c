/********************************************************************\
 * dialog-help.c -- Gnucash Help Window                             *
 * Copyright (C) 2021 Robert Fewell                                 *
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

#include <config.h>

#include <gtk/gtk.h>
#include <glib/gi18n.h>

#include "dialog-help.h"
#include "dialog-utils.h"
#include "gnc-gnome-utils.h"

#include "gnc-html-history.h"
#include "gnc-html.h"
#include "gnc-html-factory.h"
#include "gnc-ui.h"

#include "file-utils.h"

#include "gnc-component-manager.h"
#include "gnc-session.h"

#include "gnc-ui.h"
#include "gnc-ui-util.h"

#define DIALOG_HELP_CM_CLASS  "dialog-help"
#define GNC_PREFS_GROUP       "dialogs.help"

#define WEB_URL  "https://www.gnucash.org"
#define WIKI_URL "https://wiki.gnucash.org/wiki/GnuCash"
#define BUG_URL  "https://bugs.gnucash.org"

const gchar *msg_no_help_found = N_("GnuCash could not find the files for the help documentation.");
const gchar *msg_no_help_reason = N_("This is likely because the \"gnucash-docs\" package is not properly installed.");

const gchar *msg_no_help_file = N_("Unable to find html file in a valid language directory");

/** Enumeration for the completion model */
enum GncHelpComp
{
    TEXT_ITEM,
    ANCHOR_ITEM
};

typedef struct
{
    GtkWidget    *window;
    GtkWidget    *main_container;

    GncHtml      *html;

    gboolean      is_help;
    GtkWidget    *button;
    const char   *dir_name;
    const char   *anchor;
    gchar        *help_path;
    gchar        *url;

    GtkWidget    *entry;
    GtkListStore *store;

    gint          component_id;
    QofSession   *session;

}HelpDialog;

/* This static indicates the debugging module that this .o belongs to.  */
static QofLogModule log_module = GNC_MOD_GUI;

void gnc_help_dialog_back_button_cb (GtkWidget *widget, gpointer user_data);
void gnc_help_dialog_forward_button_cb (GtkWidget *widget, gpointer user_data);
void gnc_help_dialog_home_button_cb (GtkWidget *widget, gpointer user_data);

void gnc_help_dialog_web_button_cb (GtkWidget *widget, gpointer user_data);
void gnc_help_dialog_wiki_button_cb (GtkWidget *widget, gpointer user_data);
void gnc_help_dialog_bug_button_cb (GtkWidget *widget, gpointer user_data);

void gnc_help_dialog_help_guide_button_cb (GtkWidget *widget, gpointer user_data);
void gnc_help_dialog_close_button_cb (GtkWidget *widget, gpointer user_data);

static void
close_handler (gpointer user_data)
{
    HelpDialog *help_dialog = user_data;

    ENTER(" ");
    gnc_save_window_size (GNC_PREFS_GROUP, GTK_WINDOW(help_dialog->window));
    gtk_widget_destroy (GTK_WIDGET(help_dialog->window));
    LEAVE(" ");
}

static gboolean
gnc_help_dialog_window_delete_event_cb (GtkWidget *widget,
                                        GdkEvent  *event,
                                        gpointer   user_data)
{
    HelpDialog *help_dialog = user_data;
    // this cb allows the window size to be saved on closing with the X
    gnc_save_window_size (GNC_PREFS_GROUP, GTK_WINDOW(help_dialog->window));
    return FALSE;
}

static void
gnc_help_dialog_window_destroy_cb (GtkWidget *object, gpointer user_data)
{
    HelpDialog *help_dialog = user_data;

    ENTER(" ");
    gnc_unregister_gui_component_by_data (DIALOG_HELP_CM_CLASS, help_dialog);

    gnc_html_destroy (help_dialog->html);

    help_dialog->main_container = NULL;
    help_dialog->html           = NULL;

    g_free (help_dialog->url);
    g_free (help_dialog->help_path);

    if (help_dialog->window)
    {
        gtk_widget_destroy (help_dialog->window);
        help_dialog->window = NULL;
    }
    g_free (help_dialog);
    LEAVE(" ");
}

static gboolean
gnc_help_dialog_window_key_press_cb (GtkWidget *widget, GdkEventKey *event,
                                     gpointer user_data)
{
    HelpDialog *help_dialog = user_data;

    if (event->keyval == GDK_KEY_Escape)
    {
        close_handler (help_dialog);
        return TRUE;
    }
    else
        return FALSE;
}

static void
open_url (GtkWidget *parent, const gchar *url)
{
    GError *error = NULL;
    gboolean success = gtk_show_uri_on_window (NULL, url, gtk_get_current_event_time (), &error);

    if (success)
        return;

    gnc_error_dialog (GTK_WINDOW(parent), _("There was a problem opening the external link...\n '%s'"), url);

    PERR("%s", error->message);
    g_error_free (error);
}

void
gnc_help_dialog_web_button_cb (GtkWidget *widget, gpointer user_data)
{
    HelpDialog *help_dialog = user_data;
    open_url (help_dialog->window, WEB_URL);
}

void
gnc_help_dialog_wiki_button_cb (GtkWidget *widget, gpointer user_data)
{
    HelpDialog *help_dialog = user_data;
    open_url (help_dialog->window, WIKI_URL);
}

void
gnc_help_dialog_bug_button_cb (GtkWidget *widget, gpointer user_data)
{
    HelpDialog *help_dialog = user_data;
    open_url (help_dialog->window, BUG_URL);
}

static void
gnc_help_dialog_set_title (HelpDialog *help_dialog)
{
    if (help_dialog->is_help)
    {
        gtk_window_set_title (GTK_WINDOW(help_dialog->window), _("GnuCash Help Manual"));
        gtk_button_set_label (GTK_BUTTON(help_dialog->button), _("_Guide"));
    }
    else
    {
        gtk_window_set_title (GTK_WINDOW(help_dialog->window), _("GnuCash Tutorial and Concepts Guide"));
        gtk_button_set_label (GTK_BUTTON(help_dialog->button), _("_Help"));
    }
}

static gchar **
get_file_strsplit (const gchar *partial)
{
    gchar **lines;
#ifdef G_OS_WIN32
    lines = g_strsplit_set (partial, "\r\n", -1);
#else
    lines = g_strsplit_set (partial, "\n", -1);
#endif
    return lines;
}

static void
gnc_help_dialog_get_anchors (HelpDialog *help_dialog)
{
    gchar *toc_file = NULL;
    gchar *contents;
    gchar **lines;
    guint i, length;
    GHashTable *hash;

    ENTER(" ");

    if (help_dialog->entry)
        gtk_widget_hide (GTK_WIDGET(help_dialog->entry));

    toc_file = g_strconcat (help_dialog->help_path, help_dialog->dir_name, "/toc.hhc", NULL);

    if (!g_file_test (toc_file, G_FILE_TEST_EXISTS))
     {
        g_free (toc_file);
        PERR("toc.hhc file not found");
        LEAVE(" ");
        return;
    }

    PINFO("toc file full path is '%s'", toc_file);

    if (!g_file_get_contents (toc_file, &contents, NULL, NULL))
    {
        g_free (toc_file);
        LEAVE(" ");
        return;
    }
    g_free (toc_file);

    if (help_dialog->entry)
        gtk_widget_show (GTK_WIDGET(help_dialog->entry));

    hash = g_hash_table_new_full (g_str_hash, g_str_equal, g_free, NULL);

    lines = get_file_strsplit (contents);
    length = g_strv_length (lines);

    // clear the completion store
    gtk_list_store_clear (help_dialog->store);

    for (i = 0; i < length; i++)
    {
        gchar *text = NULL;
        gchar *anchor = NULL;

        if (lines[i])
        {
            if (g_str_has_prefix (lines[i], "<param name=\"Name\""))
            {
                gchar *ptr1 = g_strstr_len (lines[i], -1, "value=\"");
                if (ptr1)
                {
                    gchar *ptr2 = g_strstr_len (lines[i], -1, "\">");
                    text = g_strndup (ptr1 + 7, (ptr2 - (ptr1 + 7)));
                    i++;
                }
            }
            if (g_str_has_prefix (lines[i], "<param name=\"Local\""))
            {
                gchar *ptr1 = g_strstr_len (lines[i], -1, "value=\"");
                if (ptr1)
                {
                    gchar *ptr2 = g_strstr_len (lines[i], -1, "\">");
                    anchor = g_strndup (ptr1 + 7, (ptr2 - (ptr1 + 7)));
                }
            }
            if (text && anchor)
            {
                GtkTreeIter iter;
                gchar *test = g_strconcat (text, anchor, NULL);
                // see if text/anchor is in hash table so we only add it once.
                if (g_hash_table_lookup (hash, test) == NULL)
                {
                    gtk_list_store_append (help_dialog->store, &iter);
                    gtk_list_store_set (help_dialog->store, &iter,
                                        TEXT_ITEM, text,
                                        ANCHOR_ITEM, anchor, -1);

                    PINFO("completion text is '%s', anchor is '%s'", text, anchor);
                    g_hash_table_insert (hash, test, "test");
                }
            }
            g_free (text);
            g_free (anchor);
        }
    }
    g_hash_table_destroy (hash);
    g_strfreev (lines);
    g_free (contents);
    LEAVE(" ");
}

static gchar *
gnc_help_dialog_get_help_path (GtkWindow *parent, const char *dir_name)
{
    const gchar * const *sdatadirs = g_get_system_data_dirs ();
    const gchar * const *langs = g_get_language_names ();
    const gchar *lookfor = "gnucash/help/";
    gchar *help_path = NULL;
    gchar *help_file = NULL;
    gchar *full_path = NULL;
    gchar *url_path  = NULL;

    ENTER("dir_name is '%s'", dir_name);
    for (; *sdatadirs; sdatadirs++)
    {
        gchar *filepath = g_build_filename (*sdatadirs, lookfor, NULL);
        if (g_file_test (filepath, G_FILE_TEST_EXISTS))
            help_path = g_strdup (filepath);
        g_free (filepath);
    }

    if (!help_path)
    {
        gnc_error_dialog (GTK_WINDOW(parent), "%s\n%s", _(msg_no_help_found), _(msg_no_help_reason));
        PERR("Unable to find 'gnucash/help' directory");
        LEAVE(" ");
        return NULL;
    }

    if (g_strcmp0 (dir_name, HF_HELP) == 0)
        help_file = g_strconcat (dir_name, "/", HF_HELP_HOME, NULL);
    else
        help_file = g_strconcat (dir_name, "/", HF_GUIDE_HOME, NULL);

    for (; *langs; langs++)
    {
        gchar *filename = g_build_filename (help_path, *langs, help_file, NULL);
        if (g_file_test (filename, G_FILE_TEST_EXISTS))
            full_path = g_strdup (filename);
        g_free (filename);
    }
    g_free (help_path);

    if (full_path)
    {
        gchar *ptr = g_strrstr (full_path, help_file);
        url_path = g_strndup (full_path, (ptr - full_path));
    }
    else
    {
        gnc_error_dialog (GTK_WINDOW(parent), "%s\n%s", _(msg_no_help_found), _(msg_no_help_file));
        PERR("Unable to find valid html file in a language directory");
        g_free (help_file);
        LEAVE(" ");
        return NULL;
    }
    g_free (help_file);
    g_free (full_path);
    LEAVE("Returned help path is '%s'", url_path);
    return url_path;
}

static gchar *
gnc_help_dialog_build_url (GtkWindow *parent, const char *help_path,
                           const char *dir_name, const char *anchor)
{
    const gchar *url_start = NULL;
    gchar *help_file = NULL;
    gchar *url = NULL;
    gchar *anch = NULL;

    ENTER("help_path is '%s', dir_name is '%s', anchor is '%s'", help_path, dir_name, anchor);

    if (anchor)
    {
        gchar **split = g_strsplit (anchor, "#", 2);
        help_file = g_strconcat (help_path, dir_name, "/", split[0], NULL);
        anch = g_strdup (split[1]);
        g_strfreev (split);
    }
    else
    {
        if (g_strcmp0 (dir_name, HF_HELP) == 0)
            help_file = g_strconcat (help_path, dir_name, "/", HF_HELP_HOME, NULL);
        else
            help_file = g_strconcat (help_path, dir_name, "/", HF_GUIDE_HOME, NULL);
    }

#ifdef G_OS_WIN32
    url_start = "file:///";
#else
    url_start = "file://";
#endif

    if (g_file_test (help_file, G_FILE_TEST_EXISTS))
    {
        if (anchor)
            url = g_strconcat (url_start, help_file, "#", anch, NULL);
        else
            url = g_strconcat (url_start, help_file, NULL);
    }
    else
    {
        gnc_error_dialog (GTK_WINDOW(parent), "%s\n%s", _(msg_no_help_found), _(msg_no_help_file));
        PERR("Unable to find valid html file in a language directory");
        g_free (help_file);
        g_free (anch);
        LEAVE(" ");
        return NULL;
    }
    g_free (help_file);
    g_free (anch);

    LEAVE("Returned url is '%s'", url);
    return url;
}

static void
update_titles (HelpDialog *help_dialog, const gchar *url)
{
    gboolean changed = FALSE;

    if (g_strrstr (url, HF_HELP))
    {
        if (!help_dialog->is_help)
            changed = TRUE;
        help_dialog->is_help = TRUE;
        help_dialog->dir_name = HF_HELP;
    }
    else
    {
        if (help_dialog->is_help)
            changed = TRUE;
        help_dialog->is_help = FALSE;
        help_dialog->dir_name = HF_GUIDE;
    }
    if (changed)
    {
        gnc_help_dialog_set_title (help_dialog);
        gnc_help_dialog_get_anchors (help_dialog);
    }
}

void
gnc_help_dialog_home_button_cb (GtkWidget *widget, gpointer user_data)
{
    HelpDialog *help_dialog = user_data;
    gchar *url = NULL;

    if (help_dialog->is_help)
        url = gnc_help_dialog_build_url (GTK_WINDOW(help_dialog->window),
                                         help_dialog->help_path, HF_HELP, HF_HELP_HOME);
    else
        url = gnc_help_dialog_build_url (GTK_WINDOW(help_dialog->window),
                                         help_dialog->help_path, HF_GUIDE, HF_GUIDE_HOME);

    PINFO("Home url is: %s", url);

    if (!url)
        return;

    gnc_html_show_docs (help_dialog->html, URL_TYPE_FILE, url, "Help", FALSE);
    update_titles (help_dialog, url);
    g_free (url);
}

void
gnc_help_dialog_back_button_cb (GtkWidget *widget, gpointer user_data)
{
    HelpDialog *help_dialog = user_data;
    const gchar *url = gnc_html_go_back (GNC_HTML(help_dialog->html));

    PINFO("Back url is: %s", url);

    if (!url)
        return;

    update_titles (help_dialog, url);
}

void
gnc_help_dialog_forward_button_cb (GtkWidget *widget, gpointer user_data)
{
    HelpDialog *help_dialog = user_data;
    const gchar *url = gnc_html_go_forward (GNC_HTML(help_dialog->html));

    PINFO("Forward url is: %s", url);

    if (!url)
        return;

    update_titles (help_dialog, url);
}

void
gnc_help_dialog_close_button_cb (GtkWidget *widget, gpointer user_data)
{
    HelpDialog *help_dialog = user_data;
    gnc_close_gui_component (help_dialog->component_id);
}

static gboolean
gnc_dialog_help_file_url_cb (const char *location, const char *label,
                             gboolean new_window, GNCURLResult *result)
{
    g_return_val_if_fail (location != NULL, FALSE);
    g_return_val_if_fail (result != NULL, FALSE);

    ENTER("type '%s', location '%s', label '%s'", result->url_type, location, label);

    if (g_strcmp0 (result->url_type, URL_TYPE_SECURE) == 0)
    {
        gchar *url = g_strconcat ("https:", location, NULL);
        open_url (GTK_WIDGET(result->parent), url);
        g_free (url);
    }
    LEAVE(" ");
    return TRUE;
}

void
gnc_help_dialog_help_guide_button_cb (GtkWidget *widget, gpointer user_data)
{
    HelpDialog *help_dialog = user_data;
    gchar *url = NULL;
    const gchar *dir_name;

    if (help_dialog->is_help)
        dir_name = HF_GUIDE;
    else
        dir_name = HF_HELP;

    url = gnc_help_dialog_build_url (GTK_WINDOW(help_dialog->window),
                                     help_dialog->help_path, dir_name, NULL);

    if (url)
    {
        help_dialog->anchor = NULL;

        g_free (help_dialog->url);
        help_dialog->url = url;
        help_dialog->dir_name = dir_name;

        gnc_html_show_docs (help_dialog->html, URL_TYPE_FILE, help_dialog->url, "Help", FALSE);
        update_titles (help_dialog, help_dialog->url);
    }
}

static gboolean
gnc_help_dialog_match_selected_cb (GtkEntryCompletion *widget,
                                   GtkTreeModel       *model,
                                   GtkTreeIter        *iter,
                                   gpointer            user_data)
{
    HelpDialog *help_dialog = user_data;
    gchar *anchor = NULL;

    gtk_tree_model_get (model, iter, ANCHOR_ITEM, &anchor, -1);
    if (anchor)
    {
        gchar *url = gnc_help_dialog_build_url (GTK_WINDOW(help_dialog->window),
                                                help_dialog->help_path,
                                                help_dialog->dir_name, anchor);

        if (url)
            gnc_html_show_docs (help_dialog->html, URL_TYPE_FILE, url, "Help", FALSE);

        gtk_entry_set_text (GTK_ENTRY(help_dialog->entry), "");

        g_free (url);
        g_free (anchor);
    }
    return TRUE;
}

static gboolean
completion_func (GtkEntryCompletion *completion, const gchar *key,
                 GtkTreeIter *iter, gpointer user_data)
{
    GtkTreeModel *model = gtk_entry_completion_get_model (completion);
    gchar *item = NULL;
    gboolean ret = FALSE;

    gtk_tree_model_get (model, iter, TEXT_ITEM, &item, -1);

    if (item)
    {
        gchar *item_lower = g_ascii_strdown (item, -1);
        gchar *ptr = g_strstr_len (item_lower, -1, key);

        if (ptr)
            ret = TRUE;

        g_free (item_lower);
        g_free (item);
    }
    return ret;
}

static void
gnc_help_dialog_create (HelpDialog *help_dialog, GtkWindow *parent)
{
    GtkWidget      *window;
    GtkBuilder     *builder;
    GtkWidget      *button;
    GtkWindow      *topLvl;
    GtkWindowGroup *group;
    GtkEntryCompletion *completion;

    ENTER(" ");
    builder = gtk_builder_new ();
    gnc_builder_add_from_file (builder, "dialog-help.glade", "image1");
    gnc_builder_add_from_file (builder, "dialog-help.glade", "image2");
    gnc_builder_add_from_file (builder, "dialog-help.glade", "image3");
    gnc_builder_add_from_file (builder, "dialog-help.glade", "help_window");

    window = GTK_WIDGET(gtk_builder_get_object (builder, "help_window"));
    help_dialog->window = window;

    // add this dialog to a new window group so it is not effected by modal setting
    group = gtk_window_group_new ();
    gtk_window_group_add_window (group, GTK_WINDOW(window));
    g_object_unref (group);

    // Set the name for this dialog so it can be easily manipulated with css
    gtk_widget_set_name (GTK_WIDGET(window), "gnc-id-help-window");

    help_dialog->session = gnc_get_current_session ();

    help_dialog->main_container = GTK_WIDGET(gtk_builder_get_object (builder, "main-container"));

    topLvl = gnc_ui_get_main_window (NULL);
    help_dialog->html = gnc_html_factory_create_html ();
    gnc_html_set_parent (help_dialog->html, topLvl);

    gtk_container_add (GTK_CONTAINER(help_dialog->main_container),
                       gnc_html_get_widget (help_dialog->html));

    gtk_widget_set_hexpand (GTK_WIDGET(gnc_html_get_widget (help_dialog->html)), TRUE);
    gtk_widget_set_vexpand (GTK_WIDGET(gnc_html_get_widget (help_dialog->html)), TRUE);

    // handle any external secure links so they open in default browser
    gnc_html_register_url_handler (URL_TYPE_SECURE, gnc_dialog_help_file_url_cb);

    // disable the webkit inspector applet
    gnc_html_inspector_enable (help_dialog->html, FALSE);

    help_dialog->entry = GTK_WIDGET(gtk_builder_get_object (builder, "entry_search"));

    completion = gtk_entry_completion_new ();
    gtk_entry_set_completion (GTK_ENTRY(help_dialog->entry), completion);
    g_object_unref (completion);

    help_dialog->store = gtk_list_store_new (2, G_TYPE_STRING, G_TYPE_STRING);

    gtk_entry_completion_set_model (completion, GTK_TREE_MODEL(help_dialog->store));
    g_object_unref (GTK_TREE_MODEL(help_dialog->store));

    /* Use model column 0 as the text column and min key length */
    gtk_entry_completion_set_text_column (completion, TEXT_ITEM);
    gtk_entry_completion_set_minimum_key_length (completion, 2);

    gtk_entry_completion_set_match_func (completion,
                                        (GtkEntryCompletionMatchFunc) completion_func,
                                         NULL, NULL);

    g_signal_connect (completion, "match-selected",
                      G_CALLBACK (gnc_help_dialog_match_selected_cb), help_dialog);

    help_dialog->is_help = TRUE;

    help_dialog->button = GTK_WIDGET(gtk_builder_get_object (builder, "help_guide_button"));

    update_titles (help_dialog, help_dialog->url);

    if (help_dialog->is_help) // anchors only loaded on change so at start test
        gnc_help_dialog_get_anchors (help_dialog);

    //  typical file paths used
    // 'file:///usr/share/gnucash/help/C/gnucash-help/index.html'
    // 'file:///usr/share/gnucash/help/C/gnucash-help/ch06s15.html#print-check'
    // 'file:///c:/Program%20Files%20(x86)/gnucash/share/gnucash/help/C/gnucash-help/ch06s15.html#print-check'

    gnc_html_show_docs (help_dialog->html, URL_TYPE_FILE, help_dialog->url, "Help", FALSE);

    g_signal_connect (help_dialog->window, "destroy",
                      G_CALLBACK (gnc_help_dialog_window_destroy_cb), help_dialog);

    g_signal_connect (help_dialog->window, "delete-event",
                      G_CALLBACK(gnc_help_dialog_window_delete_event_cb), help_dialog);

    g_signal_connect (help_dialog->window, "key_press_event",
                      G_CALLBACK (gnc_help_dialog_window_key_press_cb), help_dialog);

    gtk_builder_connect_signals_full (builder, gnc_builder_connect_full_func, help_dialog);

    g_object_unref (G_OBJECT(builder));

    gnc_restore_window_size (GNC_PREFS_GROUP, GTK_WINDOW(help_dialog->window),
                             GTK_WINDOW(parent));
    LEAVE(" ");
}

static void
refresh_handler (GHashTable *changes, gpointer user_data)
{
    ENTER(" ");
    LEAVE(" ");
}

/********************************************************************\
 * gnc_help_dialog                                                  *
 * opens a window showing Gnucash Help information                  *
 *                                                                  *
 * Args:   parent  - the parent of the window to be created         *
 * Return: nothing                                                  *
\********************************************************************/
void
gnc_help_dialog_with_args (GtkWindow *parent, const char *dir_name, const char *anchor)
{
    HelpDialog *help_dialog;
    gchar *url = NULL;
    gchar *help_path = NULL;
    GList *dlgExists = NULL;

    ENTER(" ");

    help_path = gnc_help_dialog_get_help_path (parent, dir_name);
    if (!help_path)
    {
        LEAVE("no help path");
        return;
    }

    url = gnc_help_dialog_build_url (parent, help_path, dir_name, anchor);
    if (!url)
    {
        g_free (help_path);
        LEAVE("no url");
        return;
    }

    dlgExists = gnc_find_gui_components (DIALOG_HELP_CM_CLASS, NULL, NULL);

    if (dlgExists != NULL)
        help_dialog = (HelpDialog*)dlgExists->data;
    else
        help_dialog = g_new0 (HelpDialog, 1);

    if (help_dialog->help_path)
        g_free (help_dialog->help_path);
    if (help_dialog->url)
        g_free (help_dialog->url);
    help_dialog->url = url;
    help_dialog->dir_name = dir_name;
    help_dialog->anchor = anchor;
    help_dialog->help_path = help_path;

    if (dlgExists != NULL)
    {
        gnc_html_show_docs (help_dialog->html, URL_TYPE_FILE, url, "Help", FALSE);

        update_titles (help_dialog, url);
        gtk_window_present (GTK_WINDOW(help_dialog->window));
        g_list_free (dlgExists);
        LEAVE("existing help dialog raised");
        return;
    }
    gnc_help_dialog_create (help_dialog, parent);

    help_dialog->component_id = gnc_register_gui_component (DIALOG_HELP_CM_CLASS,
                                                            refresh_handler,
                                                            close_handler,
                                                            help_dialog);

    gnc_gui_component_set_session (help_dialog->component_id,
                                   help_dialog->session);

    gtk_widget_show_all (help_dialog->window);
    LEAVE("new help dialog created");
}

void
gnc_help_dialog_with_struct (gpointer user_data)
{
    help_dialog_args *args = user_data;

    if (args)
    {
        gnc_help_dialog_with_args (GTK_WINDOW(args->parent), args->dir_name, args->anchor);
        g_free (args);
    }
}

void
gnc_help_dialog_set_help_func (GFunc cb, gpointer user_data)
{
    gnc_gnome_help_set_help_dialog_func ((GFunc)cb, user_data);
}
