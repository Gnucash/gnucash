/********************************************************************\
 * dialog-assoc.c -- Associations dialog                            *
 * Copyright (C) 2020 Robert Fewell                                 *
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

#include "dialog-assoc.h"

#include "dialog-utils.h"
#include "gnc-component-manager.h"
#include "gnc-session.h"
#include "Transaction.h"

#include "gnc-plugin-page-register.h"
#include "gnc-main-window.h"
#include "gnc-prefs.h"
#include "gnc-ui.h"
#include "gnc-ui-util.h"
#include "gnc-gnome-utils.h"
#include "gnc-uri-utils.h"
#include "gnc-filepath-utils.h"
#include "Account.h"

#define DIALOG_ASSOC_CM_CLASS    "dialog-assoc"
#define GNC_PREFS_GROUP          "dialogs.trans-assoc"

/** Enumeration for the tree-store */
enum GncAssocColumn
{
    DATE_TRANS,
    DATE_INT64, // used just for sorting date_trans
    DESC_TRANS,
    DISPLAY_URI,
    AVAILABLE,
    URI_SPLIT,
    URI,
    URI_RELATIVE, // used just for sorting relative_pix
    URI_RELATIVE_PIX
};

typedef struct
{
    GtkWidget    *window;
    GtkWidget    *view;
    GtkWidget    *path_head_label;
    gchar        *path_head;
    gboolean      book_ro;
    GtkTreeModel *model;
    gint          component_id;
    QofSession   *session;
}AssocDialog;

/* This static indicates the debugging module that this .o belongs to. */
static QofLogModule log_module = GNC_MOD_GUI;

/***********************************************************************/

static gchar *
convert_uri_to_abs_path (const gchar *path_head, const gchar *uri, gchar *uri_scheme, gboolean return_uri)
{
    gchar *ret_value = NULL;

    if (!uri_scheme) // relative path
    {
        gchar *path = gnc_uri_get_path (path_head);
        gchar *file_path = gnc_file_path_absolute (path, uri);

        if (return_uri)
            ret_value = gnc_uri_create_uri ("file", NULL, 0, NULL, NULL, file_path);
        else
            ret_value = g_strdup (file_path);

        g_free (path);
        g_free (file_path);
    }

    if (g_strcmp0 (uri_scheme, "file") == 0) // absolute path
    {
        if (return_uri)
            ret_value = g_strdup (uri);
        else
            ret_value = gnc_uri_get_path (uri);
    }
    return ret_value;
}

static gchar *
assoc_get_unescape_uri (const gchar *path_head, const gchar *uri, gchar *uri_scheme)
{
    gchar *display_str = NULL;

    if (uri && *uri)
    {
        // if scheme is null or 'file' we should get a file path
        gchar *file_path = convert_uri_to_abs_path (path_head, uri, uri_scheme, FALSE);

        if (file_path)
            display_str = g_uri_unescape_string (file_path, NULL);
        else
            display_str = g_uri_unescape_string (uri, NULL);

        g_free (file_path);

#ifdef G_OS_WIN32 // make path look like a traditional windows path
        display_str = g_strdelimit (display_str, "/", '\\');
#endif
    }
    DEBUG("Return display string is '%s'", display_str);
    return display_str;
}

static gchar *
assoc_get_use_uri (const gchar *path_head, const gchar *uri, gchar *uri_scheme)
{
    gchar *use_str = NULL;

    if (uri && *uri)
    {
        // if scheme is null or 'file' we should get a file path
        gchar *file_path = convert_uri_to_abs_path (path_head, uri, uri_scheme, TRUE);

        if (file_path)
            use_str = g_strdup (file_path);
        else
            use_str = g_strdup (uri);

        g_free (file_path);
    }
    DEBUG("Return use string is '%s'", use_str);
    return use_str;
}

static gchar *
assoc_get_path_head_and_set (gboolean *path_head_set)
{
    gchar *ret_path = NULL;
    gchar *path_head = gnc_prefs_get_string (GNC_PREFS_GROUP_GENERAL, "assoc-head");
    *path_head_set = FALSE;

    if (path_head && *path_head) // not default entry
    {
        *path_head_set = TRUE;
        ret_path = g_strdup (path_head);
    }
    else
    {
        const gchar *doc = g_get_user_special_dir (G_USER_DIRECTORY_DOCUMENTS);

        if (doc)
            ret_path = gnc_uri_create_uri ("file", NULL, 0, NULL, NULL, doc);
        else
            ret_path = gnc_uri_create_uri ("file", NULL, 0, NULL, NULL, gnc_userdata_dir ());
    }
    // make sure there is a trailing '/'
    if (!g_str_has_suffix (ret_path, "/"))
    {
        gchar *folder_with_slash = g_strconcat (ret_path, "/", NULL);
        g_free (ret_path);
        ret_path = g_strdup (folder_with_slash);
        g_free (folder_with_slash);

        if (*path_head_set) // prior to 3.5, assoc-head could be with or without a trailing '/'
        {
            if (!gnc_prefs_set_string (GNC_PREFS_GROUP_GENERAL, "assoc-head", ret_path))
                PINFO ("Failed to save preference at %s, %s with %s",
                       GNC_PREFS_GROUP_GENERAL, "assoc-head", ret_path);
        }
    }
    g_free (path_head);
    return ret_path;
}

static gchar *
assoc_get_path_head (void)
{
    gboolean path_head_set = FALSE;

    return assoc_get_path_head_and_set (&path_head_set);
}

static void
assoc_set_path_head_label (GtkWidget *path_head_label)
{
    gboolean path_head_set = FALSE;
    gchar *path_head = assoc_get_path_head_and_set (&path_head_set);
    gchar *scheme = gnc_uri_get_scheme (path_head);
    gchar *path_head_str = assoc_get_unescape_uri (NULL, path_head, scheme);
    gchar *path_head_text;

    if (path_head_set)
    {
        // test for current folder being present
        if (g_file_test (path_head_str, G_FILE_TEST_IS_DIR))
            path_head_text = g_strdup_printf ("%s '%s'", _("Path head for files is,"), path_head_str);
        else
            path_head_text = g_strdup_printf ("%s '%s'", _("Path head does not exist,"), path_head_str);
    }
    else
        path_head_text = g_strdup_printf (_("Path head not set, using '%s' for relative paths"), path_head_str);

    gtk_label_set_text (GTK_LABEL(path_head_label), path_head_text);

    // Set the style context for this label so it can be easily manipulated with css
    gnc_widget_style_context_add_class (GTK_WIDGET(path_head_label), "gnc-class-highlight");

    g_free (scheme);
    g_free (path_head_str);
    g_free (path_head_text);
    g_free (path_head);
}

void
gnc_assoc_open_uri (GtkWindow *parent, const gchar *uri)
{
    if (uri && *uri)
    {
        gchar     *scheme = gnc_uri_get_scheme (uri);
        gchar  *path_head = assoc_get_path_head ();
        gchar    *run_uri = assoc_get_use_uri (path_head, uri, scheme);
        gchar *run_scheme = gnc_uri_get_scheme (run_uri);

        PINFO("Open uri scheme is '%s', uri is '%s'", run_scheme, run_uri);

        if (run_scheme) // make sure we have a scheme entry
        {
            gnc_launch_assoc (GTK_WINDOW (parent), run_uri);
            g_free (run_scheme);
        }
        g_free (run_uri);
        g_free (path_head);
        g_free (scheme);
    }
}

/***********************************************************************/

static void
location_ok_cb (GtkEditable *editable, gpointer user_data)
{
    GtkWidget *ok_button = user_data;
    gboolean have_scheme = FALSE;
    gchar *text = gtk_editable_get_chars (editable, 0, -1);
    GtkWidget *warning_hbox = g_object_get_data (G_OBJECT(editable), "whbox");

    if (text && *text)
    {
        gchar *scheme = gnc_uri_get_scheme (text);

        if (scheme)
            have_scheme = TRUE;
        g_free (scheme);
    }
    gtk_widget_set_visible (warning_hbox, !have_scheme);
    gtk_widget_set_sensitive (ok_button, have_scheme);
    g_free (text);
}

static void
assoc_file_chooser_selection_changed_cb (GtkFileChooser *chooser, GtkWidget *ok_button)
{
    gchar *file_name = gtk_file_chooser_get_filename (chooser);
    gboolean file_true = FALSE;

    /* Test for a valid filename and not a directory */
    if (file_name && !g_file_test (file_name, G_FILE_TEST_IS_DIR))
        file_true = TRUE;

    gtk_widget_set_sensitive (ok_button, file_true);
    g_free (file_name);
}

static void
uri_type_selected_cb (GtkToggleButton *button, GtkWidget *widget)
{
    GtkWidget *top = gtk_widget_get_toplevel (widget);
    GtkWidget *parent_hbox = gtk_widget_get_parent (widget);
    GtkWidget *ok_button = g_object_get_data (G_OBJECT(widget), "okbut");
    gboolean active = gtk_toggle_button_get_active (GTK_TOGGLE_BUTTON(button));

    // set the visibility of the parent hbox for widget
    gtk_widget_set_visible (parent_hbox, active);

    // make the window resize after hiding widgets
    if (active)
    {
        if (g_strcmp0 (gtk_buildable_get_name (
                             GTK_BUILDABLE(parent_hbox)), "location_hbox") == 0)
        {
            location_ok_cb (GTK_EDITABLE(widget), ok_button);
            gtk_window_resize (GTK_WINDOW(top), 600, 10); // width, height
        }
        else
        {
            assoc_file_chooser_selection_changed_cb (GTK_FILE_CHOOSER(widget), ok_button);
            gtk_window_resize (GTK_WINDOW(top), 900, 500);
        }
    }
    gtk_widget_grab_focus (GTK_WIDGET(widget));
}

static void
setup_location_dialog (GtkBuilder *builder, GtkWidget *button_loc, const gchar *uri)
{
    GtkLabel *location_label = GTK_LABEL(gtk_builder_get_object (builder, "location_label"));
    GtkEntry *entry = GTK_ENTRY(gtk_builder_get_object (builder, "location_entry"));
    gtk_toggle_button_set_active (GTK_TOGGLE_BUTTON(button_loc), TRUE);

    // set entry settings
    gtk_entry_set_width_chars (entry, 80);
    gtk_entry_set_activates_default (entry, TRUE);
    gtk_widget_grab_focus (GTK_WIDGET(entry));

    // update label and set entry text if required
    if (uri)
    {
        gtk_label_set_text (location_label, _("Amend URL:"));
        gtk_entry_set_text (entry, uri);
    }
    else
        gtk_label_set_text (location_label, _("Enter URL like http://www.gnucash.org:"));
}

static void
setup_file_dialog (GtkFileChooserWidget *fc, const gchar *path_head, const gchar *uri, gchar *scheme)
{
    gchar *display_uri = assoc_get_unescape_uri (path_head, uri, scheme);

    if (display_uri)
    {
        GtkWidget *label, *hbox;
        GtkWidget *image = gtk_image_new_from_icon_name ("dialog-warning", GTK_ICON_SIZE_SMALL_TOOLBAR);
        gchar     *use_uri = assoc_get_use_uri (path_head, uri, scheme);
        gchar     *uri_label = g_strdup_printf ("%s '%s'", _("Existing Association is"), display_uri);

        hbox = gtk_box_new (GTK_ORIENTATION_HORIZONTAL, 6);
        label = gtk_label_new (uri_label);

        if (g_file_test (display_uri, G_FILE_TEST_EXISTS))
            gtk_box_pack_start (GTK_BOX(hbox), label, FALSE, TRUE, 0);
        else
        {
            gtk_box_pack_start (GTK_BOX(hbox), image, FALSE, FALSE, 0);
            gtk_box_pack_start (GTK_BOX(hbox), label, FALSE, TRUE, 0);
        }

        PINFO("Path head: '%s', URI: '%s', Filename: '%s'", path_head, uri, display_uri);

        gtk_file_chooser_set_extra_widget (GTK_FILE_CHOOSER(fc), hbox);
        gtk_label_set_ellipsize (GTK_LABEL(label), PANGO_ELLIPSIZE_START);

        // Set the style context for this label so it can be easily manipulated with css
        gnc_widget_style_context_add_class (GTK_WIDGET(label), "gnc-class-highlight");
        gtk_file_chooser_set_uri (GTK_FILE_CHOOSER(fc), use_uri);
        gtk_widget_show_all (hbox);

        g_free (display_uri);
        g_free (uri_label);
        g_free (use_uri);
    }
}

gchar *
gnc_assoc_get_uri_dialog (GtkWindow *parent, const gchar *title, const gchar *uri)
{
    GtkWidget *dialog, *button_loc, *button_file, *ok_button, *warning_hbox;
    GtkBuilder *builder;
    gboolean uri_is_file, have_uri = FALSE;
    GtkEntry *entry;
    GtkFileChooserWidget *fc;
    GtkWidget *head_label;
    int result;
    gchar *ret_uri = NULL;
    gchar *path_head = assoc_get_path_head ();
    gchar *scheme = NULL;

    /* Create the dialog box */
    builder = gtk_builder_new();
    gnc_builder_add_from_file (builder, "dialog-assoc.glade", "association_dialog");
    dialog = GTK_WIDGET(gtk_builder_get_object (builder, "association_dialog"));
    gtk_window_set_title (GTK_WINDOW(dialog), title);

    if (parent != NULL)
        gtk_window_set_transient_for (GTK_WINDOW(dialog), GTK_WINDOW(parent));

    // Set the name and style context for this widget so it can be easily manipulated with css
    gtk_widget_set_name (GTK_WIDGET(dialog), "gnc-id-association");
    gnc_widget_style_context_add_class (GTK_WIDGET(dialog), "gnc-class-association");

    head_label = GTK_WIDGET(gtk_builder_get_object (builder, "path_head_label"));
    ok_button = GTK_WIDGET(gtk_builder_get_object (builder, "ok_button"));

    fc = GTK_FILE_CHOOSER_WIDGET(gtk_builder_get_object (builder, "file_chooser"));
    g_object_set_data (G_OBJECT(fc), "okbut", ok_button);
    g_signal_connect (G_OBJECT(fc), "selection-changed",
                      G_CALLBACK(assoc_file_chooser_selection_changed_cb), ok_button);

    button_file = GTK_WIDGET(gtk_builder_get_object (builder, "file_assoc"));
    g_signal_connect (button_file, "toggled", G_CALLBACK(uri_type_selected_cb), fc);

    gtk_widget_show_all (GTK_WIDGET(gtk_builder_get_object (builder, "file_hbox")));

    warning_hbox = GTK_WIDGET(gtk_builder_get_object (builder, "warning_hbox"));
    entry = GTK_ENTRY(gtk_builder_get_object (builder, "location_entry"));
    g_object_set_data (G_OBJECT(entry), "whbox", warning_hbox);
    g_object_set_data (G_OBJECT(entry), "okbut", ok_button);

    g_signal_connect (entry, "changed", G_CALLBACK(location_ok_cb), ok_button);

    button_loc = GTK_WIDGET(gtk_builder_get_object (builder, "loc_assoc"));
    g_signal_connect (button_loc, "toggled", G_CALLBACK(uri_type_selected_cb), entry);

    // display path head text and test if present
    assoc_set_path_head_label (head_label);

    // Check for uri is empty or NULL
    if (uri && *uri)
    {
        scheme = gnc_uri_get_scheme (uri);
        have_uri = TRUE;

        if (!scheme || g_strcmp0 (scheme, "file") == 0) // use the correct dialog
            uri_is_file = TRUE;
        else
            uri_is_file = FALSE;
    }
    else
    {
        gtk_toggle_button_set_active (GTK_TOGGLE_BUTTON(button_loc), TRUE);
        gtk_toggle_button_set_active (GTK_TOGGLE_BUTTON(button_file), TRUE);
    }

    // make sure we start with the right dialog
    if (have_uri && !uri_is_file) // location
        setup_location_dialog (builder, button_loc, uri);

    if (have_uri && uri_is_file) // file
         setup_file_dialog (fc, path_head, uri, scheme);

    g_free (scheme);
    g_object_unref (G_OBJECT(builder));

    // run the dialog
    result = gtk_dialog_run (GTK_DIALOG(dialog));
    if (result == GTK_RESPONSE_OK) //ok button
    {
        if (gtk_toggle_button_get_active (GTK_TOGGLE_BUTTON(button_loc))) // location
        {
            const gchar *dialog_uri = gtk_entry_get_text (GTK_ENTRY(entry));

            ret_uri = g_strdup (dialog_uri);

            DEBUG("Dialog Location URI: '%s'", dialog_uri);
        }
        else // file
        {
            gchar *dialog_uri = gtk_file_chooser_get_uri (GTK_FILE_CHOOSER(fc));

            PINFO("Dialog File URI: '%s', Path head: '%s'", dialog_uri, path_head);

            // relative paths do not start with a '/'
            if (g_str_has_prefix (dialog_uri, path_head))
            {
                const gchar *part = dialog_uri + strlen (path_head);
                ret_uri = g_strdup (part);
            }
            else
                ret_uri = g_strdup (dialog_uri);

            PINFO("Dialog File URI: '%s'", ret_uri);
            g_free (dialog_uri);
        }
    }
    else if (result == GTK_RESPONSE_REJECT) // remove button
        ret_uri = g_strdup ("");
    else
        ret_uri = g_strdup (uri); // any other button

    g_free (path_head);
    gtk_widget_destroy (dialog);
    return ret_uri;
}


/***********************************************************************/


static void close_handler (gpointer user_data);

static void
gnc_assoc_dialog_window_destroy_cb (GtkWidget *object, gpointer user_data)
{
    AssocDialog *assoc_dialog = user_data;

    ENTER(" ");
    gnc_unregister_gui_component (assoc_dialog->component_id);

    if (assoc_dialog->window)
    {
        g_free (assoc_dialog->path_head);
        gtk_widget_destroy (assoc_dialog->window);
        assoc_dialog->window = NULL;
    }
    g_free (assoc_dialog);
    LEAVE(" ");
}

static gboolean
gnc_assoc_dialog_window_key_press_cb (GtkWidget *widget, GdkEventKey *event, gpointer user_data)
{
    AssocDialog *assoc_dialog = user_data;

    if (event->keyval == GDK_KEY_Escape)
    {
        close_handler (assoc_dialog);
        return TRUE;
    }
    else
        return FALSE;
}

static void
assoc_dialog_update (AssocDialog *assoc_dialog)
{
    GtkTreeModel     *model;
    GtkTreeIter       iter;
    gboolean          valid;

    /* disconnect the model from the treeview */
    model = gtk_tree_view_get_model (GTK_TREE_VIEW(assoc_dialog->view));
    g_object_ref (G_OBJECT(model));
    gtk_tree_view_set_model (GTK_TREE_VIEW(assoc_dialog->view), NULL);

    /* Get first row in list store */
    valid = gtk_tree_model_get_iter_first (model, &iter);

    while (valid)
    {
        gchar *uri;
        gchar *scheme;

        gtk_tree_model_get (model, &iter, URI, &uri, -1);

        scheme = gnc_uri_get_scheme (uri);

        if (!scheme || gnc_uri_is_file_scheme (scheme))
        {
            gchar *filename = assoc_get_unescape_uri (assoc_dialog->path_head, uri, scheme);

            if (g_file_test (filename, G_FILE_TEST_EXISTS))
                gtk_list_store_set (GTK_LIST_STORE(model), &iter, AVAILABLE, _("File Found"), -1);
            else
                gtk_list_store_set (GTK_LIST_STORE(model), &iter, AVAILABLE, _("File Not Found"), -1);

            g_free (filename);
        }
        else
        {
            gchar           *escaped = g_uri_escape_string (uri, ":/.", TRUE);
            GNetworkMonitor      *nm = g_network_monitor_get_default ();
            GSocketConnectable *conn = g_network_address_parse_uri (escaped, 80, NULL);

            if (conn)
            {
                if (g_network_monitor_can_reach (nm, conn, NULL, NULL))
                    gtk_list_store_set (GTK_LIST_STORE(model), &iter, AVAILABLE, _("Address Found"), -1);
                else
                    gtk_list_store_set (GTK_LIST_STORE(model), &iter, AVAILABLE, _("Address Not Found"), -1);
            }
            g_free (escaped);
        }
        g_free (uri);
        g_free (scheme);

        valid = gtk_tree_model_iter_next (model, &iter);
    }
    /* reconnect the model to the treeview */
    gtk_tree_view_set_model (GTK_TREE_VIEW(assoc_dialog->view), model);
    g_object_unref (G_OBJECT(model));
}

static void
gnc_assoc_dialog_check_button_cb (GtkWidget * widget, gpointer user_data)
{
    AssocDialog   *assoc_dialog = user_data;
    assoc_dialog_update (assoc_dialog);
}

static void
gnc_assoc_dialog_close_button_cb (GtkWidget * widget, gpointer user_data)
{
    AssocDialog   *assoc_dialog = user_data;
    gnc_close_gui_component (assoc_dialog->component_id);
}

static void
row_selected_cb (GtkTreeView *view, GtkTreePath *path,
                  GtkTreeViewColumn  *col, gpointer user_data)
{
    AssocDialog   *assoc_dialog = user_data;
    GtkTreeIter    iter;
    Split         *split;
    gchar         *uri = NULL;

    // path describes a non-existing row - should not happen
    g_return_if_fail (gtk_tree_model_get_iter (assoc_dialog->model, &iter, path));

    gtk_tree_model_get (assoc_dialog->model, &iter, URI, &uri, URI_SPLIT, &split, -1);

    // Open associated link, subtract 1 to allow for date_int64
    if (gtk_tree_view_get_column (GTK_TREE_VIEW(assoc_dialog->view), DISPLAY_URI - 1) == col)
        gnc_assoc_open_uri (GTK_WINDOW(assoc_dialog->window), uri);

    g_free (uri);

    // Open transaction, subtract 1 to allow for date_int64
    if (gtk_tree_view_get_column (GTK_TREE_VIEW(assoc_dialog->view), DESC_TRANS - 1) == col)
    {
        GncPluginPage *page;
        GNCSplitReg   *gsr;
        Account       *account;

        if (!split)
            return;

        account = xaccSplitGetAccount (split);

        page = gnc_plugin_page_register_new (account, FALSE);
        gnc_main_window_open_page (NULL, page);
        gsr = gnc_plugin_page_register_get_gsr (page);
        gnc_split_reg_raise (gsr);

        gnc_split_reg_jump_to_split (gsr, split);
    }
}

gchar*
gnc_assoc_convert_trans_associate_uri (gpointer trans, gboolean book_ro)
{
    const gchar *uri = xaccTransGetAssociation (trans); // get the existing uri
    const gchar *part = NULL;

    if (!uri)
        return NULL;

    if (g_str_has_prefix (uri, "file:") && !g_str_has_prefix (uri,"file://"))
    {
        /* fix an earlier error when storing relative paths before version 3.5
         * they were stored starting as 'file:' or 'file:/' depending on OS
         * relative paths are stored without a leading "/" and in native form
         */
        if (g_str_has_prefix (uri,"file:/"))
            part = uri + strlen ("file:/");
        else if (g_str_has_prefix (uri,"file:"))
            part = uri + strlen ("file:");

        if (!xaccTransGetReadOnly (trans) && !book_ro)
            xaccTransSetAssociation (trans, part);

        return g_strdup (part);
    }
    return g_strdup (uri);
}

static void
add_trans_info_to_model (QofInstance* data, gpointer user_data)
{
    AssocDialog *assoc_dialog = user_data;
    Transaction *trans = GNC_TRANSACTION(data);
    gchar       *uri;
    GtkTreeIter  iter;

    // fix an earlier error when storing relative paths before version 3.5
    uri = gnc_assoc_convert_trans_associate_uri (trans, assoc_dialog->book_ro);

    if (uri && *uri)
    {
        Split *split = xaccTransGetSplit (trans, 0);
        gchar *scheme = gnc_uri_get_scheme (uri);
        gchar *display_uri;
        gboolean rel = FALSE;
        time64 t = xaccTransRetDatePosted (trans);
        char datebuff[MAX_DATE_LENGTH + 1];
        memset (datebuff, 0, sizeof(datebuff));
        if (t == 0)
            t = gnc_time (NULL);
        qof_print_date_buff (datebuff, MAX_DATE_LENGTH, t);
        gtk_list_store_append (GTK_LIST_STORE(assoc_dialog->model), &iter);

        if (!scheme) // path is relative
            rel = TRUE;

        display_uri = assoc_get_unescape_uri (assoc_dialog->path_head, uri, scheme);

        gtk_list_store_set (GTK_LIST_STORE(assoc_dialog->model), &iter,
                            DATE_TRANS, datebuff,
                            DATE_INT64, t, // used just for sorting date column
                            DESC_TRANS, xaccTransGetDescription (trans),
                            DISPLAY_URI, display_uri, AVAILABLE, _("Unknown"),
                            URI_SPLIT, split, URI, uri,
                            URI_RELATIVE, rel, // used just for sorting relative column
                            URI_RELATIVE_PIX, (rel == TRUE ? "emblem-default" : NULL), -1);
        g_free (display_uri);
        g_free (scheme);
        g_free (uri);
    }
}

static void
get_trans_info (AssocDialog *assoc_dialog)
{
    QofBook *book = gnc_get_current_book();

    assoc_dialog->book_ro = qof_book_is_readonly (book);

    /* disconnect the model from the treeview */
    assoc_dialog->model = gtk_tree_view_get_model (GTK_TREE_VIEW(assoc_dialog->view));
    g_object_ref (G_OBJECT(assoc_dialog->model));
    gtk_tree_view_set_model (GTK_TREE_VIEW(assoc_dialog->view), NULL);

    /* Clear the list store */
    gtk_list_store_clear (GTK_LIST_STORE(assoc_dialog->model));

    /* Loop through the transactions */
    qof_collection_foreach (qof_book_get_collection (book, GNC_ID_TRANS),
                            add_trans_info_to_model, assoc_dialog);

    /* reconnect the model to the treeview */
    gtk_tree_view_set_model (GTK_TREE_VIEW(assoc_dialog->view), assoc_dialog->model);
    g_object_unref (G_OBJECT(assoc_dialog->model));
}

static void
gnc_assoc_dialog_create (GtkWindow *parent, AssocDialog *assoc_dialog)
{
    GtkWidget         *window;
    GtkBuilder        *builder;
    GtkTreeSelection  *selection;
    GtkTreeViewColumn *tree_column;
    GtkWidget         *button;

    ENTER(" ");
    builder = gtk_builder_new();
    gnc_builder_add_from_file (builder, "dialog-assoc.glade", "list-store");
    gnc_builder_add_from_file (builder, "dialog-assoc.glade", "association_window");

    window = GTK_WIDGET(gtk_builder_get_object (builder, "association_window"));
    assoc_dialog->window = window;
    assoc_dialog->session = gnc_get_current_session();

    button = GTK_WIDGET(gtk_builder_get_object (builder, "check_button"));
        g_signal_connect(button, "clicked", G_CALLBACK(gnc_assoc_dialog_check_button_cb), assoc_dialog);
    button = GTK_WIDGET(gtk_builder_get_object (builder, "close_button"));
        g_signal_connect(button, "clicked", G_CALLBACK(gnc_assoc_dialog_close_button_cb), assoc_dialog);

    gtk_window_set_title (GTK_WINDOW(assoc_dialog->window), _("Transaction Associations"));

    // Set the widget name and style context for this dialog so it can be easily manipulated with css
    gtk_widget_set_name (GTK_WIDGET(window), "gnc-id-transaction-associations");
    gnc_widget_style_context_add_class (GTK_WIDGET(window), "gnc-class-association");

    assoc_dialog->view = GTK_WIDGET(gtk_builder_get_object (builder, "treeview"));
    assoc_dialog->path_head_label = GTK_WIDGET(gtk_builder_get_object (builder, "path-head"));

    assoc_dialog->path_head = assoc_get_path_head ();

    // display path head text and test if present
    assoc_set_path_head_label (assoc_dialog->path_head_label);

    // set the Associate column to be the one that expands
    tree_column = GTK_TREE_VIEW_COLUMN(gtk_builder_get_object (builder, "assoc"));
    gtk_tree_view_column_set_expand (tree_column, TRUE);

    g_signal_connect (assoc_dialog->view, "row-activated",
                      G_CALLBACK(row_selected_cb), (gpointer)assoc_dialog);

    /* default sort order */
    gtk_tree_sortable_set_sort_column_id (GTK_TREE_SORTABLE(gtk_tree_view_get_model(
                                          GTK_TREE_VIEW(assoc_dialog->view))),
                                          DATE_INT64, GTK_SORT_ASCENDING);

    // Set grid lines option to preference
    gtk_tree_view_set_grid_lines (GTK_TREE_VIEW(assoc_dialog->view), gnc_tree_view_get_grid_lines_pref ());

    selection = gtk_tree_view_get_selection (GTK_TREE_VIEW(assoc_dialog->view));
    gtk_tree_selection_set_mode (selection, GTK_SELECTION_SINGLE);

    g_signal_connect (assoc_dialog->window, "destroy",
                      G_CALLBACK(gnc_assoc_dialog_window_destroy_cb), assoc_dialog);

    g_signal_connect (assoc_dialog->window, "key_press_event",
                      G_CALLBACK(gnc_assoc_dialog_window_key_press_cb), assoc_dialog);

    gtk_builder_connect_signals_full (builder, gnc_builder_connect_full_func, assoc_dialog);

    g_object_unref (G_OBJECT(builder));

    gnc_restore_window_size (GNC_PREFS_GROUP, GTK_WINDOW(assoc_dialog->window), parent);
    get_trans_info (assoc_dialog);
    gtk_widget_show_all (GTK_WIDGET(window));

    gtk_tree_view_columns_autosize (GTK_TREE_VIEW(assoc_dialog->view));
    LEAVE(" ");
}

static void
close_handler (gpointer user_data)
{
    AssocDialog *assoc_dialog = user_data;

    ENTER(" ");
    gnc_save_window_size (GNC_PREFS_GROUP, GTK_WINDOW(assoc_dialog->window));
    gtk_widget_destroy (GTK_WIDGET(assoc_dialog->window));
    LEAVE(" ");
}

static void
refresh_handler (GHashTable *changes, gpointer user_data)
{
    ENTER(" ");
    LEAVE(" ");
}

static gboolean
show_handler (const char *klass, gint component_id,
              gpointer user_data, gpointer iter_data)
{
    AssocDialog *assoc_dialog = user_data;

    ENTER(" ");
    if (!assoc_dialog)
    {
        LEAVE("No data structure");
        return(FALSE);
    }
    gtk_window_present (GTK_WINDOW(assoc_dialog->window));
    LEAVE(" ");
    return(TRUE);
}

/********************************************************************\
 * gnc_assoc_trans_dialog                                           *
 * opens a window showing the Associations of all Transactions      *
 *                                                                  *
 * Args:   parent  - the parent of the window to be created         *
 * Return: nothing                                                  *
\********************************************************************/
void
gnc_assoc_trans_dialog (GtkWindow *parent)
{
    AssocDialog *assoc_dialog;

    ENTER(" ");
    if (gnc_forall_gui_components (DIALOG_ASSOC_CM_CLASS, show_handler, NULL))
    {
        LEAVE("Existing dialog raised");
        return;
    }
    assoc_dialog = g_new0 (AssocDialog, 1);

    gnc_assoc_dialog_create (parent, assoc_dialog);

    assoc_dialog->component_id = gnc_register_gui_component (DIALOG_ASSOC_CM_CLASS,
                                                             refresh_handler, close_handler,
                                                             assoc_dialog);

    gnc_gui_component_set_session (assoc_dialog->component_id,
                                   assoc_dialog->session);

    LEAVE(" ");
}
