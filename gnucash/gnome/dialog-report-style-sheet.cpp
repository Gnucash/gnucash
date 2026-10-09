/********************************************************************
 * dialog-report-style-sheet.c -- window for configuring HTML style *
 *                                sheets in GnuCash                 *
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
 ********************************************************************/

#include <gtk/gtk.h>
#include <glib/gi18n.h>
#include <dialog-options.hpp>
#include <gnc-optiondb.h>
#include <libguile.h>

#include <config.h>

#include "dialog-report-style-sheet.h"
#include "dialog-utils.h"
#include "gnc-component-manager.h"
#include "gnc-session.h"
#include "gnc-gtk-utils.h"
#include "gnc-gnome-utils.h"
#include "gnc-guile-utils.h"
#include "gnc-ui.h"
#include <guile-mappings.h>
#include "gnc-report.h"

#define DIALOG_STYLE_SHEETS_CM_CLASS "style-sheets-dialog"
#define GNC_PREFS_GROUP              "dialogs.style-sheet"

StyleSheetDialog * gnc_style_sheet_dialog = NULL;

struct _stylesheetdialog
{
    GtkWidget     * toplevel;
    GtkTreeView   * list_view;
    GtkListStore  * list_store;
    GtkWidget     * options_frame;
    gint            component_id;
    QofSession    * session;
};

typedef struct ss_info
{
    GncOptionsDialog  * odialog;
    GncOptionDB   * odb;
    SCM           stylesheet;
    GtkTreeRowReference *row_ref;
} ss_info;

enum
{
    COLUMN_NAME,
    COLUMN_STYLESHEET,
    COLUMN_DIALOG,
    N_COLUMNS
};
extern "C" // So that gtk_builder_connect_full can find them.
{
void gnc_style_sheet_select_dialog_new_cb (GtkWidget *widget, gpointer user_data);
void gnc_style_sheet_select_dialog_edit_cb (GtkWidget *widget, gpointer user_data);
void gnc_style_sheet_select_dialog_delete_cb (GtkWidget *widget, gpointer user_data);
void gnc_style_sheet_select_dialog_close_cb (GtkWidget *widget, gpointer user_data);
void gnc_style_sheet_select_dialog_destroy_cb (GtkWidget *widget, gpointer user_data);
}
/************************************************************
 *     Style Sheet Edit Dialog (I.E. an options dialog)     *
 ************************************************************/

static void
dirty_same_stylesheet (gpointer key, gpointer val, gpointer data)
{
    auto dirty_ss{static_cast<SCM>(data)};
    auto report{static_cast<SCM>(val)};
    SCM func, rep_ss;

    func = scm_c_eval_string ("gnc:report-stylesheet");
    if (scm_is_procedure (func))
        rep_ss = scm_call_1 (func, report);
    else
        return;

    if (scm_is_true (scm_eq_p (rep_ss, dirty_ss)))
    {
        func = scm_c_eval_string ("gnc:report-set-dirty?!");
        /* This makes _me_ feel dirty! */
        if (scm_is_procedure (func))
            scm_call_2 (func, report, SCM_BOOL_T);
    }
}

static void
gnc_style_sheet_options_apply_cb (GncOptionsDialog * propertybox,
                                  gpointer user_data)
{
    ss_info * ssi = (ss_info *)user_data;
    GList *results = NULL;

    gnc_reports_foreach (dirty_same_stylesheet, ssi->stylesheet);

    results = gnc_option_db_commit (ssi->odb);
    gnc_error_dialog_async_list (GTK_WINDOW (propertybox->get_widget()), results);
    g_list_free (results);
}

static void
gnc_style_sheet_options_close_cb (GncOptionsDialog *opt_dialog,
                                  gpointer user_data)
{
    auto ssi{static_cast<ss_info*>(user_data)};

    if (gnc_style_sheet_dialog && gtk_tree_row_reference_valid (ssi->row_ref))
    {
        auto ss = gnc_style_sheet_dialog;
        auto path = gtk_tree_row_reference_get_path (ssi->row_ref);
        GtkTreeIter iter;
        if (gtk_tree_model_get_iter (GTK_TREE_MODEL(ss->list_store), &iter, path))
            gtk_list_store_set (ss->list_store, &iter,
                                COLUMN_DIALOG, NULL,
                                -1);
        gtk_tree_path_free (path);
    }
    gtk_tree_row_reference_free (ssi->row_ref);
    delete ssi->odialog;
    /* The Scheme stylesheet owns this option database. Destroying the
     * editor releases its UI items; the database remains with the sheet. */
    scm_gc_unprotect_object (ssi->stylesheet);
    g_free (ssi);
}

static void gnc_style_sheet_select_dialog_add_one (StyleSheetDialog *ss,
                                                    SCM sheet_info,
                                                    gboolean select);

void gnc_style_sheet_select_dialog_edit_cb (GtkWidget *widget,
                                             gpointer user_data);

static ss_info *
gnc_style_sheet_dialog_create (StyleSheetDialog * ss,
                               gchar *name,
                               SCM sheet_info,
                               GtkTreeRowReference *row_ref)
{
    SCM get_options = scm_c_eval_string ("gnc:html-style-sheet-options");

    SCM            scm_dispatch = scm_call_1 (get_options, sheet_info);
    ss_info        * ssinfo = g_new0 (ss_info, 1);
    gchar          * title;
    GtkWindow      * parent = GTK_WINDOW(gtk_widget_get_toplevel (GTK_WIDGET(ss->list_view)));

    title = g_strdup_printf(_("HTML Style Sheet Properties: %s"), name);
    ssinfo->odialog = new GncOptionsDialog(title, parent);
    ssinfo->odb     = gnc_get_optiondb_from_dispatcher(scm_dispatch);
    ssinfo->stylesheet = sheet_info;
    ssinfo->row_ref    = row_ref;
    g_free (title);

    scm_gc_protect_object (ssinfo->stylesheet);
    g_object_ref (ssinfo->odialog->get_widget());

    ssinfo->odialog->build_contents(ssinfo->odb);

    ssinfo->odialog->set_apply_cb(gnc_style_sheet_options_apply_cb, ssinfo);
    ssinfo->odialog->set_close_cb(gnc_style_sheet_options_close_cb, ssinfo);
    ssinfo->odialog->set_style_sheet_help_cb();
    auto window = ssinfo->odialog->get_widget();
    gtk_window_set_transient_for (GTK_WINDOW(window),
                                  GTK_WINDOW(gnc_style_sheet_dialog->toplevel));
    gtk_window_set_destroy_with_parent (GTK_WINDOW(window), TRUE);
    gtk_window_present (GTK_WINDOW(window));
    return (ssinfo);
}

typedef struct
{
    GWeakRef owner;
    GWeakRef dialog;
    QofSession *session;
    gchar *template_name;
    gchar *style_sheet_name;
    gulong response_handler;
    bool response_captured;
} NewStyleSheetRequest;

static bool
gnc_style_sheet_session_matches (QofSession *session)
{
    if (!session)
        return !gnc_current_session_exist ();
    return gnc_current_session_exist () &&
        gnc_get_current_session () == session;
}

static void
gnc_style_sheet_new_response_cb (GtkDialog *dialog, gint response,
                                 NewStyleSheetRequest *request)
{
    if (request->response_captured)
        return;
    request->response_captured = true;
    if (request->response_handler &&
        g_signal_handler_is_connected (dialog, request->response_handler))
        g_signal_handler_disconnect (dialog, request->response_handler);
    request->response_handler = 0;
    if (response != GTK_RESPONSE_OK)
        return;

    auto combo = GTK_COMBO_BOX (g_object_get_data (G_OBJECT (dialog),
                                                    "template-combo"));
    auto entry = GTK_ENTRY (g_object_get_data (G_OBJECT (dialog),
                                                "name-entry"));
    if (!combo || !entry)
        return;

    auto names = static_cast<GList *>(g_object_get_data (G_OBJECT (dialog),
                                                          "template-names"));
    auto choice = gtk_combo_box_get_active (combo);
    request->template_name = g_strdup (static_cast<const gchar *>(
        g_list_nth_data (names, choice)));
    request->style_sheet_name = g_strdup (gtk_entry_get_text (entry));
}

static void
free_template_names (gpointer data)
{
    g_list_free_full (static_cast<GList *>(data), g_free);
}

static void
gnc_style_sheet_new_complete_cb (GtkWindow *parent, gint response,
                                 gpointer user_data)
{
    auto request = static_cast<NewStyleSheetRequest *>(user_data);
    auto owner = GTK_WIDGET (g_weak_ref_get (&request->owner));
    auto valid_owner = owner && parent && GTK_WIDGET (parent) == owner &&
        gnc_style_sheet_dialog && gnc_style_sheet_dialog->toplevel == owner &&
        gnc_style_sheet_dialog->session == request->session &&
        gnc_style_sheet_session_matches (request->session);

    if (valid_owner && response == GTK_RESPONSE_OK && request->template_name &&
        request->style_sheet_name)
    {
        if (!*request->style_sheet_name)
            gnc_error_dialog_async (GTK_WINDOW (owner), "%s",
                _("You must provide a name for the new style sheet."));
        else
        {
            auto make_ss = scm_c_eval_string ("gnc:make-html-style-sheet");
            auto sheet_info = scm_call_2 (make_ss,
                scm_from_utf8_string (request->template_name),
                scm_from_utf8_string (request->style_sheet_name));
            if (!scm_is_false (sheet_info))
            {
                auto still_valid = [&]() {
                    return gnc_style_sheet_dialog &&
                        gnc_style_sheet_dialog->toplevel == owner &&
                        gnc_style_sheet_dialog->session == request->session &&
                        gnc_style_sheet_session_matches (request->session);
                };
                if (still_valid ())
                {
                    gnc_style_sheet_select_dialog_add_one (
                        gnc_style_sheet_dialog, sheet_info, TRUE);
                    if (still_valid ())
                        gnc_style_sheet_select_dialog_edit_cb (NULL,
                                                               gnc_style_sheet_dialog);
                }
            }
        }
    }

    if (owner)
        g_object_unref (owner);
    auto dialog = g_weak_ref_get (&request->dialog);
    if (dialog)
    {
        if (request->response_handler &&
            g_signal_handler_is_connected (dialog, request->response_handler))
            g_signal_handler_disconnect (dialog, request->response_handler);
        g_object_unref (dialog);
    }
    g_weak_ref_clear (&request->owner);
    g_weak_ref_clear (&request->dialog);
    g_free (request->template_name);
    g_free (request->style_sheet_name);
    g_free (request);
}

static void
gnc_style_sheet_new (StyleSheetDialog * ssd)
{
    SCM            templates = scm_c_eval_string ("(gnc:get-html-templates)");
    SCM            t_name    = scm_c_eval_string ("gnc:html-style-sheet-template-name");
    GtkWidget    * template_combo;
    GtkTreeModel * template_model;
    GtkTreeIter    iter;
    GtkWidget    * name_entry;
    GList        * template_names = NULL;

    /* get the new name for the style sheet */
    GtkBuilder   * builder;
    GtkWidget    * dlg;

    builder = gtk_builder_new ();
    gnc_builder_add_from_file (builder, "dialog-report.glade", "template_liststore");
    gnc_builder_add_from_file (builder, "dialog-report.glade", "new_style_sheet_dialog");

    dlg = GTK_WIDGET(gtk_builder_get_object (builder, "new_style_sheet_dialog"));
    template_combo = GTK_WIDGET(gtk_builder_get_object (builder, "template_combobox"));
    name_entry     = GTK_WIDGET(gtk_builder_get_object (builder, "name_entry"));

    // Set the name for this dialog so it can be easily manipulated with css
    gtk_widget_set_name (GTK_WIDGET(dlg), "gnc-id-style-sheet-new");
    gnc_widget_style_context_add_class (GTK_WIDGET(dlg), "gnc-class-style-sheets");

    g_assert (ssd);

    template_model = gtk_combo_box_get_model (GTK_COMBO_BOX(template_combo));

    /* put in the list of style sheet type names */
    for (; !scm_is_null (templates); templates = SCM_CDR(templates))
    {
        gchar* orig_name;

        SCM t = SCM_CAR(templates);
        orig_name = gnc_scm_call_1_to_string (t_name, t);

        /* Store the untranslated names for lookup later */
        template_names = g_list_prepend (template_names, (gpointer)orig_name);

        /* The displayed name should be translated */
        gtk_list_store_append (GTK_LIST_STORE(template_model), &iter);
        gtk_list_store_set (GTK_LIST_STORE(template_model), &iter, 0, _(orig_name), -1);

        /* Note: don't g_free orig_name here - template_names still refers to it*/
    }
    gtk_combo_box_set_active (GTK_COMBO_BOX(template_combo), 0);

    /* get the name */
    gtk_window_set_transient_for (GTK_WINDOW(dlg), GTK_WINDOW(ssd->toplevel));
    auto request = g_new0 (NewStyleSheetRequest, 1);
    g_weak_ref_init (&request->owner, G_OBJECT (ssd->toplevel));
    g_weak_ref_init (&request->dialog, G_OBJECT (dlg));
    request->session = ssd->session;
    g_object_set_data (G_OBJECT (dlg), "template-combo", template_combo);
    g_object_set_data (G_OBJECT (dlg), "name-entry", name_entry);
    g_object_set_data_full (G_OBJECT (dlg), "template-names", template_names,
                            free_template_names);
    g_object_set_data_full (G_OBJECT (dlg), "builder", builder,
                            g_object_unref);
    request->response_handler = g_signal_connect (
        dlg, "response", G_CALLBACK (gnc_style_sheet_new_response_cb), request);
    gnc_dialog_run_async (GTK_DIALOG (dlg), NULL,
                          gnc_style_sheet_new_complete_cb, request);
}

/************************************************************
 *               Style Sheet Selection Dialog               *
 ************************************************************/
static void
gnc_style_sheet_select_dialog_add_one (StyleSheetDialog * ss,
                                       SCM sheet_info,
                                       gboolean select)
{
    SCM get_name;
    gchar *c_name;
    GtkTreeIter iter;

    get_name = scm_c_eval_string ("gnc:html-style-sheet-name");
    c_name = gnc_scm_call_1_to_string (get_name, sheet_info);
    if (!c_name)
        return;

    /* Keep these objects alive across model and selection notifications: an
       observer may close the owner window while either operation emits. */
    auto model = GTK_TREE_MODEL (g_object_ref (ss->list_store));
    auto list_store = GTK_LIST_STORE (model);
    auto view = GTK_TREE_VIEW (g_object_ref (ss->list_view));
    GWeakRef owner_ref;
    g_weak_ref_init (&owner_ref, G_OBJECT (ss->toplevel));
    auto ss_identity = ss;
    auto session = ss->session;

    auto owner_is_live = [&]() {
        auto owner = GTK_WIDGET (g_weak_ref_get (&owner_ref));
        auto live = owner && !gtk_widget_in_destruction (owner) &&
            gnc_style_sheet_dialog == ss_identity &&
            gnc_style_sheet_dialog->toplevel == owner &&
            gnc_style_sheet_dialog->session == session &&
            gnc_style_sheet_session_matches (session);
        if (owner)
            g_object_unref (owner);
        return live;
    };

    /* add the column name */
    scm_gc_protect_object (sheet_info);
    gtk_list_store_append (list_store, &iter);
    if (!select || owner_is_live ())
    {
        gtk_list_store_set (list_store, &iter,
                            /* Translate the displayed name */
                            COLUMN_NAME, _(c_name),
                            COLUMN_STYLESHEET, sheet_info,
                            -1);
        g_free (c_name);
        /* The translation of the name fortunately doesn't affect the
         * lookup because that is done through the sheet_info argument. */

        if (select && owner_is_live ())
        {
            GtkTreeSelection * selection = gtk_tree_view_get_selection (view);
            gtk_tree_selection_select_iter (selection, &iter);
        }
    }
    else
    {
        scm_gc_unprotect_object (sheet_info);
        g_free (c_name);
    }
    g_weak_ref_clear (&owner_ref);
    g_object_unref (view);
    g_object_unref (model);
}

static void
gnc_style_sheet_select_dialog_fill (StyleSheetDialog * ss)
{
    SCM stylesheets = scm_c_eval_string ("(gnc:get-html-style-sheets)");
    SCM sheet_info;

    /* pack it full of content */
    for (; !scm_is_null (stylesheets); stylesheets = SCM_CDR(stylesheets))
    {
        sheet_info = SCM_CAR(stylesheets);
        gnc_style_sheet_select_dialog_add_one (ss, sheet_info, FALSE);
    }
}

static void
gnc_style_sheet_select_dialog_event_cb (GtkWidget *widget,
                                        GdkEvent *event,
                                        gpointer user_data)
{
    StyleSheetDialog  * ss = (StyleSheetDialog *)user_data;

    g_return_if_fail (event != NULL);
    g_return_if_fail (ss != NULL);

    if (event->type != GDK_2BUTTON_PRESS)
        return;

    /* Synthesize a click of the edit button */
    gnc_style_sheet_select_dialog_edit_cb (NULL, ss);
}

void
gnc_style_sheet_select_dialog_new_cb (GtkWidget *widget, gpointer user_data)
{
    StyleSheetDialog  * ss = (StyleSheetDialog *)user_data;
    gnc_style_sheet_new (ss);
}

void
gnc_style_sheet_select_dialog_edit_cb (GtkWidget *widget, gpointer user_data)
{
    StyleSheetDialog  * ss = (StyleSheetDialog *)user_data;
    GtkTreeSelection  * selection = gtk_tree_view_get_selection (ss->list_view);
    GtkTreeModel      * model;
    GtkTreeIter         iter;

    if (gtk_tree_selection_get_selected (selection, &model, &iter))
    {
        GtkTreeRowReference * row_ref;
        GtkTreePath         * path;
        ss_info             * ssinfo;
        gchar               * name;

        SCM                 sheet_info;

        gtk_tree_model_get (model, &iter,
                            COLUMN_NAME, &name,
                            COLUMN_STYLESHEET, &sheet_info,
                            -1);
        /* Fire off options dialog here */
        path = gtk_tree_model_get_path (GTK_TREE_MODEL(ss->list_store), &iter);
        row_ref = gtk_tree_row_reference_new (GTK_TREE_MODEL(ss->list_store), path);
        ssinfo = gnc_style_sheet_dialog_create (ss, name, sheet_info, row_ref);
        gtk_list_store_set (ss->list_store, &iter,
                            COLUMN_DIALOG, ssinfo,
                            -1);
        gtk_tree_path_free (path);
        g_free (name);
    }
}

void
gnc_style_sheet_select_dialog_delete_cb (GtkWidget *widget, gpointer user_data)
{
    StyleSheetDialog  * ss = (StyleSheetDialog *)user_data;
    GtkTreeSelection  * selection = gtk_tree_view_get_selection (ss->list_view);
    GtkTreeModel      * model;
    GtkTreeIter         iter;

    if (gtk_tree_selection_get_selected (selection, &model, &iter))
    {
        ss_info           * ssinfo;

        SCM                 sheet_info;
        SCM                 remover;

        gtk_tree_model_get (model, &iter,
                            COLUMN_STYLESHEET, &sheet_info,
                            COLUMN_DIALOG, &ssinfo,
                            -1);
        gtk_list_store_remove (ss->list_store, &iter);

        if (ssinfo)
            gnc_style_sheet_options_close_cb (NULL, ssinfo);
        remover = scm_c_eval_string ("gnc:html-style-sheet-remove");
        scm_call_1 (remover, sheet_info);
        scm_gc_unprotect_object (sheet_info);
    }
}

void
gnc_style_sheet_select_dialog_close_cb (GtkWidget *widget, gpointer user_data)
{
    StyleSheetDialog  * ss = (StyleSheetDialog *)user_data;
    gnc_close_gui_component (ss->component_id);
}

static gboolean
gnc_style_sheet_select_dialog_delete_event_cb (GtkWidget *widget,
                                               GdkEvent  *event,
                                               gpointer   user_data)
{
    auto ss{static_cast<StyleSheetDialog*>(user_data)};
    gnc_save_window_size (GNC_PREFS_GROUP, GTK_WINDOW(ss->toplevel));
    return FALSE;
}

void
gnc_style_sheet_select_dialog_destroy_cb (GtkWidget *widget, gpointer user_data)
{
    StyleSheetDialog  *ss = (StyleSheetDialog *)user_data;

    if (!ss)
       return;

    gnc_unregister_gui_component (ss->component_id);

    g_object_unref (ss->list_store);
    if (ss->toplevel)
    {
        gtk_widget_destroy (ss->toplevel);
        ss->toplevel = NULL;
    }
    gnc_style_sheet_dialog = NULL;
    g_free (ss);
}

static void
gnc_style_sheet_window_close_handler (gpointer user_data)
{
    StyleSheetDialog  *ss = (StyleSheetDialog *)user_data;
    g_return_if_fail (ss);

    gnc_save_window_size (GNC_PREFS_GROUP, GTK_WINDOW(ss->toplevel));
    gtk_widget_destroy (ss->toplevel);
}

static gboolean
gnc_style_sheet_select_dialog_check_escape_cb (GtkWidget *widget,
                                               GdkEventKey *event,
                                               gpointer user_data)
{
    if (event->keyval == GDK_KEY_Escape)
    {
        StyleSheetDialog  * ss = (StyleSheetDialog *)user_data;
        gnc_close_gui_component (ss->component_id);
        return TRUE;
    }
    return FALSE;
}

static StyleSheetDialog *
gnc_style_sheet_select_dialog_create (GtkWindow *parent)
{
    StyleSheetDialog  * ss = g_new0 (StyleSheetDialog, 1);
    GtkBuilder        * builder;
    GtkCellRenderer   * renderer;
    GtkTreeSelection  * selection;

    builder = gtk_builder_new ();
    gnc_builder_add_from_file (builder, "dialog-report.glade", "select_style_sheet_window");

    ss->toplevel = GTK_WIDGET(gtk_builder_get_object (builder, "select_style_sheet_window"));

    ss->session = gnc_get_current_session ();

    // Set the name for this dialog so it can be easily manipulated with css
    gtk_widget_set_name (GTK_WIDGET(ss->toplevel), "gnc-id-style-sheet-select");
    gnc_widget_style_context_add_class (GTK_WIDGET(ss->toplevel), "gnc-class-style-sheets");

    ss->list_view  = GTK_TREE_VIEW(gtk_builder_get_object (builder, "style_sheet_list_view"));
    ss->list_store = gtk_list_store_new (N_COLUMNS, G_TYPE_STRING, G_TYPE_POINTER, G_TYPE_POINTER);
    gtk_tree_view_set_model (ss->list_view, GTK_TREE_MODEL(ss->list_store));

    renderer = gtk_cell_renderer_text_new ();
    gtk_tree_view_insert_column_with_attributes (ss->list_view, -1,
                                                 _("Style Sheet Name"), renderer,
                                                 "text", COLUMN_NAME,
                                                 NULL);

    selection = gtk_tree_view_get_selection (ss->list_view);
    gtk_tree_selection_set_mode (selection, GTK_SELECTION_BROWSE);

    g_signal_connect (ss->list_view, "event-after",
                      G_CALLBACK(gnc_style_sheet_select_dialog_event_cb), ss);

    g_signal_connect (ss->toplevel, "destroy",
                      G_CALLBACK(gnc_style_sheet_select_dialog_destroy_cb), ss);

    g_signal_connect (ss->toplevel, "delete-event",
                      G_CALLBACK(gnc_style_sheet_select_dialog_delete_event_cb), ss);

    g_signal_connect (ss->toplevel, "key-press-event",
                      G_CALLBACK(gnc_style_sheet_select_dialog_check_escape_cb), ss);

    gnc_style_sheet_select_dialog_fill (ss);

    gtk_builder_connect_signals_full (builder, gnc_builder_connect_full_func, ss);
    g_object_unref (G_OBJECT(builder));
    return ss;
}

void
gnc_style_sheet_dialog_open (GtkWindow *parent)
{
    if (gnc_style_sheet_dialog)
        gtk_window_present (GTK_WINDOW(gnc_style_sheet_dialog->toplevel));
    else
    {
        gnc_style_sheet_dialog =
            gnc_style_sheet_select_dialog_create (parent);

        /* register with component manager */
        gnc_style_sheet_dialog->component_id =
            gnc_register_gui_component (DIALOG_STYLE_SHEETS_CM_CLASS,
                                        NULL, //no refresh handler
                                        gnc_style_sheet_window_close_handler,
                                        gnc_style_sheet_dialog);

        gnc_gui_component_set_session (gnc_style_sheet_dialog->component_id,
                                       gnc_style_sheet_dialog->session);

        gnc_restore_window_size (GNC_PREFS_GROUP,
                                 GTK_WINDOW(gnc_style_sheet_dialog->toplevel),
                                 GTK_WINDOW(parent));
        gtk_widget_show_all (gnc_style_sheet_dialog->toplevel);
    }
}
