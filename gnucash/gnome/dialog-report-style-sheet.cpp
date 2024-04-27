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
    GList *results = NULL, *iter;

    gnc_reports_foreach (dirty_same_stylesheet, ssi->stylesheet);

    results = gnc_option_db_commit (ssi->odb);
    for (iter = results; iter; iter = iter->next)
    {
        GtkWidget *dialog = gtk_message_dialog_new(nullptr,
                                                   GTK_DIALOG_MODAL,
                                                   GTK_MESSAGE_ERROR,
                                                   GTK_BUTTONS_OK,
                                                   "%s",
                                                   (char*)iter->data);
        gtk_dialog_run(GTK_DIALOG(dialog));
        gtk_widget_destroy(dialog);
        g_free (iter->data);
    }
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
    gnc_option_db_destroy (ssi->odb);
    scm_gc_unprotect_object (ssi->stylesheet);
    g_free (ssi);
}

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

static SCM
gnc_style_sheet_new (StyleSheetDialog * ssd)
{
    SCM            make_ss   = scm_c_eval_string ("gnc:make-html-style-sheet");
    SCM            templates = scm_c_eval_string ("(gnc:get-html-templates)");
    SCM            t_name    = scm_c_eval_string ("gnc:html-style-sheet-template-name");
    SCM            new_ss    = SCM_BOOL_F;
    GtkWidget    * template_combo;
    GtkTreeModel * template_model;
    GtkTreeIter    iter;
    GtkWidget    * name_entry;
    gint           dialog_retval;
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
    dialog_retval = gtk_dialog_run (GTK_DIALOG(dlg));

    if (dialog_retval == GTK_RESPONSE_OK)
    {
        gint choice = gtk_combo_box_get_active (GTK_COMBO_BOX(template_combo));
        auto template_str{static_cast<const char *>(g_list_nth_data (template_names, choice))};
        const char *name_str     = gtk_entry_get_text(GTK_ENTRY(name_entry));
        if (name_str && strlen(name_str) == 0)
        {
            /* If the name is empty, we display an error dialog but
             * refuse to create the new style sheet. */
            gnc_error_dialog (GTK_WINDOW(ssd->toplevel), "%s", _("You must provide a name for the new style sheet."));
            name_str = NULL;
        }
        if (template_str && name_str)
        {
            new_ss = scm_call_2 (make_ss,
                                 scm_from_utf8_string (template_str),
                                 scm_from_utf8_string (name_str));
        }
    }

    g_list_free_full (template_names, g_free);

    g_object_unref (G_OBJECT(builder));

    gtk_widget_destroy (dlg);
    return (new_ss);
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

    /* add the column name */
    scm_gc_protect_object (sheet_info);
    gtk_list_store_append (ss->list_store, &iter);
    gtk_list_store_set (ss->list_store, &iter,
                        /* Translate the displayed name */
                        COLUMN_NAME, _(c_name),
                        COLUMN_STYLESHEET, sheet_info,
                        -1);
    g_free (c_name);
    /* The translation of the name fortunately doesn't affect the
     * lookup because that is done through the sheet_info argument. */

    if (select)
    {
        GtkTreeSelection * selection = gtk_tree_view_get_selection (ss->list_view);
        gtk_tree_selection_select_iter (selection, &iter);
    }
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
    SCM                 sheet_info;

    sheet_info = gnc_style_sheet_new (ss);
    if (sheet_info == SCM_BOOL_F)
        return;

    gnc_style_sheet_select_dialog_add_one (ss, sheet_info, TRUE);

    // now start the edit dialog
    gnc_style_sheet_select_dialog_edit_cb (NULL, ss);
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
