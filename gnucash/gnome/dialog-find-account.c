/********************************************************************\
 * dialog-find-account.c -- Find Account dialog                     *
 * Copyright (C) 2016 Robert Fewell                                 *
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

#include "dialog-find-account.h"

#include "dialog-utils.h"
#include "gnc-component-manager.h"
#include "gnc-prefs.h"
#include "gnc-session.h"

#include "gnc-ui-util.h"
#include "Account.h"
#include "gnc-plugin-page-account-tree.h"
#include "dialog-account.h"

#define DIALOG_FIND_ACCOUNT_CM_CLASS    "dialog-find-account"
#define GNC_PREFS_GROUP                 "dialogs.find-account"

typedef struct
{
    GtkWidget    *window;
    GtkWidget    *parent;
    QofSession   *session;
    Account      *account;
    GtkColumnView *view;
    GListStore    *rows;
    GtkSingleSelection *selection;

    GtkWidget    *radio_frame;
    GtkWidget    *radio_root;
    GtkWidget    *radio_subroot;

    GtkWidget    *filter_button;
    GtkWidget    *filter_text_entry;
    GtkWidget    *sub_label;

    gboolean      jump_close;
    gchar        *saved_filter_text;
    gint          event_handler_id;

}FindAccountDialog;

/* The legacy model kept display values separate from the account. Keep the
 * rendered state with a plain GObject so GtkColumnView exposes one stable row
 * contract to selection, activation, and item factories. */
typedef struct
{
    Account *account;
    gchar *full_name;
    gboolean placeholder;
    gboolean hidden;
    gboolean unused;
    gboolean zero_balance;
    gboolean tax_related;
} FindAccountRow;

static GQuark find_account_row_quark = 0;

static void
find_account_row_free (gpointer data)
{
    FindAccountRow *row = data;

    if (!row)
        return;
    g_free (row->full_name);
    g_free (row);
}

static GObject *
find_account_row_new (Account *account)
{
    GObject *object;
    FindAccountRow *row;
    gnc_numeric total;

    if (G_UNLIKELY (!find_account_row_quark))
        find_account_row_quark = g_quark_from_static_string ("gnc-find-account-row");

    object = G_OBJECT (g_object_new (G_TYPE_OBJECT, NULL));
    row = g_new0 (FindAccountRow, 1);
    row->account = account;
    row->full_name = gnc_account_get_full_name (account);
    total = xaccAccountGetBalanceInCurrency (account, NULL, TRUE);
    row->placeholder = xaccAccountGetPlaceholder (account);
    row->hidden = xaccAccountGetHidden (account);
    row->unused = gnc_account_and_descendants_empty (account);
    row->zero_balance = gnc_numeric_zero_p (total);
    row->tax_related = xaccAccountGetTaxRelated (account);
    g_object_set_qdata_full (object, find_account_row_quark, row,
                             find_account_row_free);
    return object;
}

static FindAccountRow *
find_account_row_get (gpointer object)
{
    return object ? g_object_get_qdata (G_OBJECT (object), find_account_row_quark) : NULL;
}

/* This static indicates the debugging module that this .o belongs to.  */
static QofLogModule log_module = GNC_MOD_GUI;

static void close_handler (gpointer user_data);

static gboolean
gnc_find_account_dialog_window_close_request_cb (GtkWindow *window,
                                                 gpointer user_data)
{
    FindAccountDialog *facc_dialog = user_data;

    // this cb allows the window size to be saved on closing with the X
    gnc_save_window_size (GNC_PREFS_GROUP,
                          GTK_WINDOW(facc_dialog->window));
    (void)window;
    return FALSE;
}

static void
gnc_find_account_dialog_window_destroy_cb (GtkWidget *object, gpointer user_data)
{
    FindAccountDialog *facc_dialog = user_data;

    ENTER(" ");
    gnc_unregister_gui_component_by_data (DIALOG_FIND_ACCOUNT_CM_CLASS, facc_dialog);

    if (facc_dialog->event_handler_id)
    {
        qof_event_unregister_handler (facc_dialog->event_handler_id);
        facc_dialog->event_handler_id = 0;
    }

    g_clear_pointer (&facc_dialog->saved_filter_text, g_free);
    g_clear_object (&facc_dialog->selection);
    g_clear_object (&facc_dialog->rows);
    facc_dialog->window = NULL;
    g_free (facc_dialog);
    (void)object;
    LEAVE(" ");
}

static gboolean
gnc_find_account_dialog_window_key_press_cb (GtkEventControllerKey *key, guint keyval,
                                             guint keycode, GdkModifierType state,
                                             gpointer user_data)
{
    FindAccountDialog *facc_dialog = user_data;

    if (keyval == GDK_KEY_Escape)
    {
        close_handler (facc_dialog);
        return TRUE;
    }
    else
        return FALSE;
}

static void
jump_to_account (FindAccountDialog *facc_dialog, Account *jump_account)
{
    if (jump_account != NULL)
        gnc_plugin_page_account_tree_open (jump_account, GTK_WINDOW(facc_dialog->parent));

    if (facc_dialog->jump_close == TRUE)
        gnc_close_gui_component_by_data (DIALOG_FIND_ACCOUNT_CM_CLASS, facc_dialog);
}

static void
gnc_find_account_dialog_jump_set (FindAccountDialog *facc_dialog)
{
    if (facc_dialog->jump_close == TRUE)
        facc_dialog->jump_close = FALSE;
    else
        facc_dialog->jump_close = TRUE;
}

static void
gnc_find_account_dialog_jump_to (FindAccountDialog *facc_dialog)
{
    GObject *object;
    FindAccountRow *row;

    object = gtk_single_selection_get_selected_item (facc_dialog->selection);
    row = find_account_row_get (object);
    jump_to_account (facc_dialog, row ? row->account : NULL);
}

static void
row_activated (GtkColumnView *view, guint position, FindAccountDialog *facc_dialog)
{
    GObject *object = g_list_model_get_item (G_LIST_MODEL (facc_dialog->rows), position);
    FindAccountRow *row = find_account_row_get (object);

    jump_to_account (facc_dialog, row ? row->account : NULL);
    g_clear_object (&object);
    (void)view;
}

static void
gnc_find_account_dialog_jump_button_cb (GtkWidget * widget, gpointer user_data)
{
    FindAccountDialog *facc_dialog = user_data;
    gnc_find_account_dialog_jump_to (facc_dialog);
}

static void
gnc_find_account_dialog_check_button_cb (GtkWidget * widget, gpointer user_data)
{
    FindAccountDialog *facc_dialog = user_data;
    gnc_find_account_dialog_jump_set (facc_dialog);
}

static void
gnc_find_account_dialog_close_button_cb (GtkWidget * widget, gpointer user_data)
{
    FindAccountDialog *facc_dialog = user_data;
    gnc_close_gui_component_by_data (DIALOG_FIND_ACCOUNT_CM_CLASS, facc_dialog);
}

static void
fill_model (FindAccountDialog *facc_dialog, Account *account)
{
    GObject *object = find_account_row_new (account);
    FindAccountRow *row = find_account_row_get (object);

    PINFO("Add to model: Account '%s'", row->full_name);
    g_list_store_append (facc_dialog->rows, object);
    g_object_unref (object);
}

static void
get_account_info (FindAccountDialog *facc_dialog, gboolean use_saved_filter)
{
    Account *root;
    GList *accts;
    GList *ptr;
    gchar *filter_text;
    gboolean radio_root;

    /* Get the state of the root radio button */
    radio_root = gtk_check_button_get_active (GTK_CHECK_BUTTON(facc_dialog->radio_root));

     /* Get list of Accounts */
    if ((facc_dialog->account == NULL) || (radio_root == TRUE))
        root = gnc_book_get_root_account (gnc_get_current_book());
    else
        root = facc_dialog->account;

    accts = gnc_account_get_descendants_sorted (root);

    if (use_saved_filter)
        filter_text = g_ascii_strdown (facc_dialog->saved_filter_text, -1);
    else
        filter_text = g_ascii_strdown (gnc_entry_get_text (GTK_ENTRY(facc_dialog->filter_text_entry)), -1);

    g_list_store_remove_all (facc_dialog->rows);
    gtk_single_selection_set_selected (facc_dialog->selection,
                                       GTK_INVALID_LIST_POSITION);

    /* Go through list of accounts */
    for (ptr = accts; ptr; ptr = g_list_next (ptr))
    {
        Account *acc = ptr->data;
        gchar   *full_name = gnc_account_get_full_name (acc);
        gchar   *match_string = g_ascii_strdown (full_name, -1);

        if ((g_strcmp0 (filter_text, "") == 0) || (g_strrstr (match_string, filter_text) != NULL))
            fill_model (facc_dialog, acc);

        g_free (match_string);
        g_free (full_name);
    }
    g_free (filter_text);
    g_list_free (accts);
}

static void
list_type_selected_cb (GtkCheckButton* button, FindAccountDialog *facc_dialog)
{
    get_account_info (facc_dialog, FALSE);
}

static void
filter_button_cb (GtkButton *button, FindAccountDialog *facc_dialog)
{
    get_account_info (facc_dialog, FALSE);

    if (facc_dialog->saved_filter_text)
        g_free (facc_dialog->saved_filter_text);

    // save the filter in case of an account event
    facc_dialog->saved_filter_text = g_strdup (gnc_entry_get_text
                                     (GTK_ENTRY(facc_dialog->filter_text_entry)));

    // Clear the filter
    gnc_entry_set_text (GTK_ENTRY(facc_dialog->filter_text_entry), "");
}

static void
filter_active_cb (GtkEntry *entry, FindAccountDialog *facc_dialog)
{
    get_account_info (facc_dialog, FALSE);

    if (facc_dialog->saved_filter_text)
        g_free (facc_dialog->saved_filter_text);

    // save the filter in case of an account event
    facc_dialog->saved_filter_text = g_strdup (gnc_entry_get_text
                                     (GTK_ENTRY(facc_dialog->filter_text_entry)));

    gtk_editable_select_region (GTK_EDITABLE(facc_dialog->filter_text_entry), 0, -1);
}

static void
gnc_find_account_event_handler (QofInstance *entity,
                                QofEventId event_type,
                                FindAccountDialog *facc_dialog,
                                gpointer evt_data)
{
    Account *account = NULL;

    g_return_if_fail (facc_dialog);    /* Required */

    if (!GNC_IS_ACCOUNT(entity))
        return;

    ENTER("entity %p of type %d, dialog %p, event_data %p",
          entity, event_type, facc_dialog, evt_data);

    account = GNC_ACCOUNT(entity);

    switch (event_type)
    {
    case QOF_EVENT_ADD:
    case QOF_EVENT_REMOVE:
    case QOF_EVENT_MODIFY:
        DEBUG("account change on %p (%s)", account, xaccAccountGetName (account));
        get_account_info (facc_dialog, TRUE);
        LEAVE(" ");
        break;

    default:
        LEAVE("unknown event type");
        return;
    }
    LEAVE(" ");
    return;
}

enum FindAccountViewColumn
{
    FIND_ACCOUNT_NAME,
    FIND_ACCOUNT_PLACEHOLDER,
    FIND_ACCOUNT_HIDDEN,
    FIND_ACCOUNT_UNUSED,
    FIND_ACCOUNT_ZERO_BALANCE,
    FIND_ACCOUNT_TAX_RELATED
};

static void
find_account_text_setup (GtkListItemFactory *factory, GtkListItem *item,
                         gpointer user_data)
{
    GtkWidget *label = gtk_label_new (NULL);

    gtk_label_set_xalign (GTK_LABEL (label), 0.0);
    gtk_label_set_ellipsize (GTK_LABEL (label), PANGO_ELLIPSIZE_END);
    gtk_list_item_set_child (item, label);
    (void)factory;
    (void)user_data;
}

static void
find_account_text_bind (GtkListItemFactory *factory, GtkListItem *item,
                        gpointer user_data)
{
    FindAccountRow *row = find_account_row_get (gtk_list_item_get_item (item));

    gtk_label_set_text (GTK_LABEL (gtk_list_item_get_child (item)),
                        row ? row->full_name : "");
    (void)factory;
    (void)user_data;
}

static gboolean
find_account_row_has_status (FindAccountRow *row, guint column)
{
    if (!row)
        return FALSE;

    switch (column)
    {
    case FIND_ACCOUNT_PLACEHOLDER: return row->placeholder;
    case FIND_ACCOUNT_HIDDEN: return row->hidden;
    case FIND_ACCOUNT_UNUSED: return row->unused;
    case FIND_ACCOUNT_ZERO_BALANCE: return row->zero_balance;
    case FIND_ACCOUNT_TAX_RELATED: return row->tax_related;
    default: return FALSE;
    }
}

static void
find_account_status_setup (GtkListItemFactory *factory, GtkListItem *item,
                           gpointer user_data)
{
    GtkWidget *image = gtk_image_new ();

    gtk_widget_set_halign (image, GTK_ALIGN_CENTER);
    gtk_list_item_set_child (item, image);
    (void)factory;
    (void)user_data;
}

static void
find_account_status_bind (GtkListItemFactory *factory, GtkListItem *item,
                          gpointer user_data)
{
    guint column = GPOINTER_TO_UINT (user_data);
    FindAccountRow *row = find_account_row_get (gtk_list_item_get_item (item));
    GtkImage *image = GTK_IMAGE (gtk_list_item_get_child (item));

    gtk_image_set_from_icon_name (image,
                                  find_account_row_has_status (row, column)
                                  ? "emblem-default" : NULL);
    (void)factory;
}

static void
find_account_add_column (FindAccountDialog *facc_dialog, const gchar *title,
                         guint column, gboolean expand)
{
    GtkListItemFactory *factory = gtk_signal_list_item_factory_new ();
    GtkColumnViewColumn *view_column;

    if (column == FIND_ACCOUNT_NAME)
    {
        g_signal_connect (factory, "setup", G_CALLBACK (find_account_text_setup), NULL);
        g_signal_connect (factory, "bind", G_CALLBACK (find_account_text_bind), NULL);
    }
    else
    {
        g_signal_connect (factory, "setup", G_CALLBACK (find_account_status_setup), NULL);
        g_signal_connect (factory, "bind", G_CALLBACK (find_account_status_bind),
                          GUINT_TO_POINTER (column));
    }

    view_column = gtk_column_view_column_new (title, factory);
    gtk_column_view_column_set_resizable (view_column, TRUE);
    gtk_column_view_column_set_expand (view_column, expand);
    gtk_column_view_append_column (facc_dialog->view, view_column);
    g_object_unref (view_column);
}

static void
gnc_find_account_dialog_create (GtkWidget *parent, FindAccountDialog *facc_dialog)
{
    GtkWidget *window;
    GtkBuilder *builder;
    GtkWidget *button;

    ENTER(" ");
    builder = gtk_builder_new();
    gnc_builder_add_from_file (builder, "dialog-find-account.glade", "find_account_window");

    window = GTK_WIDGET(gtk_builder_get_object (builder, "find_account_window"));
    facc_dialog->window = window;

    // Set the name for this dialog so it can be easily manipulated with css
    gtk_widget_set_name (GTK_WIDGET(window), "gnc-id-find-account");
    gnc_widget_style_context_add_class (GTK_WIDGET(window), "gnc-class-account");

    facc_dialog->session = gnc_get_current_session();
    facc_dialog->parent = parent;
    facc_dialog->saved_filter_text = g_strdup ("");

    gtk_window_set_title (GTK_WINDOW(facc_dialog->window), _("Find Account"));

    /* Connect the radio buttons...*/
    facc_dialog->radio_root = GTK_WIDGET(gtk_builder_get_object (builder, "radio-root"));
    facc_dialog->radio_subroot = GTK_WIDGET(gtk_builder_get_object (builder, "radio-subroot"));

    g_signal_connect (facc_dialog->radio_root, "toggled",
                      G_CALLBACK(list_type_selected_cb), (gpointer)facc_dialog);

    facc_dialog->filter_text_entry = GTK_WIDGET(gtk_builder_get_object (builder, "filter-text-entry"));
    facc_dialog->sub_label = GTK_WIDGET(gtk_builder_get_object (builder, "sub-label"));
    facc_dialog->radio_frame = GTK_WIDGET(gtk_builder_get_object (builder, "frame-radio"));
    facc_dialog->filter_button = GTK_WIDGET(gtk_builder_get_object (builder, "filter-button"));
    g_signal_connect (facc_dialog->filter_button, "clicked",
                      G_CALLBACK(filter_button_cb), (gpointer)facc_dialog);
    g_signal_connect (facc_dialog->filter_text_entry, "activate",
                      G_CALLBACK(filter_active_cb), (gpointer)facc_dialog);

    button = GTK_WIDGET(gtk_builder_get_object (builder, "jumpto_button"));
    g_signal_connect(button, "clicked", G_CALLBACK(gnc_find_account_dialog_jump_button_cb), facc_dialog);
    button = GTK_WIDGET(gtk_builder_get_object (builder, "check_button"));
    g_signal_connect(button, "clicked", G_CALLBACK(gnc_find_account_dialog_check_button_cb), facc_dialog);
    button = GTK_WIDGET(gtk_builder_get_object (builder, "close_button"));
    g_signal_connect(button, "clicked", G_CALLBACK(gnc_find_account_dialog_close_button_cb), facc_dialog);

    facc_dialog->view = GTK_COLUMN_VIEW (gtk_builder_get_object (builder, "treeview"));
    facc_dialog->rows = g_list_store_new (G_TYPE_OBJECT);
    facc_dialog->selection = gtk_single_selection_new
        (G_LIST_MODEL (g_object_ref (facc_dialog->rows)));
    gtk_column_view_set_model (facc_dialog->view, GTK_SELECTION_MODEL (facc_dialog->selection));
    gtk_column_view_set_show_row_separators (facc_dialog->view,
        gnc_prefs_get_bool (GNC_PREFS_GROUP_GENERAL, GNC_PREF_GRID_LINES_HORIZONTAL));
    gtk_column_view_set_show_column_separators (facc_dialog->view,
        gnc_prefs_get_bool (GNC_PREFS_GROUP_GENERAL, GNC_PREF_GRID_LINES_VERTICAL));
    find_account_add_column (facc_dialog, _("Account Full Name"), FIND_ACCOUNT_NAME, TRUE);
    find_account_add_column (facc_dialog, _("Place Holder"), FIND_ACCOUNT_PLACEHOLDER, FALSE);
    find_account_add_column (facc_dialog, _("Hidden"), FIND_ACCOUNT_HIDDEN, FALSE);
    find_account_add_column (facc_dialog, _("Not Used"), FIND_ACCOUNT_UNUSED, FALSE);
    find_account_add_column (facc_dialog, _("Balance Zero"), FIND_ACCOUNT_ZERO_BALANCE, FALSE);
    find_account_add_column (facc_dialog, _("Tax related"), FIND_ACCOUNT_TAX_RELATED, FALSE);
    g_signal_connect (facc_dialog->view, "activate", G_CALLBACK(row_activated), facc_dialog);

    g_signal_connect (facc_dialog->window, "destroy",
                      G_CALLBACK(gnc_find_account_dialog_window_destroy_cb), facc_dialog);

    g_signal_connect (facc_dialog->window, "close-request",
                      G_CALLBACK(gnc_find_account_dialog_window_close_request_cb), facc_dialog);

    GtkEventController *event_controller = gtk_event_controller_key_new ();
    gtk_widget_add_controller (GTK_WIDGET(facc_dialog->window), event_controller);
    g_signal_connect (event_controller,
                      "key-pressed",
                      G_CALLBACK(gnc_find_account_dialog_window_key_press_cb), facc_dialog);

gnc_builder_connect_signals_full (builder, gnc_builder_connect_full_func, facc_dialog);

    g_object_unref (G_OBJECT(builder));

    gnc_restore_window_size (GNC_PREFS_GROUP, GTK_WINDOW(facc_dialog->window), GTK_WINDOW(parent));


    if (facc_dialog->account != NULL)
    {
        gchar *sub_full_name = gnc_account_get_full_name (facc_dialog->account);
        /* Translators: %s is a full account name.
           This is a label in Search Account from context menu. */
        gchar *sub_label = g_strdup_printf (_("Su_b-accounts of '%s'"),
                                            sub_full_name);

        gtk_button_set_label (GTK_BUTTON(facc_dialog->radio_subroot), sub_label);

        g_free (sub_full_name);
        g_free (sub_label);

        gtk_check_button_set_active (GTK_CHECK_BUTTON(facc_dialog->radio_subroot), TRUE);
    }
    else
        gtk_widget_set_visible (GTK_WIDGET(facc_dialog->radio_frame), FALSE);

    // Set the filter to Wildcard
    gnc_entry_set_text (GTK_ENTRY(facc_dialog->filter_text_entry), "");

    // add a handler to listen for account events
    facc_dialog->event_handler_id = qof_event_register_handler
                             ((QofEventHandler)gnc_find_account_event_handler, facc_dialog);

    get_account_info (facc_dialog, FALSE);
    LEAVE(" ");
}

static void
close_handler (gpointer user_data)
{
    FindAccountDialog *facc_dialog = user_data;

    ENTER(" ");
    gnc_save_window_size (GNC_PREFS_GROUP,
                          GTK_WINDOW(facc_dialog->window));
    gtk_window_destroy (GTK_WINDOW(facc_dialog->window));
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
    FindAccountDialog *facc_dialog = user_data;

    ENTER(" ");
    if (!facc_dialog)
    {
        LEAVE("No data structure");
        return(FALSE);
    }
    gtk_window_present (GTK_WINDOW(facc_dialog->window));
    LEAVE(" ");
    return(TRUE);
}

/********************************************************************\
 * gnc_find_account_dialog                                          *
 * opens a window allowing for searches on account names            *
 *                                                                  *
 * Args:   parent  - the parent of the window to be created         *
 * Return: nothing                                                  *
\********************************************************************/
void
gnc_find_account_dialog (GtkWidget *parent, Account *account)
{
    FindAccountDialog *facc_dialog;
    gint component_id;

    ENTER(" ");
    if (gnc_forall_gui_components (DIALOG_FIND_ACCOUNT_CM_CLASS, show_handler, NULL))
    {
        LEAVE("Existing dialog raised");
        return;
    }
    facc_dialog = g_new0 (FindAccountDialog, 1);

    facc_dialog->account = account;
    facc_dialog->jump_close = TRUE;

    gnc_find_account_dialog_create (parent, facc_dialog);

    component_id = gnc_register_gui_component (DIALOG_FIND_ACCOUNT_CM_CLASS,
                   refresh_handler, close_handler,
                   facc_dialog);

    gnc_gui_component_set_session (component_id, facc_dialog->session);
    LEAVE(" ");
}
