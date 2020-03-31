/********************************************************************\
 * dialog-options.c -- GNOME option handling                        *
 * Copyright (C) 1998-2000 Linas Vepstas                            *
 * Copyright (c) 2006 David Hampton <hampton@employees.org>         *
 * Copyright (c) 2011 Robert Fewell                                 *
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
#include <gdk/gdk.h>
#include <glib/gi18n.h>
#include "swig-runtime.h"

#include "gnc-tree-model-budget.h" //FIXME?
#include "gnc-budget.h"

#include "dialog-options.h"
#include "dialog-utils.h"
#include "engine-helpers-guile.h"
#include "glib-helpers.h"
#include "gnc-account-sel.h"
#include "gnc-tree-view-account.h"
#include "gnc-tree-model-account.h"
#include "gnc-combott.h"
#include "gnc-commodity-edit.h"
#include "gnc-component-manager.h"
#include "gnc-general-select.h"
#include "gnc-currency-edit.h"
#include "gnc-date-edit.h"
#include "gnc-engine.h"
#include "gnc-prefs.h"
#include "gnc-gui-query.h"
#include "gnc-session.h"
#include "gnc-ui.h"
#include "guile-util.h"
#include "gnc-guile-utils.h"
#include "option-util.h"
#include "guile-mappings.h"
#include "gnc-date-format.h"
#include "misc-gnome-utils.h"

#define GNC_PREF_CLOCK_24H "clock-24h"

#define FUNC_NAME G_STRFUNC
/* TODO: clean up "register-stocks" junk
 */


/* This static indicates the debugging module that this .o belongs to.  */
static QofLogModule log_module = GNC_MOD_GUI;

#define DIALOG_OPTIONS_CM_CLASS      "dialog-options"
#define DIALOG_BOOK_OPTIONS_CM_CLASS "dialog-book-options"

#define GNC_PREFS_GROUP              "dialogs.options"

/*
 * Point where preferences switch control method from a set of
 * notebook tabs to a list.
 */
#define MAX_TAB_COUNT 6

/* A pointer to the last selected filename */
#define LAST_SELECTION "last-selection"

/* A Hash-table of GNCOptionDef_t keyed with option names. */
static GHashTable *optionTable = NULL;

static int gain_loss_accounts_in_filter = 0;

struct gnc_option_win
{
    GtkWidget  * window;
    GtkWidget  * notebook;
    GtkWidget  * page_list_view;
    GtkWidget  * page_list;

    gboolean toplevel;

    GNCOptionWinCallback apply_cb;
    gpointer             apply_cb_data;

    GNCOptionWinCallback help_cb;
    gpointer             help_cb_data;

    GNCOptionWinCallback close_cb;
    gpointer             close_cb_data;

    /* Hold onto this for a complete reset */
    GNCOptionDB *option_db;

    /* Hold on to this to unregister the right class */
    const char *component_class;

    /* widget being destroyed */
    gboolean destroyed;
};

typedef enum
{
    GNC_RD_WID_AB_BUTTON_POS = 0,
    GNC_RD_WID_AB_WIDGET_POS,
    GNC_RD_WID_REL_BUTTON_POS,
    GNC_RD_WID_REL_WIDGET_POS
} GNCRdPositions;

enum page_tree
{
    PAGE_INDEX = 0,
    PAGE_NAME,
    NUM_COLUMNS
};

typedef struct
{
    GtkWidget *gnc_currency_radiobutton_0;
    GtkWidget *gnc_currency_radiobutton_1;
    GtkWidget *gnc_currency_radiobutton_2;
    GtkWidget *book_currency_widget;
    GtkWidget *default_cost_policy_widget;
    GtkWidget *default_gain_loss_account_widget;
    GtkWidget *book_currency_table;
    GtkWidget *book_currency_vbox;
    GtkWidget *gain_loss_account_del_button;
    GtkWidget *gain_loss_account_table;
    GtkWidget *default_gain_loss_account_text;
    GNCOption *option;
    gnc_commodity *retrieved_book_currency;
    SCM retrieved_policy_scm;
    SCM retrieved_gain_loss_acct_guid_scm;
    Account *prior_gain_loss_account;

} currency_accounting_data;

static currency_accounting_data *book_currency_data = NULL;

static GNCOptionWinCallback global_help_cb = NULL;
gpointer global_help_cb_data = NULL;

static void gnc_options_dialog_reset_cb(GtkWidget * w, gpointer data);
void gnc_options_dialog_list_select_cb (GtkTreeSelection *selection,
                                        gpointer data);
void gnc_set_default_cost_policy_widget(SCM list_symbol);
void gnc_set_default_gain_loss_account_widget(gnc_commodity *commodity);
void gnc_option_changed_book_currency_widget_cb(GtkWidget *widget);
void gnc_option_changed_gain_loss_account_widget_cb(GtkTreeSelection *selection,
                                                    gpointer data);
void gnc_option_changed_gain_loss_account_del_button_widget_cb (GtkButton *button,
                                                    gpointer data);
static void component_close_handler (gpointer data);

GtkWidget *
gnc_option_get_gtk_widget (GNCOption *option)
{
    return (GtkWidget *)gnc_option_get_widget(option);
}

static void
gnc_options_dialog_changed_internal (GtkWidget *widget, gboolean sensitive)
{
    while (widget && !GTK_IS_WINDOW(widget))
        widget = gtk_widget_get_parent(widget);
    if (widget == NULL)
        return;

    /* find the ok and cancel buttons, we know where they will be so do it
       this way as opposed to using gtk_container_foreach, much less iteration */
    if (GTK_IS_CONTAINER(widget))
    {
        GList *children = gtk_container_get_children(GTK_CONTAINER(widget));
        for (GList *it = children; it; it = it->next)
        {
            if (GTK_IS_BOX (GTK_WIDGET(it->data)))
            {
                GList *children = gtk_container_get_children(GTK_CONTAINER(it->data));
                for (GList *it = children; it; it = it->next)
                {
                    if (GTK_IS_BUTTON_BOX (GTK_WIDGET(it->data)))
                    {
                        GList *children = gtk_container_get_children(GTK_CONTAINER(it->data));
                        for (GList *it = children; it; it = it->next)
                        {
                            if (g_strcmp0 (gtk_widget_get_name(GTK_WIDGET(it->data)), "ok_button") == 0)
                                gtk_widget_set_sensitive (GTK_WIDGET(it->data), sensitive);

                            if (g_strcmp0 (gtk_widget_get_name(GTK_WIDGET(it->data)), "apply_button") == 0)
                                gtk_widget_set_sensitive (GTK_WIDGET(it->data), sensitive);
                        }
                        g_list_free (children);
                    }
                }
                g_list_free (children);
            }
        }
        g_list_free (children);
    }
}

void
gnc_options_dialog_changed (GNCOptionWin *win)
{
    if (!win) return;

    gnc_options_dialog_changed_internal (win->window, TRUE);
}

void
gnc_option_changed_widget_cb(GtkWidget *widget, GNCOption *option)
{
    gnc_option_set_changed (option, TRUE);
    gnc_option_call_option_widget_changed_proc(option);
    gnc_options_dialog_changed_internal (widget, TRUE);
}

void
gnc_option_changed_option_cb(GtkWidget *dummy, GNCOption *option)
{
    GtkWidget *widget;

    widget = gnc_option_get_gtk_widget (option);
    gnc_option_changed_widget_cb(widget, option);
}

static void
gnc_date_option_set_select_method(GNCOption *option, gboolean use_absolute,
                                  gboolean set_buttons)
{
    GList* widget_list;
    GtkWidget *ab_button, *rel_button, *rel_widget, *ab_widget;
    GtkWidget *widget;

    widget = gnc_option_get_gtk_widget (option);

    widget_list = gtk_container_get_children(GTK_CONTAINER(widget));
    ab_button = g_list_nth_data(widget_list, GNC_RD_WID_AB_BUTTON_POS);
    ab_widget = g_list_nth_data(widget_list, GNC_RD_WID_AB_WIDGET_POS);
    rel_button = g_list_nth_data(widget_list, GNC_RD_WID_REL_BUTTON_POS);
    rel_widget = g_list_nth_data(widget_list, GNC_RD_WID_REL_WIDGET_POS);
    g_list_free(widget_list);

    if (use_absolute)
    {
        gtk_widget_set_sensitive(ab_widget, TRUE);
        gtk_widget_set_sensitive(rel_widget, FALSE);
        if (set_buttons)
        {
            gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(ab_button), TRUE);
        }
    }
    else
    {
        gtk_widget_set_sensitive(rel_widget, TRUE);
        gtk_widget_set_sensitive(ab_widget, FALSE);
        if (set_buttons)
        {
            gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(rel_button), TRUE);
        }
    }
}

static void
gnc_rd_option_ab_set_cb(GtkWidget *widget, gpointer *raw_option)
{
    GNCOption *option = (GNCOption *) raw_option;
    gnc_date_option_set_select_method(option, TRUE, FALSE);
    gnc_option_changed_option_cb(widget, option);
}

static void
gnc_rd_option_rel_set_cb(GtkWidget *widget, gpointer *raw_option)
{
    GNCOption *option = (GNCOption *) raw_option;
    gnc_date_option_set_select_method(option, FALSE, FALSE);
    gnc_option_changed_option_cb(widget, option);
    return;
}

static void
gnc_image_option_update_preview_cb (GtkFileChooser *chooser,
                                    GNCOption *option)
{
    gchar *filename;
    GtkImage *image;
    GdkPixbuf *pixbuf;
    gboolean have_preview;

    g_return_if_fail(chooser != NULL);

    ENTER("chooser %p, option %p", chooser, option);
    filename = gtk_file_chooser_get_preview_filename(chooser);
    DEBUG("chooser preview name is %s.", filename ? filename : "(null)");
    if (filename == NULL)
    {
        filename = g_strdup(g_object_get_data(G_OBJECT(chooser), LAST_SELECTION));
        DEBUG("using last selection of %s", filename ? filename : "(null)");
        if (filename == NULL)
        {
            LEAVE("no usable name");
            return;
        }
    }

    image = GTK_IMAGE(gtk_file_chooser_get_preview_widget(chooser));
    pixbuf = gdk_pixbuf_new_from_file_at_size(filename, 128, 128, NULL);
    g_free(filename);
    have_preview = (pixbuf != NULL);

    gtk_image_set_from_pixbuf(image, pixbuf);
    if (pixbuf)
        g_object_unref(pixbuf);

    gtk_file_chooser_set_preview_widget_active(chooser, have_preview);
    LEAVE("preview visible is %d", have_preview);
}

static void
gnc_image_option_selection_changed_cb (GtkFileChooser *chooser,
                                       GNCOption *option)
{
    gchar *filename;

    filename = gtk_file_chooser_get_preview_filename(chooser);
    if (!filename)
        return;
    g_object_set_data_full(G_OBJECT(chooser), LAST_SELECTION, filename, g_free);
}

/********************************************************************\
 * gnc_option_set_ui_value_internal                                 *
 *   sets the GUI representation of an option with either its       *
 *   current guile value, or its default value                      *
 *                                                                  *
 * Args: option      - option structure containing option           *
 *       use_default - if true, use the default value, otherwise    *
 *                     use the current value                        *
 * Return: nothing                                                  *
\********************************************************************/
static void
gnc_option_set_ui_value_internal (GNCOption *option, gboolean use_default)
{
    gboolean bad_value = FALSE;
    GtkWidget *widget;
    char *type;
    SCM getter;
    SCM value;
    GNCOptionDef_t *option_def;

    widget = gnc_option_get_gtk_widget (option);
    if (!widget)
        return;

    type = gnc_option_type(option);

    if (use_default)
        getter = gnc_option_default_getter(option);
    else
        getter = gnc_option_getter(option);

    value = scm_call_0(getter);

    option_def = gnc_options_ui_get_option (type);
    if (option_def && option_def->set_value)
    {
        bad_value = option_def->set_value (option, use_default, widget, value);
        if (bad_value)
        {
            PERR("bad value\n");
        }
    }
    else
    {
        PERR("Unknown type. Ignoring.\n");
    }

    free(type);
}

/********************************************************************\
 * gnc_option_get_ui_value_internal                                 *
 *   returns the SCM representation of the GUI option value         *
 *                                                                  *
 * Args: option - option structure containing option                *
 * Return: SCM handle to GUI option value                           *
\********************************************************************/
static SCM
gnc_option_get_ui_value_internal (GNCOption *option)
{
    SCM result = SCM_UNDEFINED;
    GtkWidget *widget;
    char *type;
    GNCOptionDef_t *option_def;

    widget = gnc_option_get_gtk_widget (option);
    if (!widget)
        return result;

    type = gnc_option_type(option);

    option_def = gnc_options_ui_get_option (type);
    if (option_def && option_def->get_value)
    {
        result = option_def->get_value (option, widget);
    }
    else
    {
        PERR("Unknown type for refresh. Ignoring.\n");
    }

    free(type);

    return result;
}

/********************************************************************\
 * gnc_option_set_selectable_internal                               *
 *   Change the selectable state of the widget that represents a    *
 *   GUI option.                                                    *
 *                                                                  *
 * Args: option      - option to change widget state for            *
 *       selectable  - if false, update the widget so that it       *
 *                     cannot be selected by the user.  If true,    *
 *                     update the widget so that it can be selected.*
 * Return: nothing                                                  *
\********************************************************************/
static void
gnc_option_set_selectable_internal (GNCOption *option, gboolean selectable)
{
    GtkWidget *widget;

    widget = gnc_option_get_gtk_widget (option);
    if (!widget)
        return;

    gtk_widget_set_sensitive (widget, selectable);
}

static void
gnc_option_default_cb(GtkWidget *widget, GNCOption *option)
{
    gnc_option_set_ui_value (option, TRUE);
    gnc_option_set_changed (option, TRUE);
    gnc_options_dialog_changed_internal (widget, TRUE);
}

static void
gnc_option_show_hidden_toggled_cb(GtkWidget *widget, GNCOption* option)
{
    AccountViewInfo avi;
    GncTreeViewAccount *tree_view;

    tree_view = GNC_TREE_VIEW_ACCOUNT(gnc_option_get_gtk_widget (option));
    gnc_tree_view_account_get_view_info (tree_view, &avi);
    avi.show_hidden = gtk_toggle_button_get_active(GTK_TOGGLE_BUTTON(widget));
    gnc_tree_view_account_set_view_info (tree_view, &avi);
    gnc_option_changed_widget_cb(widget, option);
}

static void
gnc_option_multichoice_cb(GtkWidget *widget, gpointer data)
{
    GNCOption *option = data;
    /* GtkComboBox per-item tooltip changes needed below */
    gnc_option_changed_widget_cb(widget, option);
}

static void
gnc_option_radiobutton_cb(GtkWidget *w, gpointer data)
{
    GNCOption *option = data;
    GtkWidget *widget;
    gpointer _current, _new_value;
    gint current, new_value;

    widget = gnc_option_get_gtk_widget (option);

    _current = g_object_get_data(G_OBJECT(widget), "gnc_radiobutton_index");
    current = GPOINTER_TO_INT (_current);

    _new_value = g_object_get_data (G_OBJECT(w), "gnc_radiobutton_index");
    new_value = GPOINTER_TO_INT (_new_value);

    if (current == new_value)
        return;

    g_object_set_data (G_OBJECT(widget), "gnc_radiobutton_index",
                       GINT_TO_POINTER(new_value));
    gnc_option_changed_widget_cb(widget, option);
}

static gboolean
gnc_gain_loss_account_view_filter (Account  *account, gpointer  data)
{
    GNCAccountType type = xaccAccountGetType(account);

    /* gain/loss accts must be an Income or Expense accts and not hidden;
       placeholder accounts must be included, irrespective of their currency,
       so their children are available to be considered */
    if (((type == ACCT_TYPE_INCOME) || (type == ACCT_TYPE_EXPENSE)) &&
        (!xaccAccountIsHidden(account)))
    {
        if (xaccAccountGetPlaceholder(account))
        {
            GList *placeholder_children = gnc_account_get_children (account);

            if(placeholder_children)
            { /* determine if any children qualify; just need one but don't
                 double count in gain_loss_accounts_in_filter */
                int saved_gain_loss_accounts_in_filter =
                                                gain_loss_accounts_in_filter;
                gboolean child_pass_filter = FALSE;
                GList *l = NULL;
                for (l = placeholder_children; l != NULL; l = l->next)
                {
                    Account  *child_account = l->data;
                    child_pass_filter =
                        gnc_gain_loss_account_view_filter(child_account, NULL);
                    if (child_pass_filter)
                        break;
                }
                g_list_free(placeholder_children);
                gain_loss_accounts_in_filter =
                                           saved_gain_loss_accounts_in_filter;
                return child_pass_filter;
            }
            else return FALSE; // no children, not interested
        }
        else
        {
            gnc_commodity *commodity = NULL;

            /* gain/loss accts must be in book-currency; if a book currency has been
               specified in the widget, use it to filter */
            if (gtk_combo_box_get_active (GTK_COMBO_BOX(book_currency_data->book_currency_widget)) != -1)
                commodity = gnc_currency_edit_get_currency(
                                GNC_CURRENCY_EDIT(
                                    book_currency_data->book_currency_widget));
            if (commodity)
            {
                if (gnc_commodity_equal(xaccAccountGetCommodity(account),
                                    commodity))
                {
                    gain_loss_accounts_in_filter++;
                    return TRUE;
                }
                else return FALSE;
            }
            /* else use the default currency */
            else if (gnc_commodity_equal(xaccAccountGetCommodity(account),
                                gnc_default_currency()))
            {
                gain_loss_accounts_in_filter++;
                return TRUE;
            }
            else return FALSE;
        }
    }
    else return FALSE;
}

static gboolean
gnc_gain_loss_account_all_fail_filter (Account  *account, gpointer  data)
{
    return FALSE;
}

void
gnc_set_default_cost_policy_widget(SCM list_symbol)
{
    GList *list_of_policies = gnc_get_valid_policy_list();

    if (list_of_policies)
    {
        GList *l = NULL;
        gint i = 0;
        for (l = list_of_policies; l != NULL; l = l->next)
        {
            GNCPolicy *pcy = l->data;
            if (g_strcmp0(PolicyGetName (pcy),
                               gnc_scm_symbol_to_locale_string(list_symbol))
                               == 0)
            {
                /* GtkComboBox per-item tooltip changes needed below */
                gnc_combott_set_active(
                    GNC_COMBOTT(
                        book_currency_data->default_cost_policy_widget), i);
            }
            i++;
        }
        g_list_free(list_of_policies);
    }
    else
    {
        gnc_combott_set_active (
            GNC_COMBOTT(book_currency_data->default_cost_policy_widget), -1);
    }
}

void
gnc_set_default_gain_loss_account_widget(gnc_commodity *commodity)
{
    if (book_currency_data->default_gain_loss_account_widget)
    {
        gtk_widget_destroy (
                    book_currency_data->default_gain_loss_account_widget);
        book_currency_data->default_gain_loss_account_widget = NULL;
        book_currency_data->prior_gain_loss_account = NULL;
        gain_loss_accounts_in_filter = 0;
    }
    if (book_currency_data->gain_loss_account_del_button)
    {
        gtk_widget_destroy (
                    book_currency_data->gain_loss_account_del_button);
        book_currency_data->gain_loss_account_del_button = NULL;
    }
    if (book_currency_data->default_gain_loss_account_text)
    {
        gtk_widget_destroy (
                    book_currency_data->default_gain_loss_account_text);
        book_currency_data->default_gain_loss_account_text = NULL;
    }
    if (gnc_is_new_book())
    {
        book_currency_data->default_gain_loss_account_text =
                    gtk_label_new( _("Because no accounts have " \
                        "been set up yet, you will need to return to this " \
                        "dialog (via File->Properties), after account setup, " \
                        "if you want to set a default gain/loss account.") );

        gtk_label_set_line_wrap (GTK_LABEL(book_currency_data->default_gain_loss_account_text), TRUE);

        gtk_grid_attach (GTK_GRID(book_currency_data->gain_loss_account_table),
                                  book_currency_data->default_gain_loss_account_text, 0, 1, 2, 1);
    }
    else
    {
        GtkTreeSelection *selection = NULL;
        book_currency_data->default_gain_loss_account_widget =
                            GTK_WIDGET(gnc_tree_view_account_new(FALSE));
        gain_loss_accounts_in_filter = 0;
        selection = gtk_tree_view_get_selection(GTK_TREE_VIEW(
                        book_currency_data->default_gain_loss_account_widget));
        if (!commodity) // that means not book currency
        {
            /* set the default_gain_loss_account_widget to be blank with a
               no-acct filter */
            gnc_tree_view_account_set_filter(GNC_TREE_VIEW_ACCOUNT (
                        book_currency_data->default_gain_loss_account_widget),
                        gnc_gain_loss_account_all_fail_filter,
                        NULL,  /* user data */
                        NULL  /* destroy callback */ );
            gtk_tree_selection_unselect_all (selection);
        }
        else // that means book currency
        {
            /* see if there are any accounts after filter */
            gnc_tree_view_account_set_filter(GNC_TREE_VIEW_ACCOUNT (
                        book_currency_data->default_gain_loss_account_widget),
                        gnc_gain_loss_account_view_filter,
                        NULL, /* user data */
                        NULL  /* destroy callback */);
            if (gain_loss_accounts_in_filter > 0)
            {   /* there are accounts; find out if one is selected */
                Account *gain_loss_account = NULL;
                Account *selected_account = NULL;
                GtkTreeViewColumn *col;

                book_currency_data->gain_loss_account_del_button =
                        gtk_button_new_with_label( _("Select no account") );

                g_signal_connect (GTK_BUTTON (
                        book_currency_data->gain_loss_account_del_button),
                        "clicked",
                        G_CALLBACK (
                            gnc_option_changed_gain_loss_account_del_button_widget_cb),
                        NULL);
                gtk_grid_attach (GTK_GRID(book_currency_data->gain_loss_account_table),
                                          book_currency_data->gain_loss_account_del_button, 1, 0, 1, 1);

                gtk_tree_view_set_headers_visible (GTK_TREE_VIEW(
                        book_currency_data->default_gain_loss_account_widget),
                        TRUE);
                col = gnc_tree_view_add_text_column(GNC_TREE_VIEW(
                        book_currency_data->default_gain_loss_account_widget),
                         _("Currency"), /* title */
                        "commodity", /* pref name */
                        NULL,
                        "Currency--", /* sizing text */
                        GNC_TREE_MODEL_ACCOUNT_COL_COMMODITY,
                        GNC_TREE_VIEW_COLUMN_VISIBLE_ALWAYS,
                        NULL);
                g_object_set_data(G_OBJECT(col), DEFAULT_VISIBLE,
                    GINT_TO_POINTER(1));

                // add the color background data function to the column
                gnc_tree_view_account_column_add_color (GNC_TREE_VIEW_ACCOUNT(
                         book_currency_data->default_gain_loss_account_widget), col);

                col = gnc_tree_view_add_toggle_column(GNC_TREE_VIEW(
                        book_currency_data->default_gain_loss_account_widget),
                        _("Placeholder"),
                        C_("Column header for 'Placeholder'", "P"),
                        "placeholder",
                        GNC_TREE_MODEL_ACCOUNT_COL_PLACEHOLDER,
                        GNC_TREE_VIEW_COLUMN_VISIBLE_ALWAYS,
                        NULL,
                        NULL);
                g_object_set_data(G_OBJECT(col), DEFAULT_VISIBLE,
                    GINT_TO_POINTER(1));

                // add the color background data function to the column
                gnc_tree_view_account_column_add_color (GNC_TREE_VIEW_ACCOUNT(
                         book_currency_data->default_gain_loss_account_widget), col);

                gnc_tree_view_configure_columns (GNC_TREE_VIEW(
                        book_currency_data->default_gain_loss_account_widget));
                gnc_tree_view_set_show_column_menu(GNC_TREE_VIEW(
                        book_currency_data->default_gain_loss_account_widget),
                        FALSE);
                if (book_currency_data->retrieved_gain_loss_acct_guid_scm &&
                    (scm_is_string(
                        book_currency_data->retrieved_gain_loss_acct_guid_scm)))
                {
                    GncGUID *guid= g_new (GncGUID, 1);

                    if (string_to_guid (
                        gnc_scm_to_utf8_string(
                        book_currency_data->retrieved_gain_loss_acct_guid_scm),
                        guid))
                    gain_loss_account =
                                xaccAccountLookup(guid, gnc_get_current_book());
                    g_free (guid);
                }
                if (gain_loss_account)
                {
                    (gnc_tree_view_account_set_selected_account
                        (GNC_TREE_VIEW_ACCOUNT(
                          book_currency_data->default_gain_loss_account_widget),
                        gain_loss_account));
                    selected_account =
                        gnc_tree_view_account_get_selected_account(
                            GNC_TREE_VIEW_ACCOUNT (
                            book_currency_data->default_gain_loss_account_widget));
                }
                if (selected_account)
                {
                    book_currency_data->prior_gain_loss_account =
                        selected_account;
                    gtk_widget_set_sensitive(
                        book_currency_data->gain_loss_account_del_button,
                        TRUE);
                }
                else /* none selected */
                {
                    gtk_tree_selection_unselect_all (selection);
                    gtk_widget_set_sensitive(
                        book_currency_data->gain_loss_account_del_button,
                        FALSE);
                }
            }
            else /* no accts in widget?; replace widget with text */
            {
                gtk_widget_destroy (
                    book_currency_data->default_gain_loss_account_widget);
                book_currency_data->default_gain_loss_account_widget = NULL;
                book_currency_data->prior_gain_loss_account = NULL;
                gain_loss_accounts_in_filter = 0;
                book_currency_data->default_gain_loss_account_text =
                    gtk_label_new( _("There are no income " \
                        "or expense accounts of the specified\n" \
                        "book currency; you will have to return to this " \
                        "dialog\n(via File->Properties), after account setup, " \
                        "to select a\ndefault gain/loss account.") );
                gtk_grid_attach (GTK_GRID(book_currency_data->gain_loss_account_table),
                                          book_currency_data->default_gain_loss_account_text, 0, 1, 2, 1);
            }
        }
        if (book_currency_data->default_gain_loss_account_widget)
        {
            gtk_widget_set_hexpand (GTK_WIDGET(book_currency_data->default_gain_loss_account_widget), TRUE);
            g_signal_connect (G_OBJECT (selection),
                        "changed",
                        G_CALLBACK (gnc_option_changed_gain_loss_account_widget_cb),
                        NULL);
            gtk_grid_attach (GTK_GRID(book_currency_data->gain_loss_account_table),
                                      book_currency_data->default_gain_loss_account_widget, 0, 1, 2, 1);

        }
    }
}

void
gnc_option_changed_book_currency_widget_cb(GtkWidget *widget)
{
    /* Once the book currency widget is set, need to set the
       default_gain_loss_account_widget and/or del-button or text*/
    if (gtk_combo_box_get_active (GTK_COMBO_BOX(book_currency_data->book_currency_widget)) != -1)
    {
        gnc_commodity *commodity = gnc_currency_edit_get_currency(
                                GNC_CURRENCY_EDIT(
                                    book_currency_data->book_currency_widget));

        gnc_set_default_gain_loss_account_widget(commodity);
    }
    gtk_widget_show_all(book_currency_data->book_currency_vbox);
    gnc_option_changed_widget_cb(widget, book_currency_data->option);
}

void
gnc_option_changed_gain_loss_account_widget_cb (GtkTreeSelection *selection,
                                                    gpointer data)
{
    Account *account = NULL;
    gboolean new_eq_prior_acct = FALSE;

    g_return_if_fail (book_currency_data->default_gain_loss_account_widget);
    account = gnc_tree_view_account_get_selected_account (
                    GNC_TREE_VIEW_ACCOUNT (
                        book_currency_data->default_gain_loss_account_widget));
    if (account && book_currency_data->prior_gain_loss_account)
        new_eq_prior_acct = xaccAccountEqual(account,
                                book_currency_data->prior_gain_loss_account,
                                TRUE);
    if (account && (!new_eq_prior_acct))
    { /* a new account has been selected */
        if (!xaccAccountGetPlaceholder(account))
        {
            GtkWidget *option_widget =
                        gnc_option_get_gtk_widget (book_currency_data->option);
            book_currency_data->prior_gain_loss_account = account;
            gtk_widget_set_sensitive(
                    book_currency_data->gain_loss_account_del_button, TRUE);
            gtk_widget_show_all(book_currency_data->book_currency_vbox);
            gnc_option_changed_option_cb(option_widget, book_currency_data->option);
        }
        else /*  new account, but placeholder */
        {
            const char *message = _("The account %s is a placeholder account " \
                "and does not allow transactions. " \
        	"Please choose a different account.");

            gnc_error_dialog (gnc_ui_get_gtk_window (book_currency_data->default_gain_loss_account_widget),
			      message, xaccAccountGetName (account));
            if (book_currency_data->prior_gain_loss_account)
            {
                (gnc_tree_view_account_set_selected_account
                    (GNC_TREE_VIEW_ACCOUNT(
                          book_currency_data->default_gain_loss_account_widget),
                        book_currency_data->prior_gain_loss_account));
            }
            else
            {
                gtk_tree_selection_unselect_all (selection);
            }
        }
    }
    else /* a new account has not been selected */
    {
        if (book_currency_data->prior_gain_loss_account == NULL)
        {
            gtk_tree_selection_unselect_all (selection);
            if (book_currency_data->gain_loss_account_del_button)
            {
                gtk_widget_set_sensitive(
                    book_currency_data->gain_loss_account_del_button, FALSE);
            }
        }
    }
}

void
gnc_option_changed_gain_loss_account_del_button_widget_cb (GtkButton *button, gpointer data)
{
    GtkTreeSelection *selection = NULL;
    GtkWidget *option_widget =
                        gnc_option_get_gtk_widget (book_currency_data->option);

    g_return_if_fail (book_currency_data->default_gain_loss_account_widget);
    g_return_if_fail (book_currency_data->gain_loss_account_del_button);

    selection = gtk_tree_view_get_selection (
                    GTK_TREE_VIEW (
                        book_currency_data->default_gain_loss_account_widget));
    gtk_tree_selection_unselect_all (selection);
    book_currency_data->prior_gain_loss_account = NULL;
    gtk_widget_set_sensitive(
                    book_currency_data->gain_loss_account_del_button, FALSE);
    gnc_option_changed_option_cb(option_widget, book_currency_data->option);
}

static void
gnc_option_currency_accounting_non_book_cb(GtkWidget *widget, gpointer data)
{
    gnc_currency_edit_clear_display (GNC_CURRENCY_EDIT(
                                     book_currency_data->book_currency_widget));
    gnc_combott_set_active(GNC_COMBOTT(
                                book_currency_data->default_cost_policy_widget),
                                -1);
    gnc_set_default_gain_loss_account_widget(NULL);
    gtk_widget_show_all(book_currency_data->book_currency_vbox);
    gtk_widget_set_sensitive(book_currency_data->book_currency_vbox, FALSE);
    gnc_option_radiobutton_cb(widget, (gpointer) book_currency_data->option);
}

static void
gnc_option_currency_accounting_book_cb(GtkWidget *widget, gpointer data)
{
    SCM list_symbol =
            gnc_currency_accounting_option_get_default_policy(
                                                    book_currency_data->option);
    SCM curr_scm = gnc_currency_accounting_option_get_default_currency(
                                                    book_currency_data->option);
    gnc_commodity *commodity = gnc_scm_to_commodity (curr_scm);

    if (book_currency_data->retrieved_book_currency)
    {
        gnc_currency_edit_set_currency
                (GNC_CURRENCY_EDIT(book_currency_data->book_currency_widget),
                    book_currency_data->retrieved_book_currency);
    }
    else if (commodity)
    {
        gnc_currency_edit_set_currency
                (GNC_CURRENCY_EDIT(book_currency_data->book_currency_widget),
                    commodity);
    }
    else
    {
        gnc_currency_edit_set_currency
                (GNC_CURRENCY_EDIT(book_currency_data->book_currency_widget),
                    gnc_default_currency());
    }
    if (book_currency_data->retrieved_policy_scm)
    {
        gnc_set_default_cost_policy_widget(
                                      book_currency_data->retrieved_policy_scm);
    }
    else
    {
        gnc_set_default_cost_policy_widget(list_symbol);
    }
    gtk_widget_show_all(book_currency_data->book_currency_vbox);
    gtk_widget_set_sensitive(book_currency_data->book_currency_vbox, TRUE);
    gnc_option_radiobutton_cb(widget, (gpointer) book_currency_data->option);
}

static GtkWidget *
gnc_option_create_date_widget (GNCOption *option)
{
    GtkWidget * box = NULL;
    GtkWidget *rel_button = NULL, *ab_button = NULL;
    GtkWidget *rel_widget = NULL, *ab_widget = NULL;
    GtkWidget *entry;
    gboolean show_time, use24;
    char *type;
    int num_values;

    type = gnc_option_date_option_get_subtype(option);
    show_time = gnc_option_show_time(option);
    use24 = gnc_prefs_get_bool(GNC_PREFS_GROUP_GENERAL, GNC_PREF_CLOCK_24H);

    if (g_strcmp0(type, "relative") != 0)
    {
        ab_widget = gnc_date_edit_new(time(NULL), show_time, use24);
        entry = GNC_DATE_EDIT(ab_widget)->date_entry;
        g_signal_connect(G_OBJECT(entry), "changed",
                         G_CALLBACK(gnc_option_changed_option_cb), option);
        if (show_time)
        {
            entry = GNC_DATE_EDIT(ab_widget)->time_entry;
            g_signal_connect(G_OBJECT(entry), "changed",
                             G_CALLBACK(gnc_option_changed_option_cb), option);
        }
    }

    if (g_strcmp0(type, "absolute") != 0)
    {
        int i;
        num_values = gnc_option_num_permissible_values(option);

        g_return_val_if_fail(num_values >= 0, NULL);

        {
            /* GtkComboBox still does not support per-item tooltips, so have
               created a basic one called Combott implemented in gnc-combott.
               Have highlighted changes in this file with comments for when
               the feature of per-item tooltips is implemented in gtk,
               see https://bugs.gnucash.org/show_bug.cgi?id=303717 */

            GtkListStore *store;
            GtkTreeIter  iter;

            char *itemstring;
            char *description;
            store = gtk_list_store_new(2, G_TYPE_STRING, G_TYPE_STRING);
            /* Add values to the list store, entry and tooltip */
            for (i = 0; i < num_values; i++)
            {
                itemstring = gnc_option_permissible_value_name(option, i);
                description = gnc_option_permissible_value_description(option, i);
                gtk_list_store_append (store, &iter);
                gtk_list_store_set (store, &iter, 0, itemstring, 1, description, -1);
                if (itemstring)
                    g_free(itemstring);
                if (description)
                    g_free(description);
            }
            /* Create the new Combo with tooltip and add the store */
            rel_widget = GTK_WIDGET(gnc_combott_new());
            g_object_set( G_OBJECT(rel_widget), "model", GTK_TREE_MODEL(store), NULL );
            g_object_unref(store);

            g_signal_connect(G_OBJECT(rel_widget), "changed",
                             G_CALLBACK(gnc_option_multichoice_cb), option);
        }
    }

    if (g_strcmp0(type, "absolute") == 0)
    {
        free(type);
        gnc_option_set_widget (option, ab_widget);
        return ab_widget;
    }
    else if (g_strcmp0(type, "relative") == 0)
    {
        gnc_option_set_widget (option, rel_widget);
        free(type);

        return rel_widget;
    }
    else if (g_strcmp0(type, "both") == 0)
    {
        box = gtk_box_new (GTK_ORIENTATION_HORIZONTAL, 5);
        gtk_box_set_homogeneous (GTK_BOX (box), FALSE);

        ab_button = gtk_radio_button_new(NULL);
        g_signal_connect(G_OBJECT(ab_button), "toggled",
                         G_CALLBACK(gnc_rd_option_ab_set_cb), option);

        rel_button = gtk_radio_button_new_from_widget(GTK_RADIO_BUTTON(ab_button));
        g_signal_connect(G_OBJECT(rel_button), "toggled",
                         G_CALLBACK(gnc_rd_option_rel_set_cb), option);

        gtk_box_pack_start(GTK_BOX(box), ab_button, FALSE, FALSE, 0);
        gtk_box_pack_start(GTK_BOX(box), ab_widget, FALSE, FALSE, 0);
        gtk_box_pack_start(GTK_BOX(box), rel_button, FALSE, FALSE, 0);
        gtk_box_pack_start(GTK_BOX(box), rel_widget, FALSE, FALSE, 0);

        free(type);

        gnc_option_set_widget (option, box);

        return box;
    }
    else /* can't happen */
    {
        return NULL;
    }
}

static GtkWidget *
gnc_option_create_budget_widget(GNCOption *option)
{
    GtkTreeModel *tm;
    GtkComboBox *cb;
    GtkCellRenderer *cr;

    tm = gnc_tree_model_budget_new(gnc_get_current_book());
    cb = GTK_COMBO_BOX(gtk_combo_box_new_with_model(tm));
    g_object_unref(tm);
    cr = gtk_cell_renderer_text_new();
    gtk_cell_layout_pack_start(GTK_CELL_LAYOUT(cb), cr, TRUE);

    gtk_cell_layout_set_attributes(GTK_CELL_LAYOUT(cb), cr, "text",
                                   BUDGET_NAME_COLUMN, NULL);
    return GTK_WIDGET(cb);
}

static GtkWidget *
gnc_option_create_multichoice_widget(GNCOption *option)
{
    GtkWidget *widget;
    int num_values;
    int i;

    num_values = gnc_option_num_permissible_values(option);

    g_return_val_if_fail(num_values >= 0, NULL);

    {
        /* GtkComboBox still does not support per-item tooltips, so have
           created a basic one called Combott implemented in gnc-combott.
           Have highlighted changes in this file with comments for when
           the feature of per-item tooltips is implemented in gtk,
           see https://bugs.gnucash.org/show_bug.cgi?id=303717 */
        GtkListStore *store;
        GtkTreeIter  iter;

        char *itemstring;
        char *description;
        store = gtk_list_store_new(2, G_TYPE_STRING, G_TYPE_STRING);
        /* Add values to the list store, entry and tooltip */
        for (i = 0; i < num_values; i++)
        {
            itemstring = gnc_option_permissible_value_name(option, i);
            description = gnc_option_permissible_value_description(option, i);
            gtk_list_store_append (store, &iter);
            gtk_list_store_set (store, &iter, 0,
                                (itemstring && *itemstring) ? _(itemstring) : "", 1,
                                (description && *description) ? _(description) : "", -1);
            if (itemstring)
                g_free(itemstring);
            if (description)
                g_free(description);
        }
        /* Create the new Combo with tooltip and add the store */
        widget = GTK_WIDGET(gnc_combott_new());
        g_object_set( G_OBJECT( widget ), "model", GTK_TREE_MODEL(store), NULL );
        g_object_unref(store);

        g_signal_connect(G_OBJECT(widget), "changed",
                         G_CALLBACK(gnc_option_multichoice_cb), option);
    }

    return widget;
}

static GtkWidget *
gnc_option_create_radiobutton_widget(char *name, GNCOption *option)
{
    GtkWidget *frame, *box;
    GtkWidget *widget = NULL;
    int num_values;
    char *label;
    char *tip;
    int i;

    num_values = gnc_option_num_permissible_values(option);

    g_return_val_if_fail(num_values >= 0, NULL);

    /* Create our button frame */
    frame = gtk_frame_new (name);

    /* Create the button box */
    box = gtk_box_new (GTK_ORIENTATION_HORIZONTAL, 5);
    gtk_box_set_homogeneous (GTK_BOX (box), FALSE);
    gtk_container_add (GTK_CONTAINER (frame), box);

    /* Iterate over the options and create a radio button for each one */
    for (i = 0; i < num_values; i++)
    {
        label = gnc_option_permissible_value_name(option, i);
        tip = gnc_option_permissible_value_description(option, i);

        widget =
            gtk_radio_button_new_with_label_from_widget (widget ?
                    GTK_RADIO_BUTTON (widget) :
                    NULL,
                    label && *label ? _(label) : "");
        g_object_set_data (G_OBJECT (widget), "gnc_radiobutton_index",
                           GINT_TO_POINTER (i));
        gtk_widget_set_tooltip_text(widget, tip && *tip ? _(tip) : "");
        g_signal_connect(G_OBJECT(widget), "toggled",
                         G_CALLBACK(gnc_option_radiobutton_cb), option);
        gtk_box_pack_start (GTK_BOX (box), widget, FALSE, FALSE, 0);

        if (label)
            free (label);
        if (tip)
            free (tip);
    }

    return frame;
}

static GtkWidget *
gnc_option_create_currency_accounting_widget (char *name, GNCOption *option)
{
    GtkWidget *frame = NULL,
              *widget = NULL,
              *vbox = NULL;
    int i;
    int num_values = gnc_option_num_permissible_values(option);

    g_return_val_if_fail(num_values == 3, NULL);
    book_currency_data = g_new0 (currency_accounting_data, 1);
    book_currency_data->option = option;

    /* Create the button frame */
    frame = gtk_frame_new (name);
    gtk_widget_set_halign (GTK_WIDGET(frame), GTK_ALIGN_FILL);
    gtk_widget_set_hexpand (GTK_WIDGET(frame), TRUE);

    /* Create the vertical button box */
    vbox = gtk_box_new (GTK_ORIENTATION_VERTICAL, 5);
    gtk_box_set_homogeneous (GTK_BOX (vbox), FALSE);
    gtk_container_add (GTK_CONTAINER (frame), vbox);
    gtk_widget_set_halign (GTK_WIDGET(vbox), GTK_ALIGN_FILL);
    gtk_widget_set_hexpand (GTK_WIDGET(vbox), TRUE);

#if GTK_CHECK_VERSION(3,12,0)
    gtk_widget_set_margin_end (GTK_WIDGET(vbox), 12);
#else
    gtk_widget_set_margin_right (GTK_WIDGET(vbox), 12);
#endif
    gtk_widget_set_margin_bottom (GTK_WIDGET(vbox), 12);

    /* Iterate over the three options and create a radio button for each one */
    for (i = 0; i < num_values; i++)
    {
        char *label;
        char *tip;
        GtkWidget *table = NULL;

        label = gnc_option_permissible_value_name(option, i);
        tip = gnc_option_permissible_value_description(option, i);

        widget =
            gtk_radio_button_new_with_label_from_widget (widget ?
                    GTK_RADIO_BUTTON (widget) :
                    NULL,
                    label && *label ? _(label) : "");
        g_object_set_data (G_OBJECT (widget), "gnc_radiobutton_index",
                           GINT_TO_POINTER (i));
        switch (i)
        {
        case 0:
            book_currency_data->gnc_currency_radiobutton_0 = widget;
            break;

        case 1:
            book_currency_data->gnc_currency_radiobutton_1 = widget;
            break;

        case 2:
            book_currency_data->gnc_currency_radiobutton_2 = widget;
            break;

        default:
            break;
        }
        gtk_widget_set_tooltip_text(widget, tip && *tip ? _(tip) : "");
        if (g_strcmp0(gnc_option_permissible_value_name(option, i),
                                                    "Use a Book Currency") == 0)
        {
            GtkWidget *widget_label,
                      *policy_table = gtk_grid_new();

            book_currency_data->book_currency_widget = gnc_currency_edit_new();
            book_currency_data->default_cost_policy_widget =
                                    gnc_cost_policy_select_new();
            book_currency_data->default_gain_loss_account_widget = NULL;
            book_currency_data->gain_loss_account_del_button = NULL;
            book_currency_data->default_gain_loss_account_text = NULL;
            book_currency_data->prior_gain_loss_account = NULL;

            book_currency_data->book_currency_vbox = gtk_box_new (GTK_ORIENTATION_VERTICAL, 0);
            gtk_box_set_homogeneous (GTK_BOX (book_currency_data->book_currency_vbox), FALSE);

            table = gtk_grid_new ();
            gtk_grid_attach (GTK_GRID(table), widget, 0, 0, 2, 1);
            g_signal_connect(G_OBJECT(widget), "toggled",
                         G_CALLBACK(gnc_option_currency_accounting_book_cb),
                         book_currency_data);

            book_currency_data->book_currency_table = gtk_grid_new ();
            gtk_grid_set_row_spacing (GTK_GRID (book_currency_data->book_currency_table), 6);
            gtk_grid_set_column_spacing (GTK_GRID (book_currency_data->book_currency_table), 6);

            tip = gnc_currency_accounting_option_currency_documentation(option);
            widget_label = gtk_label_new( _("Book currency") );
            gtk_widget_set_tooltip_text(book_currency_data->book_currency_table,
                        tip && *tip ? _(tip) : "");

            gtk_widget_set_halign (GTK_WIDGET(widget_label), GTK_ALIGN_START);
            gtk_widget_set_hexpand (GTK_WIDGET(widget_label), TRUE);

            gtk_grid_attach (GTK_GRID(book_currency_data->book_currency_table), widget_label, 0, 0, 1, 1);

            g_signal_connect(G_OBJECT(book_currency_data->book_currency_widget),
                             "changed",
                             G_CALLBACK(gnc_option_changed_book_currency_widget_cb),
                             NULL);

            gtk_grid_attach (GTK_GRID(book_currency_data->book_currency_table),
                                      book_currency_data->book_currency_widget, 1, 0, 1, 1);

            gtk_box_pack_start (GTK_BOX (book_currency_data->book_currency_vbox),
                                         book_currency_data->book_currency_table,
                                         TRUE, TRUE, 0);
#if GTK_CHECK_VERSION(3,12,0)
            gtk_widget_set_margin_start (GTK_WIDGET(book_currency_data->book_currency_table), 12);
#else
            gtk_widget_set_margin_left (GTK_WIDGET(book_currency_data->book_currency_table), 12);
#endif
            gtk_grid_set_row_spacing (GTK_GRID (policy_table), 6);
            gtk_grid_set_column_spacing (GTK_GRID (policy_table), 6);

            tip = gnc_currency_accounting_option_policy_documentation(option);
            widget_label = gtk_label_new( _("Default lot tracking policy") );
            gtk_widget_set_tooltip_text(policy_table, tip && *tip ? _(tip) : "");

            gtk_widget_set_halign (GTK_WIDGET(widget_label), GTK_ALIGN_START);
            gtk_widget_set_hexpand (GTK_WIDGET(widget_label), TRUE);

            gtk_grid_attach (GTK_GRID(policy_table), widget_label, 0, 1, 1, 1);

            g_signal_connect(G_OBJECT(
                             book_currency_data->default_cost_policy_widget),
                             "changed",
                             G_CALLBACK(gnc_option_multichoice_cb), option);

            gtk_grid_attach (GTK_GRID(policy_table),
                             book_currency_data->default_cost_policy_widget, 1, 1, 1, 1);

            gtk_box_pack_start (GTK_BOX (book_currency_data->book_currency_vbox),
                                         policy_table, TRUE, TRUE, 0);
#if GTK_CHECK_VERSION(3,12,0)
            gtk_widget_set_margin_start (GTK_WIDGET(policy_table), 12);
#else
            gtk_widget_set_margin_left (GTK_WIDGET(policy_table), 12);
#endif
            book_currency_data->gain_loss_account_table = gtk_grid_new ();
            gtk_grid_set_row_spacing (GTK_GRID (book_currency_data->gain_loss_account_table), 6);
            gtk_grid_set_column_spacing (GTK_GRID (book_currency_data->gain_loss_account_table), 6);

            tip = gnc_currency_accounting_option_gain_loss_account_documentation(option);
            widget_label = gtk_label_new( _("Default gain/loss account") );
            gnc_label_set_alignment (GTK_WIDGET(widget_label), 0.0, 0.5);

            gtk_widget_set_tooltip_text(book_currency_data->gain_loss_account_table,
                                        tip && *tip ? _(tip) : "");

            gtk_grid_attach (GTK_GRID(book_currency_data->gain_loss_account_table), widget_label, 0, 0, 1, 1);

            widget_label = NULL;
            gtk_box_pack_start (GTK_BOX (book_currency_data->book_currency_vbox),
                                book_currency_data->gain_loss_account_table,
                                TRUE, TRUE, 0);
#if GTK_CHECK_VERSION(3,12,0)
            gtk_widget_set_margin_start (GTK_WIDGET(book_currency_data->gain_loss_account_table), 12);
#else
            gtk_widget_set_margin_left (GTK_WIDGET(book_currency_data->gain_loss_account_table), 12);
#endif
            gtk_grid_attach (GTK_GRID(table), book_currency_data->book_currency_vbox, 1, 2, 1, 1);
        }
        else /* trading or neither */
        {
            table = gtk_grid_new ();
            gtk_grid_attach (GTK_GRID(table), widget, 0, 1, 1, 1);

            g_signal_connect(G_OBJECT(widget), "toggled",
                             G_CALLBACK(gnc_option_currency_accounting_non_book_cb),
                             book_currency_data);
        }
        gtk_box_pack_start (GTK_BOX (vbox), table, TRUE, TRUE, 0);

        if (label)
            free (label);
        if (tip)
            free (tip);
    }
    return frame;
}

static void
gnc_option_account_cb(GtkTreeSelection *selection, gpointer data)
{
    GNCOption *option = data;
    GtkTreeView *tree_view;

    tree_view = gtk_tree_selection_get_tree_view(selection);

    gnc_option_changed_widget_cb(GTK_WIDGET(tree_view), option);
}

static void
gnc_option_account_select_all_cb(GtkWidget *widget, gpointer data)
{
    GNCOption *option = data;
    GncTreeViewAccount *tree_view;
    GtkTreeSelection *selection;

    tree_view = GNC_TREE_VIEW_ACCOUNT(gnc_option_get_gtk_widget (option));
    gtk_tree_view_expand_all(GTK_TREE_VIEW(tree_view));
    selection = gtk_tree_view_get_selection(GTK_TREE_VIEW(tree_view));
    gtk_tree_selection_select_all(selection);
    gnc_option_changed_widget_cb(widget, option);
}

static void
gnc_option_account_clear_all_cb(GtkWidget *widget, gpointer data)
{
    GNCOption *option = data;
    GncTreeViewAccount *tree_view;
    GtkTreeSelection *selection;

    tree_view = GNC_TREE_VIEW_ACCOUNT(gnc_option_get_gtk_widget (option));
    selection = gtk_tree_view_get_selection(GTK_TREE_VIEW(tree_view));
    gtk_tree_selection_unselect_all(selection);
    gnc_option_changed_widget_cb(widget, option);
}

static void
gnc_option_account_select_children_cb(GtkWidget *widget, gpointer data)
{
    GNCOption *option = data;
    GncTreeViewAccount *tree_view;
    GList *acct_list = NULL, *acct_iter = NULL;

    tree_view = GNC_TREE_VIEW_ACCOUNT(gnc_option_get_gtk_widget (option));
    acct_list = gnc_tree_view_account_get_selected_accounts (tree_view);

    for (acct_iter = acct_list; acct_iter; acct_iter = acct_iter->next)
        gnc_tree_view_account_select_subaccounts (tree_view, acct_iter->data);

    g_list_free (acct_list);
}

static GtkWidget *
gnc_option_create_account_widget(GNCOption *option, char *name)
{
    gboolean multiple_selection;
    GtkWidget *scroll_win;
    GtkWidget *button;
    GtkWidget *frame;
    GtkWidget *tree;
    GtkWidget *vbox;
    GtkWidget *bbox;
    GList *acct_type_list;
    GtkTreeSelection *selection;

    multiple_selection = gnc_option_multiple_selection(option);
    acct_type_list = gnc_option_get_account_type_list(option);

    frame = gtk_frame_new(name);

    vbox = gtk_box_new (GTK_ORIENTATION_VERTICAL, 0);
    gtk_box_set_homogeneous (GTK_BOX (vbox), FALSE);

    gtk_container_add(GTK_CONTAINER(frame), vbox);

    tree = GTK_WIDGET(gnc_tree_view_account_new (FALSE));
    gtk_tree_view_set_headers_visible (GTK_TREE_VIEW(tree), FALSE);
    selection = gtk_tree_view_get_selection (GTK_TREE_VIEW(tree));
    if (multiple_selection)
        gtk_tree_selection_set_mode (selection, GTK_SELECTION_MULTIPLE);
    else
        gtk_tree_selection_set_mode (selection, GTK_SELECTION_BROWSE);

    if (acct_type_list)
    {
        GList *node;
        AccountViewInfo avi;
        int i;

        gnc_tree_view_account_get_view_info (GNC_TREE_VIEW_ACCOUNT (tree), &avi);

        for (i = 0; i < NUM_ACCOUNT_TYPES; i++)
            avi.include_type[i] = FALSE;
        avi.show_hidden = FALSE;

        for (node = acct_type_list; node; node = node->next)
        {
            GNCAccountType type = GPOINTER_TO_INT (node->data);
            avi.include_type[type] = TRUE;
        }

        gnc_tree_view_account_set_view_info (GNC_TREE_VIEW_ACCOUNT (tree), &avi);
        g_list_free (acct_type_list);
    }
    else
    {
        AccountViewInfo avi;
        int i;

        gnc_tree_view_account_get_view_info (GNC_TREE_VIEW_ACCOUNT (tree), &avi);

        for (i = 0; i < NUM_ACCOUNT_TYPES; i++)
            avi.include_type[i] = TRUE;
        avi.show_hidden = FALSE;
        gnc_tree_view_account_set_view_info (GNC_TREE_VIEW_ACCOUNT (tree), &avi);
    }

    scroll_win = gtk_scrolled_window_new(NULL, NULL);
    gtk_scrolled_window_set_policy(GTK_SCROLLED_WINDOW(scroll_win),
                                   GTK_POLICY_AUTOMATIC,
                                   GTK_POLICY_AUTOMATIC);

    gtk_box_pack_start(GTK_BOX(vbox), scroll_win, TRUE, TRUE, 0);
    gtk_container_set_border_width(GTK_CONTAINER(scroll_win), 5);
    gtk_container_add(GTK_CONTAINER(scroll_win), tree);

    bbox = gtk_button_box_new (GTK_ORIENTATION_HORIZONTAL);
    gtk_button_box_set_layout(GTK_BUTTON_BOX(bbox), GTK_BUTTONBOX_SPREAD);
    gtk_box_pack_start(GTK_BOX(vbox), bbox, FALSE, FALSE, 10);

    if (multiple_selection)
    {
        button = gtk_button_new_with_label(_("Select All"));
        gtk_box_pack_start(GTK_BOX(bbox), button, FALSE, FALSE, 0);
        gtk_widget_set_tooltip_text(button, _("Select all accounts."));

        g_signal_connect(G_OBJECT(button), "clicked",
                         G_CALLBACK(gnc_option_account_select_all_cb), option);

        button = gtk_button_new_with_label(_("Clear All"));
        gtk_box_pack_start(GTK_BOX(bbox), button, FALSE, FALSE, 0);
        gtk_widget_set_tooltip_text(button, _("Clear the selection and unselect all accounts."));

        g_signal_connect(G_OBJECT(button), "clicked",
                         G_CALLBACK(gnc_option_account_clear_all_cb), option);

        button = gtk_button_new_with_label(_("Select Children"));
        gtk_box_pack_start(GTK_BOX(bbox), button, FALSE, FALSE, 0);
        gtk_widget_set_tooltip_text(button, _("Select all descendents of selected account."));

        g_signal_connect(G_OBJECT(button), "clicked",
                         G_CALLBACK(gnc_option_account_select_children_cb), option);
    }

    button = gtk_button_new_with_label(_("Select Default"));
    gtk_box_pack_start(GTK_BOX(bbox), button, FALSE, FALSE, 0);
    gtk_widget_set_tooltip_text(button, _("Select the default account selection."));

    g_signal_connect(G_OBJECT(button), "clicked",
                     G_CALLBACK(gnc_option_default_cb), option);

    if (multiple_selection)
    {
        /* Put the "Show hidden" checkbox on a separate line since the 4 buttons make
           the dialog too wide. */
        bbox = gtk_button_box_new (GTK_ORIENTATION_HORIZONTAL);
        gtk_button_box_set_layout(GTK_BUTTON_BOX(bbox), GTK_BUTTONBOX_START);
        gtk_box_pack_start(GTK_BOX(vbox), bbox, FALSE, FALSE, 0);
    }

    button = gtk_check_button_new_with_label(_("Show Hidden Accounts"));
    gtk_box_pack_start(GTK_BOX(bbox), button, FALSE, FALSE, 0);
    gtk_widget_set_tooltip_text(button, _("Show accounts that have been marked hidden."));
    gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(button), FALSE);
    g_signal_connect(G_OBJECT(button), "toggled",
                     G_CALLBACK(gnc_option_show_hidden_toggled_cb), option);

    gnc_option_set_widget (option, tree);

    return frame;
}

static void
gnc_option_list_changed_cb(GtkTreeSelection *selection,
                           GNCOption *option)
{
    GtkTreeView *view;

    view = gtk_tree_selection_get_tree_view(selection);
    gnc_option_changed_widget_cb(GTK_WIDGET(view), option);
}

static void
gnc_option_list_select_all_cb(GtkWidget *widget, gpointer data)
{
    GNCOption *option = data;
    GtkTreeView *view;
    GtkTreeSelection *selection;

    view = GTK_TREE_VIEW(gnc_option_get_gtk_widget (option));
    selection = gtk_tree_view_get_selection(view);
    gtk_tree_selection_select_all(selection);
    gnc_option_changed_widget_cb(GTK_WIDGET(view), option);
}

static void
gnc_option_list_clear_all_cb(GtkWidget *widget, gpointer data)
{
    GNCOption *option = data;
    GtkTreeView *view;
    GtkTreeSelection *selection;

    view = GTK_TREE_VIEW(gnc_option_get_gtk_widget (option));
    selection = gtk_tree_view_get_selection(view);
    gtk_tree_selection_unselect_all(selection);
    gnc_option_changed_widget_cb(GTK_WIDGET(view), option);
}

static GtkWidget *
gnc_option_create_list_widget(GNCOption *option, char *name)
{
    GtkListStore *store;
    GtkTreeView *view;
    GtkTreeIter iter;
    GtkTreeViewColumn *column;
    GtkCellRenderer *renderer;
    GtkTreeSelection *selection;

    GtkWidget *button;
    GtkWidget *frame;
    GtkWidget *hbox;
    GtkWidget *bbox;
    gint num_values;
    gint i;

    frame = gtk_frame_new(name);
    hbox = gtk_box_new (GTK_ORIENTATION_HORIZONTAL, 0);
    gtk_box_set_homogeneous (GTK_BOX (hbox), FALSE);
    gtk_container_add(GTK_CONTAINER(frame), hbox);

    store = gtk_list_store_new(1, G_TYPE_STRING);
    view = GTK_TREE_VIEW(gtk_tree_view_new_with_model(GTK_TREE_MODEL(store)));
    g_object_unref(store);
    renderer = gtk_cell_renderer_text_new();
    column = gtk_tree_view_column_new_with_attributes("", renderer,
             "text", 0,
             NULL);
    gtk_tree_view_append_column(view, column);
    gtk_tree_view_set_headers_visible(view, FALSE);

    num_values = gnc_option_num_permissible_values(option);
    for (i = 0; i < num_values; i++)
    {
        gchar *raw_string, *string;

        raw_string = gnc_option_permissible_value_name(option, i);
        string = (raw_string && *raw_string) ? _(raw_string) : "";
        gtk_list_store_append(store, &iter);
        gtk_list_store_set(store, &iter,
                           0, string ? string : "",
                           -1);
        g_free(raw_string);
    }

    gtk_box_pack_start(GTK_BOX(hbox), GTK_WIDGET(view), FALSE, FALSE, 0);

    selection = gtk_tree_view_get_selection(view);
    gtk_tree_selection_set_mode(selection, GTK_SELECTION_MULTIPLE);
    g_signal_connect(selection, "changed",
                     G_CALLBACK(gnc_option_list_changed_cb), option);

    bbox = gtk_button_box_new (GTK_ORIENTATION_VERTICAL);
    gtk_button_box_set_layout(GTK_BUTTON_BOX(bbox), GTK_BUTTONBOX_SPREAD);
    gtk_box_pack_start(GTK_BOX(hbox), bbox, FALSE, FALSE, 10);

    button = gtk_button_new_with_label(_("Select All"));
    gtk_box_pack_start(GTK_BOX(bbox), button, FALSE, FALSE, 0);
    gtk_widget_set_tooltip_text(button, _("Select all entries."));

    g_signal_connect(G_OBJECT(button), "clicked",
                     G_CALLBACK(gnc_option_list_select_all_cb), option);

    button = gtk_button_new_with_label(_("Clear All"));
    gtk_box_pack_start(GTK_BOX(bbox), button, FALSE, FALSE, 0);
    gtk_widget_set_tooltip_text(button, _("Clear the selection and unselect all entries."));

    g_signal_connect(G_OBJECT(button), "clicked",
                     G_CALLBACK(gnc_option_list_clear_all_cb), option);

    button = gtk_button_new_with_label(_("Select Default"));
    gtk_box_pack_start(GTK_BOX(bbox), button, FALSE, FALSE, 0);
    gtk_widget_set_tooltip_text(button, _("Select the default selection."));

    g_signal_connect(G_OBJECT(button), "clicked",
                     G_CALLBACK(gnc_option_default_cb), option);

    gnc_option_set_widget (option, GTK_WIDGET(view));

    return frame;
}

static void
gnc_option_color_changed_cb(GtkColorButton *color_button, GNCOption *option)
{
    gnc_option_changed_widget_cb(GTK_WIDGET(color_button), option);
}

static void
gnc_option_font_changed_cb(GtkFontButton *font_button, GNCOption *option)
{
    gnc_option_changed_widget_cb(GTK_WIDGET(font_button), option);
}

static void
gnc_option_set_ui_widget(GNCOption *option,
                         GtkBox *page_box)
{
    GtkWidget *enclosing = NULL;
    GtkWidget *value = NULL;
    gboolean packed = FALSE;
    char *raw_name, *raw_documentation;
    char *name, *documentation;
    char *type;
    GNCOptionDef_t *option_def;

    ENTER("option %p(%s), box %p",
          option, gnc_option_name(option), page_box);
    type = gnc_option_type(option);
    if (type == NULL)
    {
        LEAVE("bad type");
        return;
    }
    else if (g_strcmp0 (type, "internal") == 0)
    {
        LEAVE("internal type");
        return;
    }

    raw_name = gnc_option_name(option);
    if (raw_name && *raw_name)
        name = _(raw_name);
    else
        name = NULL;

    raw_documentation = gnc_option_documentation(option);
    if (raw_documentation && *raw_documentation)
        documentation = _(raw_documentation);
    else
        documentation = NULL;

    option_def = gnc_options_ui_get_option (type);
    if (option_def && option_def->set_widget)
    {
        value = option_def->set_widget (option, page_box,
                                        name, documentation,
                                        /* Return values */
                                        &enclosing, &packed);
    }
    else
    {
        PERR("Unknown option type. Ignoring option \"%s\".\n", name);
    }

    if (!packed && (enclosing != NULL))
    {
        /* Pack option widget into an extra eventbox because otherwise the
           "documentation" tooltip is not displayed. */
        GtkWidget *eventbox = gtk_event_box_new();

        gtk_container_add (GTK_CONTAINER (eventbox), enclosing);

        /* Allow the text widget to expand and fill remaining space */
        if (g_strcmp0 (type, "text") == 0)
            gtk_box_pack_start (page_box, eventbox, TRUE, TRUE, 0);
        else
            gtk_box_pack_start (page_box, eventbox, FALSE, FALSE, 0);

        gtk_widget_set_tooltip_text (eventbox, documentation);
    }

    if (value != NULL)
        gtk_widget_set_tooltip_text(value, documentation);

    if (raw_name != NULL)
        free(raw_name);
    if (raw_documentation != NULL)
        free(raw_documentation);
    free(type);
    LEAVE(" ");
}

static void
gnc_options_dialog_add_option(GtkWidget *page,
                              GNCOption *option)
{
    gnc_option_set_ui_widget(option, GTK_BOX(page));
}

static gint
gnc_options_dialog_append_page(GNCOptionWin * propertybox,
                               GNCOptionSection *section)
{
    GNCOption *option;
    GtkWidget *page_label;
    GtkWidget *options_box;
    GtkWidget *page_content_box;
    GtkWidget* notebook_page;
    GtkWidget *reset_button;
    GtkWidget *listitem = NULL;
    GtkWidget *buttonbox;
    GtkWidget *options_scrolled_win;
    GtkTreeView *view;
    GtkListStore *list;
    GtkTreeIter iter;
    gint num_options;
    const char *name;
    gint i, page_count, name_offset;
    gboolean advanced;

    name = gnc_option_section_name(section);
    if (!name)
        return -1;

    if (strncmp(name, "__", 2) == 0)
        return -1;
    advanced = (strncmp(name, "_+", 2) == 0);
    name_offset = (advanced) ? 2 : 0;
    page_label = gtk_label_new(_(name + name_offset));
    PINFO("Page_label is %s", _(name + name_offset));
    gtk_widget_show(page_label);

    /* Build this options page */
    page_content_box = gtk_box_new (GTK_ORIENTATION_VERTICAL, 2);
    gtk_box_set_homogeneous (GTK_BOX (page_content_box), FALSE);

    gtk_container_set_border_width(GTK_CONTAINER(page_content_box), 12);

    options_scrolled_win = gtk_scrolled_window_new(NULL, NULL);
    gtk_box_pack_start(GTK_BOX(page_content_box), options_scrolled_win, TRUE, TRUE, 0);

    /* Build space for the content - the options box */
    options_box = gtk_box_new (GTK_ORIENTATION_VERTICAL, 5);
    gtk_box_set_homogeneous (GTK_BOX (options_box), FALSE);

    gtk_container_set_border_width(GTK_CONTAINER(options_box), 0);
    gtk_container_add (GTK_CONTAINER(options_scrolled_win), GTK_WIDGET(options_box));
    gtk_scrolled_window_set_policy(GTK_SCROLLED_WINDOW(options_scrolled_win), GTK_POLICY_NEVER, GTK_POLICY_AUTOMATIC);

    /* Create all the options */
    num_options = gnc_option_section_num_options(section);
    for (i = 0; i < num_options; i++)
    {
        option = gnc_get_option_section_option(section, i);
        gnc_options_dialog_add_option(options_box, option);
    }

    /* Add a button box at the bottom of the page */
    buttonbox = gtk_button_box_new (GTK_ORIENTATION_HORIZONTAL);
    gtk_button_box_set_layout (GTK_BUTTON_BOX (buttonbox),
                               GTK_BUTTONBOX_EDGE);
    gtk_container_set_border_width(GTK_CONTAINER (buttonbox), 5);
    gtk_box_pack_end(GTK_BOX(page_content_box), buttonbox, FALSE, FALSE, 0);

    /* The reset button on each option page */
    reset_button = gtk_button_new_with_label (_("Reset defaults"));
    gtk_widget_set_tooltip_text(reset_button,
                                _("Reset all values to their defaults."));

    g_signal_connect(G_OBJECT(reset_button), "clicked",
                     G_CALLBACK(gnc_options_dialog_reset_cb), propertybox);
    g_object_set_data(G_OBJECT(reset_button), "section", section);
    gtk_box_pack_end(GTK_BOX(buttonbox), reset_button, FALSE, FALSE, 0);
    gtk_widget_show_all(page_content_box);
    gtk_notebook_append_page(GTK_NOTEBOOK(propertybox->notebook),
                             page_content_box, page_label);

    /* Switch to selection from a list if the page count threshold is reached */
    page_count = gtk_notebook_page_num(GTK_NOTEBOOK(propertybox->notebook),
                                       page_content_box);

    if (propertybox->page_list_view)
    {
        /* Build the matching list item for selecting from large page sets */
        view = GTK_TREE_VIEW(propertybox->page_list_view);
        list = GTK_LIST_STORE(gtk_tree_view_get_model(view));

        PINFO("Page name is %s and page_count is %d", name, page_count);
        gtk_list_store_append(list, &iter);
        gtk_list_store_set(list, &iter,
                           PAGE_NAME, _(name),
                           PAGE_INDEX, page_count,
                           -1);

        if (page_count > MAX_TAB_COUNT - 1)   /* Convert 1-based -> 0-based */
        {
            gtk_widget_show(propertybox->page_list);
            gtk_notebook_set_show_tabs(GTK_NOTEBOOK(propertybox->notebook), FALSE);
            gtk_notebook_set_show_border(GTK_NOTEBOOK(propertybox->notebook), FALSE);
        }
        else
            gtk_widget_hide(propertybox->page_list);

        /* Tweak "advanced" pages for later handling. */
        if (advanced)
        {
            notebook_page = gtk_notebook_get_nth_page(GTK_NOTEBOOK(propertybox->notebook),
                            page_count);

            g_object_set_data(G_OBJECT(notebook_page), "listitem", listitem);
            g_object_set_data(G_OBJECT(notebook_page), "advanced",
                              GINT_TO_POINTER(advanced));
        }
    }
    return(page_count);
}

/********************************************************************\
 * gnc_options_dialog_build_contents                                *
 *   builds an options dialog given a property box and an options   *
 *   database and make the dialog visible                           *
 *                                                                  *
 * Args: propertybox - gnome property box to use                    *
 *       odb         - option database to use                       *
 * Return: nothing                                                  *
\********************************************************************/
void
gnc_options_dialog_build_contents (GNCOptionWin *propertybox,
                                   GNCOptionDB  *odb)
{
    gnc_options_dialog_build_contents_full (propertybox, odb, TRUE);
}

/********************************************************************\
 * gnc_options_dialog_build_contents_full                           *
 *   builds an options dialog given a property box and an options   *
 *   database and make the dialog visible depending on the          *
 *   show_dialog flag                                               *
 *                                                                  *
 * Args: propertybox - gnome property box to use                    *
 *       odb         - option database to use                       *
 *       show_dialog - should dialog be made visible or not         *
 * Return: nothing                                                  *
\********************************************************************/
void
gnc_options_dialog_build_contents_full (GNCOptionWin *propertybox,
                                        GNCOptionDB  *odb, gboolean show_dialog)
{
    GNCOptionSection *section;
    gchar *default_section_name;
    gint default_page = -1;
    gint num_sections;
    gint page;
    gint i;
    guint j;

    g_return_if_fail (propertybox != NULL);
    g_return_if_fail (odb != NULL);

    gnc_option_db_set_ui_callbacks (odb,
                                    gnc_option_get_ui_value_internal,
                                    gnc_option_set_ui_value_internal,
                                    gnc_option_set_selectable_internal);

    propertybox->option_db = odb;

    num_sections = gnc_option_db_num_sections(odb);
    default_section_name = gnc_option_db_get_default_section(odb);

    PINFO("Default Section name is %s", default_section_name);

    for (i = 0; i < num_sections; i++)
    {
        const char *section_name;

        section = gnc_option_db_get_section(odb, i);
        page = gnc_options_dialog_append_page(propertybox, section);

        section_name = gnc_option_section_name(section);
        if (g_strcmp0(section_name, default_section_name) == 0)
            default_page = page;
    }

    if (default_section_name != NULL)
        free(default_section_name);

    /* call each option widget changed callbacks once at this point,
     * now that all options widgets exist.
     */
    for (i = 0; i < num_sections; i++)
    {
        section = gnc_option_db_get_section(odb, i);

        for (j = 0; j < gnc_option_section_num_options(section); j++)
        {
            gnc_option_call_option_widget_changed_proc(
                gnc_get_option_section_option(section, j) );
        }
    }

    gtk_notebook_popup_enable(GTK_NOTEBOOK(propertybox->notebook));
    if (default_page >= 0)
    {
        /* Find the page list and set the selection to the default page */
        GtkTreeSelection* selection = gtk_tree_view_get_selection(GTK_TREE_VIEW(propertybox->page_list_view));
        GtkTreeIter iter;
        GtkTreeModel *model;

        model = gtk_tree_view_get_model(GTK_TREE_VIEW(propertybox->page_list_view));
        gtk_tree_model_iter_nth_child(model, &iter, NULL, default_page);
        gtk_tree_selection_select_iter (selection, &iter);
        gtk_notebook_set_current_page(GTK_NOTEBOOK(propertybox->notebook), default_page);
    }
    gnc_options_dialog_changed_internal(propertybox->window, FALSE);
    if (show_dialog)
        gtk_widget_show(propertybox->window);
}

GtkWidget *
gnc_options_dialog_widget(GNCOptionWin * win)
{
    return win->window;
}

GtkWidget *
gnc_options_page_list(GNCOptionWin * win)
{
    return win->page_list;
}

GtkWidget *
gnc_options_dialog_notebook(GNCOptionWin * win)
{
    return win->notebook;
}

static void
gnc_options_dialog_help_button_cb(GtkWidget * widget, GNCOptionWin *win)
{
    if (win->help_cb)
        (win->help_cb)(win, win->help_cb_data);
}

static void
gnc_options_dialog_cancel_button_cb(GtkWidget * widget, GNCOptionWin *win)
{
    gnc_save_window_size (GNC_PREFS_GROUP, GTK_WINDOW(win->window));

    if (win->close_cb)
        (win->close_cb)(win, win->close_cb_data);
    else
        gtk_widget_hide(win->window);
}

static void
gnc_options_dialog_apply_button_cb(GtkWidget * widget, GNCOptionWin *win)
{
    GNCOptionWinCallback close_cb = win->close_cb;

    win->close_cb = NULL;
    if (win->apply_cb)
        win->apply_cb (win, win->apply_cb_data);
    win->close_cb = close_cb;
    gnc_save_window_size (GNC_PREFS_GROUP, GTK_WINDOW(win->window));
    gnc_options_dialog_changed_internal (win->window, FALSE);
}

static void
gnc_options_dialog_ok_button_cb(GtkWidget * widget, GNCOptionWin *win)
{
    GNCOptionWinCallback close_cb = win->close_cb;

    win->close_cb = NULL;
    if (win->apply_cb)
        win->apply_cb (win, win->apply_cb_data);
    win->close_cb = close_cb;

    gnc_save_window_size (GNC_PREFS_GROUP, GTK_WINDOW(win->window));

    if (win->close_cb)
        (win->close_cb)(win, win->close_cb_data);
    else
        gtk_widget_hide(win->window);
}

static void
gnc_options_dialog_destroy_cb (GtkWidget *object, GNCOptionWin *win)
{
    if (!win) return;

    if (win->destroyed == FALSE)
    {
        if (win->close_cb)
            (win->close_cb)(win, win->close_cb_data);
    }
}

static gboolean
gnc_options_dialog_window_key_press_cb(GtkWidget *widget, GdkEventKey *event, gpointer data)
{
    GNCOptionWin *win = data;

    if (event->keyval == GDK_KEY_Escape)
    {
        component_close_handler (win);
        return TRUE;
    }
    else
        return FALSE;
}

static void
gnc_options_dialog_reset_cb(GtkWidget * w, gpointer data)
{
    GNCOptionWin *win = data;
    GNCOptionSection *section;
    gpointer val;

    val = g_object_get_data(G_OBJECT(w), "section");
    g_return_if_fail (val);
    g_return_if_fail (win);

    section = (GNCOptionSection*)val;
    gnc_option_db_section_reset_widgets (section);
    gnc_options_dialog_changed_internal (win->window, TRUE);
}

void
gnc_options_dialog_list_select_cb (GtkTreeSelection *selection,
                                   gpointer data)
{
    GNCOptionWin * win = data;
    GtkTreeModel *list;
    GtkTreeIter iter;
    gint index = 0;

    if (!gtk_tree_selection_get_selected(selection, &list, &iter))
        return;
    gtk_tree_model_get(list, &iter,
                       PAGE_INDEX, &index,
                       -1);
    PINFO("Index is %d", index);
    gtk_notebook_set_current_page(GTK_NOTEBOOK(win->notebook), index);
}

void
gnc_options_register_stocks (void)
{
#if 0
    static gboolean done = FALSE;

    GtkStockItem items[] =
    {
        { GTK_STOCK_APPLY       , "gnc_option_apply_button",    0, 0, NULL },
        { GTK_STOCK_HELP        , "gnc_options_dialog_help",    0, 0, NULL },
        { GTK_STOCK_OK          , "gnc_options_dialog_ok",      0, 0, NULL },
        { GTK_STOCK_CANCEL      , "gnc_options_dialog_cancel",  0, 0, NULL },
    };

    if (done)
    {
        return;
    }
    done = TRUE;

    gtk_stock_add (items, G_N_ELEMENTS (items));
#endif
}

static void
component_close_handler (gpointer data)
{
    GNCOptionWin *win = data;
    gnc_save_window_size (GNC_PREFS_GROUP, GTK_WINDOW(win->window));
    gnc_options_dialog_cancel_button_cb (NULL, win);
}

static void
refresh_handler (GHashTable *changes, gpointer user_data)
{
    gnc_commodity *commodity = NULL;
    GtkTreeIter iter;

    /* The default_gain_loss_account_widget needs to be refreshed if any
       changes have been made via account maintenance, if it exists and
       if the book currency widget has a selection */
/*    if (book_currency_data->default_gain_loss_account_widget &&
        gtk_combo_box_get_active_iter(
            GTK_COMBO_BOX(book_currency_data->book_currency_widget), &iter))
    {
        commodity = gnc_currency_edit_get_currency(
                                GNC_CURRENCY_EDIT(
                                    book_currency_data->book_currency_widget));
        gnc_set_default_gain_loss_account_widget(commodity);
        gtk_widget_show_all(book_currency_data->book_currency_vbox);
    } */
}

/* gnc_options_dialog_new:
 *
 *   - Opens the dialog-options glade file
 *   - Connects signals specified in the builder file
 *   - Sets the window's title
 *   - Initializes a new GtkNotebook, and adds it to the window
 *
 */
GNCOptionWin *
gnc_options_dialog_new(gchar *title, GtkWindow *parent)
{
    return gnc_options_dialog_new_modal(FALSE, title, NULL, parent);
}

/* gnc_options_dialog_new_modal:
 *
 *   - Opens the dialog-options glade file
 *   - Connects signals specified in the builder file
 *   - Sets the window's title
 *   - Initializes a new GtkNotebook, and adds it to the window
 *   - If modal TRUE, hides 'apply' button
 *   - If component_class is provided, it is used, otherwise,
 *     DIALOG_OPTIONS_CM_CLASS is used; this is used to distinguish the
 *     book-option dialog from report dialogs. The book-option dialog is a
 *     singleton, so if a dialog already exists it will be raised to the top of
 *     the window stack instead of creating a new dialog.
 */
GNCOptionWin *
gnc_options_dialog_new_modal(gboolean modal, gchar *title,
                             const char *component_class,
                             GtkWindow *parent)
{
    GNCOptionWin *retval;
    GtkBuilder   *builder;
    GtkWidget    *hbox;
    gint component_id;
    GtkWidget    *button;

    retval = g_new0(GNCOptionWin, 1);
    builder = gtk_builder_new();
    gnc_builder_add_from_file (builder, "dialog-options.glade", "gnucash_options_window");
    retval->window = GTK_WIDGET(gtk_builder_get_object (builder, "gnucash_options_window"));
    retval->page_list = GTK_WIDGET(gtk_builder_get_object (builder, "page_list_scroll"));

    // Set the style context for this dialog so it can be easily manipulated with css
    gnc_widget_set_style_context (GTK_WIDGET(retval->window), "GncOptionsDialog");

    /* Page List */
    {
        GtkTreeView *view;
        GtkListStore *store;
        GtkTreeSelection *selection;
        GtkCellRenderer *renderer;
        GtkTreeViewColumn *column;

        retval->page_list_view = GTK_WIDGET(gtk_builder_get_object (builder, "page_list_treeview"));

        view = GTK_TREE_VIEW(retval->page_list_view);

        store = gtk_list_store_new(NUM_COLUMNS, G_TYPE_INT, G_TYPE_STRING);
        gtk_tree_view_set_model(view, GTK_TREE_MODEL(store));
        g_object_unref(store);

        renderer = gtk_cell_renderer_text_new();
        column = gtk_tree_view_column_new_with_attributes(_("Page"), renderer,
                 "text", PAGE_NAME, NULL);
        gtk_tree_view_append_column(view, column);

        gtk_tree_view_column_set_alignment(column, 0.5);

        selection = gtk_tree_view_get_selection(view);
        gtk_tree_selection_set_mode(selection, GTK_SELECTION_BROWSE);
        g_signal_connect (selection, "changed",
                          G_CALLBACK (gnc_options_dialog_list_select_cb), retval);
    }

    button = GTK_WIDGET(gtk_builder_get_object (builder, "helpbutton"));
        g_signal_connect(button, "clicked", G_CALLBACK(gnc_options_dialog_help_button_cb), retval);
    button = GTK_WIDGET(gtk_builder_get_object (builder, "cancelbutton"));
        g_signal_connect(button, "clicked", G_CALLBACK(gnc_options_dialog_cancel_button_cb), retval);
    button = GTK_WIDGET(gtk_builder_get_object (builder, "applybutton"));
        g_signal_connect(button, "clicked", G_CALLBACK(gnc_options_dialog_apply_button_cb), retval);
    button = GTK_WIDGET(gtk_builder_get_object (builder, "okbutton"));
        g_signal_connect(button, "clicked", G_CALLBACK(gnc_options_dialog_ok_button_cb), retval);

    gtk_builder_connect_signals_full (builder, gnc_builder_connect_full_func, retval);

    gnc_restore_window_size (GNC_PREFS_GROUP, GTK_WINDOW(retval->window), parent);

    if (title)
        gtk_window_set_title(GTK_WINDOW(retval->window), title);

    /* modal */
    if (modal == TRUE)
    {
        GtkWidget *apply_button;

        apply_button = GTK_WIDGET(gtk_builder_get_object (builder, "applybutton"));
        gtk_widget_hide (apply_button);
    }

    /* glade doesn't support a notebook with zero pages */
    hbox = GTK_WIDGET(gtk_builder_get_object (builder, "notebook_placeholder"));
    retval->notebook = gtk_notebook_new();

    gtk_widget_set_vexpand (retval->notebook, TRUE);

    gtk_widget_show(retval->notebook);
    gtk_box_pack_start(GTK_BOX(hbox), retval->notebook, TRUE, TRUE, 5);

    retval->component_class =
                (component_class ? component_class : DIALOG_OPTIONS_CM_CLASS);
    component_id = gnc_register_gui_component (retval->component_class,
                    refresh_handler, component_close_handler,
                    retval);
    gnc_gui_component_set_session (component_id, gnc_get_current_session());

    /* Watch account maintenance events only if book option dialog */
    if (g_strcmp0(retval->component_class, DIALOG_BOOK_OPTIONS_CM_CLASS) == 0)
    {
        gnc_gui_component_watch_entity_type (component_id,
                                         GNC_ID_ACCOUNT,
                                         QOF_EVENT_MODIFY | QOF_EVENT_DESTROY);
    }

    g_signal_connect (retval->window, "destroy",
                      G_CALLBACK(gnc_options_dialog_destroy_cb), retval);

    g_signal_connect (retval->window, "key_press_event",
                      G_CALLBACK(gnc_options_dialog_window_key_press_cb), retval);

    g_object_unref(G_OBJECT(builder));

    retval->destroyed = FALSE;
    return retval;
}

/* Creates a new GNCOptionWin structure, but assumes you have your own
   dialog widget you want to plugin */
GNCOptionWin *
gnc_options_dialog_new_w_dialog(gchar *title, GtkWidget *window)
{
    GNCOptionWin * retval;

    retval = g_new0(GNCOptionWin, 1);
    retval->window = window;
    return retval;
}

void
gnc_options_dialog_set_apply_cb(GNCOptionWin * win, GNCOptionWinCallback cb,
                                gpointer data)
{
    win->apply_cb = cb;
    win->apply_cb_data = data;
}

void
gnc_options_dialog_set_help_cb(GNCOptionWin * win, GNCOptionWinCallback cb,
                               gpointer data)
{
    win->help_cb = cb;
    win->help_cb_data = data;
}

void
gnc_options_dialog_set_close_cb(GNCOptionWin * win, GNCOptionWinCallback cb,
                                gpointer data)
{
    win->close_cb = cb;
    win->close_cb_data = data;
}

void
gnc_options_dialog_set_global_help_cb(GNCOptionWinCallback thunk,
                                      gpointer cb_data)
{
    global_help_cb = thunk;
    global_help_cb_data = cb_data;
}

/* This is for global program preferences. */
void
gnc_options_dialog_destroy(GNCOptionWin * win)
{
    if (!win) return;

    gnc_unregister_gui_component_by_data(win->component_class, win);

    win->destroyed = TRUE;
    gtk_widget_destroy(win->window);

    win->window = NULL;
    win->notebook = NULL;
    win->apply_cb = NULL;
    win->help_cb = NULL;
    win->component_class = NULL;

    g_free(win);
}

/*****************************************************************/
/* Option Registration                                           */

/*************************
 *       SET WIDGET      *
 *************************
 *
 * gnc_option_set_ui_widget_<type>():
 *
 * You should create the widget representation for the option type,
 * and set the top-level container widget for your control in
 * *enclosing.  If you want to pack the widget into the page yourself,
 * then you may -- just set *packed to TRUE.  Otherwise, the widget
 * you return in *enclosing will be packed for you.  (*packed is
 * initialized to FALSE, so if you're not setting it to TRUE, you
 * don't have to touch it at all.)
 *
 * If you need to initialize the state of your control or to connect
 * any signals to you widgets, then you should do so in this function.
 * If you want to create a label for the widget you should use 'name'
 * for the label text.
 *
 * Somewhere in this function, you should also call
 * gnc_option_set_widget(option, value); where 'value' is the
 * GtkWidget you will actually store the value in.
 *
 * Also call gnc_option_set_ui_value(option, FALSE);
 *
 * You probably want to end with something like:
 *   gtk_widget_show_all(*enclosing);
 *
 * If you can detect state changes for your widget's value, you should also
 * gnc_option_changed_widget_cb() upon changes.
 *
 * The widget you return from this function should be the widget in
 * which you're storing the option value.
 */
static GtkWidget *
gnc_option_set_ui_widget_boolean (GNCOption *option, GtkBox *page_box,
                                  char *name, char *documentation,
                                  /* Return values */
                                  GtkWidget **enclosing, gboolean *packed)
{
    GtkWidget *value;

    *enclosing = gtk_box_new (GTK_ORIENTATION_HORIZONTAL, 5);
    gtk_box_set_homogeneous (GTK_BOX (*enclosing), FALSE);
    value = gtk_check_button_new_with_label(name);

    gnc_option_set_widget (option, value);
    gnc_option_set_ui_value(option, FALSE);

    g_signal_connect(G_OBJECT(value), "toggled",
                     G_CALLBACK(gnc_option_changed_widget_cb), option);

    gtk_box_pack_start(GTK_BOX(*enclosing), value, FALSE, FALSE, 0);
    gtk_widget_show_all(*enclosing);

    return value;
}

static GtkWidget *
gnc_option_set_ui_widget_string (GNCOption *option, GtkBox *page_box,
                                 char *name, char *documentation,
                                 /* Return values */
                                 GtkWidget **enclosing, gboolean *packed)
{
    GtkWidget *value;
    GtkWidget *label;

    label = gtk_label_new(name);
    gnc_label_set_alignment(label, 1.0, 0.5);

    *enclosing = gtk_box_new (GTK_ORIENTATION_HORIZONTAL, 5);
    gtk_box_set_homogeneous (GTK_BOX (*enclosing), FALSE);
    value = gtk_entry_new();

    gnc_option_set_widget (option, value);
    gnc_option_set_ui_value(option, FALSE);

    g_signal_connect(G_OBJECT(value), "changed",
                     G_CALLBACK(gnc_option_changed_widget_cb), option);

    gtk_box_pack_start(GTK_BOX(*enclosing), label, FALSE, FALSE, 0);
    gtk_box_pack_start(GTK_BOX(*enclosing), value, TRUE, TRUE, 0);
    gtk_widget_show_all(*enclosing);
    return value;
}

static GtkWidget *
gnc_option_set_ui_widget_text (GNCOption *option, GtkBox *page_box,
                               char *name, char *documentation,
                               /* Return values */
                               GtkWidget **enclosing, gboolean *packed)
{
    GtkWidget *value;
    GtkWidget *frame;
    GtkWidget *scroll;
    GtkTextBuffer* text_buffer;

    frame = gtk_frame_new(name);

    scroll = gtk_scrolled_window_new(NULL, NULL);
    gtk_scrolled_window_set_policy(GTK_SCROLLED_WINDOW(scroll),
                                   GTK_POLICY_NEVER,
                                   GTK_POLICY_AUTOMATIC);
    gtk_container_set_border_width(GTK_CONTAINER(scroll), 2);

    gtk_container_add(GTK_CONTAINER(frame), scroll);

    *enclosing = gtk_box_new (GTK_ORIENTATION_HORIZONTAL, 10);
    gtk_box_set_homogeneous (GTK_BOX (*enclosing), FALSE);
    value = gtk_text_view_new();
    gtk_text_view_set_wrap_mode(GTK_TEXT_VIEW(value), GTK_WRAP_WORD);
    gtk_text_view_set_editable(GTK_TEXT_VIEW(value), TRUE);
    gtk_text_view_set_accepts_tab (GTK_TEXT_VIEW(value), FALSE);
    gtk_container_add (GTK_CONTAINER (scroll), value);

    gnc_option_set_widget (option, value);
    gnc_option_set_ui_value(option, FALSE);

    text_buffer = gtk_text_view_get_buffer(GTK_TEXT_VIEW(value));
    g_signal_connect(G_OBJECT(text_buffer), "changed",
                     G_CALLBACK(gnc_option_changed_option_cb), option);

    gtk_box_pack_start(GTK_BOX(*enclosing), frame, TRUE, TRUE, 0);
    gtk_widget_show_all(*enclosing);
    return value;
}

static GtkWidget *
gnc_option_set_ui_widget_currency (GNCOption *option, GtkBox *page_box,
                                   char *name, char *documentation,
                                   /* Return values */
                                   GtkWidget **enclosing, gboolean *packed)
{
    GtkWidget *value;
    GtkWidget *label;

    label = gtk_label_new(name);
    gnc_label_set_alignment(label, 1.0, 0.5);

    *enclosing = gtk_box_new (GTK_ORIENTATION_HORIZONTAL, 5);
    gtk_box_set_homogeneous (GTK_BOX (*enclosing), FALSE);
    value = gnc_currency_edit_new();

    gnc_option_set_widget (option, value);
    gnc_option_set_ui_value(option, FALSE);

    g_signal_connect(G_OBJECT(value), "changed",
                     G_CALLBACK(gnc_option_changed_widget_cb), option);

    gtk_box_pack_start(GTK_BOX(*enclosing), label, FALSE, FALSE, 0);
    gtk_box_pack_start(GTK_BOX(*enclosing), value, FALSE, FALSE, 0);
    gtk_widget_show_all(*enclosing);
    return value;
}

static GtkWidget *
gnc_option_set_ui_widget_commodity (GNCOption *option, GtkBox *page_box,
                                    char *name, char *documentation,
                                    /* Return values */
                                    GtkWidget **enclosing, gboolean *packed)
{
    GtkWidget *value;
    GtkWidget *label;

    label = gtk_label_new(name);
    gnc_label_set_alignment(label, 1.0, 0.5);

    *enclosing = gtk_box_new (GTK_ORIENTATION_HORIZONTAL, 5);
    gtk_box_set_homogeneous (GTK_BOX (*enclosing), FALSE);
    value = gnc_general_select_new(GNC_GENERAL_SELECT_TYPE_SELECT,
                                   gnc_commodity_edit_get_string,
                                   gnc_commodity_edit_new_select,
                                   NULL);

    gnc_option_set_widget (option, value);
    gnc_option_set_ui_value(option, FALSE);

    if (documentation != NULL)
        gtk_widget_set_tooltip_text(GNC_GENERAL_SELECT(value)->entry,
                                    documentation);

    g_signal_connect(G_OBJECT(GNC_GENERAL_SELECT(value)->entry), "changed",
                     G_CALLBACK(gnc_option_changed_widget_cb), option);

    gtk_box_pack_start(GTK_BOX(*enclosing), label, FALSE, FALSE, 0);
    gtk_box_pack_start(GTK_BOX(*enclosing), value, FALSE, FALSE, 0);
    gtk_widget_show_all(*enclosing);
    return value;
}

static GtkWidget *
gnc_option_set_ui_widget_multichoice (GNCOption *option, GtkBox *page_box,
                                      char *name, char *documentation,
                                      /* Return values */
                                      GtkWidget **enclosing, gboolean *packed)
{
    GtkWidget *value;
    GtkWidget *label;

    label = gtk_label_new(name);
    gnc_label_set_alignment(label, 1.0, 0.5);

    *enclosing = gtk_box_new (GTK_ORIENTATION_HORIZONTAL, 5);
    gtk_box_set_homogeneous (GTK_BOX (*enclosing), FALSE);

    value = gnc_option_create_multichoice_widget(option);
    gnc_option_set_widget (option, value);

    gnc_option_set_ui_value(option, FALSE);
    gtk_box_pack_start(GTK_BOX(*enclosing), label, FALSE, FALSE, 0);
    gtk_box_pack_start(GTK_BOX(*enclosing), value, FALSE, FALSE, 0);
    gtk_widget_show_all(*enclosing);
    return value;
}

static GtkWidget *
gnc_option_set_ui_widget_date (GNCOption *option, GtkBox *page_box,
                               char *name, char *documentation,
                               /* Return values */
                               GtkWidget **enclosing, gboolean *packed)
{
    GtkWidget *value;
    GtkWidget *label;
    gchar *colon_name;
    GtkWidget *eventbox;

    label = gtk_label_new(name);
    gnc_label_set_alignment(label, 1.0, 0.5);

    *enclosing = gtk_box_new (GTK_ORIENTATION_HORIZONTAL, 5);
    gtk_box_set_homogeneous (GTK_BOX (*enclosing), FALSE);

    value = gnc_option_create_date_widget(option);

    gnc_option_set_widget (option, value);

    gtk_box_pack_start(GTK_BOX(*enclosing), label, FALSE, FALSE, 0);
    gtk_box_pack_start(GTK_BOX(*enclosing), value, FALSE, FALSE, 0);

    /* Pack option widget into an extra eventbox because otherwise the
       "documentation" tooltip is not displayed. */
    eventbox = gtk_event_box_new();
    gtk_container_add (GTK_CONTAINER (eventbox), *enclosing);
    gtk_box_pack_start(page_box, eventbox, FALSE, FALSE, 5);
    *packed = TRUE;

    gtk_widget_set_tooltip_text (eventbox, documentation);

    gnc_option_set_ui_value(option, FALSE);
    gtk_widget_show_all(*enclosing);
    return value;
}

static GtkWidget *
gnc_option_set_ui_widget_account_list (GNCOption *option, GtkBox *page_box,
                                       char *name, char *documentation,
                                       /* Return values */
                                       GtkWidget **enclosing, gboolean *packed)
{
    GtkWidget *value;
    GtkTreeSelection *selection;

    *enclosing = gnc_option_create_account_widget(option, name);
    value = gnc_option_get_gtk_widget (option);

    gtk_widget_set_tooltip_text(*enclosing, documentation);

    gtk_box_pack_start(page_box, *enclosing, TRUE, TRUE, 5);
    *packed = TRUE;

    //gtk_widget_realize(value);

    gnc_option_set_ui_value(option, FALSE);

    selection = gtk_tree_view_get_selection(GTK_TREE_VIEW(value));
    g_signal_connect(G_OBJECT(selection), "changed",
                     G_CALLBACK(gnc_option_account_cb), option);

    //  gtk_clist_set_row_height(GTK_CLIST(value), 0);
    //  gtk_widget_set_size_request(value, -1, GTK_CLIST(value)->row_height * 10);
    gtk_widget_show_all(*enclosing);
    return value;
}

static GtkWidget *
gnc_option_set_ui_widget_account_sel (GNCOption *option, GtkBox *page_box,
                                      char *name, char *documentation,
                                      /* Return values */
                                      GtkWidget **enclosing, gboolean *packed)
{
    GtkWidget *value;
    GtkWidget *label;
    GList *acct_type_list;

    label = gtk_label_new(name);
    gnc_label_set_alignment(label, 1.0, 0.5);

    acct_type_list = gnc_option_get_account_type_list(option);
    value = gnc_account_sel_new();
    gnc_account_sel_set_acct_filters(GNC_ACCOUNT_SEL(value), acct_type_list, NULL);

    g_signal_connect(value, "account_sel_changed",
                     G_CALLBACK(gnc_option_changed_widget_cb), option);

    gnc_option_set_widget (option, value);

    gnc_option_set_ui_value(option, FALSE);

    *enclosing = gtk_box_new (GTK_ORIENTATION_HORIZONTAL, 5);
    gtk_box_set_homogeneous (GTK_BOX (*enclosing), FALSE);
    gtk_box_pack_start(GTK_BOX(*enclosing), label, FALSE, FALSE, 0);
    gtk_box_pack_start(GTK_BOX(*enclosing), value, FALSE, FALSE, 0);
    gtk_widget_show_all(*enclosing);
    return value;
}

static GtkWidget *
gnc_option_set_ui_widget_list (GNCOption *option, GtkBox *page_box,
                               char *name, char *documentation,
                               /* Return values */
                               GtkWidget **enclosing, gboolean *packed)
{
    GtkWidget *value;
    GtkWidget *eventbox;

    *enclosing = gnc_option_create_list_widget(option, name);
    value = gnc_option_get_gtk_widget (option);

    /* Pack option widget into an extra eventbox because otherwise the
       "documentation" tooltip is not displayed. */
    eventbox = gtk_event_box_new();
    gtk_container_add (GTK_CONTAINER (eventbox), *enclosing);
    gtk_box_pack_start(page_box, eventbox, FALSE, FALSE, 5);
    *packed = TRUE;

    gtk_widget_set_tooltip_text(eventbox, documentation);

    gnc_option_set_ui_value(option, FALSE);
    gtk_widget_show(*enclosing);
    return value;
}

static GtkWidget *
gnc_option_set_ui_widget_number_range (GNCOption *option, GtkBox *page_box,
                                       char *name, char *documentation,
                                       /* Return values */
                                       GtkWidget **enclosing, gboolean *packed)
{
    GtkWidget *value;
    GtkWidget *label;
    GtkAdjustment *adj;
    gdouble lower_bound = G_MINDOUBLE;
    gdouble upper_bound = G_MAXDOUBLE;
    gdouble step_size = 1.0;
    int num_decimals = 0;

    label = gtk_label_new(name);
    gnc_label_set_alignment(label, 1.0, 0.5);

    *enclosing = gtk_box_new (GTK_ORIENTATION_HORIZONTAL, 5);
    gtk_box_set_homogeneous (GTK_BOX (*enclosing), FALSE);

    gnc_option_get_range_info(option, &lower_bound, &upper_bound,
                              &num_decimals, &step_size);
    adj = GTK_ADJUSTMENT(gtk_adjustment_new(lower_bound, lower_bound,
                                            upper_bound, step_size,
                                            step_size * 5.0,
                                            0));
    value = gtk_spin_button_new(adj, step_size, num_decimals);
    gtk_spin_button_set_numeric(GTK_SPIN_BUTTON(value), TRUE);

    {
        gdouble biggest;
        gint num_digits;

        biggest = ABS(lower_bound);
        biggest = MAX(biggest, ABS(upper_bound));

        num_digits = 0;
        while (biggest >= 1)
        {
            num_digits++;
            biggest = biggest / 10;
        }

        if (num_digits == 0)
            num_digits = 1;

        num_digits += num_decimals;

        gtk_entry_set_width_chars(GTK_ENTRY(value), num_digits);
    }

    gnc_option_set_widget (option, value);
    gnc_option_set_ui_value(option, FALSE);

    g_signal_connect(G_OBJECT(value), "changed",
                     G_CALLBACK(gnc_option_changed_widget_cb), option);

    gtk_box_pack_start(GTK_BOX(*enclosing), label, FALSE, FALSE, 0);
    gtk_box_pack_start(GTK_BOX(*enclosing), value, FALSE, FALSE, 0);
    gtk_widget_show_all(*enclosing);
    return value;
}

static GtkWidget *
gnc_option_set_ui_widget_color (GNCOption *option, GtkBox *page_box,
                                char *name, char *documentation,
                                /* Return values */
                                GtkWidget **enclosing, gboolean *packed)
{
    GtkWidget *value;
    GtkWidget *label;
    gboolean use_alpha;

    label = gtk_label_new(name);
    gnc_label_set_alignment(label, 1.0, 0.5);

    *enclosing = gtk_box_new (GTK_ORIENTATION_HORIZONTAL, 5);
    gtk_box_set_homogeneous (GTK_BOX (*enclosing), FALSE);

    use_alpha = gnc_option_use_alpha(option);

    value = gtk_color_button_new();
    gtk_color_button_set_title(GTK_COLOR_BUTTON(value), name);
    gtk_color_chooser_set_use_alpha(GTK_COLOR_CHOOSER(value), use_alpha);

    gnc_option_set_widget (option, value);
    gnc_option_set_ui_value(option, FALSE);

    g_signal_connect(G_OBJECT(value), "color-set",
                     G_CALLBACK(gnc_option_color_changed_cb), option);

    gtk_box_pack_start(GTK_BOX(*enclosing), label, FALSE, FALSE, 0);
    gtk_box_pack_start(GTK_BOX(*enclosing), value, FALSE, FALSE, 0);
    gtk_widget_show_all(*enclosing);
    return value;
}

static GtkWidget *
gnc_option_set_ui_widget_font (GNCOption *option, GtkBox *page_box,
                               char *name, char *documentation,
                               /* Return values */
                               GtkWidget **enclosing, gboolean *packed)
{
    GtkWidget *value;
    GtkWidget *label;

    label = gtk_label_new(name);
    gnc_label_set_alignment(label, 1.0, 0.5);

    *enclosing = gtk_box_new (GTK_ORIENTATION_HORIZONTAL, 5);
    gtk_box_set_homogeneous (GTK_BOX (*enclosing), FALSE);
    value = gtk_font_button_new();
    g_object_set(G_OBJECT(value),
                 "use-font", TRUE,
                 "show-style", TRUE,
                 "show-size", TRUE,
                 (char *)NULL);

    gnc_option_set_widget (option, value);

    gnc_option_set_ui_value(option, FALSE);

    g_signal_connect(G_OBJECT(value), "font-set",
                     G_CALLBACK(gnc_option_font_changed_cb), option);

    gtk_box_pack_start(GTK_BOX(*enclosing), label, FALSE, FALSE, 0);
    gtk_box_pack_start(GTK_BOX(*enclosing), value, FALSE, FALSE, 0);
    gtk_widget_show_all(*enclosing);
    return value;
}

static GtkWidget *
gnc_option_set_ui_widget_pixmap (GNCOption *option, GtkBox *page_box,
                                 char *name, char *documentation,
                                 /* Return values */
                                 GtkWidget **enclosing, gboolean *packed)
{
    GtkWidget *value;
    GtkWidget *label;
    GtkWidget *button;

    ENTER("option %p(%s), name %s", option, gnc_option_name(option), name);
    label = gtk_label_new(name);
    gnc_label_set_alignment(label, 1.0, 0.5);

    *enclosing = gtk_box_new (GTK_ORIENTATION_HORIZONTAL, 5);
    gtk_box_set_homogeneous (GTK_BOX (*enclosing), FALSE);

    button = gtk_button_new_with_label(_("Clear"));
    gtk_widget_set_tooltip_text(button, _("Clear any selected image file."));

    value = gtk_file_chooser_button_new(_("Select image"),
                                        GTK_FILE_CHOOSER_ACTION_OPEN);
    gtk_widget_set_tooltip_text(value, _("Select an image file."));
    g_object_set(G_OBJECT(value),
                 "width-chars", 30,
                 "preview-widget", gtk_image_new(),
                 (char *)NULL);
    g_signal_connect(G_OBJECT (value), "selection-changed",
                     G_CALLBACK(gnc_option_changed_widget_cb), option);
    g_signal_connect(G_OBJECT (value), "selection-changed",
                     G_CALLBACK(gnc_image_option_selection_changed_cb), option);
    g_signal_connect(G_OBJECT (value), "update-preview",
                     G_CALLBACK(gnc_image_option_update_preview_cb), option);
    g_signal_connect_swapped(G_OBJECT (button), "clicked",
                             G_CALLBACK(gtk_file_chooser_unselect_all), value);

    gnc_option_set_widget (option, value);
    gnc_option_set_ui_value(option, FALSE);

    gtk_box_pack_start(GTK_BOX(*enclosing), label, FALSE, FALSE, 0);
    gtk_box_pack_end(GTK_BOX(*enclosing), button, FALSE, FALSE, 0);
    gtk_box_pack_end(GTK_BOX(*enclosing), value, FALSE, FALSE, 0);

    gtk_widget_show(value);
    gtk_widget_show(label);
    gtk_widget_show(*enclosing);
    LEAVE("new widget = %p", value);
    return value;
}

static GtkWidget *
gnc_option_set_ui_widget_radiobutton (GNCOption *option, GtkBox *page_box,
                                      char *name, char *documentation,
                                      /* Return values */
                                      GtkWidget **enclosing, gboolean *packed)
{
    GtkWidget *value;

    *enclosing = gtk_box_new (GTK_ORIENTATION_HORIZONTAL, 5);
    gtk_box_set_homogeneous (GTK_BOX (*enclosing), FALSE);

    value = gnc_option_create_radiobutton_widget(name, option);
    gnc_option_set_widget (option, value);

    gnc_option_set_ui_value(option, FALSE);
    gtk_box_pack_start(GTK_BOX(*enclosing), value, FALSE, FALSE, 0);
    gtk_widget_show_all(*enclosing);
    return value;
}

static GtkWidget *
gnc_option_set_ui_widget_dateformat (GNCOption *option, GtkBox *page_box,
                                     char *name, char *documentation,
                                     /* Return values */
                                     GtkWidget **enclosing, gboolean *packed)
{
    *enclosing = gnc_date_format_new_with_label(name);
    gnc_option_set_widget (option, *enclosing);

    gnc_option_set_ui_value(option, FALSE);
    g_signal_connect(G_OBJECT(*enclosing), "format_changed",
                     G_CALLBACK(gnc_option_changed_option_cb), option);
    gtk_widget_show_all(*enclosing);
    return *enclosing;
}

static void
gnc_plot_size_option_set_select_method(GNCOption *option, gboolean set_buttons)
{
    GList* widget_list;
    GtkWidget *px_widget, *p_widget;
    GtkWidget *widget;

    widget = gnc_option_get_gtk_widget (option);

    widget_list = gtk_container_get_children(GTK_CONTAINER(widget));
    // px_button item 0
    px_widget = g_list_nth_data(widget_list, 1);
    // p_button item 2
    p_widget = g_list_nth_data(widget_list, 3);
    g_list_free(widget_list);

    if (set_buttons)
    {
        gtk_widget_set_sensitive(px_widget, TRUE);
        gtk_widget_set_sensitive(p_widget, FALSE);
    }
    else
    {
        gtk_widget_set_sensitive(p_widget, TRUE);
        gtk_widget_set_sensitive(px_widget, FALSE);
    }
}

static void
gnc_rd_option_px_set_cb(GtkWidget *widget, gpointer *raw_option)
{
    GNCOption *option = (GNCOption *) raw_option;
    gnc_plot_size_option_set_select_method(option, TRUE);
    gnc_option_changed_option_cb(widget, option);
}

static void
gnc_rd_option_p_set_cb(GtkWidget *widget, gpointer *raw_option)
{
    GNCOption *option = (GNCOption *) raw_option;
    gnc_plot_size_option_set_select_method(option, FALSE);
    gnc_option_changed_option_cb(widget, option);
    return;
}


static GtkWidget *
gnc_option_set_ui_widget_plot_size (GNCOption *option, GtkBox *page_box,
                                     char *name, char *documentation,
                                     /* Return values */
                                     GtkWidget **enclosing, gboolean *packed)
{
    GtkWidget *value_px, *value_percent;
    GtkWidget *label;
    GtkWidget *px_butt, *p_butt;
    GtkWidget *hbox;
    GtkAdjustment *adj_px, *adj_percent;
    gdouble lower_bound = G_MINDOUBLE;
    gdouble upper_bound = G_MAXDOUBLE;
    gdouble step_size = 1.0;
    int num_decimals = 0;

    label = gtk_label_new(name);
    gnc_label_set_alignment(label, 1.0, 0.5);

    hbox = gtk_box_new (GTK_ORIENTATION_HORIZONTAL, 5);
    gtk_box_set_homogeneous (GTK_BOX (hbox), FALSE);

    *enclosing = gtk_box_new (GTK_ORIENTATION_HORIZONTAL, 5);
    gtk_box_set_homogeneous (GTK_BOX (*enclosing), FALSE);

    gtk_box_pack_start(GTK_BOX(*enclosing), label, FALSE, FALSE, 0);
    gtk_box_pack_start(GTK_BOX(*enclosing), hbox, FALSE, FALSE, 0);

    gnc_option_get_range_info(option, &lower_bound, &upper_bound,
                              &num_decimals, &step_size);
    adj_px = GTK_ADJUSTMENT(gtk_adjustment_new(lower_bound, lower_bound,
                                            upper_bound, step_size,
                                            step_size * 5.0,
                                            0));

    value_px = gtk_spin_button_new(adj_px, step_size, num_decimals);
    gtk_spin_button_set_numeric(GTK_SPIN_BUTTON(value_px), TRUE);

    {
        gdouble biggest;
        gint num_digits;

        biggest = ABS(lower_bound);
        biggest = MAX(biggest, ABS(upper_bound));

        num_digits = 0;
        while (biggest >= 1)
        {
            num_digits++;
            biggest = biggest / 10;
        }

        if (num_digits == 0)
            num_digits = 1;

        num_digits += num_decimals;

        gtk_entry_set_width_chars(GTK_ENTRY(value_px), num_digits);
    }
    gtk_spin_button_set_value(GTK_SPIN_BUTTON(value_px), (upper_bound / 2)); //default
    g_signal_connect(G_OBJECT(value_px), "changed",
                     G_CALLBACK(gnc_option_changed_widget_cb), option);

    adj_percent = GTK_ADJUSTMENT(gtk_adjustment_new(1, 10, 100, 1, 5.0, 0));
    value_percent = gtk_spin_button_new(adj_percent, 1, 0);
    gtk_spin_button_set_numeric(GTK_SPIN_BUTTON(value_percent), TRUE);
    gtk_spin_button_set_value(GTK_SPIN_BUTTON(value_percent), 100); //default
    gtk_entry_set_width_chars(GTK_ENTRY(value_percent), 3);
    gtk_widget_set_sensitive(value_percent, FALSE);

    g_signal_connect(G_OBJECT(value_percent), "changed",
                     G_CALLBACK(gnc_option_changed_widget_cb), option);

    px_butt = gtk_radio_button_new_with_label(NULL, _("Pixels"));
    gtk_toggle_button_set_active (GTK_TOGGLE_BUTTON(px_butt), TRUE);

    g_signal_connect(G_OBJECT(px_butt), "toggled",
                         G_CALLBACK(gnc_rd_option_px_set_cb), option);

    p_butt = gtk_radio_button_new_with_label_from_widget(GTK_RADIO_BUTTON(px_butt), _("Percent"));
    g_signal_connect(G_OBJECT(p_butt), "toggled",
                         G_CALLBACK(gnc_rd_option_p_set_cb), option);

    gtk_box_pack_start(GTK_BOX(hbox), px_butt, FALSE, FALSE, 0);
    gtk_box_pack_start(GTK_BOX(hbox), value_px, FALSE, FALSE, 0);
    gtk_box_pack_start(GTK_BOX(hbox), p_butt, FALSE, FALSE, 0);
    gtk_box_pack_start(GTK_BOX(hbox), value_percent, FALSE, FALSE, 0);

    gnc_option_set_widget (option, hbox);
    gnc_option_set_ui_value (option, FALSE);

    gtk_widget_show_all(*enclosing);
    return hbox;
}

static GtkWidget *
gnc_option_set_ui_widget_budget (GNCOption *option, GtkBox *page_box,
                                 char *name, char *documentation,
                                 /* Return values */
                                 GtkWidget **enclosing, gboolean *packed)
{
    GtkWidget *value;
    GtkWidget *label;

    label = gtk_label_new(name);
    gnc_label_set_alignment(label, 1.0, 0.5);

    *enclosing = gtk_box_new (GTK_ORIENTATION_HORIZONTAL, 5);
    gtk_box_set_homogeneous (GTK_BOX (*enclosing), FALSE);

    value = gnc_option_create_budget_widget(option);

    gnc_option_set_widget (option, value);
    gnc_option_set_ui_value(option, FALSE);

    /* Maybe connect destroy handler for tree model here? */
    g_signal_connect(G_OBJECT(value), "changed",
                     G_CALLBACK(gnc_option_changed_widget_cb), option);

    gtk_box_pack_start(GTK_BOX(*enclosing), label, FALSE, FALSE, 0);
    gtk_box_pack_start(GTK_BOX(*enclosing), value, FALSE, FALSE, 0);
    gtk_widget_show_all(*enclosing);
    return value;
}

static GtkWidget *
gnc_option_set_ui_widget_currency_accounting (GNCOption *option,
                                              GtkBox *page_box,
                                              char *name, char *documentation,
                                              /* Return values */
                                              GtkWidget **enclosing,
                                              gboolean *packed)
{
    GtkWidget *value;

    *enclosing = gtk_box_new (GTK_ORIENTATION_HORIZONTAL, 5);
    gtk_box_set_homogeneous (GTK_BOX (*enclosing), FALSE);

    value = gnc_option_create_currency_accounting_widget(name, option);
    gnc_option_set_widget (option, value);

    gnc_option_set_ui_value(option, FALSE);
    gtk_box_pack_start(GTK_BOX(*enclosing), value, TRUE, TRUE, 0);
    gtk_widget_show_all(*enclosing);
    return value;
}

/*************************
 *       SET VALUE       *
 *************************
 *
 * gnc_option_set_ui_value_<type>():
 *
 *   In this function you should set the state of the gui widget to
 * correspond to the value provided in 'value'.  You should return
 * TRUE if there was an error, FALSE otherwise.
 *
 *
 */
static gboolean
gnc_option_set_ui_value_boolean (GNCOption *option, gboolean use_default,
                                 GtkWidget *widget, SCM value)
{
    if (scm_is_bool(value))
    {
        gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(widget),
                                     scm_is_true(value));
        return FALSE;
    }
    else
        return TRUE;
}

static gboolean
gnc_option_set_ui_value_string (GNCOption *option, gboolean use_default,
                                GtkWidget *widget, SCM value)
{
    if (scm_is_string(value))
    {
        const gchar *string;

        string = gnc_scm_to_utf8_string (value);
        gtk_entry_set_text(GTK_ENTRY(widget), string);
        g_free ((gpointer *) string);
        return FALSE;
    }
    else
        return TRUE;
}

static gboolean
gnc_option_set_ui_value_text (GNCOption *option, gboolean use_default,
                              GObject *object, SCM value)
{
    GtkTextBuffer *buffer;

    if (GTK_IS_TEXT_BUFFER(object))
        buffer = GTK_TEXT_BUFFER(object);
    else
        buffer = gtk_text_view_get_buffer(GTK_TEXT_VIEW(object));

    if (scm_is_string(value))
    {
        const gchar *string;

        string = gnc_scm_to_utf8_string (value);
        gtk_text_buffer_set_text (buffer, string, -1);
        free ((void*) string);
        return FALSE;
    }
    else
        return TRUE;
}

static gboolean
gnc_option_set_ui_value_currency (GNCOption *option, gboolean use_default,
                                  GtkWidget *widget, SCM value)
{
    gnc_commodity *commodity;

    commodity = gnc_scm_to_commodity (value);
    if (commodity)
    {
        gnc_currency_edit_set_currency(GNC_CURRENCY_EDIT(widget), commodity);
        return FALSE;
    }
    else
        return TRUE;
}

static gboolean
gnc_option_set_ui_value_commodity (GNCOption *option, gboolean use_default,
                                   GtkWidget *widget, SCM value)
{
    gnc_commodity *commodity;

    commodity = gnc_scm_to_commodity (value);
    if (commodity)
    {
        gnc_general_select_set_selected(GNC_GENERAL_SELECT (widget), commodity);
        return FALSE;
    }
    else
        return TRUE;
}

static gboolean
gnc_option_set_ui_value_multichoice (GNCOption *option, gboolean use_default,
                                     GtkWidget *widget, SCM value)
{
    int index;

    index = gnc_option_permissible_value_index(option, value);
    if (index < 0)
        return TRUE;
    else
    {
        /* GtkComboBox per-item tooltip changes needed below */
        gnc_combott_set_active(GNC_COMBOTT(widget), index);
        return FALSE;
    }
}

static gboolean
gnc_option_set_ui_value_date (GNCOption *option, gboolean use_default,
                              GtkWidget *widget, SCM value)
{
    int index;
    char *date_option_type;
    char *symbol_str;
    gboolean bad_value = FALSE;

    date_option_type = gnc_option_date_option_get_subtype(option);

    if (scm_is_pair(value))
    {
        symbol_str = gnc_date_option_value_get_type (value);
        if (symbol_str)
        {
            if (g_strcmp0(symbol_str, "relative") == 0)
            {
                SCM relative = gnc_date_option_value_get_relative (value);

                index = gnc_option_permissible_value_index(option, relative);
                if (g_strcmp0(date_option_type, "relative") == 0)
                {
                    /* GtkComboBox per-item tooltip changes needed below */
                    gnc_combott_set_active(GNC_COMBOTT(widget), index);
                }
                else if (g_strcmp0(date_option_type, "both") == 0)
                {
                    GList *widget_list;
                    GtkWidget *rel_date_widget;

                    widget_list = gtk_container_get_children(GTK_CONTAINER(widget));
                    rel_date_widget = g_list_nth_data(widget_list,
                                                      GNC_RD_WID_REL_WIDGET_POS);
                    g_list_free(widget_list);
                    gnc_date_option_set_select_method(option, FALSE, TRUE);
                    /* GtkComboBox per-item tooltip changes needed below */
                    gnc_combott_set_active(GNC_COMBOTT(rel_date_widget), index);
                }
                else
                {
                    bad_value = TRUE;
                }
            }
            else if (g_strcmp0(symbol_str, "absolute") == 0)
            {
                time64 time;

                time = gnc_date_option_value_get_absolute (value);

                if (g_strcmp0(date_option_type, "absolute") == 0)
                {
                    gnc_date_edit_set_time(GNC_DATE_EDIT(widget), time);
                }
                else if (g_strcmp0(date_option_type, "both") == 0)
                {
                    GList *widget_list;
                    GtkWidget *ab_widget;

                    widget_list = gtk_container_get_children(GTK_CONTAINER(widget));
                    ab_widget = g_list_nth_data(widget_list,
                                                GNC_RD_WID_AB_WIDGET_POS);
                    g_list_free(widget_list);
                    gnc_date_option_set_select_method(option, TRUE, TRUE);
                    gnc_date_edit_set_time(GNC_DATE_EDIT(ab_widget), time);
                }
                else
                {
                    bad_value = TRUE;
                }
            }
            else
            {
                bad_value = TRUE;
            }

            if (symbol_str)
                free(symbol_str);
        }
    }
    else
    {
        bad_value = TRUE;
    }

    if (date_option_type)
        free(date_option_type);

    return bad_value;
}

static gboolean
gnc_option_set_ui_value_account_list (GNCOption *option, gboolean use_default,
                                      GtkWidget *widget, SCM value)
{
    GList *list;

    list = gnc_scm_list_to_glist(value);

    gnc_tree_view_account_set_selected_accounts (GNC_TREE_VIEW_ACCOUNT(widget),
            list, TRUE);

    g_list_free(list);
    return FALSE;
}

static gboolean
gnc_option_set_ui_value_account_sel (GNCOption *option, gboolean use_default,
                                     GtkWidget *widget, SCM value)
{
    Account *acc = NULL;

    if (value != SCM_BOOL_F)
    {
        if (!SWIG_IsPointer(value))
            scm_misc_error("gnc_option_set_ui_value_account_sel",
                           "Option Value not a wcp.", value);

        acc = SWIG_MustGetPtr(value, SWIG_TypeQuery("_p_Account"), 4, 0);
    }

    //doesn't default because this function is called to set a specific account
    gnc_account_sel_set_account (GNC_ACCOUNT_SEL(widget), acc, FALSE);

    return FALSE;
}

static gboolean
gnc_option_set_ui_value_list (GNCOption *option, gboolean use_default,
                              GtkWidget *widget, SCM value)
{
    GtkTreeSelection *selection;
    GtkTreePath *path;
    gint row;

    selection = gtk_tree_view_get_selection(GTK_TREE_VIEW(widget));
    gtk_tree_selection_unselect_all(selection);

    while (scm_is_list(value) && !scm_is_null(value))
    {
        SCM item;

        item = SCM_CAR(value);
        value = SCM_CDR(value);

        row = gnc_option_permissible_value_index(option, item);
        if (row < 0)
        {
            return TRUE;
        }

        path = gtk_tree_path_new_from_indices(row, -1);
        gtk_tree_selection_select_path(selection, path);
        gtk_tree_path_free(path);
    }

    if (!scm_is_list(value) || !scm_is_null(value))
        return TRUE;

    return FALSE;
}

static gboolean
gnc_option_set_ui_value_number_range (GNCOption *option, gboolean use_default,
                                      GtkWidget *widget, SCM value)
{
    GtkSpinButton *spinner;
    gdouble d_value;

    spinner = GTK_SPIN_BUTTON(widget);

    if (scm_is_number(value))
    {
        d_value = scm_to_double(value);
        gtk_spin_button_set_value(spinner, d_value);
        return FALSE;
    }
    else
        return TRUE;
}

static gboolean
gnc_option_set_ui_value_color (GNCOption *option, gboolean use_default,
                               GtkWidget *widget, SCM value)
{

    GdkRGBA color;
    if (gnc_option_get_color_info(option, use_default,
                                  &color.red, &color.green,
                                  &color.blue, &color.alpha))
    {
        GtkColorChooser *color_button;

        DEBUG("red %f, green %f, blue %f, alpha %f",
              color.red, color.green, color.blue, color.alpha);
        color_button = GTK_COLOR_CHOOSER(widget);

        gtk_color_chooser_set_rgba(color_button, &color);
        return FALSE;
    }

    LEAVE("TRUE");
    return TRUE;
}

static gboolean
gnc_option_set_ui_value_font (GNCOption *option, gboolean use_default,
                              GtkWidget *widget, SCM value)
{
    if (scm_is_string(value))
    {
        const gchar *string;

        string = gnc_scm_to_utf8_string (value);
        if ((string != NULL) && (*string != '\0'))
        {
            GtkFontButton *font_button = GTK_FONT_BUTTON(widget);
            gtk_font_button_set_font_name(font_button, string);
        }
        g_free ((gpointer *) string);
        return FALSE;
    }
    else
        return TRUE;
}

static gboolean
gnc_option_set_ui_value_pixmap (GNCOption *option, gboolean use_default,
                                GtkWidget *widget, SCM value)
{
    ENTER("option %p(%s)", option, gnc_option_name(option));
    if (scm_is_string(value))
    {
        const gchar *string;

        string = gnc_scm_to_locale_string (value);
        if (string && *string)
        {
            gchar *test;
            DEBUG("string = %s", string);
            gtk_file_chooser_select_filename(GTK_FILE_CHOOSER(widget), string);
            test = gtk_file_chooser_get_filename(GTK_FILE_CHOOSER(widget));
            g_object_set_data_full(G_OBJECT(widget), LAST_SELECTION,
                                   g_strdup(string), g_free);
            DEBUG("Set %s, retrieved %s", string, test ? test : "(null)");
            gnc_image_option_update_preview_cb(GTK_FILE_CHOOSER(widget), option);
        }
        LEAVE("FALSE");
        g_free ((gpointer *) string);
        return FALSE;
    }

    LEAVE("TRUE");
    return TRUE;
}

static gboolean gnc_option_set_ui_value_budget(
    GNCOption *option, gboolean use_default, GtkWidget *widget, SCM value)
{
    GncBudget *bgt;

//    if (!scm_is_null(value)) {
    if (value != SCM_BOOL_F)
    {
        if (!SWIG_IsPointer(value))
            scm_misc_error("gnc_option_set_ui_value_budget",
                           "Option Value not a wcp.", value);

        bgt = SWIG_MustGetPtr(value, SWIG_TypeQuery("GncBudget *"), 4, 0);
        if (bgt)
        {
            GtkComboBox *cb = GTK_COMBO_BOX(widget);
            GtkTreeModel *tm = gtk_combo_box_get_model(cb);
            GtkTreeIter iter;
            if (gnc_tree_model_budget_get_iter_for_budget(tm, &iter, bgt))
                gtk_combo_box_set_active_iter(cb, &iter);
        }
    }


    //FIXME: Unimplemented.
    return FALSE;
}

static gboolean
gnc_option_set_ui_value_radiobutton (GNCOption *option, gboolean use_default,
                                     GtkWidget *widget, SCM value)
{
    int index;

    index = gnc_option_permissible_value_index(option, value);
    if (index < 0)
        return TRUE;
    else
    {
        GtkWidget *box, *button;
        GList *list;
        int i;
        gpointer val;

        list = gtk_container_get_children (GTK_CONTAINER (widget));
        box = list->data;
        g_list_free(list);

        list = gtk_container_get_children (GTK_CONTAINER (box));
        for (i = 0; i < index && list; i++)
            list = list->next;
        g_return_val_if_fail (list, TRUE);

        button = list->data;
        g_list_free(list);
        val = g_object_get_data (G_OBJECT (button), "gnc_radiobutton_index");
        g_return_val_if_fail (GPOINTER_TO_INT (val) == index, TRUE);

        gtk_toggle_button_set_active (GTK_TOGGLE_BUTTON (button), TRUE);
        //    g_object_set_data(G_OBJECT(widget), "gnc_radiobutton_index",
        //          GINT_TO_POINTER(index));
        return FALSE;
    }
}

static gboolean
gnc_option_set_ui_value_dateformat (GNCOption *option, gboolean use_default,
                                    GtkWidget *widget, SCM value)
{
    GNCDateFormat * gdf = GNC_DATE_FORMAT(widget);
    QofDateFormat format;
    GNCDateMonthFormat months;
    gboolean years;
    char *custom;

    if (gnc_dateformat_option_value_parse(value, &format, &months, &years, &custom))
        return TRUE;

    gnc_date_format_set_format(gdf, format);
    gnc_date_format_set_months(gdf, months);
    gnc_date_format_set_years(gdf, years);
    gnc_date_format_set_custom(gdf, custom);
    gnc_date_format_refresh(gdf);

    if (custom)
        free(custom);

    return FALSE;
}

static gboolean
gnc_option_set_ui_value_plot_size (GNCOption *option, gboolean use_default,
                                    GtkWidget *widget, SCM value)
{
    GList* widget_list;
    GtkWidget *px_button, *p_button, *px_widget, *p_widget;
    char *symbol_str;
    gdouble d_value;

    widget_list = gtk_container_get_children(GTK_CONTAINER(widget));
    px_button = g_list_nth_data(widget_list, 0);
    px_widget = g_list_nth_data(widget_list, 1);
    p_button = g_list_nth_data(widget_list, 2);
    p_widget = g_list_nth_data(widget_list, 3);
    g_list_free(widget_list);

    if (scm_is_pair(value))
    {
        symbol_str = gnc_plot_size_option_value_get_type(value);
        d_value = gnc_plot_size_option_value_get_value(value);

        if (symbol_str)
        {
            if (g_strcmp0(symbol_str, "pixels") == 0) // pixel values
            {
                gtk_spin_button_set_value(GTK_SPIN_BUTTON(px_widget), d_value);
                gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(px_button), TRUE);
            }
            else // percent values
            {
                gtk_spin_button_set_value(GTK_SPIN_BUTTON(p_widget), (d_value));
                gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(p_button), TRUE);
            }
            return FALSE;
        }
    }
    return TRUE;
}

static gboolean
gnc_option_set_ui_value_currency_accounting (GNCOption *option,
                                             gboolean use_default,
                                             GtkWidget *widget, SCM value)
{
    if (scm_is_pair(value))
    {
        SCM rb_symbol;

        rb_symbol = gnc_currency_accounting_option_value_get_method (value);

        if (rb_symbol)
        {
            int index;

            index = gnc_option_permissible_value_index(option, rb_symbol);
            if (index < 0)
                return TRUE;
            else
            {
                GtkWidget *button = NULL;
                gpointer val;

                switch (index)
                {
                    case 0:
                        button = book_currency_data->gnc_currency_radiobutton_0;
                        break;
                    case 1:
                        button = book_currency_data->gnc_currency_radiobutton_1;
                        break;
                    case 2:
                        button = book_currency_data->gnc_currency_radiobutton_2;
                        break;
                    default:
                        return TRUE;
                }

                val = g_object_get_data (G_OBJECT (button),
                                            "gnc_radiobutton_index");
                g_return_val_if_fail (GPOINTER_TO_INT (val) == index, TRUE);

                if (g_strcmp0(gnc_option_permissible_value_name(option,
                                                                 index),
                                "Use a Book Currency") == 0)
                {
                    gnc_commodity *commodity = NULL;
                    SCM curr_scm =
                        gnc_currency_accounting_option_value_get_book_currency
                            (value);
                    SCM list_symbol =
                        gnc_currency_accounting_option_value_get_default_policy
                            (value);
                    SCM acct_guid_scm =
                        gnc_currency_accounting_option_value_get_default_account
                            (value);

                    commodity = gnc_scm_to_commodity (curr_scm);
                    if (commodity)
                    {
                        book_currency_data->retrieved_book_currency = commodity;
                    }
                    else
                    {
                        book_currency_data->retrieved_book_currency = NULL;
                    }
                    if (list_symbol)
                    {
                        book_currency_data->retrieved_policy_scm = list_symbol;
                    }
                    else
                    {
                        book_currency_data->retrieved_policy_scm = NULL;
                    }
                    if (acct_guid_scm)
                    {
                        book_currency_data->retrieved_gain_loss_acct_guid_scm =
                                                                acct_guid_scm;
                    }
                    else
                    {
                        book_currency_data->retrieved_gain_loss_acct_guid_scm =
                                                                        NULL;
                    }
                }
                gtk_toggle_button_set_active (GTK_TOGGLE_BUTTON (button), TRUE);
                /* when an unselected button in a group is clicked the clicked
                   button receives the “toggled” signal, as does the
                   previously selected button; however, if the first button
                   is active when the currency-accounting dialog is created,
                   that is, it's read from the option, the "toggled" handler
                   is not called while it is if any other button is active.
                   To get desired result, that is, to set sensitivity to
                   FALSE, explicitly call the handler here if first button */
                if (index == 0)
                {
                    gnc_option_currency_accounting_non_book_cb(button,
                        (gpointer) book_currency_data);
                }
                return FALSE;
            }
        }
        return TRUE;
    }
    return TRUE;
}

/*************************
 *       GET VALUE       *
 *************************
 *
 * gnc_option_get_ui_value_<type>():
 *
 * 'widget' will be the widget returned from the
 * gnc_option_set_ui_widget_<type>() function.
 *
 * You should return a SCM value corresponding to the current state of the
 * gui widget.
 *
 */
static SCM
gnc_option_get_ui_value_boolean (GNCOption *option, GtkWidget *widget)
{
    gboolean active;

    active = gtk_toggle_button_get_active(GTK_TOGGLE_BUTTON(widget));
    return SCM_BOOL(active);
}

static SCM
gnc_option_get_ui_value_string (GNCOption *option, GtkWidget *widget)
{
    char * string;
    SCM result;

    string = gtk_editable_get_chars(GTK_EDITABLE(widget), 0, -1);
    result = scm_from_utf8_string(string ? string : "");
    g_free(string);
    return result;
}

static SCM
gnc_option_get_ui_value_text (GNCOption *option, GtkWidget *widget)
{
    char * string;
    SCM result;

    string = xxxgtk_textview_get_text (GTK_TEXT_VIEW(widget));
    result = scm_from_utf8_string(string ? string : "");
    g_free(string);
    return result;
}

static SCM
gnc_option_get_ui_value_currency (GNCOption *option, GtkWidget *widget)
{
    gnc_commodity *commodity;

    commodity =
        gnc_currency_edit_get_currency(GNC_CURRENCY_EDIT(widget));

    return (gnc_commodity_to_scm (commodity));
}

static SCM
gnc_option_get_ui_value_commodity (GNCOption *option, GtkWidget *widget)
{
    gnc_commodity *commodity;

    commodity =
        gnc_general_select_get_selected(GNC_GENERAL_SELECT(widget));

    return (gnc_commodity_to_scm(commodity));
}

static SCM
gnc_option_get_ui_value_multichoice (GNCOption *option, GtkWidget *widget)
{
    int index;

    /* GtkComboBox per-item tooltip changes needed below */
    index = gnc_combott_get_active(GNC_COMBOTT(widget));
    return (gnc_option_permissible_value(option, index));
}

static SCM
gnc_option_get_ui_value_date (GNCOption *option, GtkWidget *widget)
{
    int index;
    SCM type, val, result = SCM_UNDEFINED;
    char *subtype = gnc_option_date_option_get_subtype(option);

    if (g_strcmp0(subtype, "relative") == 0)
    {
        /* GtkComboBox per-item tooltip changes needed below */
        index = gnc_combott_get_active(GNC_COMBOTT(widget));

        type = scm_from_locale_symbol ("relative");
        val = gnc_option_permissible_value(option, index);
        result = scm_cons(type, val);
    }
    else if (g_strcmp0(subtype, "absolute") == 0)
    {
        time64 time;
        time = gnc_date_edit_get_date(GNC_DATE_EDIT(widget));
        result = scm_cons(scm_from_locale_symbol ("absolute"), scm_from_int64(time));
    }
    else if (g_strcmp0(subtype, "both") == 0)
    {
        time64 time;
        int index;
        SCM val;
        GList *widget_list;
        GtkWidget *ab_button, *rel_widget, *ab_widget;

        widget_list = gtk_container_get_children(GTK_CONTAINER(widget));
        ab_button = g_list_nth_data(widget_list,  GNC_RD_WID_AB_BUTTON_POS);
        ab_widget = g_list_nth_data(widget_list,  GNC_RD_WID_AB_WIDGET_POS);
        rel_widget = g_list_nth_data(widget_list, GNC_RD_WID_REL_WIDGET_POS);
        g_list_free(widget_list);

        /* if it's an absolute date */
        if (gtk_toggle_button_get_active(GTK_TOGGLE_BUTTON(ab_button)))
        {
            time = gnc_date_edit_get_date(GNC_DATE_EDIT(ab_widget));
            result = scm_cons(scm_from_locale_symbol ("absolute"), scm_from_int64 (time));
        }
        else
        {
            /* GtkComboBox per-item tooltip changes needed below */
            index = gnc_combott_get_active(GNC_COMBOTT(rel_widget));

            val = gnc_option_permissible_value(option, index);
            result = scm_cons(scm_from_locale_symbol ("relative"), val);
        }
    }
    g_free(subtype);
    return result;
}

static SCM
gnc_option_get_ui_value_account_list (GNCOption *option, GtkWidget *widget)
{
    GncTreeViewAccount *tree;
    GList *list;
    SCM result;

    tree = GNC_TREE_VIEW_ACCOUNT(widget);
    list = gnc_tree_view_account_get_selected_accounts (tree);

    /* handover list */
    result = gnc_glist_to_scm_list(list, "_p_Account");
    g_list_free(list);
    return result;
}

static SCM
gnc_option_get_ui_value_account_sel (GNCOption *option, GtkWidget *widget)
{
    GNCAccountSel *gas;
    Account* acc;

    gas = GNC_ACCOUNT_SEL(widget);
    acc = gnc_account_sel_get_account (gas);

    if (!acc)
        return SCM_BOOL_F;

    return SWIG_NewPointerObj(acc, SWIG_TypeQuery("_p_Account"), 0);
}

static SCM
gnc_option_get_ui_value_budget(GNCOption *option, GtkWidget *widget)
{
    GncBudget *bgt;
    GtkComboBox *cb;
    GtkTreeModel *tm;
    GtkTreeIter iter;

    cb = GTK_COMBO_BOX(widget);
    gtk_combo_box_get_active_iter(cb, &iter);
    tm = gtk_combo_box_get_model(cb);
    bgt = gnc_tree_model_budget_get_budget(tm, &iter);

    if (!bgt)
        return SCM_BOOL_F;

    return SWIG_NewPointerObj(bgt, SWIG_TypeQuery("_p_budget_s"), 0);
}

static SCM
gnc_option_get_ui_value_list (GNCOption *option, GtkWidget *widget)
{
    GtkTreeSelection *selection;
    GtkTreePath *path;
    SCM result;
    gboolean selected;
    gint num_rows;
    gint row;

    selection = gtk_tree_view_get_selection(GTK_TREE_VIEW(widget));
    num_rows = gnc_option_num_permissible_values(option);
    result = scm_c_eval_string("'()");

    for (row = 0; row < num_rows; row++)
    {
        path = gtk_tree_path_new_from_indices(row, -1);
        selected = gtk_tree_selection_path_is_selected(selection, path);
        gtk_tree_path_free(path);
        if (selected)
            result = scm_cons(gnc_option_permissible_value(option, row), result);
    }

    return (scm_reverse(result));
}

static SCM
gnc_option_get_ui_value_number_range (GNCOption *option, GtkWidget *widget)
{
    GtkSpinButton *spinner;
    gdouble value;

    spinner = GTK_SPIN_BUTTON(widget);

    value = gtk_spin_button_get_value(spinner);

    return (scm_from_double (value));
}

static SCM
gnc_option_get_ui_value_color (GNCOption *option, GtkWidget *widget)
{
    SCM result;
    GtkColorChooser *color_button;
    GdkRGBA color;
    gdouble scale;

    ENTER("option %p(%s), widget %p",
          option, gnc_option_name(option), widget);

    color_button = GTK_COLOR_CHOOSER(widget);
    gtk_color_chooser_get_rgba(color_button, &color);

    scale = gnc_option_color_range(option);

    result = SCM_EOL;
    result = scm_cons(scm_from_double (color.alpha * scale), result);
    result = scm_cons(scm_from_double (color.blue * scale), result);
    result = scm_cons(scm_from_double (color.green * scale), result);
    result = scm_cons(scm_from_double (color.red * scale), result);
    return result;
}

static SCM
gnc_option_get_ui_value_font (GNCOption *option, GtkWidget *widget)
{
    GtkFontButton *font_button = GTK_FONT_BUTTON(widget);
    const gchar * string;

    string = gtk_font_button_get_font_name(font_button);
    return (string ? scm_from_utf8_string(string) : SCM_BOOL_F);
}

static SCM
gnc_option_get_ui_value_pixmap (GNCOption *option, GtkWidget *widget)
{
    gchar *string;
    SCM result;

    string = gtk_file_chooser_get_filename(GTK_FILE_CHOOSER(widget));
    DEBUG("filename %s", string ? string : "(null)");
    result = scm_from_utf8_string(string ? string : "");
    g_free(string);
    return result;
}

static SCM
gnc_option_get_ui_value_radiobutton (GNCOption *option, GtkWidget *widget)
{
    gpointer _index;
    int index;

    _index = g_object_get_data(G_OBJECT(widget), "gnc_radiobutton_index");
    index = GPOINTER_TO_INT(_index);

    return (gnc_option_permissible_value(option, index));
}

static SCM
gnc_option_get_ui_value_dateformat (GNCOption *option, GtkWidget *widget)
{
    GNCDateFormat *gdf = GNC_DATE_FORMAT(widget);
    QofDateFormat format;
    GNCDateMonthFormat months;
    gboolean years;
    const char* custom;

    format = gnc_date_format_get_format(gdf);
    months = gnc_date_format_get_months(gdf);
    years = gnc_date_format_get_years(gdf);
    custom = gnc_date_format_get_custom(gdf);

    return (gnc_dateformat_option_set_value(format, months, years, custom));
}

static SCM
gnc_option_get_ui_value_plot_size (GNCOption *option, GtkWidget *widget)
{
    GList* widget_list;
    GtkWidget *px_button, *px_widget, *p_widget;
    gdouble d_value;
    SCM type, val;

    widget_list = gtk_container_get_children(GTK_CONTAINER(widget));
    px_button = g_list_nth_data(widget_list, 0);
    px_widget = g_list_nth_data(widget_list, 1);
    // p_button item 2
    p_widget = g_list_nth_data(widget_list, 3);
    g_list_free(widget_list);

    if (gtk_toggle_button_get_active (GTK_TOGGLE_BUTTON(px_button)))
    {
        type = scm_from_locale_symbol("pixels");
        d_value = gtk_spin_button_get_value(GTK_SPIN_BUTTON(px_widget));
    }
    else
    {
        type = scm_from_locale_symbol("percent");
        d_value = gtk_spin_button_get_value(GTK_SPIN_BUTTON(p_widget));
    }
    val = scm_from_double(d_value);
    return scm_cons(type, val);
}

static SCM
gnc_option_get_ui_value_currency_accounting (
                                        GNCOption *option, GtkWidget *widget)
{
    gpointer _index;
    int index;
    SCM value = SCM_EOL;

    _index = g_object_get_data(G_OBJECT(widget), "gnc_radiobutton_index");
    index = GPOINTER_TO_INT(_index);

    /* build the return list in reverse order */
    if (g_strcmp0(gnc_option_permissible_value_name(option, index),
                  "Use a Book Currency") == 0)
    {
        gnc_commodity *commodity = NULL;
        int policy_index;
        SCM val;
        GList *list_of_policies = NULL;
        const char *str = NULL;

        if (book_currency_data->default_gain_loss_account_widget)
        {
            /* get account from widget, if one selected */
            Account *gain_loss_account = NULL;

            gain_loss_account =
                gnc_tree_view_account_get_selected_account
                    (GNC_TREE_VIEW_ACCOUNT (
                        book_currency_data->default_gain_loss_account_widget));

            if (gain_loss_account == NULL)
            {
                val = SCM_BOOL_F;
            }
            else
            {
                gchar *gain_loss_account_guid = guid_to_string (
                                        xaccAccountGetGUID (gain_loss_account));

                val = scm_from_utf8_string (gain_loss_account_guid);
                g_free (gain_loss_account_guid);
            }
        }
        else
        {
            val = SCM_BOOL_F;
        }
        value = scm_cons(val, value);

        list_of_policies = gnc_get_valid_policy_list();
        if (list_of_policies && book_currency_data->default_cost_policy_widget)
        {
            GList *l = NULL;
            gint i = 0;
            /* GtkComboBox per-item tooltip changes needed below */
            policy_index =
                gnc_combott_get_active(GNC_COMBOTT(
                            book_currency_data->default_cost_policy_widget));
            for (l = list_of_policies; l != NULL; l = l->next)
            {
                GNCPolicy *pcy = l->data;
                if(i == policy_index)
                    str = PolicyGetName (pcy);
                i++;
            }
            g_list_free(list_of_policies);
        }
        if (str)
        {
            val = scm_from_locale_symbol(str);
        }
        else
        {
            val = SCM_BOOL_F;
        }
        value = scm_cons(val, value);

        if (gtk_combo_box_get_active (GTK_COMBO_BOX(book_currency_data->book_currency_widget)) != -1)
        {
            commodity =
                gnc_currency_edit_get_currency(
                    GNC_CURRENCY_EDIT(
                        book_currency_data->book_currency_widget));
            if (commodity)
            {
                val = gnc_commodity_to_scm(commodity);
            }
            else
            {
                val = SCM_BOOL_F;
            }
        }
        else
        {
            val = SCM_BOOL_F;
        }
        value = scm_cons(val, value);
    }

    return (scm_cons (gnc_option_permissible_value(option, index), value));
}

/************************************/
/*          INITIALIZATION          */
/************************************/
static void gnc_options_initialize_options (void)
{
    static GNCOptionDef_t options[] =
    {
        {
            "boolean", gnc_option_set_ui_widget_boolean,
            gnc_option_set_ui_value_boolean, gnc_option_get_ui_value_boolean
        },
        {
            "string", gnc_option_set_ui_widget_string,
            gnc_option_set_ui_value_string, gnc_option_get_ui_value_string
        },
        {
            "text", gnc_option_set_ui_widget_text,
            (GNCOptionUISetValue)gnc_option_set_ui_value_text,
            gnc_option_get_ui_value_text
        },
        {
            "currency", gnc_option_set_ui_widget_currency,
            gnc_option_set_ui_value_currency, gnc_option_get_ui_value_currency
        },
        {
            "commodity", gnc_option_set_ui_widget_commodity,
            gnc_option_set_ui_value_commodity, gnc_option_get_ui_value_commodity
        },
        {
            "multichoice", gnc_option_set_ui_widget_multichoice,
            gnc_option_set_ui_value_multichoice, gnc_option_get_ui_value_multichoice
        },
        {
            "date", gnc_option_set_ui_widget_date,
            gnc_option_set_ui_value_date, gnc_option_get_ui_value_date
        },
        {
            "account-list", gnc_option_set_ui_widget_account_list,
            gnc_option_set_ui_value_account_list, gnc_option_get_ui_value_account_list
        },
        {
            "account-sel", gnc_option_set_ui_widget_account_sel,
            gnc_option_set_ui_value_account_sel, gnc_option_get_ui_value_account_sel
        },
        {
            "list", gnc_option_set_ui_widget_list,
            gnc_option_set_ui_value_list, gnc_option_get_ui_value_list
        },
        {
            "number-range", gnc_option_set_ui_widget_number_range,
            gnc_option_set_ui_value_number_range, gnc_option_get_ui_value_number_range
        },
        {
            "color", gnc_option_set_ui_widget_color,
            gnc_option_set_ui_value_color, gnc_option_get_ui_value_color
        },
        {
            "font", gnc_option_set_ui_widget_font,
            gnc_option_set_ui_value_font, gnc_option_get_ui_value_font
        },
        {
            "pixmap", gnc_option_set_ui_widget_pixmap,
            gnc_option_set_ui_value_pixmap, gnc_option_get_ui_value_pixmap
        },
        {
            "radiobutton", gnc_option_set_ui_widget_radiobutton,
            gnc_option_set_ui_value_radiobutton, gnc_option_get_ui_value_radiobutton
        },
        {
            "dateformat", gnc_option_set_ui_widget_dateformat,
            gnc_option_set_ui_value_dateformat, gnc_option_get_ui_value_dateformat
        },
        {
            "plot-size", gnc_option_set_ui_widget_plot_size,
            gnc_option_set_ui_value_plot_size, gnc_option_get_ui_value_plot_size
        },
        {
            "budget", gnc_option_set_ui_widget_budget,
            gnc_option_set_ui_value_budget, gnc_option_get_ui_value_budget
        },
        {
            "currency-accounting",
            gnc_option_set_ui_widget_currency_accounting,
            gnc_option_set_ui_value_currency_accounting,
            gnc_option_get_ui_value_currency_accounting
        },
        { NULL, NULL, NULL, NULL }
    };
    int i;

    for (i = 0; options[i].option_name; i++)
        gnc_options_ui_register_option (&(options[i]));
}

/* Register a new option type in the UI */
void gnc_options_ui_register_option (GNCOptionDef_t *option)
{
    g_return_if_fail (optionTable);
    g_return_if_fail (option);

    /* FIXME: should protect against repeat insertion. */
    g_hash_table_insert (optionTable, (gpointer)(option->option_name), option);
}

GNCOptionDef_t * gnc_options_ui_get_option (const char *option_name)
{
    GNCOptionDef_t *retval;
    g_return_val_if_fail (optionTable, NULL);
    g_return_val_if_fail (option_name, NULL);

    retval = g_hash_table_lookup (optionTable, option_name);
    if (!retval)
    {
        PERR("Option lookup for type '%s' failed!", option_name);
    }
    return retval;
}

void gnc_options_ui_initialize (void)
{
    SWIG_GetModule(NULL); /* Work-around for SWIG bug. */
    //  gnc_options_register_stocks ();
    g_return_if_fail (optionTable == NULL);
    optionTable = g_hash_table_new (g_str_hash, g_str_equal);

    /* add known types */
    gnc_options_initialize_options ();
}

struct scm_cb
{
    SCM apply_cb;
    SCM close_cb;
};

static void
scm_apply_cb (GNCOptionWin *win, gpointer data)
{
    struct scm_cb *cbdata = data;

    if (gnc_option_db_get_changed (win->option_db))
    {
        GList *results = NULL, *iter;
        results = gnc_option_db_commit (win->option_db);
        for (iter = results; iter; iter = iter->next)
        {
            GtkWidget *dialog = gtk_message_dialog_new(GTK_WINDOW(gnc_options_dialog_widget(win)),
                                                       0,
                                                       GTK_MESSAGE_ERROR,
                                                       GTK_BUTTONS_OK,
                                                       "%s",
                                                       (char*)iter->data);
            gtk_dialog_run(GTK_DIALOG(dialog));
            gtk_widget_destroy(dialog);
            g_free (iter->data);
        }
        g_list_free (results);

        if (cbdata->apply_cb != SCM_BOOL_F)
        {
            scm_call_0 (cbdata->apply_cb);
        }
    }
}

static void
scm_close_cb (GNCOptionWin *win, gpointer data)
{
    struct scm_cb *cbdata = data;

    if (cbdata->close_cb != SCM_BOOL_F)
    {
        scm_call_0 (cbdata->close_cb);
        scm_gc_unprotect_object (cbdata->close_cb);
    }

    if (cbdata->apply_cb != SCM_BOOL_F)
        scm_gc_unprotect_object (cbdata->apply_cb);

    g_free (cbdata);
}

/* Both apply_cb and close_cb should be scheme functions with 0 arguments.
 * References to these functions will be held until the close_cb is called
 */
void
gnc_options_dialog_set_scm_callbacks (GNCOptionWin *win, SCM apply_cb,
                                      SCM close_cb)
{
    struct scm_cb *cbdata;

    cbdata = g_new0 (struct scm_cb, 1);
    cbdata->apply_cb = apply_cb;
    cbdata->close_cb = close_cb;

    if (apply_cb != SCM_BOOL_F)
        scm_gc_protect_object (cbdata->apply_cb);

    if (close_cb != SCM_BOOL_F)
        scm_gc_protect_object (cbdata->close_cb);

    gnc_options_dialog_set_apply_cb (win, scm_apply_cb, cbdata);
    gnc_options_dialog_set_close_cb (win, scm_close_cb, cbdata);
}
