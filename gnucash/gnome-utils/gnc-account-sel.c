/**
 * gnc-account-sel.c -- combobox style account selection widget
 *
 * Copyright (C) 2002 Joshua Sled <jsled@asynchronous.org>
 * All rights reserved.
 * Copyright (C) 2006 David Hampton <hampton@employees.org>
 *
 * Gnucash is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * as published by the Free Software Foundation; either version 2 of the
 * License, or (at your option) any later version.
 *
 * Gnucash is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Library General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, contact:
 *
 * Free Software Foundation           Voice:  +1-617-542-5942
 * 51 Franklin Street, Fifth Floor    Fax:    +1-617-542-2652
 * Boston, MA  02110-1301,  USA       gnu@gnu.org
 **/

#include <config.h>

#include <stdbool.h>
#include <gtk/gtk.h>
#include <glib/gi18n.h>

#include "account-quickfill.h"
#include "dialog-account.h"
#include "gnc-account-sel.h"
#include "gnc-commodity.h"
#include "gnc-gtk-utils.h"
#include "gnc-ui-util.h"
#include "qof.h"
#include "gnc-session.h"
#include "dialog-utils.h"

#define QKEY "gas_shared_quickfill"

/* Signal codes */
enum
{
    ACCOUNT_SEL_CHANGED,
    LAST_SIGNAL
};

enum account_cols
{
    ACCT_COL_NAME = 0,
    ACCT_COL_PTR,
    NUM_ACCT_COLS
};

#define BUFLEN 1024

struct _GNCAccountSel
{
    GtkBox hbox;
    gboolean isModal;
    GtkListStore *store;
    GtkComboBox *combo;
    GList *acctTypeFilters;
    GList *acctCommodityFilters;
    GList *acctExcludeList;
    gnc_commodity *default_new_commodity;

    /* The state of this pointer also serves as a flag about what state
     * the widget is in WRT the new-account-button ability. */
    GtkWidget *newAccountButton;
    GtkTreeRowReference *saved_account_ref;
    gulong row_changed_id;
    gulong row_deleted_id;

    char sep_key_prefix[BUFLEN];
    gboolean hide_placeholder;
    gboolean hide_hidden;
};

enum
{
    PROP_0,
    PROP_HIDE_PLACEHOLDER,
    PROP_HIDE_HIDDEN,
    PROP_HORIZONTAL_EXPAND,
    PROP_COMBO_ENTRY_WIDTH,
};

static guint account_sel_signals [LAST_SIGNAL] = { 0 };

static void gnc_account_sel_finalize (GObject *object);
static void gnc_account_sel_dispose (GObject *object);

static void gas_set_property (GObject      *object,
                              guint         param_id,
                              const GValue *value,
                              GParamSpec   *pspec);

static void gas_get_property (GObject    *object,
                              guint       param_id,
                              GValue     *value,
                              GParamSpec *pspec);

static void gas_new_account_click (GtkButton *b, gpointer ud);

#define GNC_ACCOUNT_SEL_PATH "gnc-account-sel-path"

G_DEFINE_TYPE (GNCAccountSel, gnc_account_sel, GTK_TYPE_BOX)

static void
gas_set_property (GObject *object, guint param_id,
                  const GValue *value, GParamSpec *pspec)
{
    GNCAccountSel *gas;

    g_return_if_fail (object != NULL);
    g_return_if_fail (GNC_IS_ACCOUNT_SEL(object));

    gas = GNC_ACCOUNT_SEL(object);

    switch (param_id)
    {
        case PROP_HIDE_PLACEHOLDER:
            gas->hide_placeholder = g_value_get_boolean (value);
            break;

        case PROP_HIDE_HIDDEN:
            gas->hide_hidden = g_value_get_boolean (value);
            break;

        case PROP_HORIZONTAL_EXPAND:
            gtk_widget_set_hexpand (GTK_WIDGET(gas), g_value_get_boolean (value));
            gtk_widget_set_hexpand (GTK_WIDGET(gas->combo), g_value_get_boolean (value));
            break;

        case PROP_COMBO_ENTRY_WIDTH:
            {
                GtkEntry *entry = GTK_ENTRY(gtk_bin_get_child (GTK_BIN(gas->combo)));
                gboolean expand = FALSE;
                gint width = g_value_get_int (value);

                if (width == -1)
                    expand = TRUE;

                gtk_widget_set_hexpand (GTK_WIDGET(gas), expand);
                gtk_widget_set_hexpand (GTK_WIDGET(gas->combo), expand);

                gtk_entry_set_width_chars (entry, width);
                gtk_widget_queue_resize (GTK_WIDGET(gas));
            }
            break;

        default:
            G_OBJECT_WARN_INVALID_PROPERTY_ID(object, param_id, pspec);
            break;
    }
}

static void
gas_get_property (GObject *object, guint param_id,
                  GValue *value, GParamSpec *pspec)
{
    GNCAccountSel *gas;

    g_return_if_fail (object != NULL);
    g_return_if_fail (GNC_IS_ACCOUNT_SEL(object));

    gas = GNC_ACCOUNT_SEL(object);

    switch (param_id)
    {
        case PROP_HIDE_PLACEHOLDER:
            g_value_set_boolean (value, gas->hide_placeholder);
            break;

        case PROP_HIDE_HIDDEN:
            g_value_set_boolean (value, gas->hide_hidden);
            break;

        case PROP_HORIZONTAL_EXPAND:
            g_value_set_boolean (value, gtk_widget_get_hexpand (GTK_WIDGET(gas)));
            break;

        case PROP_COMBO_ENTRY_WIDTH:
            {
                GtkEntry *entry = GTK_ENTRY(gtk_bin_get_child (GTK_BIN(gas->combo)));
                g_value_set_int (value, gtk_entry_get_width_chars (entry));
            }
            break;

        default:
            G_OBJECT_WARN_INVALID_PROPERTY_ID(object, param_id, pspec);
            break;
    }
}

static void
gnc_account_sel_class_init (GNCAccountSelClass *klass)
{
    GObjectClass *object_class = G_OBJECT_CLASS(klass);

    object_class->finalize = gnc_account_sel_finalize;
    object_class->dispose = gnc_account_sel_dispose;

    object_class->set_property = gas_set_property;
    object_class->get_property = gas_get_property;

    g_object_class_install_property (
        object_class, PROP_HIDE_PLACEHOLDER,
        g_param_spec_boolean("hide-placeholder", "Hide Placeholder",
                             "Placeholder accounts are hidden", TRUE,
                             G_PARAM_READWRITE));

    g_object_class_install_property (
        object_class, PROP_HIDE_HIDDEN,
        g_param_spec_boolean("hide-hidden", "Hide Hidden",
                             "Hidden accounts are hidden", TRUE,
                             G_PARAM_READWRITE));

    g_object_class_install_property (
        object_class, PROP_HIDE_HIDDEN,
        g_param_spec_boolean("horizontal-expand", "Horizontal Expand",
                             "Should GAS take all horizontal space", TRUE,
                             G_PARAM_READWRITE));

    g_object_class_install_property (
        object_class, PROP_COMBO_ENTRY_WIDTH,
        g_param_spec_int("entry-width", "Number of Charactors",
                         "Set the width of the combo entry",
                         -1, 100, -1, G_PARAM_READWRITE));

    account_sel_signals [ACCOUNT_SEL_CHANGED] =
        g_signal_new ("account_sel_changed",
                      G_OBJECT_CLASS_TYPE (object_class),
                      G_SIGNAL_RUN_FIRST,
                      0,
                      NULL,
                      NULL,
                      g_cclosure_marshal_VOID__VOID,
                      G_TYPE_NONE,
                      0);
}

static void
combo_changed_cb (GNCAccountSel *gas, gpointer combo)
{
    GtkTreeModel *fmodel;
    GtkTreeIter fiter;
    GtkTreeIter iter;
    GtkTreePath *path = NULL;
    GtkTreePath *saved_account_path = NULL;
    gboolean emit_signal = TRUE;

    if (!gtk_combo_box_get_active_iter (GTK_COMBO_BOX(gas->combo), &fiter))
        return;

    fmodel = gtk_combo_box_get_model (GTK_COMBO_BOX(gas->combo));
    gtk_tree_model_filter_convert_iter_to_child_iter (GTK_TREE_MODEL_FILTER(fmodel),
                                                      &iter, &fiter);

    path = gtk_tree_model_get_path (GTK_TREE_MODEL(gas->store), &iter);

    if (gas->saved_account_ref)
    {
        saved_account_path = gtk_tree_row_reference_get_path (gas->saved_account_ref);
        gtk_tree_row_reference_free (gas->saved_account_ref);
    }
    gas->saved_account_ref = gtk_tree_row_reference_new (GTK_TREE_MODEL(gas->store), path);

    if (saved_account_path)
    {
        if (gtk_tree_path_compare (path, saved_account_path) == 0)
            emit_signal = FALSE;
    }
    gtk_tree_path_free (saved_account_path);
    gtk_tree_path_free (path);

    if (emit_signal)
        g_signal_emit_by_name (gas, "account_sel_changed");
}

static char*
normalize_and_fold (char* utf8_string)
{
    char *normalized, *folded;
    g_return_val_if_fail (utf8_string && *utf8_string, NULL);

    normalized = g_utf8_normalize (utf8_string, -1, G_NORMALIZE_NFC);
    if (!normalized)
        return NULL;
    folded = g_utf8_casefold (normalized, -1);
    g_free (normalized);
    return folded;
}

static gboolean
completion_function (GtkEntryCompletion *completion, const char *key,
                     GtkTreeIter *iter, gpointer user_data)
{
    GNCAccountSel *gas = GNC_ACCOUNT_SEL(user_data);
    GtkTreeModel *fmodel = gtk_combo_box_get_model (GTK_COMBO_BOX(gas->combo));
    gchar *full_name = NULL;
    gboolean ret = FALSE;

    gtk_tree_model_get (fmodel, iter, ACCT_COL_NAME, &full_name, -1);

    if (full_name && *full_name)
    {
        gchar *full_name_folded = normalize_and_fold (full_name);

        // key is normalised and casefolded
        if (g_strrstr (full_name_folded, key) != NULL)
            ret = TRUE;

        g_free (full_name_folded);
    }
    g_free (full_name);
    return ret;
}

static char*
normalize_and_lower (const char* utf8_string)
{
    char *normalized, *lowered;
    g_return_val_if_fail (utf8_string && *utf8_string, NULL);

    normalized = g_utf8_normalize (utf8_string, -1, G_NORMALIZE_NFC);
    if (!normalized)
        return NULL;
    lowered = g_utf8_strdown (normalized, -1);
    g_free (normalized);
    return lowered;
}

/* Set gas->sep_key_prefix to the account_full_name or to the longest
 * common characters in the account_full_name.
 */
static void
set_prefix_from_account_name (GNCAccountSel *gas, char* account_full_name,
                              gint item_offset_to_sep_char,
                              gint *sep_key_prefix_len)
{
    if (item_offset_to_sep_char < *sep_key_prefix_len)
    {
        *sep_key_prefix_len = item_offset_to_sep_char;
        memset (gas->sep_key_prefix, 0, BUFLEN);
        g_utf8_strncpy (gas->sep_key_prefix, account_full_name, *sep_key_prefix_len);
    }

    if (item_offset_to_sep_char == *sep_key_prefix_len)
    {
        char tmp_prefix[BUFLEN];

        memset (tmp_prefix, 0, BUFLEN);
        g_utf8_strncpy (tmp_prefix, account_full_name, *sep_key_prefix_len);

        if (g_strcmp0 (gas->sep_key_prefix, tmp_prefix) != 0)
        {
            do
            {
                gchar *tmp = g_strdup (gas->sep_key_prefix);
                (*sep_key_prefix_len)--;

                memset (tmp_prefix, 0, BUFLEN);
                g_utf8_strncpy (tmp_prefix, account_full_name, *sep_key_prefix_len);
                memset (gas->sep_key_prefix, 0, BUFLEN);
                g_utf8_strncpy (gas->sep_key_prefix, tmp, *sep_key_prefix_len);
                g_free (tmp);

            } while (g_strcmp0 (gas->sep_key_prefix, tmp_prefix) != 0);
        }
    }
}

static inline gboolean
find_next_separator (char* account_full_name,
                     gint *item_offset_to_sep_char,
                     gunichar sep_unichar)
{
    const char* c;
    gunichar uc;
    gboolean found = FALSE;

    c = g_utf8_offset_to_pointer (account_full_name, *item_offset_to_sep_char);
    (*item_offset_to_sep_char)++;

    while (*c)
    {
        uc = g_utf8_get_char (c);
        if (uc == sep_unichar)
        {
            found = TRUE;
            break;
        }
        c = g_utf8_next_char (c);
        (*item_offset_to_sep_char)++;
    }
    return found;
}

/* Callback for Account separator key */
static void
entry_insert_text_cb (GtkEntry *entry, const gchar *text, gint length,
                      gint *position, gpointer user_data)
{
    GNCAccountSel *gas = GNC_ACCOUNT_SEL(user_data);
    GtkTreeModel *fmodel = gtk_combo_box_get_model (GTK_COMBO_BOX(gas->combo));
    const gchar *sep_char = gnc_get_account_separator_string ();
    gchar *lower_entered_text;
    glong entered_len;
    gunichar sep_unichar;
    gint sep_key_prefix_len = G_MAXINT;
    GtkTreeIter iter;
    gboolean valid;

    if (g_strcmp0 (text, sep_char) != 0)
        return;

    memset (gas->sep_key_prefix, 0, BUFLEN);

    const gchar *entered_text = gtk_entry_get_text (entry);

    if (!(entered_text && *entered_text))
        return;

    lower_entered_text = normalize_and_lower (entered_text);
    entered_len = g_utf8_strlen (lower_entered_text, -1); //characters
    sep_unichar = gnc_get_account_separator ();

    // Get the first item in the list
    valid = gtk_tree_model_get_iter_first (fmodel, &iter);

    // Walk through the list, reading each full name
    while (valid)
    {
        gchar *account_full_name;

        gtk_tree_model_get (fmodel, &iter, ACCT_COL_NAME, &account_full_name, -1);

        if (account_full_name && *account_full_name)
        {
            gchar *lower_account_full_name = normalize_and_lower (account_full_name);

            if (g_str_has_prefix (lower_account_full_name, lower_entered_text))
            {
                gint item_offset_to_sep_char = entered_len;
                gboolean found = find_next_separator (account_full_name,
                                                      &item_offset_to_sep_char,
                                                      sep_unichar);

                if (found)
                    set_prefix_from_account_name (gas, account_full_name,
                                                  item_offset_to_sep_char,
                                                  &sep_key_prefix_len);
            }
            g_free (lower_account_full_name);
        }
        g_free (account_full_name);
        valid = gtk_tree_model_iter_next (fmodel, &iter);
    }
    if (gas->sep_key_prefix[0] == 0)
        g_utf8_strncpy (gas->sep_key_prefix, entered_text, entered_len);

    g_free (lower_entered_text);

    if (gas->sep_key_prefix[0] != 0)
    {
        g_signal_handlers_block_by_func (GTK_EDITABLE(entry), (gpointer) entry_insert_text_cb, user_data);
        gtk_editable_delete_text (GTK_EDITABLE(entry), 0, -1);
        gtk_editable_set_position (GTK_EDITABLE(entry), 0);
        gtk_editable_insert_text (GTK_EDITABLE(entry), gas->sep_key_prefix, -1, position);
        g_signal_handlers_unblock_by_func (GTK_EDITABLE(entry), (gpointer) entry_insert_text_cb, user_data);
        g_signal_stop_emission_by_name (GTK_EDITABLE(entry), "insert_text");
    }
}

static void
update_entry_and_refilter (GNCAccountSel *gas)
{
    GtkEntry *entry = GTK_ENTRY(gtk_bin_get_child (GTK_BIN(gas->combo)));
    GtkTreeModel *fmodel = gtk_combo_box_get_model (GTK_COMBO_BOX(gas->combo));

    gtk_editable_delete_text (GTK_EDITABLE(entry), 0, -1);
    if (gas->saved_account_ref)
        gtk_tree_row_reference_free (gas->saved_account_ref);
    gas->saved_account_ref = NULL;
    gtk_combo_box_set_active (GTK_COMBO_BOX(gas->combo), -1);
    gtk_tree_model_filter_refilter (GTK_TREE_MODEL_FILTER(fmodel));
}

static void
toggle_placeholder_cb (GtkWidget *widget, gpointer user_data)
{
    GNCAccountSel *gas = GNC_ACCOUNT_SEL(user_data);
    gas->hide_placeholder = gtk_check_menu_item_get_active (GTK_CHECK_MENU_ITEM(widget));
    update_entry_and_refilter (gas);
}

static void
toggle_hidden_cb (GtkWidget *widget, gpointer user_data)
{
    GNCAccountSel *gas = GNC_ACCOUNT_SEL(user_data);
    gas->hide_hidden = gtk_check_menu_item_get_active (GTK_CHECK_MENU_ITEM(widget));
    update_entry_and_refilter (gas);
}

static void
icon_release_cb (GtkEntry *entry, GtkEntryIconPosition icon_pos,
                 GdkEvent* event, gpointer user_data)
{
    GNCAccountSel *gas = GNC_ACCOUNT_SEL(user_data);
    GtkWidget *menu, *h_placeholder, *h_hidden;

    if (icon_pos != GTK_ENTRY_ICON_SECONDARY)
        return;

    menu = gtk_menu_new ();
    h_placeholder = gtk_check_menu_item_new_with_mnemonic (_("Hide _Placeholder Accounts"));
    gtk_check_menu_item_set_active (GTK_CHECK_MENU_ITEM(h_placeholder), gas->hide_placeholder);
    h_hidden = gtk_check_menu_item_new_with_mnemonic (_("Hide _Hidden Accounts"));
    gtk_check_menu_item_set_active (GTK_CHECK_MENU_ITEM(h_hidden), gas->hide_hidden);
    gtk_menu_attach_to_widget (GTK_MENU(menu), GTK_WIDGET(gas), NULL);
    gtk_menu_shell_append (GTK_MENU_SHELL(menu), h_placeholder);
    gtk_menu_shell_append (GTK_MENU_SHELL(menu), h_hidden);
    gtk_widget_show_all (menu);

    g_signal_connect (G_OBJECT(h_placeholder), "toggled",
                      G_CALLBACK(toggle_placeholder_cb), gas);
    g_signal_connect (G_OBJECT(h_hidden), "toggled",
                      G_CALLBACK(toggle_hidden_cb), gas);

    gtk_menu_popup_at_pointer (GTK_MENU(menu), (GdkEvent *)event);
}

/* An account is included if gas->acctTypeFilters or gas->acctCommodityFilters
 * is populated and the account is in the list (both lists if both are populated)
 * and not in gas->acctExcludeList
 *
 * If no list is populated then all accounts are included.
 */
static gboolean
account_is_included (GNCAccountSel *gas, Account *acc)
{
    if (gas->acctExcludeList && g_list_find (gas->acctExcludeList, acc))
        return false;

    /* Filter as we've been configured to do. */
    if (gas->acctTypeFilters && !g_list_find (gas->acctTypeFilters, GINT_TO_POINTER (xaccAccountGetType (acc))))
        return false;

    if (gas->acctCommodityFilters && !g_list_find (gas->acctCommodityFilters, xaccAccountGetCommodity (acc)))
        return false;

    return true;
}

static gboolean
account_is_visible_func (GtkTreeModel *model, GtkTreeIter *iter, gpointer user_data)
{
    GNCAccountSel *gas = GNC_ACCOUNT_SEL(user_data);
    Account *acc;

    gtk_tree_model_get (GTK_TREE_MODEL(gas->store), iter, ACCT_COL_PTR, &acc, -1);

    if (!acc)
        return true;

    if (!account_is_included (gas, acc))
        return false;

    if (gas->hide_placeholder && xaccAccountGetPlaceholder (acc))
        return false;

    if (gas->hide_placeholder && xaccAccountIsHidden (acc))
        return false;

    return true;
}

static void
row_has_been_deleted_in_store_cb (GtkTreeModel *model, GtkTreePath *path, gpointer user_data)
{
    GNCAccountSel *gas = GNC_ACCOUNT_SEL(user_data);
    GtkTreePath *saved_account_path;

    if (!gas->saved_account_ref)
        return;

    saved_account_path = gtk_tree_row_reference_get_path (gas->saved_account_ref);

    if (saved_account_path == NULL) // path is already invalid after row delete
    {
        GtkEntry *entry = GTK_ENTRY(gtk_bin_get_child (GTK_BIN(gas->combo)));

        g_signal_handlers_block_by_func (gas->combo, combo_changed_cb , gas);
        gtk_combo_box_set_active (GTK_COMBO_BOX(gas->combo), -1);
        gtk_editable_delete_text (GTK_EDITABLE(entry), 0, -1);
        gtk_tree_row_reference_free (gas->saved_account_ref);
        gas->saved_account_ref = NULL;
        g_signal_emit_by_name (gas, "account_sel_changed");
        g_signal_handlers_unblock_by_func (gas->combo, combo_changed_cb , gas);
    }
    gtk_tree_path_free (saved_account_path);
}

static void
row_has_been_changed_in_store_cb (GtkTreeModel *model, GtkTreePath *path,
                                  GtkTreeIter *iter, gpointer user_data)
{
    GNCAccountSel *gas = GNC_ACCOUNT_SEL(user_data);
    GtkTreePath *saved_account_path;

    if (!gas->saved_account_ref)
        return;

    saved_account_path = gtk_tree_row_reference_get_path (gas->saved_account_ref);

    if (gtk_tree_path_compare (path, saved_account_path) == 0)
    {
        GtkEntry *entry = GTK_ENTRY(gtk_bin_get_child (GTK_BIN(gas->combo)));
        gchar *account_full_name = NULL;
        gint position = 0;

        g_signal_handlers_block_by_func (gas->combo, combo_changed_cb , gas);

        gtk_tree_model_get (model, iter, ACCT_COL_NAME, &account_full_name, -1);

        gtk_editable_delete_text (GTK_EDITABLE(entry), 0, -1);
        gtk_editable_insert_text (GTK_EDITABLE(entry), account_full_name, -1, &position);
        gtk_editable_set_position (GTK_EDITABLE(entry), -1);
        g_free (account_full_name);

        g_signal_handlers_unblock_by_func (gas->combo, combo_changed_cb , gas);

        // see if account visibility has changed
        if (!account_is_visible_func (model, iter, gas))
            update_entry_and_refilter (gas);
    }
    gtk_tree_path_free (saved_account_path);
}


static void
gnc_account_sel_init (GNCAccountSel *gas)
{
    GtkWidget *widget;
    GtkWidget *entry;
    GtkEntryCompletion *completion;
    Account *root = gnc_get_current_root_account ();
    GtkTreeModel *filter_model;

    gtk_orientable_set_orientation (GTK_ORIENTABLE(gas), GTK_ORIENTATION_HORIZONTAL);

    gas->default_new_commodity = NULL;
    gas->acctTypeFilters = NULL;
    gas->acctCommodityFilters = NULL;
    gas->acctExcludeList = NULL;
    gas->newAccountButton = NULL;
    gas->hide_placeholder = TRUE;
    gas->hide_hidden = TRUE;
    gas->saved_account_ref = NULL;
    gas->row_changed_id = 0;
    gas->row_deleted_id = 0;

    g_object_set (gas, "spacing", 2, (gchar*)NULL);

    // Set the name for this widget so it can be easily manipulated with css
    gtk_widget_set_name (GTK_WIDGET(gas), "gnc-id-account-select");

    // We are just using the quickfill list store which will be the same for all
    gas->store = gnc_get_shared_account_name_list_store (root, QKEY, NULL, NULL);

    // set sort order
    gtk_tree_sortable_set_sort_column_id (GTK_TREE_SORTABLE(gas->store),
                                          ACCT_COL_NAME, GTK_SORT_ASCENDING);

    // the filter will be unique for each GAS.
    filter_model = gtk_tree_model_filter_new (GTK_TREE_MODEL(gas->store), NULL);
    gtk_tree_model_filter_set_visible_func (GTK_TREE_MODEL_FILTER(filter_model),
                                            account_is_visible_func, gas, NULL);

    widget = gtk_combo_box_new_with_model_and_entry (GTK_TREE_MODEL(filter_model));
    g_object_unref (G_OBJECT(filter_model));
    gas->combo = GTK_COMBO_BOX(widget);
    gtk_combo_box_set_entry_text_column (GTK_COMBO_BOX(widget), ACCT_COL_NAME);

    gtk_container_add (GTK_CONTAINER(gas), widget);

    // set the default horizontal expansion to TRUE
    gtk_widget_set_hexpand (GTK_WIDGET(gas), TRUE);
    gtk_widget_set_hexpand (GTK_WIDGET(gas->combo), TRUE);

    entry = gtk_bin_get_child (GTK_BIN(gas->combo));
    gtk_entry_set_icon_from_icon_name (GTK_ENTRY(entry), GTK_ENTRY_ICON_SECONDARY,
                                       "preferences-system-symbolic");
    gtk_entry_set_icon_tooltip_text (GTK_ENTRY(entry), GTK_ENTRY_ICON_SECONDARY,
                                     _("Set the visibility of placeholder and hidden accounts."));
    g_signal_connect (G_OBJECT(entry), "icon-release",
                      G_CALLBACK(icon_release_cb), gas);
    g_signal_connect (G_OBJECT(entry), "insert_text",
                      G_CALLBACK(entry_insert_text_cb), gas);

    /* Add completion. */
    gnc_cbwe_require_list_item (GTK_COMBO_BOX(widget));
    completion = gtk_entry_get_completion (GTK_ENTRY(entry));
    gtk_entry_completion_set_match_func (completion,
                                         (GtkEntryCompletionMatchFunc)completion_function,
                                         gas, NULL);

    // Set default entry to none and blank entry
    gtk_combo_box_set_active (GTK_COMBO_BOX(gas->combo), -1);
    gtk_editable_delete_text (GTK_EDITABLE(entry), 0, -1);

    gas->row_deleted_id = g_signal_connect (G_OBJECT(gas->store), "row-deleted",
                                            G_CALLBACK(row_has_been_deleted_in_store_cb), gas);

    gas->row_changed_id = g_signal_connect (G_OBJECT(gas->store), "row-changed",
                                            G_CALLBACK(row_has_been_changed_in_store_cb), gas);

    g_signal_connect_swapped (gas->combo, "changed",
                              G_CALLBACK(combo_changed_cb), gas);
}

GtkWidget *
gnc_account_sel_new (void)
{
    GNCAccountSel *gas = g_object_new (GNC_TYPE_ACCOUNT_SEL, NULL);

    return GTK_WIDGET(gas);
}

typedef struct
{
    GNCAccountSel *gas;
    Account       *acct;
} gas_find_data;

static gboolean
gnc_account_sel_find_account (GtkTreeModel *fmodel,
                              GtkTreePath *path,
                              GtkTreeIter *iter,
                              gas_find_data *data)
{
    Account *model_acc;

    gtk_tree_model_get (fmodel, iter, ACCT_COL_PTR, &model_acc, -1);
    if (data->acct != model_acc)
        return FALSE;

    gtk_combo_box_set_active_iter (GTK_COMBO_BOX(data->gas->combo), iter);
    return TRUE;
}

/* If the account is included in the filters, set hide_placeholder
 * and hide_hidden accordingly to show it.
 */
static void
check_account_can_be_seen (GNCAccountSel *gas, GtkTreeModel *fmodel, Account *acct)
{
    gboolean changed = FALSE;
    gboolean included = account_is_included (gas, acct);

    if (included)
    {
        gboolean test = xaccAccountGetPlaceholder (acct);

        if (test && gas->hide_placeholder == test)
        {
            gas->hide_placeholder = !test;
            changed = TRUE;
        }

        test = xaccAccountIsHidden (acct);
        if (test && gas->hide_hidden == test)
        {
            gas->hide_hidden = !test;
            changed = TRUE;
        }
        if (changed)
            gtk_tree_model_filter_refilter (GTK_TREE_MODEL_FILTER(fmodel));
    }
}

void
gnc_account_sel_set_account (GNCAccountSel *gas, Account *acct,
                             gboolean set_default_acct)
{
    GtkTreeModel *fmodel;
    gas_find_data data;

    g_return_if_fail (gas != NULL);
    g_return_if_fail (GNC_IS_ACCOUNT_SEL(gas));

    fmodel = gtk_combo_box_get_model (GTK_COMBO_BOX(gas->combo));

    if (acct)
        check_account_can_be_seen (gas, fmodel, acct);

    if (set_default_acct)
    {
        gtk_combo_box_set_active (GTK_COMBO_BOX(gas->combo), 0);
        if (!acct)
            return;
    }
    else
    {
        gtk_combo_box_set_active (GTK_COMBO_BOX(gas->combo), -1);
        if (!acct)
        {
            GtkEntry *entry = GTK_ENTRY(gtk_bin_get_child (GTK_BIN(gas->combo)));
            gtk_editable_delete_text (GTK_EDITABLE(entry), 0, -1);
            return;
        }
    }
    data.gas = gas;
    data.acct = acct;
    gtk_tree_model_foreach (GTK_TREE_MODEL(fmodel),
                            (GtkTreeModelForeachFunc)gnc_account_sel_find_account,
                            &data);
}

Account*
gnc_account_sel_get_account (GNCAccountSel *gas)
{
    GtkTreeModel *fmodel;
    GtkTreeIter fiter;
    GtkTreeIter iter;
    Account *acc;

    g_return_val_if_fail (gas != NULL, NULL);
    g_return_val_if_fail (GNC_IS_ACCOUNT_SEL(gas), NULL);

    if (!gtk_combo_box_get_active_iter (GTK_COMBO_BOX(gas->combo), &fiter))
        return NULL;

    fmodel = gtk_combo_box_get_model (GTK_COMBO_BOX(gas->combo));

    gtk_tree_model_filter_convert_iter_to_child_iter (GTK_TREE_MODEL_FILTER(fmodel),
                                                      &iter, &fiter);

    gtk_tree_model_get (GTK_TREE_MODEL(gas->store), &iter,
                        ACCT_COL_PTR, &acc, -1);
    return acc;
}

void
gnc_account_sel_set_acct_filters (GNCAccountSel *gas, GList *typeFilters,
                                  GList *commodityFilters)
{
    g_return_if_fail (gas != NULL);
    g_return_if_fail (GNC_IS_ACCOUNT_SEL(gas));

    if (gas->acctTypeFilters != NULL)
    {
        g_list_free (gas->acctTypeFilters);
        gas->acctTypeFilters = NULL;
    }

    if (gas->acctCommodityFilters != NULL)
    {
        g_list_free (gas->acctCommodityFilters);
        gas->acctCommodityFilters = NULL;
    }

    /* This works because the GNCAccountTypes in the list are
     * ints-casted-as-pointers. */
    if (typeFilters)
        gas->acctTypeFilters = g_list_copy (typeFilters);

    /* Save the commodity filter list */
    if (commodityFilters)
        gas->acctCommodityFilters = g_list_copy (commodityFilters);

    update_entry_and_refilter (gas);
}


void
gnc_account_sel_set_acct_exclude_filter (GNCAccountSel *gas,
                                         GList *excludeFilter)
{
    g_return_if_fail (gas != NULL);
    g_return_if_fail (GNC_IS_ACCOUNT_SEL(gas));

    if (gas->acctExcludeList != NULL)
    {
        g_list_free (gas->acctExcludeList);
        gas->acctExcludeList = NULL;
    }

    if (excludeFilter)
        gas->acctExcludeList = g_list_copy (excludeFilter);

    update_entry_and_refilter (gas);
}

void
gnc_account_sel_set_default_new_commodity (GNCAccountSel *gas, gnc_commodity *new_commodity)
{
    g_return_if_fail (gas);
    g_return_if_fail (GNC_IS_COMMODITY (new_commodity));
    gas->default_new_commodity = new_commodity;
}

static void
gnc_account_sel_finalize (GObject *object)
{
    GNCAccountSel *gas;

    g_return_if_fail (object != NULL);
    g_return_if_fail (GNC_IS_ACCOUNT_SEL(object));

    gas = GNC_ACCOUNT_SEL(object);

    if (gas->acctTypeFilters)
        g_list_free (gas->acctTypeFilters);

    if (gas->acctCommodityFilters)
        g_list_free (gas->acctCommodityFilters);

    if (gas->acctExcludeList)
        g_list_free (gas->acctExcludeList);

    G_OBJECT_CLASS (gnc_account_sel_parent_class)->finalize (object);
}

static void
gnc_account_sel_dispose (GObject *object)
{
    GNCAccountSel *gas;

    g_return_if_fail (object != NULL);
    g_return_if_fail (GNC_IS_ACCOUNT_SEL(object));

    gas = GNC_ACCOUNT_SEL(object);

    if (gas->row_changed_id > 0)
        g_signal_handler_disconnect (G_OBJECT(gas->store), gas->row_changed_id);
    gas->row_changed_id = 0;

    if (gas->row_deleted_id > 0)
        g_signal_handler_disconnect (G_OBJECT(gas->store), gas->row_deleted_id);
    gas->row_deleted_id = 0;

    if (gas->saved_account_ref)
        gtk_tree_row_reference_free (gas->saved_account_ref);
    gas->saved_account_ref = NULL;

    G_OBJECT_CLASS (gnc_account_sel_parent_class)->dispose (object);
}

void
gnc_account_sel_set_new_account_ability (GNCAccountSel *gas,
                                         gboolean state)
{
    g_return_if_fail (gas != NULL);
    g_return_if_fail (GNC_IS_ACCOUNT_SEL(gas));

    if (state == (gas->newAccountButton != NULL))
    {
        /* We're already in that state; don't do anything. */
        return;
    }

    if (gas->newAccountButton)
    {
        g_assert (state == TRUE);
        /* destroy the existing button. */
        gtk_container_remove (GTK_CONTAINER(gas), gas->newAccountButton);
        gtk_widget_destroy (gas->newAccountButton);
        gas->newAccountButton = NULL;
        return;
    }

    /* Translators: This is a button label displayed in the account selector
     * control used in several dialogs. When pressed it opens the New Account
     * dialog.
     */
    gas->newAccountButton = gtk_button_new_with_label (_("New…"));
    g_signal_connect (gas->newAccountButton,
                      "clicked",
                      G_CALLBACK(gas_new_account_click),
                      gas);

    gtk_box_pack_start (GTK_BOX(gas), gas->newAccountButton,
                        FALSE, FALSE, 0);
}

void
gnc_account_sel_set_new_account_modal (GNCAccountSel *gas,
                                       gboolean state)
{
    g_return_if_fail (gas != NULL);
    g_return_if_fail (GNC_IS_ACCOUNT_SEL(gas));

    gas->isModal = state;
}

static void
gas_new_account_click (GtkButton *b, gpointer user_data)
{
    GNCAccountSel *gas = (GNCAccountSel*)user_data;
    GtkWindow *parent = GTK_WINDOW(gtk_widget_get_toplevel (GTK_WIDGET(gas)));

    if (gas->isModal)
    {
        Account *account = gnc_ui_new_accounts_from_name_with_defaults (parent, NULL, gas->acctTypeFilters,
                                                                        gas->default_new_commodity, NULL);
        if (account)
            gnc_account_sel_set_account (gas, account, FALSE);
    }
    else
        gnc_ui_new_account_with_types_and_commodity (parent, gnc_get_current_book(),
                                                     gas->acctTypeFilters, gas->default_new_commodity);
}

gint
gnc_account_sel_get_visible_account_num (GNCAccountSel *gas)
{
    GtkTreeModel *fmodel;

    g_return_val_if_fail (gas != NULL, 0);
    g_return_val_if_fail (GNC_IS_ACCOUNT_SEL(gas), 0);

    fmodel = gtk_combo_box_get_model (GTK_COMBO_BOX(gas->combo));

    return gtk_tree_model_iter_n_children (fmodel, NULL);
}
