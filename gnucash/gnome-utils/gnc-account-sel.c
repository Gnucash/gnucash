/**
 * gnc-account-sel.c -- account selection widget
 *
 * Copyright (C) 2002 Joshua Sled <jsled@asynchronous.org>
 * All rights reserved.
 * Copyright (C) 2006 David Hampton <hampton@employees.org>
 *
 * GnuCash is free software; you can redistribute it and/or modify it under
 * the terms of the GNU Library General Public License as published by the
 * Free Software Foundation; either version 2 of the License, or (at your
 * option) any later version.
 **/

#include <config.h>

#include <stdbool.h>
#include <gtk/gtk.h>
#include <glib/gi18n.h>

#include "account-quickfill.h"
#include "dialog-account.h"
#include "gnc-account-sel.h"
#include "gnc-commodity.h"
#include "gnc-ui-util.h"
#include "qof.h"
#include "gnc-session.h"
#include "dialog-utils.h"

#define QKEY "gas_shared_quickfill"
#define BUFLEN 1024

enum
{
    ACCOUNT_SEL_CHANGED,
    LAST_SIGNAL
};

enum
{
    PROP_0,
    PROP_HIDE_PLACEHOLDER,
    PROP_HIDE_HIDDEN,
    PROP_HORIZONTAL_EXPAND,
    PROP_COMBO_ENTRY_WIDTH,
};

struct _GNCAccountSel
{
    GtkBox hbox;
    gboolean isModal;
    GListModel *store;
    GListStore *matches;
    GtkSingleSelection *match_selection;
    GtkEntry *entry;
    GtkPopover *match_popover;
    GtkPopover *visibility_popover;
    GtkListView *match_view;
    Account *selected_account;
    GList *acctTypeFilters;
    GList *acctCommodityFilters;
    GList *acctExcludeList;
    gnc_commodity *default_new_commodity;
    GtkWidget *newAccountButton;
    gulong items_changed_id;
    guint refresh_source_id;
    char sep_key_prefix[BUFLEN];
    gboolean hide_placeholder;
    gboolean hide_hidden;
    gboolean updating_entry;
    gboolean showing_all;
};

static guint account_sel_signals [LAST_SIGNAL] = { 0 };

static void gnc_account_sel_finalize (GObject *object);
static void gnc_account_sel_dispose (GObject *object);
static void gas_new_account_click (GtkButton *button, gpointer user_data);
static void gas_update_matches (GNCAccountSel *gas, gboolean show_all,
                                gboolean present);

G_DEFINE_TYPE (GNCAccountSel, gnc_account_sel, GTK_TYPE_BOX)

static void
gas_set_property (GObject *object, guint param_id,
                  const GValue *value, GParamSpec *pspec)
{
    GNCAccountSel *gas = GNC_ACCOUNT_SEL (object);

    switch (param_id)
    {
    case PROP_HIDE_PLACEHOLDER:
        gas->hide_placeholder = g_value_get_boolean (value);
        break;
    case PROP_HIDE_HIDDEN:
        gas->hide_hidden = g_value_get_boolean (value);
        break;
    case PROP_HORIZONTAL_EXPAND:
        gtk_widget_set_hexpand (GTK_WIDGET (gas), g_value_get_boolean (value));
        gtk_widget_set_hexpand (GTK_WIDGET (gas->entry), g_value_get_boolean (value));
        break;
    case PROP_COMBO_ENTRY_WIDTH:
        {
            gint width = g_value_get_int (value);
            gboolean expand = width == -1;

            gtk_widget_set_hexpand (GTK_WIDGET (gas), expand);
            gtk_widget_set_hexpand (GTK_WIDGET (gas->entry), expand);
            gtk_editable_set_width_chars (GTK_EDITABLE (gas->entry), width);
        }
        break;
    default:
        G_OBJECT_WARN_INVALID_PROPERTY_ID (object, param_id, pspec);
        break;
    }
}

static void
gas_get_property (GObject *object, guint param_id,
                  GValue *value, GParamSpec *pspec)
{
    GNCAccountSel *gas = GNC_ACCOUNT_SEL (object);

    switch (param_id)
    {
    case PROP_HIDE_PLACEHOLDER:
        g_value_set_boolean (value, gas->hide_placeholder);
        break;
    case PROP_HIDE_HIDDEN:
        g_value_set_boolean (value, gas->hide_hidden);
        break;
    case PROP_HORIZONTAL_EXPAND:
        g_value_set_boolean (value, gtk_widget_get_hexpand (GTK_WIDGET (gas)));
        break;
    case PROP_COMBO_ENTRY_WIDTH:
        g_value_set_int (value,
                         gtk_editable_get_width_chars (GTK_EDITABLE (gas->entry)));
        break;
    default:
        G_OBJECT_WARN_INVALID_PROPERTY_ID (object, param_id, pspec);
        break;
    }
}

static void
gnc_account_sel_class_init (GNCAccountSelClass *klass)
{
    GObjectClass *object_class = G_OBJECT_CLASS (klass);

    object_class->finalize = gnc_account_sel_finalize;
    object_class->dispose = gnc_account_sel_dispose;
    object_class->set_property = gas_set_property;
    object_class->get_property = gas_get_property;

    g_object_class_install_property (
        object_class, PROP_HIDE_PLACEHOLDER,
        g_param_spec_boolean ("hide-placeholder", "Hide Placeholder",
                              "Placeholder accounts are hidden", TRUE,
                              G_PARAM_READWRITE));
    g_object_class_install_property (
        object_class, PROP_HIDE_HIDDEN,
        g_param_spec_boolean ("hide-hidden", "Hide Hidden",
                              "Hidden accounts are hidden", TRUE,
                              G_PARAM_READWRITE));
    g_object_class_install_property (
        object_class, PROP_HORIZONTAL_EXPAND,
        g_param_spec_boolean ("horizontal-expand", "Horizontal Expand",
                              "Should GAS take all horizontal space", TRUE,
                              G_PARAM_READWRITE));
    g_object_class_install_property (
        object_class, PROP_COMBO_ENTRY_WIDTH,
        g_param_spec_int ("entry-width", "Number of Characters",
                          "Set the width of the account entry",
                          -1, 100, -1, G_PARAM_READWRITE));

    account_sel_signals [ACCOUNT_SEL_CHANGED] =
        g_signal_new ("account_sel_changed",
                      G_OBJECT_CLASS_TYPE (object_class),
                      G_SIGNAL_RUN_FIRST,
                      0, NULL, NULL, g_cclosure_marshal_VOID__VOID,
                      G_TYPE_NONE, 0);
}

static char*
normalize_and_fold (const char *utf8_string)
{
    char *normalized;
    char *folded;

    g_return_val_if_fail (utf8_string && *utf8_string, NULL);
    normalized = g_utf8_normalize (utf8_string, -1, G_NORMALIZE_NFC);
    if (!normalized)
        return NULL;
    folded = g_utf8_casefold (normalized, -1);
    g_free (normalized);
    return folded;
}

static char*
normalize_and_lower (const char *utf8_string)
{
    char *normalized;
    char *lowered;

    g_return_val_if_fail (utf8_string && *utf8_string, NULL);
    normalized = g_utf8_normalize (utf8_string, -1, G_NORMALIZE_NFC);
    if (!normalized)
        return NULL;
    lowered = g_utf8_strdown (normalized, -1);
    g_free (normalized);
    return lowered;
}

static gboolean
account_is_included (GNCAccountSel *gas, Account *account)
{
    if (gas->acctExcludeList && g_list_find (gas->acctExcludeList, account))
        return FALSE;
    if (gas->acctTypeFilters &&
        !g_list_find (gas->acctTypeFilters,
                      GINT_TO_POINTER (xaccAccountGetType (account))))
        return FALSE;
    if (gas->acctCommodityFilters &&
        !g_list_find (gas->acctCommodityFilters,
                      xaccAccountGetCommodity (account)))
        return FALSE;
    return TRUE;
}

static gboolean
account_item_is_visible (GNCAccountSel *gas, GncAccountListItem *item)
{
    Account *account = gnc_account_list_item_get_account (item);

    if (!account)
        return TRUE;
    if (!account_is_included (gas, account))
        return FALSE;
    if (gas->hide_placeholder && xaccAccountGetPlaceholder (account))
        return FALSE;
    if (gas->hide_hidden && xaccAccountIsHidden (account))
        return FALSE;
    return TRUE;
}

static GncAccountListItem*
gas_find_account_item (GNCAccountSel *gas, Account *account)
{
    guint n_items = g_list_model_get_n_items (gas->store);

    for (guint index = 0; index < n_items; index++)
    {
        GncAccountListItem *item = GNC_ACCOUNT_LIST_ITEM (
            g_list_model_get_item (gas->store, index));

        if (gnc_account_list_item_get_account (item) == account)
            return item;
        g_object_unref (item);
    }
    return NULL;
}

static void
gas_set_entry_text (GNCAccountSel *gas, const char *text)
{
    gas->updating_entry = TRUE;
    gtk_editable_set_text (GTK_EDITABLE (gas->entry), text ? text : "");
    gas->updating_entry = FALSE;
}

static void
gas_select_account (GNCAccountSel *gas, Account *account, gboolean emit_signal)
{
    GncAccountListItem *item = NULL;
    gboolean changed;

    if (account)
    {
        item = gas_find_account_item (gas, account);
        if (!item)
            return;
    }

    changed = gas->selected_account != account;
    gas->selected_account = account;
    gas_set_entry_text (gas, item ? gnc_account_list_item_get_name (item) : "");
    g_clear_object (&item);

    if (changed && emit_signal)
        g_signal_emit (gas, account_sel_signals [ACCOUNT_SEL_CHANGED], 0);
}

static gint
account_item_compare (gconstpointer left, gconstpointer right)
{
    const GncAccountListItem *left_item = *(GncAccountListItem * const *)left;
    const GncAccountListItem *right_item = *(GncAccountListItem * const *)right;

    return g_utf8_collate (gnc_account_list_item_get_name ((GncAccountListItem *)left_item),
                           gnc_account_list_item_get_name ((GncAccountListItem *)right_item));
}

static void
gas_update_matches (GNCAccountSel *gas, gboolean show_all, gboolean present)
{
    const char *entry_text = gtk_editable_get_text (GTK_EDITABLE (gas->entry));
    char *folded_query = NULL;
    GPtrArray *items;
    guint n_items;

    gas->showing_all = show_all;
    g_list_store_remove_all (gas->matches);

    if (!show_all && (!entry_text || !*entry_text))
    {
        gtk_popover_popdown (gas->match_popover);
        return;
    }

    if (!show_all)
    {
        folded_query = normalize_and_fold (entry_text);
        if (!folded_query)
            return;
    }

    items = g_ptr_array_new_with_free_func (g_object_unref);
    n_items = g_list_model_get_n_items (gas->store);
    for (guint index = 0; index < n_items; index++)
    {
        GncAccountListItem *item = GNC_ACCOUNT_LIST_ITEM (
            g_list_model_get_item (gas->store, index));
        gboolean matches = account_item_is_visible (gas, item);

        if (matches && folded_query)
        {
            char *folded_name = normalize_and_fold (gnc_account_list_item_get_name (item));
            matches = folded_name && g_strrstr (folded_name, folded_query) != NULL;
            g_free (folded_name);
        }
        if (matches)
            g_ptr_array_add (items, item);
        else
            g_object_unref (item);
    }
    g_free (folded_query);

    g_ptr_array_sort (items, account_item_compare);
    for (guint index = 0; index < items->len; index++)
        g_list_store_append (gas->matches, g_ptr_array_index (items, index));
    g_ptr_array_unref (items);

    if (present && g_list_model_get_n_items (G_LIST_MODEL (gas->matches)) > 0)
        gtk_popover_popup (gas->match_popover);
    else if (g_list_model_get_n_items (G_LIST_MODEL (gas->matches)) == 0)
        gtk_popover_popdown (gas->match_popover);
}

static void
match_item_setup (GtkSignalListItemFactory *factory, GtkListItem *list_item,
                  gpointer user_data)
{
    GtkWidget *label = gtk_label_new (NULL);

    gtk_label_set_xalign (GTK_LABEL (label), 0.0);
    gtk_label_set_ellipsize (GTK_LABEL (label), PANGO_ELLIPSIZE_END);
    gtk_list_item_set_child (list_item, label);
    (void)factory;
    (void)user_data;
}

static void
match_item_bind (GtkSignalListItemFactory *factory, GtkListItem *list_item,
                 gpointer user_data)
{
    GncAccountListItem *item = GNC_ACCOUNT_LIST_ITEM (
        gtk_list_item_get_item (list_item));
    GtkLabel *label = GTK_LABEL (gtk_list_item_get_child (list_item));

    gtk_label_set_text (label, gnc_account_list_item_get_name (item));
    (void)factory;
    (void)user_data;
}

static void
match_view_activate_cb (GtkListView *view, guint position, gpointer user_data)
{
    GNCAccountSel *gas = GNC_ACCOUNT_SEL (user_data);
    GncAccountListItem *item = GNC_ACCOUNT_LIST_ITEM (
        g_list_model_get_item (G_LIST_MODEL (gas->matches), position));

    if (item)
    {
        gas_select_account (gas, gnc_account_list_item_get_account (item), TRUE);
        g_object_unref (item);
    }
    gtk_popover_popdown (gas->match_popover);
    (void)view;
}

static void
entry_changed_cb (GtkEditable *editable, gpointer user_data)
{
    GNCAccountSel *gas = GNC_ACCOUNT_SEL (user_data);

    if (gas->updating_entry)
        return;
    if (gas->selected_account)
    {
        gas->selected_account = NULL;
        g_signal_emit (gas, account_sel_signals [ACCOUNT_SEL_CHANGED], 0);
    }
    gas_update_matches (gas, FALSE, TRUE);
    (void)editable;
}

static void
entry_activate_cb (GtkEntry *entry, gpointer user_data)
{
    GNCAccountSel *gas = GNC_ACCOUNT_SEL (user_data);
    const char *text = gtk_editable_get_text (GTK_EDITABLE (entry));
    guint n_items = g_list_model_get_n_items (G_LIST_MODEL (gas->matches));

    for (guint index = 0; index < n_items; index++)
    {
        GncAccountListItem *item = GNC_ACCOUNT_LIST_ITEM (
            g_list_model_get_item (G_LIST_MODEL (gas->matches), index));

        if (g_utf8_collate (text, gnc_account_list_item_get_name (item)) == 0)
        {
            gas_select_account (gas, gnc_account_list_item_get_account (item), TRUE);
            g_object_unref (item);
            break;
        }
        g_object_unref (item);
    }
    gtk_popover_popdown (gas->match_popover);
}

static void
set_prefix_from_account_name (GNCAccountSel *gas, char *account_full_name,
                              gint item_offset_to_sep_char,
                              gint *sep_key_prefix_len)
{
    if (item_offset_to_sep_char < *sep_key_prefix_len)
    {
        *sep_key_prefix_len = item_offset_to_sep_char;
        memset (gas->sep_key_prefix, 0, BUFLEN);
        g_utf8_strncpy (gas->sep_key_prefix, account_full_name,
                        *sep_key_prefix_len);
    }

    if (item_offset_to_sep_char == *sep_key_prefix_len)
    {
        char tmp_prefix[BUFLEN] = { 0 };

        g_utf8_strncpy (tmp_prefix, account_full_name, *sep_key_prefix_len);
        if (g_strcmp0 (gas->sep_key_prefix, tmp_prefix) != 0)
        {
            do
            {
                char *prefix = g_strdup (gas->sep_key_prefix);

                (*sep_key_prefix_len)--;
                memset (tmp_prefix, 0, BUFLEN);
                g_utf8_strncpy (tmp_prefix, account_full_name,
                                *sep_key_prefix_len);
                memset (gas->sep_key_prefix, 0, BUFLEN);
                g_utf8_strncpy (gas->sep_key_prefix, prefix,
                                *sep_key_prefix_len);
                g_free (prefix);
            } while (g_strcmp0 (gas->sep_key_prefix, tmp_prefix) != 0);
        }
    }
}

static gboolean
find_next_separator (char *account_full_name, gint *item_offset_to_sep_char,
                     gunichar separator)
{
    const char *character = g_utf8_offset_to_pointer (account_full_name,
                                                       *item_offset_to_sep_char);

    (*item_offset_to_sep_char)++;
    while (*character)
    {
        if (g_utf8_get_char (character) == separator)
            return TRUE;
        character = g_utf8_next_char (character);
        (*item_offset_to_sep_char)++;
    }
    return FALSE;
}

static void
entry_insert_text_cb (GtkEntry *entry, const gchar *text, gint length,
                      gint *position, gpointer user_data)
{
    GNCAccountSel *gas = GNC_ACCOUNT_SEL (user_data);
    const gchar *separator = gnc_get_account_separator_string ();
    const gchar *entered_text;
    char *lower_entered_text;
    glong entered_length;
    gint separator_prefix_length = G_MAXINT;
    gunichar separator_character;
    guint n_items;

    if (g_strcmp0 (text, separator) != 0)
        return;

    memset (gas->sep_key_prefix, 0, BUFLEN);
    entered_text = gtk_editable_get_text (GTK_EDITABLE (entry));
    if (!entered_text || !*entered_text)
        return;

    lower_entered_text = normalize_and_lower (entered_text);
    if (!lower_entered_text)
        return;
    entered_length = g_utf8_strlen (lower_entered_text, -1);
    separator_character = gnc_get_account_separator ();
    n_items = g_list_model_get_n_items (gas->store);

    for (guint index = 0; index < n_items; index++)
    {
        GncAccountListItem *item = GNC_ACCOUNT_LIST_ITEM (
            g_list_model_get_item (gas->store, index));
        const char *name = gnc_account_list_item_get_name (item);

        if (account_item_is_visible (gas, item) && name && *name)
        {
            char *lower_name = normalize_and_lower (name);

            if (lower_name && g_str_has_prefix (lower_name, lower_entered_text))
            {
                gint separator_offset = entered_length;

                if (find_next_separator ((char *)name, &separator_offset,
                                         separator_character))
                    set_prefix_from_account_name (gas, (char *)name,
                                                  separator_offset,
                                                  &separator_prefix_length);
            }
            g_free (lower_name);
        }
        g_object_unref (item);
    }
    g_free (lower_entered_text);

    if (gas->sep_key_prefix[0] == 0)
        g_utf8_strncpy (gas->sep_key_prefix, entered_text, entered_length);

    if (gas->sep_key_prefix[0] != 0)
    {
        g_signal_handlers_block_by_func (entry, entry_insert_text_cb, user_data);
        gtk_editable_delete_text (GTK_EDITABLE (entry), 0, -1);
        gtk_editable_set_position (GTK_EDITABLE (entry), 0);
        gtk_editable_insert_text (GTK_EDITABLE (entry), gas->sep_key_prefix,
                                  -1, position);
        g_signal_handlers_unblock_by_func (entry, entry_insert_text_cb, user_data);
        g_signal_stop_emission_by_name (entry, "insert-text");
    }
    (void)length;
}

static void
gas_reset_for_filters (GNCAccountSel *gas)
{
    gas_select_account (gas, NULL, TRUE);
    gas_update_matches (gas, FALSE, FALSE);
}

static void
toggle_placeholder_cb (GtkCheckButton *button, gpointer user_data)
{
    GNCAccountSel *gas = GNC_ACCOUNT_SEL (user_data);

    gas->hide_placeholder = gtk_check_button_get_active (button);
    gas_reset_for_filters (gas);
}

static void
toggle_hidden_cb (GtkCheckButton *button, gpointer user_data)
{
    GNCAccountSel *gas = GNC_ACCOUNT_SEL (user_data);

    gas->hide_hidden = gtk_check_button_get_active (button);
    gas_reset_for_filters (gas);
}

static void
entry_icon_press_cb (GtkEntry *entry, GtkEntryIconPosition position,
                     gpointer user_data)
{
    GNCAccountSel *gas = GNC_ACCOUNT_SEL (user_data);

    if (position == GTK_ENTRY_ICON_PRIMARY)
        gas_update_matches (gas, TRUE, TRUE);
    else if (position == GTK_ENTRY_ICON_SECONDARY)
        gtk_popover_popup (gas->visibility_popover);
    (void)entry;
}

static void
check_account_can_be_seen (GNCAccountSel *gas, Account *account)
{
    gboolean changed = FALSE;

    if (!account_is_included (gas, account))
        return;

    if (xaccAccountGetPlaceholder (account) && gas->hide_placeholder)
    {
        gas->hide_placeholder = FALSE;
        changed = TRUE;
    }
    if (xaccAccountIsHidden (account) && gas->hide_hidden)
    {
        gas->hide_hidden = FALSE;
        changed = TRUE;
    }
    if (changed)
        gas_update_matches (gas, FALSE, FALSE);
}

static gboolean
gas_model_changed_idle (gpointer user_data)
{
    GNCAccountSel *gas = GNC_ACCOUNT_SEL (user_data);
    GncAccountListItem *item = NULL;

    gas->refresh_source_id = 0;
    if (gas->selected_account)
        item = gas_find_account_item (gas, gas->selected_account);
    if (!item || !account_item_is_visible (gas, item))
        gas_select_account (gas, NULL, TRUE);
    else
        gas_set_entry_text (gas, gnc_account_list_item_get_name (item));
    g_clear_object (&item);
    gas_update_matches (gas, gas->showing_all, FALSE);
    return G_SOURCE_REMOVE;
}

static void
store_items_changed_cb (GListModel *model, guint position, guint removed,
                        guint added, gpointer user_data)
{
    GNCAccountSel *gas = GNC_ACCOUNT_SEL (user_data);

    if (!gas->refresh_source_id)
        gas->refresh_source_id = g_idle_add_full (G_PRIORITY_DEFAULT_IDLE,
                                                  gas_model_changed_idle,
                                                  g_object_ref (gas),
                                                  g_object_unref);
    (void)model;
    (void)position;
    (void)removed;
    (void)added;
}

static void
gnc_account_sel_init (GNCAccountSel *gas)
{
    Account *root = gnc_get_current_root_account ();
    GtkListItemFactory *factory;
    GtkWidget *scroller;
    GtkWidget *visibility_box;
    GtkWidget *hide_placeholder;
    GtkWidget *hide_hidden;

    gtk_orientable_set_orientation (GTK_ORIENTABLE (gas), GTK_ORIENTATION_HORIZONTAL);
    gtk_box_set_spacing (GTK_BOX (gas), 2);
    gtk_widget_set_name (GTK_WIDGET (gas), "gnc-id-account-select");
    gtk_widget_set_hexpand (GTK_WIDGET (gas), TRUE);

    gas->hide_placeholder = TRUE;
    gas->hide_hidden = TRUE;
    gas->store = g_object_ref (gnc_get_shared_account_name_list_model (root, QKEY,
                                                                        NULL, NULL));
    gas->matches = g_list_store_new (GNC_TYPE_ACCOUNT_LIST_ITEM);
    gas->match_selection = gtk_single_selection_new (
        G_LIST_MODEL (g_object_ref (gas->matches)));

    gas->entry = GTK_ENTRY (gtk_entry_new ());
    gtk_widget_set_hexpand (GTK_WIDGET (gas->entry), TRUE);
    gtk_entry_set_icon_from_icon_name (gas->entry, GTK_ENTRY_ICON_PRIMARY,
                                       "pan-down-symbolic");
    gtk_entry_set_icon_tooltip_text (gas->entry, GTK_ENTRY_ICON_PRIMARY,
                                     _("Show all accounts."));
    gtk_entry_set_icon_from_icon_name (gas->entry, GTK_ENTRY_ICON_SECONDARY,
                                       "preferences-system-symbolic");
    gtk_entry_set_icon_tooltip_text (
        gas->entry, GTK_ENTRY_ICON_SECONDARY,
        _("Set the visibility of placeholder and hidden accounts."));
    gtk_box_prepend (GTK_BOX (gas), GTK_WIDGET (gas->entry));

    factory = gtk_signal_list_item_factory_new ();
    g_signal_connect (factory, "setup", G_CALLBACK (match_item_setup), NULL);
    g_signal_connect (factory, "bind", G_CALLBACK (match_item_bind), NULL);
    gas->match_view = GTK_LIST_VIEW (gtk_list_view_new (
        GTK_SELECTION_MODEL (g_object_ref (gas->match_selection)), factory));
    g_signal_connect (gas->match_view, "activate",
                      G_CALLBACK (match_view_activate_cb), gas);

    scroller = gtk_scrolled_window_new ();
    gtk_scrolled_window_set_policy (GTK_SCROLLED_WINDOW (scroller),
                                    GTK_POLICY_NEVER, GTK_POLICY_AUTOMATIC);
    gtk_scrolled_window_set_child (GTK_SCROLLED_WINDOW (scroller),
                                   GTK_WIDGET (gas->match_view));
    gtk_widget_set_size_request (scroller, 360, 240);
    gas->match_popover = GTK_POPOVER (gtk_popover_new ());
    gtk_popover_set_autohide (gas->match_popover, TRUE);
    gtk_popover_set_has_arrow (gas->match_popover, FALSE);
    gtk_popover_set_child (gas->match_popover, scroller);
    gtk_widget_set_parent (GTK_WIDGET (gas->match_popover),
                           GTK_WIDGET (gas->entry));

    visibility_box = gtk_box_new (GTK_ORIENTATION_VERTICAL, 6);
    hide_placeholder = gtk_check_button_new_with_mnemonic (
        _("Hide _Placeholder Accounts"));
    hide_hidden = gtk_check_button_new_with_mnemonic (_("Hide _Hidden Accounts"));
    gtk_check_button_set_active (GTK_CHECK_BUTTON (hide_placeholder),
                                 gas->hide_placeholder);
    gtk_check_button_set_active (GTK_CHECK_BUTTON (hide_hidden), gas->hide_hidden);
    gtk_box_append (GTK_BOX (visibility_box), hide_placeholder);
    gtk_box_append (GTK_BOX (visibility_box), hide_hidden);
    gas->visibility_popover = GTK_POPOVER (gtk_popover_new ());
    gtk_popover_set_child (gas->visibility_popover, visibility_box);
    gtk_widget_set_parent (GTK_WIDGET (gas->visibility_popover),
                           GTK_WIDGET (gas->entry));
    g_signal_connect (hide_placeholder, "toggled",
                      G_CALLBACK (toggle_placeholder_cb), gas);
    g_signal_connect (hide_hidden, "toggled", G_CALLBACK (toggle_hidden_cb), gas);

    g_signal_connect (gas->entry, "changed", G_CALLBACK (entry_changed_cb), gas);
    g_signal_connect (gas->entry, "activate", G_CALLBACK (entry_activate_cb), gas);
    g_signal_connect (gas->entry, "insert-text",
                      G_CALLBACK (entry_insert_text_cb), gas);
    g_signal_connect (gas->entry, "icon-press",
                      G_CALLBACK (entry_icon_press_cb), gas);
    gas->items_changed_id = g_signal_connect (gas->store, "items-changed",
                                               G_CALLBACK (store_items_changed_cb),
                                               gas);
}

GtkWidget*
gnc_account_sel_new (void)
{
    return GTK_WIDGET (g_object_new (GNC_TYPE_ACCOUNT_SEL, NULL));
}

static Account*
gas_first_visible_account (GNCAccountSel *gas)
{
    GncAccountListItem *best = NULL;
    guint n_items = g_list_model_get_n_items (gas->store);

    for (guint index = 0; index < n_items; index++)
    {
        GncAccountListItem *item = GNC_ACCOUNT_LIST_ITEM (
            g_list_model_get_item (gas->store, index));

        if (!account_item_is_visible (gas, item))
            g_object_unref (item);
        else if (!best || account_item_compare (&item, &best) < 0)
        {
            g_clear_object (&best);
            best = item;
        }
        else
            g_object_unref (item);
    }

    if (!best)
        return NULL;
    Account *account = gnc_account_list_item_get_account (best);
    g_object_unref (best);
    return account;
}

void
gnc_account_sel_set_account (GNCAccountSel *gas, Account *account,
                             gboolean set_default_account)
{
    g_return_if_fail (GNC_IS_ACCOUNT_SEL (gas));

    if (account)
        check_account_can_be_seen (gas, account);
    if (!account && set_default_account)
        account = gas_first_visible_account (gas);
    if (!account && !set_default_account)
    {
        gas_select_account (gas, NULL, TRUE);
        return;
    }

    GncAccountListItem *item = gas_find_account_item (gas, account);
    if (item && account_item_is_visible (gas, item))
        gas_select_account (gas, account, TRUE);
    g_clear_object (&item);
}

Account*
gnc_account_sel_get_account (GNCAccountSel *gas)
{
    GncAccountListItem *item;
    Account *account;

    g_return_val_if_fail (GNC_IS_ACCOUNT_SEL (gas), NULL);
    if (!gas->selected_account)
        return NULL;

    item = gas_find_account_item (gas, gas->selected_account);
    if (!item || !account_item_is_visible (gas, item))
    {
        g_clear_object (&item);
        return NULL;
    }
    account = gnc_account_list_item_get_account (item);
    g_object_unref (item);
    return account;
}

void
gnc_account_sel_set_acct_filters (GNCAccountSel *gas, GList *type_filters,
                                  GList *commodity_filters)
{
    g_return_if_fail (GNC_IS_ACCOUNT_SEL (gas));

    g_list_free (gas->acctTypeFilters);
    gas->acctTypeFilters = NULL;
    g_list_free (gas->acctCommodityFilters);
    gas->acctCommodityFilters = NULL;
    if (type_filters)
        gas->acctTypeFilters = g_list_copy (type_filters);
    if (commodity_filters)
        gas->acctCommodityFilters = g_list_copy (commodity_filters);
    gas_reset_for_filters (gas);
}

void
gnc_account_sel_set_acct_exclude_filter (GNCAccountSel *gas,
                                         GList *exclude_filter)
{
    g_return_if_fail (GNC_IS_ACCOUNT_SEL (gas));

    g_list_free (gas->acctExcludeList);
    gas->acctExcludeList = NULL;
    if (exclude_filter)
        gas->acctExcludeList = g_list_copy (exclude_filter);
    gas_reset_for_filters (gas);
}

void
gnc_account_sel_set_default_new_commodity (GNCAccountSel *gas,
                                           gnc_commodity *new_commodity)
{
    g_return_if_fail (GNC_IS_ACCOUNT_SEL (gas));
    g_return_if_fail (GNC_IS_COMMODITY (new_commodity));
    gas->default_new_commodity = new_commodity;
}

void
gnc_account_sel_set_new_account_ability (GNCAccountSel *gas, gboolean state)
{
    g_return_if_fail (GNC_IS_ACCOUNT_SEL (gas));

    if (state == (gas->newAccountButton != NULL))
        return;
    if (gas->newAccountButton)
    {
        gtk_box_remove (GTK_BOX (gas), gas->newAccountButton);
        gas->newAccountButton = NULL;
        return;
    }

    gas->newAccountButton = gtk_button_new_with_label (_("New…"));
    g_signal_connect (gas->newAccountButton, "clicked",
                      G_CALLBACK (gas_new_account_click), gas);
    gtk_box_append (GTK_BOX (gas), gas->newAccountButton);
}

void
gnc_account_sel_set_new_account_modal (GNCAccountSel *gas, gboolean state)
{
    g_return_if_fail (GNC_IS_ACCOUNT_SEL (gas));
    gas->isModal = state;
}

typedef struct
{
    GWeakRef selector;
} AccountSelCreateRequest;

static void
account_sel_create_request_finished (Account *account, gboolean accepted,
                                     gpointer user_data)
{
    AccountSelCreateRequest *request = user_data;
    GNCAccountSel *selector = GNC_ACCOUNT_SEL (g_weak_ref_get (&request->selector));

    if (selector && accepted && account &&
        gnc_account_get_book (account) == gnc_get_current_book ())
        gnc_account_sel_set_account (selector, account, FALSE);
    g_clear_object (&selector);
    g_weak_ref_clear (&request->selector);
    g_free (request);
}

static void
gas_new_account_click (GtkButton *button, gpointer user_data)
{
    GNCAccountSel *gas = GNC_ACCOUNT_SEL (user_data);
    GtkRoot *root = gtk_widget_get_root (GTK_WIDGET (gas));
    GtkWindow *parent = GTK_IS_WINDOW (root) ? GTK_WINDOW (root) : NULL;

    if (gas->isModal)
    {
        AccountSelCreateRequest *request = g_new0 (AccountSelCreateRequest, 1);
        g_weak_ref_init (&request->selector, G_OBJECT (gas));
        gnc_ui_new_accounts_from_name_with_defaults_async (
            parent, NULL, gas->acctTypeFilters, gas->default_new_commodity, NULL,
            account_sel_create_request_finished, request);
    }
    else
        gnc_ui_new_account_with_types_and_commodity (
            parent, gnc_get_current_book (), gas->acctTypeFilters,
            gas->default_new_commodity);
    (void)button;
}

gint
gnc_account_sel_get_visible_account_num (GNCAccountSel *gas)
{
    gint count = 0;
    guint n_items;

    g_return_val_if_fail (GNC_IS_ACCOUNT_SEL (gas), 0);
    n_items = g_list_model_get_n_items (gas->store);
    for (guint index = 0; index < n_items; index++)
    {
        GncAccountListItem *item = GNC_ACCOUNT_LIST_ITEM (
            g_list_model_get_item (gas->store, index));

        if (account_item_is_visible (gas, item))
            count++;
        g_object_unref (item);
    }
    return count;
}

static void
gnc_account_sel_finalize (GObject *object)
{
    GNCAccountSel *gas = GNC_ACCOUNT_SEL (object);

    g_list_free (gas->acctTypeFilters);
    g_list_free (gas->acctCommodityFilters);
    g_list_free (gas->acctExcludeList);
    G_OBJECT_CLASS (gnc_account_sel_parent_class)->finalize (object);
}

static void
disconnect_callbacks_recursive (GtkWidget *widget, gpointer data)
{
    GtkWidget *child;

    if (!widget)
        return;
    g_signal_handlers_disconnect_by_data (widget, data);
    for (child = gtk_widget_get_first_child (widget); child;
         child = gtk_widget_get_next_sibling (child))
        disconnect_callbacks_recursive (child, data);
}

static void
unparent_popover (GtkPopover **popover)
{
    GtkPopover *current = g_steal_pointer (popover);

    if (!current)
        return;
    gtk_popover_popdown (current);
    gtk_widget_unparent (GTK_WIDGET (current));
}

static void
gnc_account_sel_dispose (GObject *object)
{
    GNCAccountSel *gas = GNC_ACCOUNT_SEL (object);

    if (gas->refresh_source_id)
        g_source_remove (gas->refresh_source_id);
    gas->refresh_source_id = 0;
    if (gas->store && gas->items_changed_id)
        g_signal_handler_disconnect (gas->store, gas->items_changed_id);
    gas->items_changed_id = 0;

    disconnect_callbacks_recursive (GTK_WIDGET (gas), gas);
    disconnect_callbacks_recursive (GTK_WIDGET (gas->match_popover), gas);
    disconnect_callbacks_recursive (GTK_WIDGET (gas->visibility_popover), gas);
    unparent_popover (&gas->match_popover);
    unparent_popover (&gas->visibility_popover);
    gas->entry = NULL;
    gas->match_view = NULL;
    gas->newAccountButton = NULL;
    gas->selected_account = NULL;
    gas->default_new_commodity = NULL;
    g_clear_object (&gas->match_selection);
    g_clear_object (&gas->matches);
    g_clear_object (&gas->store);
    G_OBJECT_CLASS (gnc_account_sel_parent_class)->dispose (object);
}
