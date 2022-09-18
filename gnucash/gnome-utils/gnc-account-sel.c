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

#include <gtk/gtk.h>
#include <glib/gi18n.h>

#include "dialog-account.h"
#include "gnc-account-sel.h"
#include "gnc-commodity.h"
#include "gnc-gtk-utils.h"
#include "gnc-ui-util.h"
#include "qof.h"
#include "gnc-session.h"
#include "dialog-utils.h"

#define ACCT_DATA_TAG "gnc-account-sel_acct"

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
    gboolean initDone;
    gboolean isModal;
    GtkListStore *store;
    GtkComboBox *combo;
    GList *acctTypeFilters;
    GList *acctCommodityFilters;
    gint eventHandlerId;
    /* The state of this pointer also serves as a flag about what state
     * the widget is in WRT the new-account-button ability. */
    GtkWidget *newAccountButton;
    gint currentSelection;
    char sep_key_prefix[BUFLEN];
};

static guint account_sel_signals [LAST_SIGNAL] = { 0 };

static void gnc_account_sel_init (GNCAccountSel *gas);
static void gnc_account_sel_class_init (GNCAccountSelClass *klass);
static void gnc_account_sel_finalize (GObject *object);
static void gnc_account_sel_dispose (GObject *object);

static void gas_filter_accounts (gpointer data, gpointer user_data);

static void gas_populate_list (GNCAccountSel *gas);

static void gas_new_account_click (GtkButton *b, gpointer ud);

static GtkBox *parent_class;

GType
gnc_account_sel_get_type (void)
{
    static GType account_sel_type = 0;

    if (account_sel_type == 0)
    {
        GTypeInfo account_sel_info =
        {
            sizeof (GNCAccountSelClass),
            NULL,
            NULL,
            (GClassInitFunc) gnc_account_sel_class_init,
            NULL,
            NULL,
            sizeof (GNCAccountSel),
            0,
            (GInstanceInitFunc) gnc_account_sel_init
        };

        account_sel_type = g_type_register_static (GTK_TYPE_BOX,
                                                   "GNCAccountSel",
                                                   &account_sel_info, 0);
    }
    return account_sel_type;
}

static void
gnc_account_sel_event_cb (QofInstance *entity,
                          QofEventId event_type,
                          gpointer user_data,
                          gpointer event_data)
{
    if (!(event_type == QOF_EVENT_CREATE
       || event_type == QOF_EVENT_MODIFY
       || event_type == QOF_EVENT_DESTROY)
       || !GNC_IS_ACCOUNT(entity))
    {
        return;
    }
    gas_populate_list ((GNCAccountSel*)user_data);
}

static void
gnc_account_sel_class_init (GNCAccountSelClass *klass)
{
    GObjectClass *object_class = G_OBJECT_CLASS(klass);

    parent_class = g_type_class_peek_parent (klass);

    object_class->finalize = gnc_account_sel_finalize;
    object_class->dispose = gnc_account_sel_dispose;

    account_sel_signals [ACCOUNT_SEL_CHANGED] =
        g_signal_new ("account_sel_changed",
                      G_OBJECT_CLASS_TYPE (object_class),
                      G_SIGNAL_RUN_FIRST,
                      G_STRUCT_OFFSET (GNCAccountSelClass, account_sel_changed),
                      NULL,
                      NULL,
                      g_cclosure_marshal_VOID__VOID,
                      G_TYPE_NONE,
                      0);
}

static void
combo_changed_cb (GNCAccountSel *gas, gpointer combo)
{
    gint selected = gtk_combo_box_get_active (GTK_COMBO_BOX(combo));
    if (selected == gas->currentSelection)
        return;

    gas->currentSelection = selected;
    g_signal_emit_by_name (gas, "account_sel_changed");
}

static char*
normalize_and_fold (char* utf8_string)
{
    char *normalized, *folded;
    g_return_val_if_fail (utf8_string && *utf8_string, NULL);

    normalized = g_utf8_normalize (utf8_string, -1, G_NORMALIZE_ALL);
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
    gchar *full_name = NULL;
    gboolean ret = FALSE;

    gtk_tree_model_get (GTK_TREE_MODEL(gas->store), iter,
                        ACCT_COL_NAME, &full_name, -1);

    if (full_name && *full_name)
    {
        gchar *fold_full_name = normalize_and_fold (full_name);

        // key is normalised and casefolded
        if (g_strrstr (fold_full_name, key) != NULL)
            ret = TRUE;

        g_free (fold_full_name);
    }
    g_free (full_name);
    return ret;
}

static char*
normalize_and_lower (char* utf8_string)
{
    char *normalized, *lowered;
    g_return_val_if_fail (utf8_string && *utf8_string, NULL);

    normalized = g_utf8_normalize (utf8_string, -1, G_NORMALIZE_ALL);
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
    gchar *entered_text, *lower_entered_text;
    glong entered_len;
    gunichar sep_unichar;
    gint sep_key_prefix_len = G_MAXINT;
    GtkTreeIter iter;
    gboolean valid;

    if (g_strcmp0 (text, sep_char) != 0)
        return;

    memset (gas->sep_key_prefix, 0, BUFLEN);

    entered_text = gtk_editable_get_chars (GTK_EDITABLE(entry), 0, -1);

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
    g_free (entered_text);

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
gnc_account_sel_init (GNCAccountSel *gas)
{
    GtkWidget *widget;
    GtkWidget *entry;
    GtkEntryCompletion *completion;

    gtk_orientable_set_orientation (GTK_ORIENTABLE(gas), GTK_ORIENTATION_HORIZONTAL);

    gas->initDone = FALSE;
    gas->acctTypeFilters = FALSE;
    gas->newAccountButton = NULL;
    gas->currentSelection = -1;

    g_object_set (gas, "spacing", 2, (gchar*)NULL);

    // Set the name for this widget so it can be easily manipulated with css
    gtk_widget_set_name (GTK_WIDGET(gas), "gnc-id-account-select");

    gas->store = gtk_list_store_new (NUM_ACCT_COLS, G_TYPE_STRING, G_TYPE_POINTER);
    widget = gtk_combo_box_new_with_model_and_entry (GTK_TREE_MODEL(gas->store));
    gas->combo = GTK_COMBO_BOX(widget);
    gtk_combo_box_set_entry_text_column (GTK_COMBO_BOX(widget), ACCT_COL_NAME);
    g_signal_connect_swapped (gas->combo, "changed",
                              G_CALLBACK(combo_changed_cb), gas);
    gtk_container_add (GTK_CONTAINER(gas), widget);

    entry = gtk_bin_get_child (GTK_BIN(gas->combo));
    g_signal_connect (G_OBJECT(entry), "insert_text",
                      G_CALLBACK(entry_insert_text_cb), gas);

    /* Add completion. */
    gnc_cbwe_require_list_item (GTK_COMBO_BOX(widget));
    completion = gtk_entry_get_completion (GTK_ENTRY(entry));
    gtk_entry_completion_set_match_func (completion,
                                         (GtkEntryCompletionMatchFunc)completion_function,
                                         gas, NULL);

    /* Get the accounts, place into combo list */
    gas_populate_list (gas);

    // set sort order
    gtk_tree_sortable_set_sort_column_id (GTK_TREE_SORTABLE(gas->store),
                                          ACCT_COL_NAME, GTK_SORT_ASCENDING);

    gas->eventHandlerId =
        qof_event_register_handler (gnc_account_sel_event_cb, gas);

    gas->initDone = TRUE;
}

void
gnc_account_sel_set_hexpand (GNCAccountSel *gas, gboolean expand)
{
    gtk_widget_set_hexpand (GTK_WIDGET(gas), expand);
    gtk_widget_set_hexpand (GTK_WIDGET(gas->combo), expand);
}

typedef struct
{
    GNCAccountSel *gas;
    GList *outList;
} account_filter_data;

static void
gas_populate_list (GNCAccountSel *gas)
{
    account_filter_data atnd;
    Account *root;
    Account *acc;
    GtkTreeIter iter;
    GtkEntry *entry;
    gint i, active = -1;
    GList *accts, *ptr;
    const gchar *currentSel;

    entry = GTK_ENTRY(gtk_bin_get_child (GTK_BIN(gas->combo)));
    currentSel = gtk_entry_get_text (entry);

    g_signal_handlers_block_by_func (gas->combo, combo_changed_cb , gas);

    root = gnc_book_get_root_account (gnc_get_current_book ());
    accts = gnc_account_get_descendants_sorted (root);

    atnd.gas        = gas;
    atnd.outList    = NULL;

    g_list_foreach (accts, gas_filter_accounts, (gpointer)&atnd);
    g_list_free (accts);
    atnd.outList = g_list_reverse (atnd.outList);

    gtk_list_store_clear (gas->store);
    for (ptr = atnd.outList, i = 0; ptr; ptr = g_list_next(ptr), i++)
    {
        acc = ptr->data;
        if (acc)
        {
            gchar *name = gnc_account_get_full_name (acc);

            if (!(name && *name))
                return;

            gtk_list_store_append (gas->store, &iter);
            gtk_list_store_set (gas->store, &iter,
                                ACCT_COL_NAME, name,
                                ACCT_COL_PTR,  acc,
                                -1);

            if (g_utf8_collate (name, currentSel) == 0)
                active = i;

            g_free (name);
        }
    }

    /* If the account which was in the text box before still exists, then
     * reset to it. */
    if (active != -1)
        gtk_combo_box_set_active (GTK_COMBO_BOX(gas->combo), active);

    g_signal_handlers_unblock_by_func (gas->combo, combo_changed_cb , gas);

    g_list_free (atnd.outList);
}

static void
gas_filter_accounts (gpointer data, gpointer user_data)
{
    account_filter_data *atnd;
    Account *a;

    atnd = (account_filter_data*)user_data;
    a = (Account*)data;
    /* Filter as we've been configured to do. */
    if (atnd->gas->acctTypeFilters)
    {
        /* g_list_find is the poor-mans '(member ...)', especially
         * easy when the data pointers in the list are just casted
         * account type identifiers. */
        if (g_list_find (atnd->gas->acctTypeFilters,
                          GINT_TO_POINTER(xaccAccountGetType (a)))
                == NULL)
        {
            return;
        }
    }

    if (atnd->gas->acctCommodityFilters)
    {
        if (g_list_find_custom (atnd->gas->acctCommodityFilters,
                                GINT_TO_POINTER(xaccAccountGetCommodity (a)),
                                gnc_commodity_compare_void)
                == NULL)
        {
            return;
        }
    }
    atnd->outList = g_list_prepend (atnd->outList, a);
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
gnc_account_sel_find_account (GtkTreeModel *model,
                              GtkTreePath *path,
                              GtkTreeIter *iter,
                              gas_find_data *data)
{
    Account *model_acc;

    gtk_tree_model_get (model, iter, ACCT_COL_PTR, &model_acc, -1);
    if (data->acct != model_acc)
        return FALSE;

    gtk_combo_box_set_active_iter (GTK_COMBO_BOX(data->gas->combo), iter);
    return TRUE;
}

void
gnc_account_sel_set_account (GNCAccountSel *gas, Account *acct,
                             gboolean set_default_acct)
{
    gas_find_data data;

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
    gtk_tree_model_foreach (GTK_TREE_MODEL(gas->store),
                            (GtkTreeModelForeachFunc)gnc_account_sel_find_account,
                            &data);
}

Account*
gnc_account_sel_get_account (GNCAccountSel *gas)
{
    GtkTreeIter iter;
    Account *acc;

    if (!gtk_combo_box_get_active_iter (GTK_COMBO_BOX(gas->combo), &iter))
        return NULL;

    gtk_tree_model_get (GTK_TREE_MODEL(gas->store), &iter,
                        ACCT_COL_PTR, &acc,
                        -1);
    return acc;
}

void
gnc_account_sel_set_acct_filters (GNCAccountSel *gas, GList *typeFilters,
                                  GList *commodityFilters)
{

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

    /* If both filters are null, then no filters exist. */
    if ((!typeFilters) && (!commodityFilters))
        return;

    /* This works because the GNCAccountTypes in the list are
     * ints-casted-as-pointers. */
    if (typeFilters)
        gas->acctTypeFilters = g_list_copy (typeFilters);

    /* Save the commodity filter list */
    if (commodityFilters)
        gas->acctCommodityFilters = g_list_copy (commodityFilters);

    gas_populate_list (gas);
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

    G_OBJECT_CLASS (parent_class)->finalize (object);
}

static void
gnc_account_sel_dispose (GObject *object)
{
    GNCAccountSel *gas;

    g_return_if_fail (object != NULL);
    g_return_if_fail (GNC_IS_ACCOUNT_SEL(object));

    gas = GNC_ACCOUNT_SEL(object);

    if (gas->store)
    {
        g_object_unref (gas->store);
        gas->store = NULL;
    }

    if (gas->eventHandlerId)
    {
        qof_event_unregister_handler (gas->eventHandlerId);
        gas->eventHandlerId = 0;
    }

    G_OBJECT_CLASS (parent_class)->dispose (object);
}

void
gnc_account_sel_set_new_account_ability (GNCAccountSel *gas,
                                         gboolean state)
{
    g_return_if_fail (gas != NULL);

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

    /* create the button. */
    gas->newAccountButton = gtk_button_new_with_label (_("New..."));
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
    gas->isModal = state;
}

static void
gas_new_account_click (GtkButton *b, gpointer ud)
{
    GNCAccountSel *gas = (GNCAccountSel*)ud;
    Account *account = NULL;
    GtkWindow *parent = GTK_WINDOW(gtk_widget_get_toplevel (GTK_WIDGET(gas)));
    if (gas->isModal)
    {
        account = gnc_ui_new_accounts_from_name_window_with_types (parent, NULL,
                                                                   gas->acctTypeFilters);
        if (account)
            gnc_account_sel_set_account (gas, account, FALSE);
    }
    else
        gnc_ui_new_account_with_types (parent, gnc_get_current_book(),
                                       gas->acctTypeFilters);
}

gint
gnc_account_sel_get_num_account (GNCAccountSel *gas)
{
    if (NULL == gas)
        return 0;

    return gtk_tree_model_iter_n_children (GTK_TREE_MODEL(gas->store), NULL);
}

void
gnc_account_sel_purge_account (GNCAccountSel *gas,
                               Account *target,
                               gboolean recursive)
{
    GtkTreeModel *model = GTK_TREE_MODEL(gas->store);
    GtkTreeIter iter;
    Account *acc;
    gboolean more;

    if (!gtk_tree_model_get_iter_first (model, &iter))
        return;

    if (!recursive)
    {
        do
        {
            gtk_tree_model_get (model, &iter, ACCT_COL_PTR, &acc, -1);
            if (acc == target)
            {
                gtk_list_store_remove (gas->store, &iter);
                break;
            }
        }
        while (gtk_tree_model_iter_next (model, &iter));
    }
    else
    {
        do
        {
            gtk_tree_model_get (model, &iter, ACCT_COL_PTR, &acc, -1);
            while (acc)
            {
                if (acc == target)
                    break;
                acc = gnc_account_get_parent (acc);
            }

            if (acc == target)
                more = gtk_list_store_remove (gas->store, &iter);
            else
                more = gtk_tree_model_iter_next (model, &iter);
        }
        while (more);
    }
    gtk_combo_box_set_active (GTK_COMBO_BOX(gas->combo), 0);
}
