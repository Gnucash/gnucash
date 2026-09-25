/*
 * assistant-ab-initial.c -- Initialise the AqBanking wizard
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

/**
 * @internal
 * @file assistant-ab-initial.c
 * @brief AqBanking setup functionality
 * @author Copyright (C) 2002 Christian Stimming <stimming@tuhh.de>
 * @author Copyright (C) 2006 David Hampton <hampton@employees.org>
 * @author Copyright (C) 2008 Andreas Koehler <andi5.py@gmx.net>
 * @author Copyright (C) 2011 Robert Fewell
 * @author Copyright (C) 2020 Peter Zimmerer <pkzw@web.de>
 */

#include <config.h>

#include <platform.h>
#if PLATFORM(WINDOWS)
#include <windows.h>
#endif

#include "gnc-ab-utils.h" /* For version macros */

#include <aqbanking/banking.h>
#include <aqbanking/types/account_spec.h>
#include <gwenhywfar/gui.h>
#include <glib.h>
#include <glib/gi18n.h>
#include <glib/gstdio.h>
#include <gdk/gdk.h>
#ifdef HAVE_SYS_WAIT_H
#    include <sys/wait.h>
#endif
#include <fcntl.h>
#include <unistd.h>
#include <stdint.h>

#include "dialog-utils.h"
#include "assistant-ab-initial.h"
#include "gnc-ab-kvp.h"
#include "gnc-ab-utils.h"
#include "gnc-component-manager.h"
#include "gnc-string-utils.h"
#include "gnc-ui.h"
#include "gnc-ui-util.h"
#include "gnc-session.h"
#include "import-account-matcher.h"
/* This static indicates the debugging module that this .o belongs to.  */
static QofLogModule log_module = GNC_MOD_ASSISTANT;

#define GNC_PREFS_GROUP "dialogs.ab-initial"
#define ASSISTANT_AB_INITIAL_CM_CLASS "assistant-ab-initial"

typedef struct _ABInitialInfo ABInitialInfo;
typedef struct _AccCbData AccCbData;
typedef struct _RevLookupData RevLookupData;

#define AAI_PAGE_COUNT 4

static const gchar *const aai_page_names[AAI_PAGE_COUNT] =
{
    "intro", "setup", "match", "finish"
};

static const gchar *const aai_page_titles[AAI_PAGE_COUNT] =
{
    N_("Initial Online Banking Setup"),
    N_("Start Online Banking Setup"),
    N_("Match Online accounts with GnuCash accounts"),
    N_("Online Banking Setup Finished")
};

static guint aai_ab_account_hash(gconstpointer v);
static gboolean aai_ab_account_equal(gconstpointer v1, gconstpointer v2);
static gboolean banking_has_accounts(AB_BANKING *banking);
static void hash_from_kvp_acc_cb(Account *gnc_acc, gpointer user_data);
static ABInitialInfo *single_info = NULL;
static gchar *ab_account_longname(const GNC_AB_ACCOUNT_SPEC *ab_acc);
static GNC_AB_ACCOUNT_SPEC *update_account_list_acc_cb(GNC_AB_ACCOUNT_SPEC *ab_acc, gpointer user_data);
static void update_account_list(ABInitialInfo *info);
static gboolean find_gnc_acc_cb(gpointer key, gpointer value, gpointer user_data);
static void account_list_clicked_cb (GtkColumnView *view, guint position, gpointer user_data);
static void delete_account_match(ABInitialInfo *info, RevLookupData *data);
static void delete_selected_match_cb(gpointer data, gpointer user_data);
static void insert_acc_into_revhash_cb(gpointer ab_acc, gpointer gnc_acc, gpointer revhash);
static void remove_acc_from_revhash_cb(gpointer ab_acc, gpointer gnc_acc, gpointer revhash);
static void clear_kvp_acc_cb(gpointer key, gpointer value, gpointer user_data);
static void save_kvp_acc_cb(gpointer key, gpointer value, gpointer user_data);
static void aai_close_handler(gpointer user_data);
static ABInitialInfo *aai_info_ref (ABInitialInfo *info);
static void aai_info_unref (ABInitialInfo *info);
static void aai_request_close (ABInitialInfo *info);
static void aai_prepare_current_page (ABInitialInfo *info);
static void aai_update_navigation (ABInitialInfo *info);
static void aai_match_page_prepare (ABInitialInfo *info);

struct _ABInitialInfo
{
    gatomicrefcount ref_count;
    GtkWindow *window;
    GtkStack *stack;
    GtkLabel *page_title;
    GtkWidget *back_button;
    GtkWidget *next_button;
    GtkWidget *apply_button;
    GtkWidget *cancel_button;
    GtkWidget *setup_button;
    guint page_index;
    gboolean setup_running;
    gboolean close_requested;
    gboolean destroyed;

    /* Account match page. */
    gboolean match_page_prepared;
    GtkColumnView *account_view;
    GListStore *account_store;
    GtkMultiSelection *account_selection;

    /* AqBanking data. */
    AB_BANKING *api;
    /* AB_ACCOUNT* -> Account* -- the API owns the keys. */
    GHashTable *gnc_hash;
    /* Reverse hash table for lookup of matched GnuCash accounts. */
    GHashTable *gnc_revhash;
};
struct _AccCbData
{
    AB_BANKING *api;
    GHashTable *hash;
};

struct _RevLookupData
{
    Account *gnc_acc;
    GNC_AB_ACCOUNT_SPEC *ab_acc;
};

#define ACCOUNT_ROW_AB_ACCOUNT "ab-account"
#define ACCOUNT_ROW_GNC_NAME "gnc-name"
#define ACCOUNT_ROW_CHANGED "changed"

static void
account_row_factory_setup (GtkListItemFactory *factory, GtkListItem *list_item, gpointer user_data)
{
    GtkWidget *child;
    if (GPOINTER_TO_INT (user_data) == 2)
    {
        child = gtk_check_button_new ();
        gtk_widget_set_sensitive (child, FALSE);
    }
    else
    {
        child = gtk_label_new (NULL);
        gtk_label_set_xalign (GTK_LABEL (child), 0.0);
        gtk_label_set_ellipsize (GTK_LABEL (child), PANGO_ELLIPSIZE_END);
    }
    gtk_list_item_set_child (list_item, child);
}

static void
account_row_factory_bind (GtkListItemFactory *factory, GtkListItem *list_item, gpointer user_data)
{
    GtkStringObject *row = GTK_STRING_OBJECT (gtk_list_item_get_item (list_item));
    GtkWidget *child = gtk_list_item_get_child (list_item);
    switch (GPOINTER_TO_INT (user_data))
    {
    case 0:
        gtk_label_set_text (GTK_LABEL (child), gtk_string_object_get_string (row));
        break;
    case 1:
        gtk_label_set_text (GTK_LABEL (child), g_object_get_data (G_OBJECT (row), ACCOUNT_ROW_GNC_NAME));
        break;
    default:
        gtk_check_button_set_active (GTK_CHECK_BUTTON (child),
                                     GPOINTER_TO_INT (g_object_get_data (G_OBJECT (row), ACCOUNT_ROW_CHANGED)));
        break;
    }
}

static GtkColumnViewColumn *
account_view_add_column (GtkColumnView *view, const gchar *title, gint column, gboolean expand)
{
    GtkListItemFactory *factory = gtk_signal_list_item_factory_new ();
    GtkColumnViewColumn *result;
    g_signal_connect (factory, "setup", G_CALLBACK (account_row_factory_setup), GINT_TO_POINTER (column));
    g_signal_connect (factory, "bind", G_CALLBACK (account_row_factory_bind), GINT_TO_POINTER (column));
    result = gtk_column_view_column_new (title, factory);
    gtk_column_view_column_set_expand (result, expand);
    gtk_column_view_append_column (view, result);
    g_object_unref (result);
    return result;
}

static gint
account_row_index (ABInitialInfo *info, GNC_AB_ACCOUNT_SPEC *ab_acc)
{
    for (guint index = 0; index < g_list_model_get_n_items (G_LIST_MODEL (info->account_store)); index++)
    {
        GtkStringObject *row = g_list_model_get_item (G_LIST_MODEL (info->account_store), index);
        gboolean found = aai_ab_account_equal (g_object_get_data (G_OBJECT (row), ACCOUNT_ROW_AB_ACCOUNT), ab_acc);
        g_object_unref (row);
        if (found)
            return index;
    }
    return -1;
}

static void
account_row_update (ABInitialInfo *info, GNC_AB_ACCOUNT_SPEC *ab_acc, const gchar *gnc_name)
{
    gint index = account_row_index (info, ab_acc);
    if (index >= 0)
    {
        GtkStringObject *row = g_list_model_get_item (G_LIST_MODEL (info->account_store), index);
        g_object_set_data_full (G_OBJECT (row), ACCOUNT_ROW_GNC_NAME, g_strdup (gnc_name), g_free);
        g_object_set_data (G_OBJECT (row), ACCOUNT_ROW_CHANGED, GINT_TO_POINTER (TRUE));
        g_list_model_items_changed (G_LIST_MODEL (info->account_store), index, 1, 1);
        g_object_unref (row);
    }
}

static ABInitialInfo *
aai_info_ref (ABInitialInfo *info)
{
    g_return_val_if_fail (info, NULL);
    g_atomic_ref_count_inc (&info->ref_count);
    return info;
}

static void
aai_info_unref (ABInitialInfo *info)
{
    if (!info || !g_atomic_ref_count_dec (&info->ref_count))
        return;

    g_clear_object (&info->account_selection);
    g_clear_object (&info->account_store);
    g_clear_pointer (&info->gnc_hash, g_hash_table_destroy);
    g_clear_pointer (&info->gnc_revhash, g_hash_table_destroy);
    if (info->api)
        gnc_AB_BANKING_delete (info->api);
    g_free (info);
}

static gboolean
aai_current_page_complete (ABInitialInfo *info)
{
    g_return_val_if_fail (info, FALSE);

    switch (info->page_index)
    {
    case 0:
    case 2:
    case 3:
        return TRUE;
    case 1:
        return info->api && banking_has_accounts (info->api);
    default:
        return FALSE;
    }
}

static void
aai_update_navigation (ABInitialInfo *info)
{
    gboolean running;
    gboolean complete;
    GtkWidget *default_widget;

    if (!info || info->destroyed || !info->window)
        return;

    running = info->setup_running;
    complete = aai_current_page_complete (info);
    gtk_label_set_text (info->page_title, _(aai_page_titles[info->page_index]));
    gtk_widget_set_visible (info->back_button, info->page_index > 0);
    gtk_widget_set_sensitive (info->back_button, !running && info->page_index > 0);
    gtk_widget_set_visible (info->next_button, info->page_index + 1 < AAI_PAGE_COUNT);
    gtk_widget_set_sensitive (info->next_button, !running && complete &&
                              info->page_index + 1 < AAI_PAGE_COUNT);
    gtk_widget_set_visible (info->apply_button, info->page_index + 1 == AAI_PAGE_COUNT);
    gtk_widget_set_sensitive (info->apply_button, !running && complete &&
                              info->page_index + 1 == AAI_PAGE_COUNT);
    gtk_widget_set_sensitive (info->cancel_button, !running);
    gtk_widget_set_sensitive (info->setup_button, !running);

    default_widget = info->page_index + 1 == AAI_PAGE_COUNT
        ? info->apply_button : info->next_button;
    gtk_window_set_default_widget (info->window, default_widget);
}

static void
aai_match_page_prepare (ABInitialInfo *info)
{
    Account *root;
    AccCbData data;

    if (!info || info->destroyed || !info->api)
        return;

    if (!info->match_page_prepared)
    {
        root = gnc_book_get_root_account (gnc_get_current_book ());
        info->gnc_hash = g_hash_table_new (&aai_ab_account_hash, &aai_ab_account_equal);
        data.api = info->api;
        data.hash = info->gnc_hash;
        gnc_account_foreach_descendant (root, (AccountCb) hash_from_kvp_acc_cb, &data);
        info->gnc_revhash = g_hash_table_new (NULL, NULL);
        g_hash_table_foreach (data.hash, (GHFunc) insert_acc_into_revhash_cb,
                              info->gnc_revhash);
        info->match_page_prepared = TRUE;
    }
    update_account_list (info);
}

static void
aai_prepare_current_page (ABInitialInfo *info)
{
    if (!info || info->destroyed)
        return;

    if (info->page_index == 2)
        aai_match_page_prepare (info);
    aai_update_navigation (info);
}

static void
aai_set_page (ABInitialInfo *info, guint page_index)
{
    if (!info || info->destroyed || page_index >= AAI_PAGE_COUNT)
        return;

    info->page_index = page_index;
    gtk_stack_set_visible_child_name (info->stack, aai_page_names[page_index]);
    aai_prepare_current_page (info);
}

static void
aai_request_close (ABInitialInfo *info)
{
    if (!info || info->destroyed || !info->window)
        return;

    if (info->setup_running)
    {
        info->close_requested = TRUE;
        return;
    }
    gtk_window_destroy (info->window);
}

static gboolean
aai_key_pressed_cb (GtkEventControllerKey *controller, guint keyval,
                    guint keycode, GdkModifierType state, gpointer user_data)
{
    (void) controller;
    (void) keycode;
    (void) state;
    if (keyval != GDK_KEY_Escape)
        return FALSE;

    aai_request_close (user_data);
    return TRUE;
}

static gboolean
aai_window_close_request_cb (GtkWindow *window, gpointer user_data)
{
    (void) window;
    aai_request_close (user_data);
    return TRUE;
}

static void
aai_back_clicked_cb (GtkButton *button, gpointer user_data)
{
    ABInitialInfo *info = user_data;

    (void) button;
    if (info && !info->destroyed && !info->setup_running && info->page_index > 0)
        aai_set_page (info, info->page_index - 1);
}

static void
aai_next_clicked_cb (GtkButton *button, gpointer user_data)
{
    ABInitialInfo *info = user_data;

    (void) button;
    if (info && !info->destroyed && !info->setup_running &&
        aai_current_page_complete (info) && info->page_index + 1 < AAI_PAGE_COUNT)
        aai_set_page (info, info->page_index + 1);
}

static void
aai_cancel_clicked_cb (GtkButton *button, gpointer user_data)
{
    (void) button;
    aai_request_close (user_data);
}

static void
aai_destroy_cb (GtkWidget *object, gpointer user_data)
{
    ABInitialInfo *info = user_data;

    (void) object;
    if (!info || info->destroyed)
        return;

    info->destroyed = TRUE;
    if (info->window)
        gnc_save_window_size (GNC_PREFS_GROUP, info->window);
    if (single_info == info)
        single_info = NULL;
    gnc_unregister_gui_component_by_data (ASSISTANT_AB_INITIAL_CM_CLASS, info);
    info->window = NULL;
    info->stack = NULL;
    if (info->account_view)
        gtk_column_view_set_model (info->account_view, NULL);
    info->account_view = NULL;
    aai_info_unref (info);
}

static void
aai_button_clicked_cb (GtkButton *button, gpointer user_data)
{
    ABInitialInfo *info = user_data;
    GWEN_DIALOG *dialog;
    gint result;

    (void) button;
    if (!info || info->destroyed || info->setup_running || !info->api)
        return;

    info->setup_running = TRUE;
    aai_update_navigation (info);
    dialog = AB_Banking_CreateSetupDialog (info->api);
    if (!dialog)
    {
        PERR ("Could not lookup Setup Dialog of aqbanking!");
    }
    else
    {
        /* AqBanking owns this native setup interaction; GnuCash does not run
         * a GTK dialog loop here. Closing this window is deferred until it
         * returns so that its AB_BANKING instance remains valid. */
        result = GWEN_Gui_ExecDialog (dialog, 0);
        if (result <= 0)
            PERR ("Setup Dialog of aqbanking aborted/rejected, code %d", result);
        GWEN_Dialog_free (dialog);
    }
    info->setup_running = FALSE;
    aai_update_navigation (info);
    if (info->close_requested)
        aai_request_close (info);
}

static void
delete_account_match (ABInitialInfo *info, RevLookupData *data)
{
    g_return_if_fail (info && !info->destroyed && info->gnc_hash &&
                      info->account_view && data && data->ab_acc);

    g_hash_table_remove (info->gnc_hash, data->ab_acc);
    account_row_update (info, data->ab_acc, "");
}

static void
delete_selected_match_cb (gpointer data, gpointer user_data)
{
    RevLookupData lookup_data = { NULL, NULL };
    ABInitialInfo *info = user_data;
    GtkStringObject *row = data;

    g_return_if_fail (row && info && !info->destroyed && info->account_view);
    lookup_data.ab_acc = g_object_get_data (G_OBJECT (row), ACCOUNT_ROW_AB_ACCOUNT);
    if (lookup_data.ab_acc)
        delete_account_match (info, &lookup_data);
}

static void
aai_match_delete_button_clicked_cb (GtkButton *button, gpointer user_data)
{
    ABInitialInfo *info = user_data;

    (void) button;
    g_return_if_fail (info && !info->destroyed && info->api &&
                      info->account_view && info->gnc_hash);

    for (guint index = 0;
         index < g_list_model_get_n_items (G_LIST_MODEL (info->account_store));
         index++)
    {
        if (gtk_selection_model_is_selected (GTK_SELECTION_MODEL (info->account_selection), index))
        {
            GtkStringObject *row = g_list_model_get_item (G_LIST_MODEL (info->account_store), index);
            delete_selected_match_cb (row, info);
            g_object_unref (row);
        }
    }
}

static guint
aai_ab_account_hash (gconstpointer value)
{
    if (!value)
        return 0;
    return AB_AccountSpec_GetUniqueId ((const GNC_AB_ACCOUNT_SPEC *) value);
}

static gboolean
aai_ab_account_equal (gconstpointer first, gconstpointer second)
{
    if (!first || !second)
        return first == second;
    return AB_AccountSpec_GetUniqueId ((const GNC_AB_ACCOUNT_SPEC *) first) ==
           AB_AccountSpec_GetUniqueId ((const GNC_AB_ACCOUNT_SPEC *) second);
}

static void
insert_acc_into_revhash_cb (gpointer ab_acc, gpointer gnc_acc, gpointer revhash)
{
    g_return_if_fail (revhash && gnc_acc && ab_acc);
    g_hash_table_insert (revhash, gnc_acc, ab_acc);
}

static void
remove_acc_from_revhash_cb (gpointer ab_acc, gpointer gnc_acc, gpointer revhash)
{
    g_return_if_fail (revhash && gnc_acc);
    g_hash_table_remove (revhash, gnc_acc);
}
static void
aai_apply_clicked_cb (GtkButton *button, gpointer user_data)
{
    ABInitialInfo *info = user_data;

    (void) button;
    if (!info || info->destroyed || info->setup_running || info->page_index != 3 ||
        !info->gnc_hash || !info->gnc_revhash)
        return;

    /* Accounts that remain in the reverse table no longer have a mapping and
     * must have their AqBanking KVPs removed before the remaining mappings are
     * persisted. */
    g_hash_table_foreach (info->gnc_hash, (GHFunc) remove_acc_from_revhash_cb,
                          info->gnc_revhash);
    g_hash_table_foreach (info->gnc_revhash, (GHFunc) clear_kvp_acc_cb, NULL);
    g_hash_table_foreach (info->gnc_hash, (GHFunc) save_kvp_acc_cb, NULL);
    aai_request_close (info);
}
static gboolean
banking_has_accounts(AB_BANKING *banking)
{
    GNC_AB_ACCOUNT_SPEC_LIST *accl = NULL;
    gboolean result = FALSE;

    g_return_val_if_fail(banking, FALSE);

    if (AB_Banking_GetAccountSpecList (banking, &accl) >= 0 &&
        accl && AB_AccountSpec_List_GetCount (accl))
        result = TRUE;
    if (accl)
        AB_AccountSpec_List_free (accl);

    return result;
}

static void
hash_from_kvp_acc_cb(Account *gnc_acc, gpointer user_data)
{
    AccCbData *data = user_data;
    GNC_AB_ACCOUNT_SPEC *ab_acc;

    ab_acc = gnc_ab_get_ab_account(data->api, gnc_acc);
    if (ab_acc)
        g_hash_table_insert(data->hash, ab_acc, gnc_acc);
}

static gchar *
ab_account_longname(const GNC_AB_ACCOUNT_SPEC *ab_acc)
{
    gchar *bankname = NULL;
    gchar *result = NULL;
    const char *bankcode, *subAccountId, *account_number;

    g_return_val_if_fail(ab_acc, NULL);

    bankcode = AB_AccountSpec_GetBankCode(ab_acc);
    subAccountId = AB_AccountSpec_GetSubAccountNumber(ab_acc);
    account_number = AB_AccountSpec_GetAccountNumber (ab_acc);
    /* Translators: Strings are 1. Bank code, 2. Bank name,
       3. Account Number, 4. Subaccount ID                  */
    result = g_strdup_printf(_("Bank code %s (%s), Account %s (%s)"),
                             bankcode,
                             bankname ? bankname : "",
                             account_number,
                             subAccountId ? subAccountId : "");
    g_free(bankname);

    return result;

}

static GNC_AB_ACCOUNT_SPEC *
update_account_list_acc_cb(GNC_AB_ACCOUNT_SPEC *ab_acc, gpointer user_data)
{
    ABInitialInfo *info = user_data;
    gchar *gnc_name, *ab_name;
    Account *gnc_acc;
    GtkStringObject *row;

    g_return_val_if_fail(ab_acc && info, NULL);

    ab_name = ab_account_longname(ab_acc);

    /* Get corresponding gnucash account */
    gnc_acc = g_hash_table_lookup(info->gnc_hash, ab_acc);

    /* Build the text for the gnucash account. */
    if (gnc_acc)
        gnc_name = gnc_account_get_full_name(gnc_acc);
    else
        gnc_name = g_strdup("");

    /* Add item to the list store */
    row = gtk_string_object_new (ab_name);
    g_object_set_data (G_OBJECT (row), ACCOUNT_ROW_AB_ACCOUNT, ab_acc);
    g_object_set_data_full (G_OBJECT (row), ACCOUNT_ROW_GNC_NAME, g_strdup (gnc_name), g_free);
    g_object_set_data (G_OBJECT (row), ACCOUNT_ROW_CHANGED, GINT_TO_POINTER (FALSE));
    g_list_store_append (info->account_store, row);
    g_object_unref (row);
    g_free(gnc_name);
    g_free(ab_name);

    return NULL;
}

static void
update_account_list(ABInitialInfo *info)
{
    GNC_AB_ACCOUNT_SPEC_LIST *acclist = NULL;

    g_return_if_fail(info && info->api && info->gnc_hash);

    /* Refill the list */
    g_list_store_remove_all (info->account_store);
    if (AB_Banking_GetAccountSpecList(info->api, &acclist) >= 0 && acclist)
        AB_AccountSpec_List_ForEach(acclist, update_account_list_acc_cb, info);
    else
        g_warning("update_account_list: Oops, account list from AB_Banking "
                  "is NULL");

}

static gboolean
find_gnc_acc_cb(gpointer key, gpointer value, gpointer user_data)
{
    RevLookupData *data = user_data;

    g_return_val_if_fail(data, TRUE);

    if (value == data->gnc_acc)
    {
        data->ab_acc = (GNC_AB_ACCOUNT_SPEC*) key;
        return TRUE;
    }
    return FALSE;
}

typedef struct
{
    ABInitialInfo *info;
    GWeakRef window;
    GNC_AB_ACCOUNT_SPEC *ab_acc;
    Account *old_value;
} AccountPickerSelection;

static void
account_picker_finished_cb (Account *gnc_acc, gboolean accepted, gpointer user_data)
{
    AccountPickerSelection *selection = user_data;
    ABInitialInfo *info = selection->info;
    GtkWidget *window = g_weak_ref_get (&selection->window);

    if (window && accepted && !info->destroyed &&
        GTK_WINDOW (window) == info->window && info->gnc_hash &&
        selection->old_value != gnc_acc)
    {
        if (gnc_acc)
        {
            RevLookupData data;
            gchar *gnc_name;

            data.gnc_acc = gnc_acc;
            data.ab_acc = NULL;
            g_hash_table_find (info->gnc_hash, (GHRFunc) find_gnc_acc_cb, &data);
            if (data.ab_acc)
                delete_account_match (info, &data);

            g_hash_table_insert (info->gnc_hash, selection->ab_acc, gnc_acc);
            gnc_name = gnc_account_get_full_name (gnc_acc);
            account_row_update (info, selection->ab_acc, gnc_name);
            g_free (gnc_name);
        }
        else
        {
            g_hash_table_remove (info->gnc_hash, selection->ab_acc);
            account_row_update (info, selection->ab_acc, "");
        }
    }

    g_clear_object (&window);
    g_weak_ref_clear (&selection->window);
    aai_info_unref (info);
    g_free (selection);
}

static void
account_list_clicked_cb (GtkColumnView *view, guint position, gpointer user_data)
{
    ABInitialInfo *info = user_data;
    GtkStringObject *row;
    GNC_AB_ACCOUNT_SPEC *ab_acc;
    gchar *longname;
    Account *old_value;
    const gchar *currency;
    gnc_commodity *commodity = NULL;
    AccountPickerSelection *selection;

    g_return_if_fail(info);

    PINFO("Row has been double-clicked.");

    row = g_list_model_get_item (G_LIST_MODEL (info->account_store), position);
    if (!row)
        return;
    ab_acc = g_object_get_data (G_OBJECT (row), ACCOUNT_ROW_AB_ACCOUNT);

    if (ab_acc)
    {
        old_value = g_hash_table_lookup(info->gnc_hash, ab_acc);

        longname = ab_account_longname(ab_acc);
        currency = AB_AccountSpec_GetCurrency(ab_acc);
        if (currency && *currency)
        {
            commodity = gnc_commodity_table_lookup(
                            gnc_commodity_table_get_table(gnc_get_current_book()),
                            GNC_COMMODITY_NS_CURRENCY,
                            currency);
        }

        selection = g_new0 (AccountPickerSelection, 1);
        selection->info = aai_info_ref (info);
        selection->ab_acc = ab_acc;
        selection->old_value = old_value;
        g_weak_ref_init (&selection->window, info->window);
        gnc_import_select_account_async (GTK_WIDGET (info->window), NULL, TRUE,
                                         longname, commodity, ACCT_TYPE_BANK,
                                         old_value, account_picker_finished_cb,
                                         selection);
        g_free(longname);
    }
    g_object_unref (row);
}

static void
clear_kvp_acc_cb(gpointer gnc_acc, gpointer ab_acc, gpointer user_data)
{
    g_return_if_fail(gnc_acc);
    /* Delete "online-id" and complete "hbci..." KVPs for GnuCash account */
    gnc_account_delete_map_entry((Account *) gnc_acc, "online_id", NULL, NULL, FALSE);
    gnc_account_delete_map_entry((Account *) gnc_acc, "hbci", NULL, NULL, FALSE);
}

static void
save_kvp_acc_cb(gpointer key, gpointer value, gpointer user_data)
{
    GNC_AB_ACCOUNT_SPEC *ab_acc = key;
    Account *gnc_acc = value;
    guint32 ab_account_uid;
    const gchar *ab_accountid, *gnc_accountid;
    const gchar *ab_bankcode, *gnc_bankcode;
    gchar *ab_online_id;
    const gchar *gnc_online_id;

    g_return_if_fail(ab_acc && gnc_acc);

    ab_account_uid = AB_AccountSpec_GetUniqueId(ab_acc);
    if (gnc_ab_get_account_uid(gnc_acc) != ab_account_uid)
        gnc_ab_set_account_uid(gnc_acc, ab_account_uid);

    ab_accountid = AB_AccountSpec_GetAccountNumber(ab_acc);
    gnc_accountid = gnc_ab_get_account_accountid(gnc_acc);
    if (ab_accountid
            && (!gnc_accountid
                || (strcmp(ab_accountid, gnc_accountid) != 0)))
        gnc_ab_set_account_accountid(gnc_acc, ab_accountid);

    ab_bankcode = AB_AccountSpec_GetBankCode(ab_acc);
    gnc_bankcode = gnc_ab_get_account_bankcode(gnc_acc);
    if (ab_bankcode
            && (!gnc_bankcode
                || (strcmp(gnc_bankcode, ab_bankcode) != 0)))
        gnc_ab_set_account_bankcode(gnc_acc, ab_bankcode);

    ab_online_id = gnc_ab_create_online_id(ab_bankcode, ab_accountid);
    gnc_online_id = xaccAccountGetOnlineID(gnc_acc);
    if (ab_online_id && (!gnc_online_id || (strcmp(ab_online_id, gnc_online_id) != 0)))
        xaccAccountSetOnlineID(gnc_acc, ab_online_id);
    g_free(ab_online_id);
}

static void
aai_close_handler (gpointer user_data)
{
    aai_request_close (user_data);
}

static ABInitialInfo *
gnc_ab_initial_assistant_new (void)
{
    GtkBuilder *builder;
    GtkScrolledWindow *account_scrolledwindow;
    GtkWidget *delete_button;
    GtkEventController *key_controller;
    ABInitialInfo *info;
    gint component_id;

    info = g_new0 (ABInitialInfo, 1);
    g_atomic_ref_count_init (&info->ref_count);
    builder = gtk_builder_new ();
    if (!gnc_builder_add_from_file (builder, "assistant-ab-initial.glade",
                                    "aqbanking_init_assistant"))
    {
        g_object_unref (builder);
        aai_info_unref (info);
        return NULL;
    }

    info->window = GTK_WINDOW (gtk_builder_get_object (builder, "aqbanking_init_assistant"));
    info->stack = GTK_STACK (gtk_builder_get_object (builder, "assistant_stack"));
    info->page_title = GTK_LABEL (gtk_builder_get_object (builder, "assistant_page_title"));
    info->back_button = GTK_WIDGET (gtk_builder_get_object (builder, "assistant_back_button"));
    info->next_button = GTK_WIDGET (gtk_builder_get_object (builder, "assistant_next_button"));
    info->apply_button = GTK_WIDGET (gtk_builder_get_object (builder, "assistant_apply_button"));
    info->cancel_button = GTK_WIDGET (gtk_builder_get_object (builder, "assistant_cancel_button"));
    info->setup_button = GTK_WIDGET (gtk_builder_get_object (builder, "ab_assistant_button"));
    delete_button = GTK_WIDGET (gtk_builder_get_object (builder, "ab_match_delete_button"));
    account_scrolledwindow = GTK_SCROLLED_WINDOW (gtk_builder_get_object (
        builder, "account_scrolledwindow"));
    if (!info->window || !info->stack || !info->page_title || !info->back_button ||
        !info->next_button || !info->apply_button || !info->cancel_button ||
        !info->setup_button || !delete_button || !account_scrolledwindow)
    {
        g_warning ("assistant-ab-initial.glade is missing a required object");
        g_object_unref (builder);
        aai_info_unref (info);
        return NULL;
    }

    info->api = gnc_AB_BANKING_new ();
    if (!info->api)
    {
        g_warning ("Could not initialise AqBanking for the initial assistant");
        g_object_unref (builder);
        aai_info_unref (info);
        return NULL;
    }

    info->account_store = g_list_store_new (GTK_TYPE_STRING_OBJECT);
    info->account_selection = gtk_multi_selection_new (
        G_LIST_MODEL (g_object_ref (info->account_store)));
    info->account_view = GTK_COLUMN_VIEW (gtk_column_view_new (
        GTK_SELECTION_MODEL (g_object_ref (info->account_selection))));
    account_view_add_column (info->account_view, _("Online Banking Account Name"), 0, FALSE);
    account_view_add_column (info->account_view, _("GnuCash Account Name"), 1, TRUE);
    account_view_add_column (info->account_view, _("New?"), 2, FALSE);
    gtk_scrolled_window_set_child (account_scrolledwindow, GTK_WIDGET (info->account_view));

    g_signal_connect (info->window, "close-request",
                      G_CALLBACK (aai_window_close_request_cb), info);
    g_signal_connect (info->window, "destroy", G_CALLBACK (aai_destroy_cb), info);
    g_signal_connect (info->back_button, "clicked", G_CALLBACK (aai_back_clicked_cb), info);
    g_signal_connect (info->next_button, "clicked", G_CALLBACK (aai_next_clicked_cb), info);
    g_signal_connect (info->apply_button, "clicked", G_CALLBACK (aai_apply_clicked_cb), info);
    g_signal_connect (info->cancel_button, "clicked", G_CALLBACK (aai_cancel_clicked_cb), info);
    g_signal_connect (info->setup_button, "clicked", G_CALLBACK (aai_button_clicked_cb), info);
    g_signal_connect (delete_button, "clicked",
                      G_CALLBACK (aai_match_delete_button_clicked_cb), info);
    g_signal_connect (info->account_view, "activate",
                      G_CALLBACK (account_list_clicked_cb), info);

    key_controller = gtk_event_controller_key_new ();
    g_signal_connect (key_controller, "key-pressed", G_CALLBACK (aai_key_pressed_cb), info);
    gtk_widget_add_controller (GTK_WIDGET (info->window), key_controller);

    gnc_restore_window_size (GNC_PREFS_GROUP, info->window,
                             gnc_ui_get_main_window (NULL));
    component_id = gnc_register_gui_component (ASSISTANT_AB_INITIAL_CM_CLASS,
                                                NULL, aai_close_handler, info);
    gnc_gui_component_set_session (component_id, gnc_get_current_session ());
    aai_set_page (info, 0);

    g_object_unref (builder);
    return info;
}

void
gnc_ab_initial_assistant (void)
{
    if (!single_info)
        single_info = gnc_ab_initial_assistant_new ();
    if (single_info && !single_info->destroyed)
        gtk_window_present (single_info->window);
}
