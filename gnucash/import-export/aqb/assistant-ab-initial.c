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
#ifdef AQBANKING6
#include <aqbanking/types/account_spec.h>
#include <gwenhywfar/gui.h>
#endif
#include <glib.h>
#include <glib/gi18n.h>
#include <glib/gstdio.h>
#include <gdk/gdkkeysyms.h>
#ifdef HAVE_SYS_WAIT_H
#    include <sys/wait.h>
#endif
#include <fcntl.h>
#include <unistd.h>

#include "dialog-utils.h"
#include "assistant-ab-initial.h"
#include "gnc-ab-kvp.h"
#include "gnc-ab-utils.h"
#include "gnc-component-manager.h"
#include "gnc-glib-utils.h"
#include "gnc-ui.h"
#include "gnc-ui-util.h"
#include "gnc-session.h"
#include "import-account-matcher.h"
#include "import-utilities.h"
#ifndef AQBANKING6
# include <aqbanking/dlg_setup.h>
#endif
/* This static indicates the debugging module that this .o belongs to.  */
static QofLogModule log_module = GNC_MOD_ASSISTANT;

#define GNC_PREFS_GROUP "dialogs.ab-initial"
#define ASSISTANT_AB_INITIAL_CM_CLASS "assistant-ab-initial"

typedef struct _ABInitialInfo ABInitialInfo;
typedef struct _DeferredInfo DeferredInfo;
typedef struct _AccCbData AccCbData;
typedef struct _RevLookupData RevLookupData;

void aai_on_prepare (GtkAssistant  *assistant, GtkWidget *page,
                     gpointer user_data);

void aai_on_finish (GtkAssistant *gtkassistant, gpointer user_data);
void aai_on_cancel (GtkAssistant *assistant, gpointer user_data);
void aai_destroy_cb(GtkWidget *object, gpointer user_data);

gboolean aai_key_press_event_cb(GtkWidget *widget, GdkEventKey *event, gpointer user_data);

void aai_wizard_page_prepare (GtkAssistant *assistant, gpointer user_data);
void aai_wizard_button_clicked_cb(GtkButton *button, gpointer user_data);
void aai_match_delete_button_clicked_cb(GtkButton *button, gpointer user_data);

#ifdef AQBANKING6
static guint aai_ab_account_hash(gconstpointer v);
static gboolean aai_ab_account_equal(gconstpointer v1, gconstpointer v2);
#endif
void aai_match_page_prepare (GtkAssistant *assistant, gpointer user_data);

static gboolean banking_has_accounts(AB_BANKING *banking);
static void hash_from_kvp_acc_cb(Account *gnc_acc, gpointer user_data);
static ABInitialInfo *single_info = NULL;
static gchar *ab_account_longname(const GNC_AB_ACCOUNT_SPEC *ab_acc);
static GNC_AB_ACCOUNT_SPEC *update_account_list_acc_cb(GNC_AB_ACCOUNT_SPEC *ab_acc, gpointer user_data);
static void update_account_list(ABInitialInfo *info);
static gboolean find_gnc_acc_cb(gpointer key, gpointer value, gpointer user_data);
static gboolean clear_line_cb(GtkTreeModel *model, GtkTreePath *path, GtkTreeIter *iter, gpointer user_data);
static void account_list_clicked_cb (GtkTreeView *view, GtkTreePath *path,
                                     GtkTreeViewColumn  *col, gpointer user_data);
static void delete_account_match(ABInitialInfo *info, RevLookupData *data);
static void delete_selected_match_cb(gpointer data, gpointer user_data);
static void insert_acc_into_revhash_cb(gpointer ab_acc, gpointer gnc_acc, gpointer revhash);
static void remove_acc_from_revhash_cb(gpointer ab_acc, gpointer gnc_acc, gpointer revhash);
static void clear_kvp_acc_cb(gpointer key, gpointer value, gpointer user_data);
static void save_kvp_acc_cb(gpointer key, gpointer value, gpointer user_data);
static void aai_close_handler(gpointer user_data);

struct _ABInitialInfo
{
    GtkWidget *window;
    GtkWidget *assistant;

    /* account match page */
    gboolean match_page_prepared;
    GtkTreeView *account_view;
    GtkListStore *account_store;

    /* managed by child_exit_cb */
    DeferredInfo *deferred_info;

    /* AqBanking stuff */
    AB_BANKING *api;
    /* AB_ACCOUNT* -> Account* -- DO NOT DELETE THE KEYS! */
    GHashTable *gnc_hash;
    /* Reverse hash table for lookup of matched GnuCash accounts */
    GHashTable *gnc_revhash;
};

struct _DeferredInfo
{
    ABInitialInfo *initial_info;
    gchar *wizard_path;
    gboolean qt_probably_unavailable;
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

enum account_list_cols
{
    ACCOUNT_LIST_COL_INDEX = 0,
    ACCOUNT_LIST_COL_AB_NAME,
    ACCOUNT_LIST_COL_AB_ACCT,
    ACCOUNT_LIST_COL_GNC_NAME,
    ACCOUNT_LIST_COL_CHECKED,
    NUM_ACCOUNT_LIST_COLS
};

gboolean
aai_key_press_event_cb(GtkWidget *widget, GdkEventKey *event, gpointer user_data)
{
    if (event->keyval == GDK_KEY_Escape)
    {
        gtk_widget_destroy(widget);
        return TRUE;
    }
    else
    {
        return FALSE;
    }
}

void
aai_on_cancel (GtkAssistant *gtkassistant, gpointer user_data)
{
    ABInitialInfo *info = user_data;

    gtk_widget_destroy(info->window);
}

void
aai_destroy_cb(GtkWidget *object, gpointer user_data)
{
    ABInitialInfo *info = user_data;
    g_return_if_fail (single_info && info == single_info);

    gnc_unregister_gui_component_by_data(ASSISTANT_AB_INITIAL_CM_CLASS, info);

    if (info->deferred_info)
    {
        g_message("Online Banking assistant is being closed but the wizard is still "
                  "running.  Inoring.");

        /* Tell child_exit_cb() that there is no assistant anymore */
        info->deferred_info->initial_info = NULL;
    }

    if (info->gnc_hash)
    {
#ifndef AQBANKING6
        AB_Banking_OnlineFini(info->api);
#endif
        g_hash_table_destroy(info->gnc_hash);
        info->gnc_hash = NULL;
    }

    if (info->gnc_revhash)
    {
        g_hash_table_destroy(info->gnc_revhash);
        info->gnc_revhash = NULL;
    }

    if (info->api)
    {
        gnc_AB_BANKING_delete(info->api);
        info->api = NULL;
    }

    gtk_widget_destroy(info->window);
    info->window = NULL;

    g_free(info);
    single_info = NULL;
}

void
aai_wizard_page_prepare (GtkAssistant *assistant, gpointer user_data)
{
    ABInitialInfo *info = user_data;
    gint num = gtk_assistant_get_current_page (assistant);
    GtkWidget *page = gtk_assistant_get_nth_page (assistant, num);

    g_return_if_fail(info->api);

    /* Enable the Assistant Buttons if we accounts */
    if (banking_has_accounts(info->api))
        gtk_assistant_set_page_complete (assistant, page, TRUE);
    else
        gtk_assistant_set_page_complete (assistant, page, FALSE);
}

void
aai_wizard_button_clicked_cb(GtkButton *button, gpointer user_data)
{
    ABInitialInfo *info = user_data;
    gint num = gtk_assistant_get_current_page (GTK_ASSISTANT(info->window));
    GtkWidget *page = gtk_assistant_get_nth_page (GTK_ASSISTANT(info->window), num);

    AB_BANKING *banking = info->api;
    g_return_if_fail(banking);

    ENTER("user_data: %p", user_data);

    if (info->deferred_info)
    {
        LEAVE("Wizard is still running");
        return;
    }

    {
#ifdef AQBANKING6
        GWEN_DIALOG *dlg = AB_Banking_CreateSetupDialog(banking); 
#else
        GWEN_DIALOG *dlg = AB_SetupDialog_new(banking);

        if (AB_Banking_OnlineInit(banking) != 0)
        {
            PERR("Got error on AB_Banking_OnlineInit!");
        }
#endif
        if (!dlg)
        {
            PERR("Could not lookup Setup Dialog of aqbanking!");
        }
        else
        {
            int rv = GWEN_Gui_ExecDialog(dlg, 0);
            if (rv <= 0)
            {
                /* Dialog was aborted/rejected */
                PERR("Setup Dialog of aqbanking aborted/rejected !");
            }
            GWEN_Dialog_free(dlg);
        }
#ifndef AQBANKING6
        if (AB_Banking_OnlineFini(banking) != 0)
        {
            PERR("Got error on AB_Banking_OnlineFini!");
        }
#endif
    }

    /* Enable the Assistant Buttons if we accounts */
    if (banking_has_accounts(info->api))
        gtk_assistant_set_page_complete (GTK_ASSISTANT(info->window), page, TRUE);
    else
        gtk_assistant_set_page_complete (GTK_ASSISTANT(info->window), page, FALSE);

    LEAVE(" ");
}

static void delete_account_match(ABInitialInfo *info, RevLookupData *data)
{
    g_return_if_fail(info && info->gnc_hash &&
        info->account_view && data && data->ab_acc);

    g_hash_table_remove(info->gnc_hash, data->ab_acc);
    gtk_tree_model_foreach(
        GTK_TREE_MODEL(info->account_store),
        (GtkTreeModelForeachFunc) clear_line_cb,
        data);
}

static void
delete_selected_match_cb(gpointer data, gpointer user_data)
{
    GNC_AB_ACCOUNT_SPEC *ab_acc = NULL;
    GtkTreeIter iter;
    GtkTreeModel *model = NULL;
    RevLookupData revLookupData = {NULL, NULL};

    GtkTreePath *path = (GtkTreePath *) data;
    ABInitialInfo *info = (ABInitialInfo *) user_data;
    g_return_if_fail(path && info && info->account_view);

    model = gtk_tree_view_get_model(info->account_view);
    g_return_if_fail(model);

    if (gtk_tree_model_get_iter(model, &iter, path))
    {
        gtk_tree_model_get(model, &iter, ACCOUNT_LIST_COL_AB_ACCT, &revLookupData.ab_acc, -1);
        if (revLookupData.ab_acc)
            delete_account_match(info, &revLookupData);
    }
}

void
aai_match_delete_button_clicked_cb(GtkButton *button, gpointer user_data)
{
    GList *selected_matches = NULL;
    GtkTreeSelection *selection = NULL;
    ABInitialInfo *info = (ABInitialInfo *) user_data;

    g_return_if_fail(info && info->api && info->account_view && info->gnc_hash);

    PINFO("Selected account matches are deleted");

    selection = gtk_tree_view_get_selection (info->account_view);
    if (selection)
    {
        selected_matches = gtk_tree_selection_get_selected_rows (selection, NULL);
        if (selected_matches)
        {
            g_list_foreach (selected_matches, delete_selected_match_cb, info);
            g_list_free_full (
                selected_matches,
                (GDestroyNotify) gtk_tree_path_free);
        }
    }
}

#ifdef AQBANKING6
static guint
aai_ab_account_hash (gconstpointer v)
{
	if (v == NULL)
		return 0;
	else
		/* Use the account unique id as hash value */
		return AB_AccountSpec_GetUniqueId((const GNC_AB_ACCOUNT_SPEC *) v);
}

static gboolean
aai_ab_account_equal (gconstpointer v1, gconstpointer v2)
{
	if (v1 == NULL || v2 == NULL)
		return v1 == v2;
	else
	{
		/* Use the account unique id to check for equality */
		uint32_t uid1 = AB_AccountSpec_GetUniqueId((const GNC_AB_ACCOUNT_SPEC *) v1);
		uint32_t uid2 = AB_AccountSpec_GetUniqueId((const GNC_AB_ACCOUNT_SPEC *) v2);
		return uid1 == uid2;
	}
}
#endif

static void
insert_acc_into_revhash_cb(gpointer ab_acc, gpointer gnc_acc, gpointer revhash)
{
    g_return_if_fail(revhash && gnc_acc && ab_acc);
    g_hash_table_insert((GHashTable *) revhash, gnc_acc, ab_acc);
}

static void
remove_acc_from_revhash_cb(gpointer ab_acc, gpointer gnc_acc, gpointer revhash)
{
    g_return_if_fail(revhash && gnc_acc);
    g_hash_table_remove((GHashTable *) revhash, gnc_acc);
}

void
aai_match_page_prepare (GtkAssistant *assistant, gpointer user_data)
{
    ABInitialInfo *info = user_data;
    gint num = gtk_assistant_get_current_page (assistant);
    GtkWidget *page = gtk_assistant_get_nth_page (assistant, num);

    Account *root;
    AccCbData data;

    g_return_if_fail(info && info->api);

    /* Do not run this twice */
    if (!info->match_page_prepared)
    {
#ifndef AQBANKING6
        /* Load aqbanking accounts */
        AB_Banking_OnlineInit(info->api);
#endif
        /* Determine current mapping */
        root = gnc_book_get_root_account(gnc_get_current_book());
#ifdef AQBANKING6
        info->gnc_hash = g_hash_table_new(&aai_ab_account_hash, &aai_ab_account_equal);
#else
        info->gnc_hash = g_hash_table_new(&g_direct_hash, &g_direct_equal);
#endif
        data.api = info->api;
        data.hash = info->gnc_hash;
        gnc_account_foreach_descendant(root, (AccountCb) hash_from_kvp_acc_cb, &data);
        /* Memorize initial matches in reverse hash table */
        info->gnc_revhash = g_hash_table_new(NULL, NULL);
        g_hash_table_foreach(data.hash, (GHFunc) insert_acc_into_revhash_cb, (gpointer) info->gnc_revhash);

        info->match_page_prepared = TRUE;
    }
    /* Update the graphical representation */
    update_account_list(info);

    /* Enable the Assistant Buttons */
    gtk_assistant_set_page_complete (assistant, page, TRUE);
}

void
aai_on_finish (GtkAssistant *assistant, gpointer user_data)
{
    ABInitialInfo *info = user_data;

    g_return_if_fail(info && info->gnc_hash && info->gnc_revhash);

    /* Remove GnuCash accounts from reverse hash table which are still
     * matched to an AqBanking account. For the remaining GnuCash accounts
     * the KVPs must be cleared (i.e. deleted).
     * Please note that the value (i.e. the GnuCash account) stored in info->gnc_hash
     * is used as key for info->gnc_revhash */
    g_hash_table_foreach(info->gnc_hash, (GHFunc) remove_acc_from_revhash_cb, info->gnc_revhash);
    /* Commit the changes */
    g_hash_table_foreach(info->gnc_revhash, (GHFunc) clear_kvp_acc_cb, NULL);
    g_hash_table_foreach(info->gnc_hash, (GHFunc) save_kvp_acc_cb, NULL);

    gtk_widget_destroy(info->window);
}

static gboolean
banking_has_accounts(AB_BANKING *banking)
{
    GNC_AB_ACCOUNT_SPEC_LIST *accl = NULL;
    gboolean result = FALSE;

    g_return_val_if_fail(banking, FALSE);

#ifdef AQBANKING6
    if (AB_Banking_GetAccountSpecList (banking, &accl) >= 0 &&
        accl && AB_AccountSpec_List_GetCount (accl))
        result = TRUE;
    if (accl)
        AB_AccountSpec_List_free (accl);
#else
    AB_Banking_OnlineInit(banking);

    accl = AB_Banking_GetAccounts(banking);
    if (accl && (AB_Account_List2_GetSize(accl) > 0))
        result = TRUE;

    if (accl)
        AB_Account_List2_free(accl);

    AB_Banking_OnlineFini(banking);
#endif

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
    const char *ab_bankname, *bankcode, *subAccountId, *account_number;

    g_return_val_if_fail(ab_acc, NULL);

#ifdef AQBANKING6
    bankcode = AB_AccountSpec_GetBankCode(ab_acc);
    subAccountId = AB_AccountSpec_GetSubAccountNumber(ab_acc);
    account_number = AB_AccountSpec_GetAccountNumber (ab_acc);
#else
    ab_bankname = AB_Account_GetBankName(ab_acc);
    bankname = ab_bankname ? gnc_utf8_strip_invalid_strdup(ab_bankname) : NULL;
    bankcode = AB_Account_GetBankCode(ab_acc);
    subAccountId = AB_Account_GetSubAccountId(ab_acc);
    account_number = AB_Account_GetAccountNumber (ab_acc);
#endif
    /* Translators: Strings are 1. Bank code, 2. Bank name,
     * 3. Account Number,  4. Subaccount ID                  */
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
    GtkTreeIter iter;

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
    gtk_list_store_append(info->account_store, &iter);
    gtk_list_store_set(info->account_store, &iter,
                       ACCOUNT_LIST_COL_AB_NAME, ab_name,
                       ACCOUNT_LIST_COL_AB_ACCT, ab_acc,
                       ACCOUNT_LIST_COL_GNC_NAME, gnc_name,
                       ACCOUNT_LIST_COL_CHECKED, FALSE,
                       -1);
    g_free(gnc_name);
    g_free(ab_name);

    return NULL;
}

static void
update_account_list(ABInitialInfo *info)
{
    GNC_AB_ACCOUNT_SPEC_LIST *acclist = NULL;

    g_return_if_fail(info && info->api && info->gnc_hash);

    /* Detach model from view while updating */
    g_object_ref(info->account_store);
    gtk_tree_view_set_model(info->account_view, NULL);

    /* Refill the list */
    gtk_list_store_clear(info->account_store);
#ifdef AQBANKING6
    if (AB_Banking_GetAccountSpecList(info->api, &acclist) >= 0 && acclist)
        AB_AccountSpec_List_ForEach(acclist, update_account_list_acc_cb, info);
#else
    acclist = AB_Banking_GetAccounts(info->api);
    if (acclist)
        AB_Account_List2_ForEach(acclist, update_account_list_acc_cb, info);
#endif
    else
        g_warning("update_account_list: Oops, account list from AB_Banking "
                  "is NULL");

    /* Attach model to view again */
    gtk_tree_view_set_model(info->account_view,
                            GTK_TREE_MODEL(info->account_store));

    g_object_unref(info->account_store);
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

static gboolean
clear_line_cb(GtkTreeModel *model, GtkTreePath *path, GtkTreeIter *iter,
              gpointer user_data)
{
    RevLookupData *data = user_data;
    GtkListStore *store = GTK_LIST_STORE(model);
    gpointer ab_acc;

    g_return_val_if_fail(data && store, FALSE);

    gtk_tree_model_get(model, iter, ACCOUNT_LIST_COL_AB_ACCT, &ab_acc, -1);

#ifdef AQBANKING6
    if (aai_ab_account_equal(ab_acc, data->ab_acc))
#else
    if (ab_acc == data->ab_acc)
#endif
    {
        gtk_list_store_set(store, iter, ACCOUNT_LIST_COL_GNC_NAME, "",
                           ACCOUNT_LIST_COL_CHECKED, TRUE, -1);
        return TRUE;
    }
    return FALSE;
}

static void
account_list_clicked_cb (GtkTreeView *view, GtkTreePath *path,
                         GtkTreeViewColumn  *col, gpointer user_data)
{
    ABInitialInfo *info = user_data;
    GtkTreeModel *model;
    GtkTreeIter iter;
    GNC_AB_ACCOUNT_SPEC *ab_acc;
    gchar *longname, *gnc_name;
    Account *old_value, *gnc_acc;
    const gchar *currency;
    gnc_commodity *commodity = NULL;
    gboolean ok_pressed;

    g_return_if_fail(info);

    PINFO("Row has been double-clicked.");

    model = gtk_tree_view_get_model(view);

    if (!gtk_tree_model_get_iter(model, &iter, path))
        return; /* path describes a non-existing row - should not happen */

    gtk_tree_model_get(model, &iter, ACCOUNT_LIST_COL_AB_ACCT, &ab_acc, -1);

    if (ab_acc)
    {
        old_value = g_hash_table_lookup(info->gnc_hash, ab_acc);

        longname = ab_account_longname(ab_acc);
#ifdef AQBANKING6
        currency = AB_AccountSpec_GetCurrency(ab_acc);
#else
        currency = AB_Account_GetCurrency(ab_acc);
#endif
        if (currency && *currency)
        {
            commodity = gnc_commodity_table_lookup(
                            gnc_commodity_table_get_table(gnc_get_current_book()),
                            GNC_COMMODITY_NS_CURRENCY,
                            currency);
        }

        gnc_acc = gnc_import_select_account(info->window, NULL, TRUE,
                                            longname, commodity, ACCT_TYPE_BANK,
                                            old_value, &ok_pressed);
        g_free(longname);

        if (ok_pressed && old_value != gnc_acc)
        {
            if (gnc_acc)
            {
                RevLookupData data;

                /* Lookup and clear other mappings to gnc_acc */
                data.gnc_acc = gnc_acc;
                data.ab_acc = NULL;
                g_hash_table_find(info->gnc_hash, (GHRFunc) find_gnc_acc_cb,
                                  &data);
                if (data.ab_acc)
                    delete_account_match(info, &data);

                /* Map ab_acc to gnc_acc */
                g_hash_table_insert(info->gnc_hash, ab_acc, gnc_acc);
                gnc_name = gnc_account_get_full_name(gnc_acc);
                gtk_list_store_set(info->account_store, &iter,
                                   ACCOUNT_LIST_COL_GNC_NAME, gnc_name,
                                   ACCOUNT_LIST_COL_CHECKED, TRUE,
                                   -1);
                g_free(gnc_name);

            }
            else
            {
                g_hash_table_remove(info->gnc_hash, ab_acc);
                gtk_list_store_set(info->account_store, &iter,
                                   ACCOUNT_LIST_COL_GNC_NAME, "",
                                   ACCOUNT_LIST_COL_CHECKED, TRUE,
                                   -1);
            }
        }
    }
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
#ifdef AQBANKING6
    gchar *ab_online_id;
    const gchar *gnc_online_id;
#endif

    g_return_if_fail(ab_acc && gnc_acc);

#ifdef AQBANKING6
    ab_account_uid = AB_AccountSpec_GetUniqueId(ab_acc);
#else
    ab_account_uid = AB_Account_GetUniqueId(ab_acc);
#endif
    if (gnc_ab_get_account_uid(gnc_acc) != ab_account_uid)
        gnc_ab_set_account_uid(gnc_acc, ab_account_uid);

#ifdef AQBANKING6
    ab_accountid = AB_AccountSpec_GetAccountNumber(ab_acc);
#else
    ab_accountid = AB_Account_GetAccountNumber(ab_acc);
#endif
    gnc_accountid = gnc_ab_get_account_accountid(gnc_acc);
    if (ab_accountid
            && (!gnc_accountid
                || (strcmp(ab_accountid, gnc_accountid) != 0)))
        gnc_ab_set_account_accountid(gnc_acc, ab_accountid);

#ifdef AQBANKING6
    ab_bankcode = AB_AccountSpec_GetBankCode(ab_acc);
#else
    ab_bankcode = AB_Account_GetBankCode(ab_acc);
#endif
    gnc_bankcode = gnc_ab_get_account_bankcode(gnc_acc);
    if (ab_bankcode
            && (!gnc_bankcode
                || (strcmp(gnc_bankcode, ab_bankcode) != 0)))
        gnc_ab_set_account_bankcode(gnc_acc, ab_bankcode);

#ifdef AQBANKING6
    ab_online_id = gnc_ab_create_online_id(ab_bankcode, ab_accountid);
    gnc_online_id = gnc_import_get_acc_online_id(gnc_acc);
    if (ab_online_id && (!gnc_online_id || (strcmp(ab_online_id, gnc_online_id) != 0)))
        gnc_import_set_acc_online_id(gnc_acc, ab_online_id);
    g_free(ab_online_id);
#endif
}

static void
aai_close_handler(gpointer user_data)
{
    ABInitialInfo *info = user_data;

    gnc_save_window_size(GNC_PREFS_GROUP, GTK_WINDOW(info->window));
    gtk_widget_destroy(info->window);
}

void aai_on_prepare (GtkAssistant  *assistant, GtkWidget *page,
                     gpointer user_data)
{
    switch (gtk_assistant_get_current_page(assistant))
    {
    case 1:
        /* Current page is wizard button page */
        aai_wizard_page_prepare (assistant , user_data );
        break;
    case 2:
        /* Current page is match page */
        aai_match_page_prepare (assistant , user_data );
        break;
    }
}

static ABInitialInfo *
gnc_ab_initial_assistant_new(void)
{
    GtkBuilder *builder;
    GtkTreeViewColumn *column;
    GtkTreeSelection *selection;
    gint component_id;

    ABInitialInfo *info = g_new0(ABInitialInfo, 1);
    builder = gtk_builder_new();
    gnc_builder_add_from_file (builder, "assistant-ab-initial.glade", "aqbanking_init_assistant");

    info->window = GTK_WIDGET(gtk_builder_get_object (builder, "aqbanking_init_assistant"));

    info->api = gnc_AB_BANKING_new();
    info->deferred_info = NULL;
    info->gnc_hash = NULL;

    info->match_page_prepared = FALSE;
    info->account_view =
        GTK_TREE_VIEW(gtk_builder_get_object (builder, "account_page_view"));

    info->account_store = gtk_list_store_new(NUM_ACCOUNT_LIST_COLS,
                          G_TYPE_INT, G_TYPE_STRING,
                          G_TYPE_POINTER, G_TYPE_STRING,
                          G_TYPE_BOOLEAN);
    gtk_tree_view_set_model(info->account_view,
                            GTK_TREE_MODEL(info->account_store));
    g_object_unref(info->account_store);

    column = gtk_tree_view_column_new_with_attributes(
                 _("Online Banking Account Name"), gtk_cell_renderer_text_new(),
                 "text", ACCOUNT_LIST_COL_AB_NAME, (gchar*) NULL);
    gtk_tree_view_append_column(info->account_view, column);

    column = gtk_tree_view_column_new_with_attributes(
                 _("GnuCash Account Name"), gtk_cell_renderer_text_new(),
                 "text", ACCOUNT_LIST_COL_GNC_NAME, (gchar*) NULL);
    gtk_tree_view_column_set_expand(column, TRUE);
    gtk_tree_view_append_column(info->account_view, column);

    column = gtk_tree_view_column_new_with_attributes(
                 _("New?"), gtk_cell_renderer_toggle_new(),
                 "active", ACCOUNT_LIST_COL_CHECKED, (gchar*) NULL);
    gtk_tree_view_append_column(info->account_view, column);

    selection = gtk_tree_view_get_selection(info->account_view);
    gtk_tree_selection_set_mode (selection, GTK_SELECTION_MULTIPLE);

    gnc_restore_window_size (GNC_PREFS_GROUP,
                             GTK_WINDOW(info->window), gnc_ui_get_main_window(NULL));

    g_signal_connect(info->account_view, "row-activated",
                     G_CALLBACK(account_list_clicked_cb), info);

    g_signal_connect (G_OBJECT(info->window), "destroy",
                      G_CALLBACK (aai_destroy_cb), info);

    gtk_builder_connect_signals(builder, info);
    g_object_unref(G_OBJECT(builder));

    component_id = gnc_register_gui_component(ASSISTANT_AB_INITIAL_CM_CLASS,
                   NULL, aai_close_handler, info);

    gnc_gui_component_set_session(component_id, gnc_get_current_session());
    return info;
}

void
gnc_ab_initial_assistant(void)
{
    if (!single_info)
        single_info = gnc_ab_initial_assistant_new();
    gtk_widget_show(single_info->window);
}

