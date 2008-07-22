/*
 * druid-ab-initial.c --
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
 * @file druid-ab-initial.c
 * @brief AqBanking setup functionality
 * @author Copyright (C) 2002 Christian Stimming <stimming@tuhh.de>
 * @author Copyright (C) 2006 David Hampton <hampton@employees.org>
 * @author Copyright (C) 2008 Andreas Koehler <andi5.py@gmx.net>
 */

#include "config.h"

#include <aqbanking/banking.h>
#include <glib.h>
#include <glib/gi18n.h>
#include <glib/gstdio.h>
#include <gdk/gdkkeysyms.h>
#include <fcntl.h>
#include <unistd.h>

#include "dialog-utils.h"
#include "druid-ab-initial.h"
#include "druid-utils.h"
#include "gnc-ab-kvp.h"
#include "gnc-ab-utils.h"
#include "gnc-component-manager.h"
#include "gnc-glib-utils.h"
#include "gnc-ui.h"
#include "gnc-ui-util.h"
#include "gnc-session.h"
#include "import-account-matcher.h"

/* This static indicates the debugging module that this .o belongs to.  */
static QofLogModule log_module = G_LOG_DOMAIN;

#define DRUID_AB_INITIAL_CM_CLASS "druid-ab-initial"

typedef struct _ABInitialInfo ABInitialInfo;
typedef struct _DeferredInfo DeferredInfo;
typedef struct _AccCbData AccCbData;
typedef struct _RevLookupData RevLookupData;

gboolean dai_key_press_event_cb(GtkWidget *widget, GdkEventKey *event, gpointer user_data);
void dai_cancel_cb(GnomeDruid *druid, gpointer user_data);
void dai_destroy_cb(GtkObject *object, gpointer user_data);
void dai_wizard_page_prepare_cb(GnomeDruidPage *druid_page, GtkWidget *widget, gpointer user_data);
void dai_wizard_button_clicked_cb(GtkButton *button, gpointer user_data);
void dai_match_page_prepare_cb(GnomeDruidPage *druid_page, GtkWidget *widget, gpointer user_data);
void dai_finish_cb(GnomeDruidPage *druid_page, GtkWidget *widget, gpointer user_data);

static gboolean banking_has_accounts(AB_BANKING *banking);
static void hash_from_kvp_acc_cb(Account *gnc_acc, gpointer user_data);
static void druid_enable_next_button(ABInitialInfo *info);
static void druid_disable_next_button(ABInitialInfo *info);
static void child_exit_cb(GPid pid, gint status, gpointer data);
static gchar *ab_account_longname(const AB_ACCOUNT *ab_acc);
static AB_ACCOUNT *update_account_list_acc_cb(AB_ACCOUNT *ab_acc, gpointer user_data);
static void update_account_list(ABInitialInfo *info);
static gboolean find_gnc_acc_cb(gpointer key, gpointer value, gpointer user_data);
static gboolean clear_line_cb(GtkTreeModel *model, GtkTreePath *path, GtkTreeIter *iter, gpointer user_data);
static void account_list_changed_cb(GtkTreeSelection *selection, gpointer user_data);
static void clear_kvp_acc_cb(Account *gnc_acc, gpointer user_data);
static void save_kvp_acc_cb(gpointer key, gpointer value, gpointer user_data);
static void cm_close_handler(gpointer user_data);

struct _ABInitialInfo {
    GtkWidget *window;
    GtkWidget *druid;

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
};

struct _DeferredInfo {
    ABInitialInfo *initial_info;
    gchar *wizard_path;
    gboolean qt_probably_unavailable;
};

struct _AccCbData {
    AB_BANKING *api;
    GHashTable *hash;
};

struct _RevLookupData {
    Account *gnc_acc;
    AB_ACCOUNT *ab_acc;
};

enum account_list_cols {
    ACCOUNT_LIST_COL_INDEX = 0,
    ACCOUNT_LIST_COL_AB_NAME,
    ACCOUNT_LIST_COL_AB_ACCT,
    ACCOUNT_LIST_COL_GNC_NAME,
    ACCOUNT_LIST_COL_CHECKED,
    NUM_ACCOUNT_LIST_COLS
};

gboolean
dai_key_press_event_cb(GtkWidget *widget, GdkEventKey *event, gpointer user_data)
{
    if (event->keyval == GDK_Escape) {
        gtk_widget_destroy(widget);
        return TRUE;
    } else {
        return FALSE;
    }
}

void
dai_cancel_cb(GnomeDruid *druid, gpointer user_data)
{
    ABInitialInfo *info = user_data;

    gtk_widget_destroy(info->window);
}

void
dai_destroy_cb(GtkObject *object, gpointer user_data)
{
    ABInitialInfo *info = user_data;

    gnc_unregister_gui_component_by_data(DRUID_AB_INITIAL_CM_CLASS, info);

    if (info->deferred_info) {
        g_message("Online Banking druid is being closed but the wizard is still "
                  "running.  Inoring.");

        /* Tell child_exit_cb() that there is no druid anymore */
        info->deferred_info->initial_info = NULL;
    }

    if (info->gnc_hash) {
        AB_Banking_OnlineFini(info->api);
        g_hash_table_destroy(info->gnc_hash);
        info->gnc_hash = NULL;
    }

    if (info->api) {
        gnc_AB_BANKING_delete(info->api);
        info->api = NULL;
    }

    gtk_widget_destroy(info->window);
    info->window = NULL;

    g_free(info);
}

void
dai_wizard_page_prepare_cb(GnomeDruidPage *druid_page, GtkWidget *widget,
                           gpointer user_data)
{
    ABInitialInfo *info = user_data;

    g_return_if_fail(info->api);

    if (banking_has_accounts(info->api))
        druid_enable_next_button(info);
    else
        druid_disable_next_button(info);
}

void
dai_wizard_button_clicked_cb(GtkButton *button, gpointer user_data)
{
    ABInitialInfo *info = user_data;
    AB_BANKING *banking = info->api;
    GWEN_BUFFER *buf;
    gboolean wizard_exists;
    const gchar *wizard_path;
    gboolean qt_probably_unavailable = FALSE;

    g_return_if_fail(banking);

    ENTER("user_data: %p", user_data);

    if (info->deferred_info) {
        LEAVE("Wizard is still running");
        return;
    }

    /* This is the point where we look for and start an external
     * application shipped with aqbanking that contains the setup druid
     * for AqBanking related stuff.  It requires qt (but not kde).  This
     * application contains the very verbose step-by-step setup wizard
     * for the AqBanking account, and the application is shared with
     * other AqBanking-based financial managers that offer the AqBanking
     * features (e.g. KMyMoney).  See gnucash-devel discussion here
     * https://lists.gnucash.org/pipermail/gnucash-devel/2004-December/012351.html
     */
    buf = GWEN_Buffer_new(NULL, 300, 0, 0);
    AB_Banking_FindWizard(banking, "", NULL, buf);
    wizard_exists = *GWEN_Buffer_GetStart(buf) != 0;
    wizard_path = GWEN_Buffer_GetStart(buf);

    if (wizard_exists) {
        /* Really check whether the file exists */
        gint fd = g_open(wizard_path, O_RDONLY, 0);
        if (fd == -1)
            wizard_exists = FALSE;
        else
            close(fd);
    }

#ifdef G_OS_WIN32
    {
        const char *check_file = "qtdemo.exe";
        gchar *found_program = g_find_program_in_path(check_file);
        if (found_program) {
            g_debug("Yes, we found the Qt demo program in %s\n", found_program);
            g_free(found_program);
        } else {
            g_warning("Ouch, no Qt demo program was found. Qt not installed?\n");
            qt_probably_unavailable = TRUE;
        }
    }
#endif

    druid_disable_next_button(info);

    if (wizard_exists) {
        /* Call the qt wizard. See the note above about why this
         * approach is chosen. */

        GPid pid;
        GError *error = NULL;
        gchar *argv[2];
        gboolean spawned;

        argv[0] = g_strdup (wizard_path);
        argv[1] = NULL;
        spawned = g_spawn_async (NULL, argv, NULL, G_SPAWN_DO_NOT_REAP_CHILD,
                                 NULL, NULL, &pid, &error);
        g_free (argv[0]);

        if (error)
            g_critical(
                "Error on starting AqBanking setup wizard: Code %d: %s",
                error->code, error->message ? error->message : "(null)");

        if (!spawned) {
            g_critical("Could not start AqBanking setup wizard: %s",
                       error->message ? error->message : "(null)");
            g_error_free (error);
        } else {
            /* Keep a reference to info that can survive info */
            info->deferred_info = g_new0(DeferredInfo, 1);
            info->deferred_info->initial_info = info;
            info->deferred_info->wizard_path = g_strdup(wizard_path);
            info->deferred_info->qt_probably_unavailable =
                qt_probably_unavailable;

            g_child_watch_add (pid, child_exit_cb, info->deferred_info);
        }
    } else {
        g_warning("on_aqhbci_button: Oops, no aqhbci setup wizard found.");
        gnc_error_dialog
            (info->window,
             _("The external program \"AqBanking Setup Wizard\" has not "
               "been found. \n\n"
               "The %s package should include the "
               "program \"qt3-wizard\".  Please check your installation to "
               "ensure this program is present.  On some distributions this "
               "may require installing additional packages."),
             QT3_WIZARD_PACKAGE);
        druid_disable_next_button(info);
    }

    GWEN_Buffer_free(buf);

    LEAVE(" ");
}

void
dai_match_page_prepare_cb(GnomeDruidPage *druid_page, GtkWidget *widget,
                          gpointer user_data)
{
    ABInitialInfo *info = user_data;
    Account *root;
    AccCbData data;

    g_return_if_fail(info && info->api);

    /* No way back */
    gnome_druid_set_buttons_sensitive(GNOME_DRUID(info->druid),
                                      FALSE, TRUE, TRUE, TRUE);

    /* Do not run this twice */
    if (info->match_page_prepared)
        return;
    else
        info->match_page_prepared = TRUE;

    /* Load aqbanking accounts */
    AB_Banking_OnlineInit(info->api);

    /* Determine current mapping */
    root = gnc_book_get_root_account(gnc_get_current_book());
    info->gnc_hash = g_hash_table_new(&g_direct_hash, &g_direct_equal);
    data.api = info->api;
    data.hash = info->gnc_hash;
    gnc_account_foreach_descendant(
        root, (AccountCb) hash_from_kvp_acc_cb, &data);

    /* Update the graphical representation */
    update_account_list(info);
}

void
dai_finish_cb(GnomeDruidPage *druid_page, GtkWidget *widget,
              gpointer user_data)
{
    ABInitialInfo *info = user_data;
    Account *root;

    g_return_if_fail(info && info->gnc_hash);

    /* Commit the changes */
    root = gnc_book_get_root_account(gnc_get_current_book());
    gnc_account_foreach_descendant(root, (AccountCb) clear_kvp_acc_cb, NULL);
    g_hash_table_foreach(info->gnc_hash, (GHFunc) save_kvp_acc_cb, NULL);

    gtk_widget_destroy(info->window);
}

static gboolean
banking_has_accounts(AB_BANKING *banking)
{
    AB_ACCOUNT_LIST2 *accl;
    gboolean result;

    g_return_val_if_fail(banking, FALSE);

    AB_Banking_OnlineInit(banking);

    accl = AB_Banking_GetAccounts(banking);
    if (accl && (AB_Account_List2_GetSize(accl) > 0))
        result = TRUE;
    else
        result = FALSE;

    if (accl)
        AB_Account_List2_free(accl);

    AB_Banking_OnlineFini(banking);

    return result;
}

static void
hash_from_kvp_acc_cb(Account *gnc_acc, gpointer user_data)
{
    AccCbData *data = user_data;
    AB_ACCOUNT *ab_acc;

    ab_acc = gnc_ab_get_ab_account(data->api, gnc_acc);
    if (ab_acc)
        g_hash_table_insert(data->hash, ab_acc, gnc_acc);
}

static void
druid_enable_next_button(ABInitialInfo *info)
{
    g_return_if_fail(info);
    gnome_druid_set_buttons_sensitive(GNOME_DRUID(info->druid),
                                      TRUE, TRUE, TRUE, TRUE);
}

static void
druid_disable_next_button(ABInitialInfo *info)
{
    g_return_if_fail(info);
    gnome_druid_set_buttons_sensitive(GNOME_DRUID(info->druid),
                                      TRUE, FALSE, TRUE, TRUE);
}

static void
child_exit_cb(GPid pid, gint status, gpointer data)
{
    DeferredInfo *deferred_info = data;
    ABInitialInfo *info = deferred_info->initial_info;
    gint exit_status;

#ifdef G_OS_WIN32
    exit_status = status;
#else
    exit_status = WEXITSTATUS(status);
#endif

    g_spawn_close_pid(pid);

    if (!info) {
        g_message("Online Banking wizard exited, but the druid has been "
                  "destroyed already");
        goto cleanup_child_exit_cb;
    }

    if (exit_status == 0) {
        druid_enable_next_button(info);
    } else {
        if (deferred_info->qt_probably_unavailable) {
            g_warning("on_aqhbci_button: Oops, aqhbci wizard return nonzero "
                      "value: %d. The called program was \"%s\".\n",
                      exit_status, deferred_info->wizard_path);
            gnc_error_dialog
                (info->window,
                 _("The external program \"AqBanking Setup Wizard\" failed "
                   "to run successfully because the "
                   "additional software \"Qt\" was not found.  "
                   "Please install the \"Qt/Windows Open Source Edition\" "
                   "from Trolltech by downloading it from www.trolltech.com"
                   "\n\n"
                   "If you have installed Qt already, you will have to adapt "
                   "the PATH variable of your system appropriately.  "
                   "Contact the GnuCash developers if you need further "
                   "assistance on how to install Qt correctly."
                   "\n\n"
                   "Online Banking cannot be setup without Qt.  Press \"Close\" "
                   "now, then \"Cancel\" to cancel the Online Banking setup."));
        } else {
            g_warning("on_aqhbci_button: Oops, aqhbci wizard return nonzero "
                      "value: %d. The called program was \"%s\".\n",
                      exit_status, deferred_info->wizard_path);
            gnc_error_dialog
                (info->window,
                 _("The external program \"AqBanking Setup Wizard\" failed "
                   "to run successfully.  Online Banking can only be setup "
                   "if this wizard has run successfully.  "
                   "Please try running the \"AqBanking Setup Wizard\" again."));
        }
        druid_disable_next_button(info);
    }

cleanup_child_exit_cb:
    g_free(deferred_info->wizard_path);
    g_free(deferred_info);
    if (info)
        info->deferred_info = NULL;
}

static gchar *
ab_account_longname(const AB_ACCOUNT *ab_acc)
{
    gchar *bankname;
    gchar *result;
    const char *bankcode;

    g_return_val_if_fail(ab_acc, NULL);

    bankname = gnc_utf8_strip_invalid_strdup(AB_Account_GetBankName(ab_acc));
    bankcode = AB_Account_GetBankCode(ab_acc);

    /* Translators: Strings are 1. Account code, 2. Bank name, 3. Bank code. */
    if (bankname && *bankname)
        result = g_strdup_printf(_("%s at %s (code %s)"),
                                 AB_Account_GetAccountNumber(ab_acc),
                                 bankname,
                                 bankcode);
    else
        result = g_strdup_printf(_("%s at bank code %s"),
                                 AB_Account_GetAccountNumber(ab_acc),
                                 bankcode);
    g_free(bankname);

    return result;

}

static AB_ACCOUNT *
update_account_list_acc_cb(AB_ACCOUNT *ab_acc, gpointer user_data)
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
        gnc_name = xaccAccountGetFullName(gnc_acc);
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
    AB_ACCOUNT_LIST2 *acclist;

    g_return_if_fail(info && info->api && info->gnc_hash);

    /* Detach model from view while updating */
    g_object_ref(info->account_store);
    gtk_tree_view_set_model(info->account_view, NULL);

    /* Refill the list */
    gtk_list_store_clear(info->account_store);
    acclist = AB_Banking_GetAccounts(info->api);
    if (acclist)
        AB_Account_List2_ForEach(acclist, update_account_list_acc_cb, info);
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

    if (value == data->gnc_acc) {
        data->ab_acc = (AB_ACCOUNT*) key;
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

    if (ab_acc == data->ab_acc) {
        gtk_list_store_set(store, iter, ACCOUNT_LIST_COL_GNC_NAME, "",
                           ACCOUNT_LIST_COL_CHECKED, TRUE, -1);
        return TRUE;
    }
    return FALSE;
}

static void
account_list_changed_cb(GtkTreeSelection *selection, gpointer user_data)
{
    ABInitialInfo *info = user_data;
    GtkTreeModel *model;
    GtkTreeIter iter;
    AB_ACCOUNT *ab_acc;
    gchar *longname, *gnc_name;
    Account *old_value, *gnc_acc;
    const gchar *currency;
    gnc_commodity *commodity = NULL;
    gboolean ok_pressed;

    g_return_if_fail(info);

    if (!gtk_tree_selection_get_selected(selection, &model, &iter))
        return;
    gtk_tree_model_get(model, &iter, ACCOUNT_LIST_COL_AB_ACCT, &ab_acc, -1);

    /* Avoid recursion when unselecting the item again */
    g_signal_handlers_block_by_func(selection, account_list_changed_cb, info);
    gtk_tree_selection_unselect_iter(selection, &iter);
    g_signal_handlers_unblock_by_func(selection, account_list_changed_cb, info);

    if (ab_acc) {
        old_value = g_hash_table_lookup(info->gnc_hash, ab_acc);

        longname = ab_account_longname(ab_acc);
        currency = AB_Account_GetCurrency(ab_acc);
        if (currency && *currency) {
            commodity = gnc_commodity_table_lookup(
                gnc_commodity_table_get_table(gnc_get_current_book()),
                GNC_COMMODITY_NS_CURRENCY,
                currency);
        }

        gnc_acc = gnc_import_select_account(info->window, NULL, TRUE,
                                            longname, commodity, ACCT_TYPE_BANK,
                                            old_value, &ok_pressed);
        g_free(longname);

        if (ok_pressed && old_value != gnc_acc) {
            if (gnc_acc) {
                RevLookupData data;

                /* Lookup and clear other mappings to gnc_acc */
                data.gnc_acc = gnc_acc;
                data.ab_acc = NULL;
                g_hash_table_find(info->gnc_hash, (GHRFunc) find_gnc_acc_cb,
                                  &data);
                if (data.ab_acc) {
                    g_hash_table_remove(info->gnc_hash, data.ab_acc);
                    gtk_tree_model_foreach(
                        GTK_TREE_MODEL(info->account_store),
                        (GtkTreeModelForeachFunc) clear_line_cb,
                        &data);
                }

                /* Map ab_acc to gnc_acc */
                g_hash_table_insert(info->gnc_hash, ab_acc, gnc_acc);
                gnc_name = xaccAccountGetFullName(gnc_acc);
                gtk_list_store_set(info->account_store, &iter,
                                   ACCOUNT_LIST_COL_GNC_NAME, gnc_name,
                                   ACCOUNT_LIST_COL_CHECKED, TRUE,
                                   -1);
                g_free(gnc_name);

            } else {
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
clear_kvp_acc_cb(Account *gnc_acc, gpointer user_data)
{
    if (gnc_ab_get_account_uid(gnc_acc))
        gnc_ab_set_account_uid(gnc_acc, 0);
    if (gnc_ab_get_account_accountid(gnc_acc))
        gnc_ab_set_account_accountid(gnc_acc, "");
    if (gnc_ab_get_account_bankcode(gnc_acc))
        gnc_ab_set_account_bankcode(gnc_acc, "");
}

static void
save_kvp_acc_cb(gpointer key, gpointer value, gpointer user_data)
{
    AB_ACCOUNT *ab_acc = key;
    Account *gnc_acc = value;
    guint32 ab_account_uid;
    const gchar *ab_accountid, *gnc_accountid;
    const gchar *ab_bankcode, *gnc_bankcode;

    g_return_if_fail(ab_acc && gnc_acc);

    ab_account_uid = AB_Account_GetUniqueId(ab_acc);
    if (gnc_ab_get_account_uid(gnc_acc) != ab_account_uid)
        gnc_ab_set_account_uid(gnc_acc, ab_account_uid);

    ab_accountid = AB_Account_GetAccountNumber(ab_acc);
    gnc_accountid = gnc_ab_get_account_accountid(gnc_acc);
    if (ab_accountid
        && (!gnc_accountid
            || (strcmp(ab_accountid, gnc_accountid) != 0)))
        gnc_ab_set_account_accountid(gnc_acc, ab_accountid);

    ab_bankcode = AB_Account_GetBankCode(ab_acc);
    gnc_bankcode = gnc_ab_get_account_bankcode(gnc_acc);
    if (ab_bankcode
        && (!gnc_bankcode
            || (strcmp(gnc_bankcode, ab_bankcode) != 0)))
        gnc_ab_set_account_bankcode(gnc_acc, ab_bankcode);
}

static void
cm_close_handler(gpointer user_data)
{
    ABInitialInfo *info = user_data;

    gtk_widget_destroy(info->window);
}

void
gnc_ab_initial_druid(void)
{
    ABInitialInfo *info;
    GladeXML *xml;
    GtkTreeViewColumn *column;
    GtkTreeSelection *selection;
    gint component_id;

    info = g_new0(ABInitialInfo, 1);

    xml = gnc_glade_xml_new("aqbanking.glade", "AqBanking Init Druid");

    info->window = glade_xml_get_widget(xml, "AqBanking Init Druid");
    g_object_set_data_full(G_OBJECT(info->window), "xml", xml, g_object_unref);
    glade_xml_signal_autoconnect_full(xml, gnc_glade_autoconnect_full_func,
                                      info);

    info->druid = glade_xml_get_widget(xml, "ab_init_druid");
    gnc_druid_set_colors(GNOME_DRUID(info->druid));

    info->api = gnc_AB_BANKING_new();
    info->deferred_info = NULL;
    info->gnc_hash = NULL;

    info->match_page_prepared = FALSE;
    info->account_view =
        GTK_TREE_VIEW(glade_xml_get_widget(xml, "account_page_view"));
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
    g_signal_connect(selection, "changed",
                     G_CALLBACK(account_list_changed_cb), info);

    component_id = gnc_register_gui_component(DRUID_AB_INITIAL_CM_CLASS,
                                              NULL, cm_close_handler, info);
    gnc_gui_component_set_session(component_id, gnc_get_current_session());

    gtk_widget_show(info->window);
}
