/********************************************************************\
 * assistant-hierarchy.c -- account hierarchy creation functionality*
 * Copyright (C) 2001 Gnumatic, Inc.                                *
 * Copyright (C) 2006 David Hampton <hampton@employees.org>         *
 * Copyright (C) 2010 Geert Janssens                                *
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

#include <gtk/gtk.h>
#include <glib/gi18n.h>
#include <glib/gstdio.h>
#include <dialog-options.hpp>
#include <gnc-optiondb.h>
#include <gnc-optiondb-impl.hpp>
#include <libguile.h>

#include <config.h>

#include <platform.h>
#if PLATFORM(WINDOWS)
#include <windows.h>
#endif

#include <sys/stat.h>
#include <sys/types.h>
#include <unistd.h>
#ifdef MAC_INTEGRATION
#include <Foundation/Foundation.h>
#endif
#include "gnc-account-merge.h"
#include "dialog-new-user.h"
#include "dialog-utils.h"
#include "dialog-file-access.h"
#include "assistant-hierarchy.h"
#include "gnc-amount-edit.h"
#include "gnc-currency-edit.h"
#include "gnc-exp-parser.h"
#include "gnc-general-select.h"
#include "gnc-gnome-utils.h"
#include "gnc-prefs.h"
#include "gnc-hooks.h"
#include "gnc-component-manager.h"
#include "gnc-path.h"
#include "gnc-gui-query.h"
#include "gnc-ui.h"
#include "gnc-ui-util.h"
#include "io-example-account.h"
#include "top-level.h"
#include "gnc-main-window.h"
#include "gnc-plugin-page-account-tree.h"

#include "gnc-engine.h"

static QofLogModule log_module = GNC_MOD_IMPORT;

#define GNC_PREFS_GROUP           "dialogs.new-hierarchy"
#define GNC_PREF_SHOW_ON_NEW_FILE "show-on-new-file"
#define DIALOG_BOOK_OPTIONS_CM_CLASS "dialog-book-options"

typedef struct _HierarchyCategoryRow HierarchyCategoryRow;
typedef struct _HierarchyCategoryRowClass HierarchyCategoryRowClass;
typedef struct _HierarchyAccountRow HierarchyAccountRow;
typedef struct _HierarchyAccountRowClass HierarchyAccountRowClass;
typedef struct _HierarchyAccountRef HierarchyAccountRef;
typedef struct _HierarchyLocale HierarchyLocale;
typedef struct hierarchy_data hierarchy_data;

struct _HierarchyCategoryRow
{
    GObject parent_instance;
    GncExampleAccount *account_set;
    gboolean checked;
};

struct _HierarchyCategoryRowClass
{
    GObjectClass parent_class;
};

struct _HierarchyAccountRow
{
    GObject parent_instance;
    QofBook *book;
    GncGUID guid;
};

struct _HierarchyAccountRowClass
{
    GObjectClass parent_class;
};

struct _HierarchyAccountRef
{
    hierarchy_data *data;
    QofBook *book;
    GncGUID guid;
    guint column;
};

struct _HierarchyLocale
{
    gchar *language;
    gchar *region;
    gchar *name;
};

GType hierarchy_category_row_get_type ();
GType hierarchy_account_row_get_type ();

G_DEFINE_FINAL_TYPE (HierarchyCategoryRow, hierarchy_category_row, G_TYPE_OBJECT)
G_DEFINE_FINAL_TYPE (HierarchyAccountRow, hierarchy_account_row, G_TYPE_OBJECT)

typedef enum
{
    HIERARCHY_ACCOUNT_COLUMN_NAME,
    HIERARCHY_ACCOUNT_COLUMN_CODE,
    HIERARCHY_ACCOUNT_COLUMN_DESCRIPTION,
    HIERARCHY_ACCOUNT_COLUMN_NOTES,
    HIERARCHY_ACCOUNT_COLUMN_TYPE,
    HIERARCHY_ACCOUNT_COLUMN_PLACEHOLDER,
    HIERARCHY_ACCOUNT_COLUMN_BALANCE,
    HIERARCHY_ACCOUNT_COLUMN_USE_EXISTING
} HierarchyAccountColumn;

struct hierarchy_data
{
    GtkWindow *dialog;
    GtkStack *stack;
    GtkWidget *pages[6];
    guint page_count;
    guint current_page;
    guint currency_page;
    guint categories_page;
    guint final_page;
    GtkWidget *back_button;
    GtkWidget *next_button;
    GtkWidget *apply_button;
    GtkWidget *book_options_page;
    /* Keep the reparented notebook alive until its option UI items are released. */
    GtkWidget *book_options_notebook;

    GtkWidget *currency_selector;
    GtkWidget *currency_selector_label;

    GtkDropDown *language_combo;
    GtkDropDown *region_combo;
    GtkWidget *region_label;
    GtkStringList *language_model;
    GtkStringList *region_model;
    GPtrArray *locales;
    GPtrArray *visible_locales;

    gchar *gnc_accounts_dir;

    GtkColumnView *categories_view;
    GListStore *category_rows;
    GtkSortListModel *sorted_category_rows;
    GtkSingleSelection *category_selection;
    GtkColumnViewColumn *categories_title_column;
    guint initial_category_position;
    GtkTextView *category_description;
    GtkLabel *category_accounts_label;
    GtkColumnView *category_accounts_view;
    GListStore *category_account_roots;
    GtkTreeListModel *category_account_tree;
    GtkNoSelection *category_account_selection;
    GSList *example_account_list;
    gboolean category_set_changed;

    GtkColumnView *final_account_view;
    GListStore *final_account_roots;
    GtkTreeListModel *final_account_tree;
    GtkSingleSelection *final_account_selection;
    GHashTable *balance_hash;

    Account *our_account_tree;

    gboolean account_list_added;
    gboolean next_ok;
    gboolean use_defaults;
    gboolean new_book;
    gboolean closing;
    gboolean updating_locale;

    GncOptionDB *options;
    GncOptionsDialog *optionwin;

    GncHierarchyAssistantFinishedCallback when_completed;

};

static void hierarchy_update_navigation (hierarchy_data *data);
static void hierarchy_show_page (hierarchy_data *data, guint page);
static void hierarchy_cancel (hierarchy_data *data);
static void hierarchy_finish (hierarchy_data *data);
static void hierarchy_reload_categories (hierarchy_data *data,
                                         const gchar *locale_name);
static void on_choose_account_categories_prepare (hierarchy_data *data);
static void on_final_account_prepare (hierarchy_data *data);
static void on_select_currency_prepare (hierarchy_data *data);

extern "C"
{
void select_all_clicked (GtkButton *button, hierarchy_data *data);
void clear_all_clicked (GtkButton *button, hierarchy_data *data);
}
// ------------------------------------------------------------

static void
destroy_hash_helper (gpointer key, gpointer value, gpointer user_data)
{
    auto balance{static_cast<gnc_numeric*>(value)};

    g_free (balance);
    (void)key;
    (void)user_data;
}

static gnc_numeric
get_final_balance (GHashTable *hash, Account *account)
{
    if (!hash || !account)
        return gnc_numeric_zero ();

    auto balance{static_cast<gnc_numeric*>(g_hash_table_lookup(hash, account))};
    if (balance)
        return *balance;
    return gnc_numeric_zero ();
}

static void
set_final_balance (GHashTable *hash, Account *account, gnc_numeric in_balance)
{
    if (!hash || !account)
        return;

    auto balance{static_cast<gnc_numeric*>(g_hash_table_lookup(hash, account))};
    if (balance)
    {
        *balance = in_balance;
        return;
    }

    balance = g_new (gnc_numeric, 1);
    *balance = in_balance;
    g_hash_table_insert (hash, account, balance);
}

#ifdef MAC_INTEGRATION
/* Repeat retrieving the locale from defaults in case it was overridden in
 * gnucash-bin because it wasn't a supported POSIX locale.
 */
static char*
mac_locale()
{
    NSAutoreleasePool *pool = [[NSAutoreleasePool alloc] init];
    NSLocale* locale = [NSLocale currentLocale];
    NSString* locale_str;
    char *retval = nullptr;
    @try
    {
        locale_str =[[[locale objectForKey: NSLocaleLanguageCode]
		       stringByAppendingString: @"_"]
		      stringByAppendingString:
		      [locale objectForKey: NSLocaleCountryCode]];
    }
    @catch (NSException *err)
    {
	locale_str = @"_";
    }
/* If we didn't get a valid current locale, the string will be just "_" */
    if ([locale_str isEqualToString: @"_"])
	locale_str = @"en_US";
    retval = g_strdup([locale_str UTF8String]);
    [pool drain];
    return retval;
}
#endif
static gchar*
gnc_get_ea_locale_dir(const char *top_dir)
{
    static const char* default_locale = "C";
    gchar *ret;
    gchar *locale;
    GStatBuf buf;
    int i;

#if PLATFORM(WINDOWS)
    /* On win32, setlocale() doesn't say anything useful, so we check
     * g_win32_getlocale(). Unfortunately it checks the value of $LANG first,
     * and the user might have worked around the absence of sv in gettext's
     * Microsoft Conversion Array by setting it to "Swedish_Sweden", so first
     * check that.
     */
    const gchar *env_locale;
    env_locale = g_getenv("LANG");
    if (g_strcmp0(env_locale, "Swedish_Sweden") == 0)
        locale = g_strdup("sv_SE");
    else if (g_strcmp0(env_locale, "Swedish_Finland") == 0)
        locale =g_strdup("sv_FI");
    else if (g_strcmp0(env_locale, "Swedish_Åland Islands") == 0)
        locale =g_strdup("sv_AX");
     else
    {
        locale = g_win32_getlocale();
        if (!locale)
        {
            PWARN ("Couldn't retrieve locale. Falling back to default one.");
            locale = g_strdup ("C");
        }
    }
#elif defined MAC_INTEGRATION
    locale = mac_locale();
# else
    locale = g_strdup(setlocale(LC_MESSAGES, nullptr));
#endif

    i = strlen(locale);
    ret = g_build_filename(top_dir, locale, (char *)nullptr);

    while (g_stat(ret, &buf) != 0)
    {
        i--;
        if (i < 1)
        {
            g_free(ret);
            ret = g_build_filename(top_dir, default_locale, (char *)nullptr);
            break;
        }
        locale[i] = '\0';
        g_free(ret);
        ret = g_build_filename(top_dir, locale, (char *)nullptr);
    }

    g_free(locale);

    return ret;
}

static void
hierarchy_category_row_class_init (HierarchyCategoryRowClass *klass)
{
    (void)klass;
}

static void
hierarchy_category_row_init (HierarchyCategoryRow *row)
{
    (void)row;
}

static HierarchyCategoryRow *
hierarchy_category_row_new (GncExampleAccount *account_set, gboolean checked)
{
    auto row = static_cast<HierarchyCategoryRow*>(g_object_new
        (hierarchy_category_row_get_type (), nullptr));

    row->account_set = account_set;
    row->checked = checked;
    return row;
}

static void
hierarchy_account_row_class_init (HierarchyAccountRowClass *klass)
{
    (void)klass;
}

static void
hierarchy_account_row_init (HierarchyAccountRow *row)
{
    (void)row;
}

static HierarchyAccountRow *
hierarchy_account_row_new (Account *account, QofBook *book)
{
    auto row = static_cast<HierarchyAccountRow*>(g_object_new
        (hierarchy_account_row_get_type (), nullptr));

    row->book = book;
    row->guid = *xaccAccountGetGUID (account);
    return row;
}

static Account *
hierarchy_account_row_get_account (const HierarchyAccountRow *row)
{
    if (!row || !row->book)
        return nullptr;

    return xaccAccountLookup (&row->guid, row->book);
}

static void
hierarchy_account_ref_free (gpointer data)
{
    g_free (data);
}

static HierarchyAccountRef *
hierarchy_account_ref_new (hierarchy_data *data, Account *account,
                           HierarchyAccountColumn column)
{
    auto ref = g_new0 (HierarchyAccountRef, 1);

    ref->data = data;
    ref->book = qof_instance_get_book (QOF_INSTANCE (account));
    ref->guid = *xaccAccountGetGUID (account);
    ref->column = column;
    return ref;
}

static Account *
hierarchy_account_ref_get_account (const HierarchyAccountRef *ref)
{
    if (!ref || !ref->book)
        return nullptr;

    return xaccAccountLookup (&ref->guid, ref->book);
}

static void
hierarchy_locale_free (gpointer data)
{
    auto locale = static_cast<HierarchyLocale*>(data);

    if (!locale)
        return;
    g_free (locale->language);
    g_free (locale->region);
    g_free (locale->name);
    g_free (locale);
}

static gint
hierarchy_locale_compare (gconstpointer a, gconstpointer b)
{
    auto locale_a = *static_cast<HierarchyLocale * const *>(a);
    auto locale_b = *static_cast<HierarchyLocale * const *>(b);

    return g_strcmp0 (locale_a->name, locale_b->name);
}

static void
hierarchy_load_locales (hierarchy_data *data)
{
    GHashTable *languages;
    GDir *directory;
    const gchar *name;

    data->locales = g_ptr_array_new_with_free_func (hierarchy_locale_free);
    data->visible_locales = g_ptr_array_new ();
    languages = g_hash_table_new_full (g_str_hash, g_str_equal, g_free, nullptr);
    directory = g_dir_open (data->gnc_accounts_dir, 0, nullptr);
    if (!directory)
    {
        g_hash_table_destroy (languages);
        return;
    }

    while ((name = g_dir_read_name (directory)) != nullptr)
    {
        gchar **parts;
        auto locale = g_new0 (HierarchyLocale, 1);

        parts = g_strsplit (name, "_", 2);
        locale->name = g_strdup (name);
        locale->language = g_strdup (g_strcmp0 (name, "C") == 0
                                    ? "en" : parts[0]);
        locale->region = g_strdup (g_strcmp0 (name, "C") == 0
                                  ? "US" : parts[1] ? parts[1] : "--");
        g_ptr_array_add (data->locales, locale);
        g_hash_table_add (languages, g_strdup (locale->language));
        g_strfreev (parts);
    }
    g_dir_close (directory);

    g_ptr_array_sort (data->locales, hierarchy_locale_compare);
    data->language_model = gtk_string_list_new (nullptr);
    {
        GList *names = g_hash_table_get_keys (languages);
        names = g_list_sort (names, (GCompareFunc)g_strcmp0);
        for (auto item = names; item; item = item->next)
            gtk_string_list_append (data->language_model,
                                    static_cast<const gchar*>(item->data));
        g_list_free (names);
    }
    g_hash_table_destroy (languages);
    gtk_drop_down_set_model (data->language_combo,
                             G_LIST_MODEL (data->language_model));
}

static const HierarchyLocale *
hierarchy_selected_locale (const hierarchy_data *data)
{
    const guint position = gtk_drop_down_get_selected (data->region_combo);

    if (position == GTK_INVALID_LIST_POSITION ||
        position >= data->visible_locales->len)
        return nullptr;
    return static_cast<const HierarchyLocale*>(g_ptr_array_index
        (data->visible_locales, position));
}

static void
hierarchy_rebuild_regions (hierarchy_data *data, const gchar *language,
                           const gchar *preferred_name)
{
    guint selected = GTK_INVALID_LIST_POSITION;

    data->updating_locale = TRUE;
    g_clear_object (&data->region_model);
    data->region_model = gtk_string_list_new (nullptr);
    g_ptr_array_set_size (data->visible_locales, 0);

    for (guint i = 0; i < data->locales->len; i++)
    {
        auto locale = static_cast<HierarchyLocale*>(g_ptr_array_index
            (data->locales, i));

        if (g_strcmp0 (locale->language, language) != 0)
            continue;
        if (preferred_name && g_strcmp0 (locale->name, preferred_name) == 0)
            selected = data->visible_locales->len;
        gtk_string_list_append (data->region_model, locale->region);
        g_ptr_array_add (data->visible_locales, locale);
    }

    gtk_drop_down_set_model (data->region_combo, G_LIST_MODEL (data->region_model));
    if (data->visible_locales->len > 0)
    {
        if (selected == GTK_INVALID_LIST_POSITION)
            selected = 0;
        gtk_drop_down_set_selected (data->region_combo, selected);
    }
    gtk_widget_set_visible (data->region_label, data->visible_locales->len == 1);
    gtk_widget_set_visible (GTK_WIDGET (data->region_combo),
                            data->visible_locales->len != 1);
    if (data->visible_locales->len == 1)
    {
        auto locale = static_cast<HierarchyLocale*>(g_ptr_array_index
            (data->visible_locales, 0));
        gtk_label_set_text (GTK_LABEL (data->region_label), locale->region);
    }
    data->updating_locale = FALSE;
}

static void
hierarchy_region_changed_cb (GtkDropDown *dropdown, GParamSpec *pspec,
                             hierarchy_data *data)
{
    auto locale = hierarchy_selected_locale (data);

    if (!data->updating_locale && locale && data->account_list_added)
        hierarchy_reload_categories (data, locale->name);
    (void)dropdown;
    (void)pspec;
}

static void
hierarchy_language_changed_cb (GtkDropDown *dropdown, GParamSpec *pspec,
                               hierarchy_data *data)
{
    const guint position = gtk_drop_down_get_selected (dropdown);
    const gchar *language;
    auto locale = hierarchy_selected_locale (data);

    if (data->updating_locale || position == GTK_INVALID_LIST_POSITION)
        return;
    language = gtk_string_list_get_string (data->language_model, position);
    hierarchy_rebuild_regions (data, language, locale ? locale->name : nullptr);
    locale = hierarchy_selected_locale (data);
    if (locale && data->account_list_added)
        hierarchy_reload_categories (data, locale->name);
    (void)pspec;
}

static void
update_language_region_combos (hierarchy_data *data, const gchar *locale_dir)
{
    gchar *default_name;
    const gchar *default_language = nullptr;

    hierarchy_load_locales (data);
    if (g_list_model_get_n_items (G_LIST_MODEL (data->language_model)) == 0)
        return;

    default_name = g_path_get_basename (locale_dir);
    for (guint i = 0; i < data->locales->len; i++)
    {
        auto locale = static_cast<HierarchyLocale*>(g_ptr_array_index
            (data->locales, i));
        if (g_strcmp0 (locale->name, default_name) == 0)
        {
            default_language = locale->language;
            break;
        }
    }
    if (!default_language)
        default_language = static_cast<HierarchyLocale*>(g_ptr_array_index
            (data->locales, 0))->language;

    for (guint i = 0; i < g_list_model_get_n_items
             (G_LIST_MODEL (data->language_model)); i++)
    {
        const gchar *language = gtk_string_list_get_string (data->language_model, i);
        if (g_strcmp0 (language, default_language) == 0)
        {
            gtk_drop_down_set_selected (data->language_combo, i);
            break;
        }
    }
    hierarchy_rebuild_regions (data, default_language, default_name);
    g_signal_connect (data->language_combo, "notify::selected",
                      G_CALLBACK (hierarchy_language_changed_cb), data);
    g_signal_connect (data->region_combo, "notify::selected",
                      G_CALLBACK (hierarchy_region_changed_cb), data);
    g_free (default_name);
}
/************************************************************
 *                  Choose Categories Page                  *
 ************************************************************/

typedef struct
{
    hierarchy_data *data;
    HierarchyAccountColumn column;
    gboolean editable;
} HierarchyAccountFactoryData;

static GListModel *
hierarchy_account_children_model (gpointer item, gpointer user_data)
{
    auto row = static_cast<HierarchyAccountRow*>(item);
    auto account = hierarchy_account_row_get_account (row);
    auto children = g_list_store_new (hierarchy_account_row_get_type ());

    if (account)
    {
        auto account_children = gnc_account_get_children_sorted (account);
        for (auto node = account_children; node; node = node->next)
        {
            auto child = hierarchy_account_row_new (static_cast<Account*>(node->data),
                                                    row->book);
            g_list_store_append (children, child);
            g_object_unref (child);
        }
        g_list_free (account_children);
    }
    (void)user_data;
    return G_LIST_MODEL (children);
}

static GListStore *
hierarchy_account_roots_new (Account *root, QofBook *book)
{
    auto roots = g_list_store_new (hierarchy_account_row_get_type ());

    if (!root)
        return roots;

    auto account_children = gnc_account_get_children_sorted (root);
    for (auto node = account_children; node; node = node->next)
    {
        auto row = hierarchy_account_row_new (static_cast<Account*>(node->data), book);
        g_list_store_append (roots, row);
        g_object_unref (row);
    }
    g_list_free (account_children);
    return roots;
}

static HierarchyAccountRow *
hierarchy_list_item_get_account_row (GtkListItem *list_item,
                                     GtkTreeListRow **tree_row)
{
    auto item = gtk_list_item_get_item (list_item);

    if (!item || !GTK_IS_TREE_LIST_ROW (item))
        return nullptr;
    if (tree_row)
        *tree_row = GTK_TREE_LIST_ROW (item);
    return static_cast<HierarchyAccountRow*>(gtk_tree_list_row_get_item
        (GTK_TREE_LIST_ROW (item)));
}

static const gchar *
hierarchy_account_column_text (Account *account, HierarchyAccountColumn column)
{
    const gchar *text = "";

    if (!account)
        return text;

    switch (column)
    {
    case HIERARCHY_ACCOUNT_COLUMN_NAME:
        text = xaccAccountGetName (account);
        break;
    case HIERARCHY_ACCOUNT_COLUMN_CODE:
        text = xaccAccountGetCode (account);
        break;
    case HIERARCHY_ACCOUNT_COLUMN_DESCRIPTION:
        text = xaccAccountGetDescription (account);
        break;
    case HIERARCHY_ACCOUNT_COLUMN_NOTES:
        text = xaccAccountGetNotes (account);
        break;
    case HIERARCHY_ACCOUNT_COLUMN_TYPE:
        text = xaccAccountGetTypeStr (xaccAccountGetType (account));
        break;
    default:
        break;
    }

    return text ? text : "";
}

static gboolean
hierarchy_account_balance_is_editable (Account *account)
{
    GncAccountMergeDisposition disposition;

    if (!account || xaccAccountGetPlaceholder (account) ||
        xaccAccountGetType (account) == ACCT_TYPE_EQUITY ||
        xaccAccountGetType (account) == ACCT_TYPE_TRADING)
        return FALSE;
    disposition = determine_merge_disposition
        (gnc_book_get_root_account (gnc_get_current_book ()), account);
    return disposition == GNC_ACCOUNT_MERGE_DISPOSITION_CREATE_NEW;
}

static const gchar *
hierarchy_account_balance_text (Account *account, hierarchy_data *data)
{
    auto balance = get_final_balance (data->balance_hash, account);

    if (!account || gnc_numeric_zero_p (balance))
        return "";
    return xaccPrintAmount (balance, gnc_account_print_info (account, FALSE));
}

static const gchar *
hierarchy_account_use_existing_text (Account *account)
{
    if (!account)
        return "";
    return determine_merge_disposition (gnc_book_get_root_account
        (gnc_get_current_book ()), account) ==
        GNC_ACCOUNT_MERGE_DISPOSITION_USE_EXISTING ? _("Yes") : _("No");
}

static void
hierarchy_account_entry_commit (GtkEditable *editable, gpointer user_data)
{
    auto ref = static_cast<HierarchyAccountRef*>(g_object_get_data
        (G_OBJECT (editable), "hierarchy-account-ref"));
    auto account = hierarchy_account_ref_get_account (ref);
    const gchar *text;

    (void)user_data;
    if (!ref || !ref->data || ref->data->closing || !account)
        return;
    text = gtk_editable_get_text (editable);
    switch (ref->column)
    {
    case HIERARCHY_ACCOUNT_COLUMN_NAME:
    {
        auto parent = gnc_account_get_parent (account);
        auto existing = parent ? gnc_account_lookup_by_name (parent, text) : nullptr;
        if (!existing || existing == account)
            xaccAccountSetName (account, text);
        break;
    }
    case HIERARCHY_ACCOUNT_COLUMN_CODE:
        xaccAccountSetCode (account, text);
        break;
    case HIERARCHY_ACCOUNT_COLUMN_DESCRIPTION:
        xaccAccountSetDescription (account, text);
        break;
    case HIERARCHY_ACCOUNT_COLUMN_NOTES:
        xaccAccountSetNotes (account, text);
        break;
    case HIERARCHY_ACCOUNT_COLUMN_BALANCE:
    {
        char *error_loc = nullptr;
        auto amount = gnc_numeric_zero ();

        if (gnc_exp_parser_parse (text, &amount, &error_loc))
        {
            amount = gnc_numeric_convert (amount,
                                          xaccAccountGetCommoditySCU (account),
                                          GNC_HOW_RND_ROUND_HALF_UP);
            set_final_balance (ref->data->balance_hash, account, amount);
            qof_event_gen (QOF_INSTANCE (account), QOF_EVENT_MODIFY, nullptr);
        }
        else
            gtk_editable_set_text (editable, "");
        g_free (error_loc);
        break;
    }
    default:
        break;
    }
}

static void
hierarchy_account_entry_focus_leave (GtkEventControllerFocus *controller,
                                     gpointer user_data)
{
    auto widget = gtk_event_controller_get_widget (GTK_EVENT_CONTROLLER (controller));

    if (widget)
        hierarchy_account_entry_commit (GTK_EDITABLE (widget), user_data);
}

static void
hierarchy_account_placeholder_toggled (GtkCheckButton *button, gpointer user_data)
{
    auto ref = static_cast<HierarchyAccountRef*>(g_object_get_data
        (G_OBJECT (button), "hierarchy-account-ref"));
    auto account = hierarchy_account_ref_get_account (ref);

    if (!ref || !ref->data || ref->data->closing || !account)
        return;
    xaccAccountSetPlaceholder (account, gtk_check_button_get_active (button));
    if (xaccAccountGetPlaceholder (account))
        set_final_balance (ref->data->balance_hash, account, gnc_numeric_zero ());
    qof_event_gen (QOF_INSTANCE (account), QOF_EVENT_MODIFY, nullptr);
    gtk_widget_queue_draw (GTK_WIDGET (ref->data->final_account_view));
    (void)user_data;
}

static void
hierarchy_account_cell_setup (GtkListItemFactory *factory, GtkListItem *list_item,
                              gpointer user_data)
{
    auto factory_data = static_cast<HierarchyAccountFactoryData*>(user_data);
    GtkWidget *child;

    switch (factory_data->column)
    {
    case HIERARCHY_ACCOUNT_COLUMN_NAME:
    {
        auto entry = gtk_entry_new ();
        auto expander = gtk_tree_expander_new ();
        auto controller = gtk_event_controller_focus_new ();

        gtk_entry_set_has_frame (GTK_ENTRY (entry), FALSE);
        gtk_editable_set_editable (GTK_EDITABLE (entry), factory_data->editable);
        gtk_widget_set_hexpand (entry, TRUE);
        gtk_tree_expander_set_child (GTK_TREE_EXPANDER (expander), entry);
        g_signal_connect (entry, "activate",
                          G_CALLBACK (hierarchy_account_entry_commit), nullptr);
        g_signal_connect (controller, "leave",
                          G_CALLBACK (hierarchy_account_entry_focus_leave), nullptr);
        gtk_widget_add_controller (entry, controller);
        child = expander;
        break;
    }
    case HIERARCHY_ACCOUNT_COLUMN_CODE:
    case HIERARCHY_ACCOUNT_COLUMN_DESCRIPTION:
    case HIERARCHY_ACCOUNT_COLUMN_NOTES:
    case HIERARCHY_ACCOUNT_COLUMN_BALANCE:
    {
        auto entry = gtk_entry_new ();
        auto controller = gtk_event_controller_focus_new ();

        gtk_entry_set_has_frame (GTK_ENTRY (entry), FALSE);
        gtk_editable_set_editable (GTK_EDITABLE (entry), factory_data->editable);
        if (factory_data->column == HIERARCHY_ACCOUNT_COLUMN_BALANCE)
            gtk_editable_set_alignment (GTK_EDITABLE (entry), 1.0);
        g_signal_connect (entry, "activate",
                          G_CALLBACK (hierarchy_account_entry_commit), nullptr);
        g_signal_connect (controller, "leave",
                          G_CALLBACK (hierarchy_account_entry_focus_leave), nullptr);
        gtk_widget_add_controller (entry, controller);
        child = entry;
        break;
    }
    case HIERARCHY_ACCOUNT_COLUMN_PLACEHOLDER:
        child = gtk_check_button_new ();
        g_signal_connect (child, "toggled",
                          G_CALLBACK (hierarchy_account_placeholder_toggled), nullptr);
        break;
    default:
        child = gtk_label_new (nullptr);
        gtk_label_set_ellipsize (GTK_LABEL (child), PANGO_ELLIPSIZE_END);
        break;
    }
    gtk_list_item_set_child (list_item, child);
    (void)factory;
}

static void
hierarchy_account_cell_bind (GtkListItemFactory *factory, GtkListItem *list_item,
                             gpointer user_data)
{
    auto factory_data = static_cast<HierarchyAccountFactoryData*>(user_data);
    GtkTreeListRow *tree_row = nullptr;
    g_autoptr(GObject) row_object = G_OBJECT
        (hierarchy_list_item_get_account_row (list_item, &tree_row));
    auto row = reinterpret_cast<HierarchyAccountRow*>(row_object);
    auto account = hierarchy_account_row_get_account (row);
    auto child = gtk_list_item_get_child (list_item);
    GtkWidget *editable = child;

    if (factory_data->column == HIERARCHY_ACCOUNT_COLUMN_NAME)
    {
        gtk_tree_expander_set_list_row (GTK_TREE_EXPANDER (child), tree_row);
        editable = gtk_tree_expander_get_child (GTK_TREE_EXPANDER (child));
    }

    if (!account)
    {
        if (GTK_IS_EDITABLE (editable))
            gtk_editable_set_text (GTK_EDITABLE (editable), "");
        else if (GTK_IS_LABEL (child))
            gtk_label_set_text (GTK_LABEL (child), "");
        return;
    }

    if (factory_data->column == HIERARCHY_ACCOUNT_COLUMN_PLACEHOLDER)
    {
        g_signal_handlers_block_by_func (child,
                                         (gpointer)hierarchy_account_placeholder_toggled,
                                         nullptr);
        gtk_check_button_set_active (GTK_CHECK_BUTTON (child),
                                     xaccAccountGetPlaceholder (account));
        g_signal_handlers_unblock_by_func (child,
                                           (gpointer)hierarchy_account_placeholder_toggled,
                                           nullptr);
        g_object_set_data_full (G_OBJECT (child), "hierarchy-account-ref",
                                hierarchy_account_ref_new (factory_data->data,
                                                           account,
                                                           factory_data->column),
                                hierarchy_account_ref_free);
    }
    else if (factory_data->column == HIERARCHY_ACCOUNT_COLUMN_BALANCE)
    {
        gtk_editable_set_text (GTK_EDITABLE (editable),
                               hierarchy_account_balance_text (account,
                                                               factory_data->data));
        gtk_widget_set_sensitive (editable, hierarchy_account_balance_is_editable
                                  (account));
        g_object_set_data_full (G_OBJECT (editable), "hierarchy-account-ref",
                                hierarchy_account_ref_new (factory_data->data,
                                                           account,
                                                           factory_data->column),
                                hierarchy_account_ref_free);
    }
    else if (factory_data->column == HIERARCHY_ACCOUNT_COLUMN_USE_EXISTING)
        gtk_label_set_text (GTK_LABEL (child), hierarchy_account_use_existing_text (account));
    else if (factory_data->column == HIERARCHY_ACCOUNT_COLUMN_TYPE)
        gtk_label_set_text (GTK_LABEL (child), hierarchy_account_column_text
                            (account, factory_data->column));
    else
    {
        gtk_editable_set_text (GTK_EDITABLE (editable), hierarchy_account_column_text
                               (account, factory_data->column));
        g_object_set_data_full (G_OBJECT (editable), "hierarchy-account-ref",
                                hierarchy_account_ref_new (factory_data->data,
                                                           account,
                                                           factory_data->column),
                                hierarchy_account_ref_free);
    }
    (void)factory;
}

static void
hierarchy_append_account_column (hierarchy_data *data, GtkColumnView *view,
                                 const gchar *title, HierarchyAccountColumn column,
                                 gboolean expand, gboolean editable)
{
    auto factory_data = g_new0 (HierarchyAccountFactoryData, 1);
    auto factory = gtk_signal_list_item_factory_new ();
    auto view_column = gtk_column_view_column_new (title, factory);

    factory_data->data = data;
    factory_data->column = column;
    factory_data->editable = editable;
    g_object_set_data_full (G_OBJECT (factory), "hierarchy-factory-data",
                            factory_data, g_free);
    g_signal_connect (factory, "setup", G_CALLBACK (hierarchy_account_cell_setup),
                      factory_data);
    g_signal_connect (factory, "bind", G_CALLBACK (hierarchy_account_cell_bind),
                      factory_data);
    gtk_column_view_column_set_resizable (view_column, TRUE);
    gtk_column_view_column_set_expand (view_column, expand);
    gtk_column_view_append_column (view, view_column);
    g_object_unref (view_column);
}

static void hierarchy_category_toggled (GtkCheckButton *button, gpointer user_data);

static gint
hierarchy_category_sort (gconstpointer first, gconstpointer second,
                         gpointer user_data)
{
    auto first_row = static_cast<const HierarchyCategoryRow*>(first);
    auto second_row = static_cast<const HierarchyCategoryRow*>(second);
    const guint column = GPOINTER_TO_UINT (user_data);
    gint comparison;

    if (column == 0)
        comparison = first_row->checked == second_row->checked ? 0
            : first_row->checked ? -1 : 1;
    else
        comparison = g_utf8_collate (first_row->account_set->title,
                                     second_row->account_set->title);
    return comparison;
}

static void
hierarchy_category_cell_setup (GtkListItemFactory *factory, GtkListItem *list_item,
                               gpointer user_data)
{
    GtkWidget *child;

    if (GPOINTER_TO_UINT (user_data) == 0)
    {
        child = gtk_check_button_new ();
        g_signal_connect (child, "toggled",
                          G_CALLBACK (hierarchy_category_toggled), nullptr);
    }
    else
    {
        child = gtk_label_new (nullptr);
        gtk_label_set_xalign (GTK_LABEL (child), 0.0);
        gtk_label_set_ellipsize (GTK_LABEL (child), PANGO_ELLIPSIZE_END);
    }
    gtk_list_item_set_child (list_item, child);
    (void)factory;
}

static void
hierarchy_category_toggled (GtkCheckButton *button, gpointer user_data)
{
    auto row = static_cast<HierarchyCategoryRow*>(g_object_get_data
        (G_OBJECT (button), "hierarchy-category-row"));
    auto data = static_cast<hierarchy_data*>(g_object_get_data
        (G_OBJECT (button), "hierarchy-data"));

    (void)user_data;
    if (!row || !data || data->closing)
        return;
    row->checked = gtk_check_button_get_active (button);
    data->category_set_changed = TRUE;
    hierarchy_update_navigation (data);
}

static void
hierarchy_category_cell_bind (GtkListItemFactory *factory, GtkListItem *list_item,
                              gpointer user_data)
{
    auto row = static_cast<HierarchyCategoryRow*>(gtk_list_item_get_item (list_item));
    auto child = gtk_list_item_get_child (list_item);
    auto data = static_cast<hierarchy_data*>(g_object_get_data
        (G_OBJECT (factory), "hierarchy-data"));

    if (GPOINTER_TO_UINT (user_data) == 0)
    {
        g_signal_handlers_block_by_func (child, (gpointer)hierarchy_category_toggled,
                                         nullptr);
        gtk_check_button_set_active (GTK_CHECK_BUTTON (child), row && row->checked);
        g_signal_handlers_unblock_by_func (child,
                                           (gpointer)hierarchy_category_toggled,
                                           nullptr);
        g_object_set_data (G_OBJECT (child), "hierarchy-category-row", row);
        g_object_set_data (G_OBJECT (child), "hierarchy-data", data);
    }
    else
        gtk_label_set_text (GTK_LABEL (child), row && row->account_set
                            ? row->account_set->title : "");
}

static void
hierarchy_append_category_column (hierarchy_data *data, const gchar *title,
                                  guint column, gboolean expand)
{
    auto factory = gtk_signal_list_item_factory_new ();
    auto view_column = gtk_column_view_column_new (title, factory);
    auto sorter = gtk_custom_sorter_new (hierarchy_category_sort,
                                         GUINT_TO_POINTER (column), nullptr);

    g_object_set_data (G_OBJECT (factory), "hierarchy-data", data);
    g_signal_connect (factory, "setup", G_CALLBACK (hierarchy_category_cell_setup),
                      GUINT_TO_POINTER (column));
    g_signal_connect (factory, "bind", G_CALLBACK (hierarchy_category_cell_bind),
                      GUINT_TO_POINTER (column));
    gtk_column_view_column_set_expand (view_column, expand);
    gtk_column_view_column_set_resizable (view_column, TRUE);
    gtk_column_view_column_set_sorter (view_column, GTK_SORTER (sorter));
    gtk_column_view_append_column (data->categories_view, view_column);
    if (column == 1)
        data->categories_title_column = view_column;
    g_object_unref (sorter);
    g_object_unref (view_column);
}

static void
hierarchy_clear_preview (hierarchy_data *data)
{
    gtk_column_view_set_model (data->category_accounts_view, nullptr);
    g_clear_object (&data->category_account_selection);
    g_clear_object (&data->category_account_tree);
    g_clear_object (&data->category_account_roots);
}

static void
hierarchy_set_preview (hierarchy_data *data, GncExampleAccount *account_set)
{
    gchar *title;
    gchar *markup;
    GtkTextBuffer *buffer;

    hierarchy_clear_preview (data);
    buffer = gtk_text_view_get_buffer (data->category_description);
    if (!account_set)
    {
        gtk_label_set_markup (data->category_accounts_label,
                              "<b>Accounts in Category</b>");
        gtk_text_buffer_set_text (buffer, "", -1);
        return;
    }

    title = g_strdup_printf (_("Accounts in '%s'"), account_set->title);
    markup = g_strdup_printf ("<b>%s</b>", title);
    gtk_label_set_markup (data->category_accounts_label, markup);
    gtk_text_buffer_set_text (buffer, account_set->long_description
                              ? account_set->long_description
                              : _("No description provided."), -1);
    g_free (markup);
    g_free (title);

    data->category_account_roots = hierarchy_account_roots_new (account_set->root,
                                                                  account_set->book);
    data->category_account_tree = gtk_tree_list_model_new
        (G_LIST_MODEL (g_object_ref (data->category_account_roots)), FALSE, TRUE,
         hierarchy_account_children_model, nullptr, nullptr);
    data->category_account_selection = gtk_no_selection_new
        (G_LIST_MODEL (g_object_ref (data->category_account_tree)));
    gtk_column_view_set_model (data->category_accounts_view,
                               GTK_SELECTION_MODEL (data->category_account_selection));
}

static void
hierarchy_category_selection_changed (GtkSelectionModel *selection, guint position,
                                      guint n_items, hierarchy_data *data)
{
    auto item = gtk_single_selection_get_selected_item (data->category_selection);
    auto row = item ? static_cast<HierarchyCategoryRow*>(item) : nullptr;

    hierarchy_set_preview (data, row ? row->account_set : nullptr);
    (void)selection;
    (void)position;
    (void)n_items;
}

static void
categories_page_enable_next (hierarchy_data *data)
{
    data->next_ok = FALSE;
    for (guint i = 0; i < g_list_model_get_n_items (G_LIST_MODEL (data->category_rows)); i++)
    {
        auto item = g_list_model_get_item (G_LIST_MODEL (data->category_rows), i);
        auto row = static_cast<HierarchyCategoryRow*>(item);
        data->next_ok = row->checked;
        g_object_unref (item);
        if (data->next_ok)
            break;
    }
    hierarchy_update_navigation (data);
}

static void
hierarchy_clear_categories (hierarchy_data *data)
{
    hierarchy_clear_preview (data);
    gtk_single_selection_set_selected (data->category_selection,
                                       GTK_INVALID_LIST_POSITION);
    g_list_store_remove_all (data->category_rows);
    if (data->example_account_list)
    {
        gnc_free_example_account_list (data->example_account_list);
        data->example_account_list = nullptr;
    }
}

static void
hierarchy_reload_categories (hierarchy_data *data, const gchar *locale_name)
{
    gchar *account_path;
    guint selected = GTK_INVALID_LIST_POSITION;
    guint position = 0;
    HierarchyCategoryRow *selected_row = nullptr;

    if (!locale_name)
        return;
    gnc_suspend_gui_refresh ();
    hierarchy_clear_categories (data);
    account_path = g_build_filename (data->gnc_accounts_dir, locale_name, nullptr);
    qof_event_suspend ();
    data->example_account_list = gnc_load_example_account_list (account_path);
    qof_event_resume ();

    for (auto node = data->example_account_list; node; node = node->next, position++)
    {
        auto account_set = static_cast<GncExampleAccount*>(node->data);
        const gboolean checked = data->use_defaults && account_set->start_selected;
        auto row = hierarchy_category_row_new (account_set, checked);

        g_list_store_append (data->category_rows, row);
        g_object_unref (row);
        if (checked && !selected_row)
            selected_row = row;
    }
    if (selected_row)
    {
        for (guint i = 0; i < g_list_model_get_n_items
                 (G_LIST_MODEL (data->sorted_category_rows)); i++)
        {
            auto item = g_list_model_get_item
                (G_LIST_MODEL (data->sorted_category_rows), i);
            if (item == G_OBJECT (selected_row))
                selected = i;
            g_object_unref (item);
            if (selected != GTK_INVALID_LIST_POSITION)
                break;
        }
    }
    if (selected == GTK_INVALID_LIST_POSITION && position > 0)
        selected = 0;
    data->initial_category_position = selected;
    gtk_single_selection_set_selected (data->category_selection, selected);
    if (selected != GTK_INVALID_LIST_POSITION)
        gtk_column_view_scroll_to (data->categories_view, selected, nullptr,
                                   GTK_LIST_SCROLL_FOCUS, nullptr);
    data->category_set_changed = position > 0;
    categories_page_enable_next (data);
    gnc_resume_gui_refresh ();
    g_free (account_path);
}

static void
account_categories_tree_view_prepare (hierarchy_data *data)
{
    gchar *locale_dir;
    auto locale = static_cast<const HierarchyLocale*>(nullptr);

    data->gnc_accounts_dir = gnc_path_get_accountsdir ();
    locale_dir = gnc_get_ea_locale_dir (data->gnc_accounts_dir);
    update_language_region_combos (data, locale_dir);
    locale = hierarchy_selected_locale (data);
    if (locale)
        hierarchy_reload_categories (data, locale->name);
    g_free (locale_dir);
}

void
on_choose_account_categories_prepare (hierarchy_data *data)
{
    if (!data->account_list_added)
    {
        data->account_list_added = TRUE;
        account_categories_tree_view_prepare (data);
    }
    categories_page_enable_next (data);
}

static void
hierarchy_category_row_changed (hierarchy_data *data, guint position,
                                HierarchyCategoryRow *row)
{
    gpointer items[] = { row };

    g_list_store_splice (data->category_rows, position, 1, items, 1);
}

void
select_all_clicked (GtkButton *button, hierarchy_data *data)
{
    for (guint i = 0; i < g_list_model_get_n_items (G_LIST_MODEL (data->category_rows)); i++)
    {
        auto item = g_list_model_get_item (G_LIST_MODEL (data->category_rows), i);
        auto row = static_cast<HierarchyCategoryRow*>(item);

        if (row->account_set && !row->account_set->exclude_from_select_all)
            row->checked = TRUE;
        hierarchy_category_row_changed (data, i, row);
        g_object_unref (item);
    }
    data->category_set_changed = TRUE;
    gtk_widget_queue_draw (GTK_WIDGET (data->categories_view));
    categories_page_enable_next (data);
    (void)button;
}

void
clear_all_clicked (GtkButton *button, hierarchy_data *data)
{
    for (guint i = 0; i < g_list_model_get_n_items (G_LIST_MODEL (data->category_rows)); i++)
    {
        auto item = g_list_model_get_item (G_LIST_MODEL (data->category_rows), i);
        auto row = static_cast<HierarchyCategoryRow*>(item);

        if (row->account_set && !row->account_set->exclude_from_select_all)
            row->checked = FALSE;
        hierarchy_category_row_changed (data, i, row);
        g_object_unref (item);
    }
    data->category_set_changed = TRUE;
    gtk_widget_queue_draw (GTK_WIDGET (data->categories_view));
    categories_page_enable_next (data);
    (void)button;
}
/************************************************************
 *                  Opening Balances Page                   *
 ************************************************************/

static void
delete_our_account_tree (hierarchy_data *data)
{
    if (data->our_account_tree != nullptr)
    {
        xaccAccountBeginEdit (data->our_account_tree);
        xaccAccountDestroy (data->our_account_tree);
        data->our_account_tree = nullptr;
    }
}

static Account*
clone_account (const Account* from, gnc_commodity *com)
{
    Account *ret;

    ret = xaccCloneAccount (from, gnc_get_current_book ());

    xaccAccountSetCommodity (ret, com);

    return ret;
}

struct add_group_data_struct
{
    Account *to;
    Account *parent;
    gnc_commodity *com;
};

static void
add_groups_for_each (Account *toadd, gpointer data)
{
    auto dadata{static_cast<struct add_group_data_struct*>(data)};
    Account *foundact;

    foundact = gnc_account_lookup_by_name(dadata->to, xaccAccountGetName(toadd));

    if (!foundact)
    {
        foundact = clone_account (toadd, dadata->com);

        if (dadata->to)
            gnc_account_append_child (dadata->to, foundact);
        else if (dadata->parent)
            gnc_account_append_child (dadata->parent, foundact);
        else
        {
            g_warning ("add_groups_for_each: no valid parent");
        }
    }

    {
        if (gnc_account_n_children(toadd) > 0)
        {
            struct add_group_data_struct downdata;

            downdata.to = foundact;
            downdata.parent = foundact;
            downdata.com = dadata->com;

            gnc_account_foreach_child (toadd, add_groups_for_each, &downdata);
        }
    }
}

static void
add_new_accounts_with_random_guids (Account *into, Account *from,
                                    gnc_commodity *com)
{
    struct add_group_data_struct data;
    data.to = into;
    data.parent = nullptr;
    data.com = com;

    gnc_account_foreach_child (from, add_groups_for_each, &data);
}

static Account *
hierarchy_merge_accounts (GSList *dalist, gnc_commodity *com)
{
    GSList *mark;
    Account *ret = xaccMallocAccount (gnc_get_current_book ());

    for (mark = dalist; mark; mark = mark->next)
    {
        auto xea{static_cast<GncExampleAccount*>(mark->data)};

        add_new_accounts_with_random_guids (ret, xea->root, com);
    }

    return ret;
}

static GSList *
get_selected_account_list (hierarchy_data *data)
{
    GSList *account_sets = nullptr;

    for (guint i = 0; i < g_list_model_get_n_items (G_LIST_MODEL (data->category_rows)); i++)
    {
        auto item = g_list_model_get_item (G_LIST_MODEL (data->category_rows), i);
        auto row = static_cast<HierarchyCategoryRow*>(item);

        if (row->checked && row->account_set)
            account_sets = g_slist_prepend (account_sets, row->account_set);
        g_object_unref (item);
    }
    return account_sets;
}

static void
hierarchy_clear_final_accounts (hierarchy_data *data)
{
    gtk_column_view_set_model (data->final_account_view, nullptr);
    g_clear_object (&data->final_account_selection);
    g_clear_object (&data->final_account_tree);
    g_clear_object (&data->final_account_roots);
}

static void
hierarchy_set_final_accounts (hierarchy_data *data)
{
    data->final_account_roots = hierarchy_account_roots_new
        (data->our_account_tree, gnc_get_current_book ());
    data->final_account_tree = gtk_tree_list_model_new
        (G_LIST_MODEL (g_object_ref (data->final_account_roots)), FALSE, TRUE,
         hierarchy_account_children_model, nullptr, nullptr);
    data->final_account_selection = gtk_single_selection_new
        (G_LIST_MODEL (g_object_ref (data->final_account_tree)));
    gtk_single_selection_set_autoselect (data->final_account_selection, FALSE);
    gtk_column_view_set_model (data->final_account_view,
                               GTK_SELECTION_MODEL (data->final_account_selection));
}

void
on_final_account_prepare (hierarchy_data *data)
{
    GSList *account_sets;
    gnc_commodity *commodity;

    if (!data->category_set_changed)
        return;
    data->category_set_changed = FALSE;

    gnc_suspend_gui_refresh ();
    hierarchy_clear_final_accounts (data);
    delete_our_account_tree (data);

    account_sets = get_selected_account_list (data);
    commodity = gnc_currency_edit_get_currency (GNC_CURRENCY_EDIT (data->currency_selector));
    data->our_account_tree = hierarchy_merge_accounts (account_sets, commodity);
    g_slist_free (account_sets);
    hierarchy_set_final_accounts (data);
    gnc_resume_gui_refresh ();
}
static void
hierarchy_release_models (hierarchy_data *data)
{
    g_clear_object (&data->category_selection);
    g_clear_object (&data->sorted_category_rows);
    g_clear_object (&data->category_rows);
    g_clear_object (&data->category_account_selection);
    g_clear_object (&data->category_account_tree);
    g_clear_object (&data->category_account_roots);
    g_clear_object (&data->final_account_selection);
    g_clear_object (&data->final_account_tree);
    g_clear_object (&data->final_account_roots);
    g_clear_object (&data->language_model);
    g_clear_object (&data->region_model);
    g_clear_pointer (&data->locales, g_ptr_array_unref);
    g_clear_pointer (&data->visible_locales, g_ptr_array_unref);
}

static void
hierarchy_destroy_book_options (hierarchy_data *data)
{
    auto optionwin = data->optionwin;
    auto options = data->options;
    auto notebook = data->book_options_notebook;

    data->optionwin = nullptr;
    data->options = nullptr;
    data->book_options_notebook = nullptr;
    delete optionwin;
    if (notebook)
    {
        auto parent = gtk_widget_get_parent (notebook);

        if (parent)
            gtk_box_remove (GTK_BOX (parent), notebook);
        g_object_unref (notebook);
    }
    if (options)
        delete options;
}

static void
gnc_hierarchy_destroy_cb (GtkWidget *widget, hierarchy_data *data)
{
    if (data->balance_hash)
    {
        g_hash_table_foreach (data->balance_hash, destroy_hash_helper, nullptr);
        g_hash_table_destroy (data->balance_hash);
        data->balance_hash = nullptr;
    }
    if (data->example_account_list)
    {
        gnc_free_example_account_list (data->example_account_list);
        data->example_account_list = nullptr;
    }
    if (data->optionwin || data->options)
        hierarchy_destroy_book_options (data);
    hierarchy_release_models (data);
    g_free (data->gnc_accounts_dir);
    g_free (data);
    (void)widget;
}

static void
hierarchy_destroy_window (hierarchy_data *data)
{
    gnc_save_window_size (GNC_PREFS_GROUP, data->dialog);
    gtk_window_destroy (data->dialog);
}

static void
hierarchy_cancel (hierarchy_data *data)
{
    if (!data || data->closing)
        return;

    data->closing = TRUE;
    gnc_suspend_gui_refresh ();
    hierarchy_clear_final_accounts (data);
    delete_our_account_tree (data);
    hierarchy_destroy_window (data);
    gnc_resume_gui_refresh ();
}

static void
starting_balance_helper (Account *account, hierarchy_data *data)
{
    auto balance = get_final_balance (data->balance_hash, account);

    if (gnc_reverse_balance (account))
        balance = gnc_numeric_neg (balance);
    if (!gnc_numeric_zero_p (balance) &&
        gnc_commodity_is_currency (xaccAccountGetCommodity (account)))
        gnc_account_create_opening_balance (account, balance, gnc_time (nullptr),
                                            gnc_get_current_book ());
}

static void
hierarchy_finish (hierarchy_data *data)
{
    GncHierarchyAssistantFinishedCallback when_completed;
    gnc_commodity *commodity;
    Account *root;

    if (!data || data->closing)
        return;
    data->closing = TRUE;
    when_completed = data->when_completed;
    commodity = gnc_currency_edit_get_currency
        (GNC_CURRENCY_EDIT (data->currency_selector));
    root = gnc_get_current_root_account ();
    ENTER (" ");

    if (!gnc_using_equity_type_opening_balance_account (gnc_get_current_book ()))
        gnc_set_use_equity_type_opening_balance_account (gnc_get_current_book ());
    if (data->our_account_tree)
        gnc_account_foreach_descendant (data->our_account_tree,
                                        (AccountCb)starting_balance_helper, data);

    gnc_suspend_gui_refresh ();
    if (data->new_book && (data->optionwin || data->options))
        hierarchy_destroy_book_options (data);
    account_trees_merge (gnc_get_current_root_account (), data->our_account_tree);
    hierarchy_clear_final_accounts (data);
    delete_our_account_tree (data);
    xaccAccountSetCommodity (root, commodity);
    hierarchy_destroy_window (data);
    gnc_resume_gui_refresh ();

    if (when_completed)
        (*when_completed) ();
    LEAVE (" ");
}

void
on_select_currency_prepare (hierarchy_data *data)
{
    if (!data->new_book)
        return;

    gnc_book_options_dialog_apply_helper (data->options);
    gnc_currency_edit_set_currency (GNC_CURRENCY_EDIT (data->currency_selector),
                                    gnc_default_currency ());
    gtk_label_set_text (GTK_LABEL (data->currency_selector_label),
                        _("Please choose the currency to use for new accounts."));
    gtk_widget_set_sensitive (data->currency_selector, TRUE);
}

static void
assistant_insert_book_options_page (hierarchy_data *data)
{
    GtkWidget *options;
    GtkWidget *parent;

    data->options = gnc_option_db_new ();
    gnc_option_db_book_options (data->options);
    qof_book_load_options (gnc_get_current_book (), gnc_option_db_load, data->options);
    gnc_option_db_clean (data->options);

    data->optionwin = new GncOptionsDialog (true, _("New Book Options"),
                                             DIALOG_BOOK_OPTIONS_CM_CLASS, nullptr);
    data->optionwin->build_contents (data->options, false);
    gnc_options_dialog_set_new_book_option_values (data->options);

    options = data->optionwin->get_notebook ();
    parent = gtk_widget_get_parent (options);
    data->book_options_notebook = GTK_WIDGET (g_object_ref (options));
    if (parent)
        gtk_box_remove (GTK_BOX (parent), options);
    gtk_box_append (GTK_BOX (data->book_options_page), options);
    gtk_widget_set_visible (options, TRUE);
}

static gboolean
hierarchy_page_is_complete (hierarchy_data *data, guint page)
{
    return page != data->categories_page || data->next_ok;
}

static void
hierarchy_update_navigation (hierarchy_data *data)
{
    const gboolean is_last = data->current_page + 1 == data->page_count;
    GtkWidget *default_widget = nullptr;
    auto stack_page = gtk_stack_get_page (data->stack, data->pages[data->current_page]);

    gtk_widget_set_sensitive (data->back_button, data->current_page != 0);
    gtk_widget_set_visible (data->next_button, !is_last);
    gtk_widget_set_sensitive (data->next_button,
                              hierarchy_page_is_complete (data, data->current_page));
    gtk_widget_set_visible (data->apply_button, is_last);
    gtk_widget_set_sensitive (data->apply_button, is_last);
    if (is_last)
        default_widget = data->apply_button;
    else if (gtk_widget_get_sensitive (data->next_button))
        default_widget = data->next_button;
    gtk_window_set_default_widget (data->dialog, default_widget);
    if (stack_page)
        gtk_window_set_title (data->dialog, gtk_stack_page_get_title (stack_page));
}

static void
hierarchy_show_page (hierarchy_data *data, guint page)
{
    g_return_if_fail (page < data->page_count);

    if (page == data->currency_page)
        on_select_currency_prepare (data);
    else if (page == data->categories_page)
        on_choose_account_categories_prepare (data);
    else if (page == data->final_page)
        on_final_account_prepare (data);

    data->current_page = page;
    gtk_stack_set_visible_child (data->stack, data->pages[page]);
    hierarchy_update_navigation (data);
}

static void
hierarchy_back_clicked_cb (GtkButton *button, hierarchy_data *data)
{
    if (data->current_page > 0)
        hierarchy_show_page (data, data->current_page - 1);
    (void)button;
}

static void
hierarchy_next_clicked_cb (GtkButton *button, hierarchy_data *data)
{
    if (data->current_page + 1 < data->page_count &&
        hierarchy_page_is_complete (data, data->current_page))
        hierarchy_show_page (data, data->current_page + 1);
    (void)button;
}

static void
hierarchy_apply_clicked_cb (GtkButton *button, hierarchy_data *data)
{
    hierarchy_finish (data);
    (void)button;
}

static void
hierarchy_cancel_clicked_cb (GtkButton *button, hierarchy_data *data)
{
    hierarchy_cancel (data);
    (void)button;
}

static gboolean
hierarchy_close_request_cb (GtkWindow *window, hierarchy_data *data)
{
    hierarchy_cancel (data);
    (void)window;
    return TRUE;
}

static GtkWidget *
gnc_create_hierarchy_assistant (gboolean use_defaults,
                                GncHierarchyAssistantFinishedCallback when_completed)
{
    auto data = g_new0 (hierarchy_data, 1);
    auto builder = gtk_builder_new ();
    GtkWidget *box;

    data->new_book = gnc_is_new_book ();
    data->use_defaults = use_defaults;
    data->when_completed = when_completed;
    data->initial_category_position = GTK_INVALID_LIST_POSITION;

    gnc_builder_add_from_file (builder, "assistant-hierarchy.glade",
                               "hierarchy_assistant");
    data->dialog = GTK_WINDOW (gtk_builder_get_object (builder, "hierarchy_assistant"));
    data->stack = GTK_STACK (gtk_builder_get_object (builder, "hierarchy_stack"));
    data->book_options_page = GTK_WIDGET (gtk_builder_get_object
                                           (builder, "book_options_page"));
    gtk_widget_set_name (GTK_WIDGET (data->dialog),
                         "gnc-id-assistant-account-hierarchy");

    data->pages[data->page_count++] = GTK_WIDGET (gtk_builder_get_object
                                                   (builder, "intro_page_label"));
    if (data->new_book)
    {
        data->pages[data->page_count++] = data->book_options_page;
        assistant_insert_book_options_page (data);
    }
    else
        gtk_stack_remove (data->stack, data->book_options_page);
    data->currency_page = data->page_count;
    data->pages[data->page_count++] = GTK_WIDGET (gtk_builder_get_object
                                                   (builder,
                                                    "currency_book_option_page_vbox"));
    data->categories_page = data->page_count;
    data->pages[data->page_count++] = GTK_WIDGET (gtk_builder_get_object
                                                   (builder,
                                                    "choose_account_types_vbox"));
    data->final_page = data->page_count;
    data->pages[data->page_count++] = GTK_WIDGET (gtk_builder_get_object
                                                   (builder, "final_account_vbox"));
    data->pages[data->page_count++] = GTK_WIDGET (gtk_builder_get_object
                                                   (builder, "finish_page_label"));
    data->back_button = GTK_WIDGET (gtk_builder_get_object (builder, "hierarchy_back"));
    data->next_button = GTK_WIDGET (gtk_builder_get_object (builder, "hierarchy_next"));
    data->apply_button = GTK_WIDGET (gtk_builder_get_object (builder, "hierarchy_apply"));

    data->currency_selector = gnc_currency_edit_new ();
    gnc_currency_edit_set_currency (GNC_CURRENCY_EDIT (data->currency_selector),
                                    gnc_default_currency ());
    box = GTK_WIDGET (gtk_builder_get_object (builder, "currency_chooser_hbox"));
    gtk_box_append (GTK_BOX (box), data->currency_selector);
    data->currency_selector_label = GTK_WIDGET (gtk_builder_get_object
                                                 (builder, "choose_currency_label"));

    data->language_combo = GTK_DROP_DOWN (gtk_builder_get_object (builder,
                                                                    "language_combo"));
    data->region_combo = GTK_DROP_DOWN (gtk_builder_get_object (builder,
                                                                  "region_combo"));
    data->region_label = GTK_WIDGET (gtk_builder_get_object (builder, "region_label"));
    data->categories_view = GTK_COLUMN_VIEW (gtk_builder_get_object
                                              (builder, "account_categories_view"));
    data->category_rows = g_list_store_new (hierarchy_category_row_get_type ());
    data->sorted_category_rows = gtk_sort_list_model_new
        (G_LIST_MODEL (g_object_ref (data->category_rows)), nullptr);
    hierarchy_append_category_column (data, _("Selected"), 0, FALSE);
    hierarchy_append_category_column (data, _("Account Types"), 1, TRUE);
    gtk_sort_list_model_set_sorter (data->sorted_category_rows,
                                    gtk_column_view_get_sorter (data->categories_view));
    gtk_column_view_sort_by_column (data->categories_view,
                                    data->categories_title_column,
                                    GTK_SORT_ASCENDING);
    data->category_selection = gtk_single_selection_new
        (G_LIST_MODEL (g_object_ref (data->sorted_category_rows)));
    gtk_single_selection_set_autoselect (data->category_selection, FALSE);
    gtk_column_view_set_model (data->categories_view,
                               GTK_SELECTION_MODEL (data->category_selection));
    g_signal_connect (data->category_selection, "selection-changed",
                      G_CALLBACK (hierarchy_category_selection_changed), data);
    data->category_description = GTK_TEXT_VIEW (gtk_builder_get_object
                                                 (builder, "account_types_description"));
    data->category_accounts_label = GTK_LABEL (gtk_builder_get_object
                                                (builder, "accounts_in_category_label"));
    data->category_accounts_view = GTK_COLUMN_VIEW (gtk_builder_get_object
                                                     (builder, "account_preview_view"));
    hierarchy_append_account_column (data, data->category_accounts_view, _("Account"),
                                     HIERARCHY_ACCOUNT_COLUMN_NAME, TRUE, FALSE);

    data->final_account_view = GTK_COLUMN_VIEW (gtk_builder_get_object
                                                 (builder, "final_account_view"));
    hierarchy_append_account_column (data, data->final_account_view, _("Account"),
                                     HIERARCHY_ACCOUNT_COLUMN_NAME, TRUE, TRUE);
    hierarchy_append_account_column (data, data->final_account_view, _("Code"),
                                     HIERARCHY_ACCOUNT_COLUMN_CODE, FALSE, TRUE);
    hierarchy_append_account_column (data, data->final_account_view, _("Description"),
                                     HIERARCHY_ACCOUNT_COLUMN_DESCRIPTION, TRUE, TRUE);
    hierarchy_append_account_column (data, data->final_account_view, _("Notes"),
                                     HIERARCHY_ACCOUNT_COLUMN_NOTES, TRUE, TRUE);
    hierarchy_append_account_column (data, data->final_account_view, _("Type"),
                                     HIERARCHY_ACCOUNT_COLUMN_TYPE, FALSE, FALSE);
    hierarchy_append_account_column (data, data->final_account_view, _("Placeholder"),
                                     HIERARCHY_ACCOUNT_COLUMN_PLACEHOLDER, FALSE, TRUE);
    hierarchy_append_account_column (data, data->final_account_view, _("Opening Balance"),
                                     HIERARCHY_ACCOUNT_COLUMN_BALANCE, FALSE, TRUE);
    hierarchy_append_account_column (data, data->final_account_view, _("Use Existing"),
                                     HIERARCHY_ACCOUNT_COLUMN_USE_EXISTING, FALSE, FALSE);

    data->balance_hash = g_hash_table_new (nullptr, nullptr);
    gnc_restore_window_size (GNC_PREFS_GROUP, data->dialog,
                             gnc_ui_get_main_window (nullptr));
    g_signal_connect (data->dialog, "destroy", G_CALLBACK (gnc_hierarchy_destroy_cb),
                      data);
    g_signal_connect (data->dialog, "close-request",
                      G_CALLBACK (hierarchy_close_request_cb), data);
    g_signal_connect (data->back_button, "clicked",
                      G_CALLBACK (hierarchy_back_clicked_cb), data);
    g_signal_connect (data->next_button, "clicked",
                      G_CALLBACK (hierarchy_next_clicked_cb), data);
    g_signal_connect (data->apply_button, "clicked",
                      G_CALLBACK (hierarchy_apply_clicked_cb), data);
    g_signal_connect (gtk_builder_get_object (builder, "select_all_button"), "clicked",
                      G_CALLBACK (select_all_clicked), data);
    g_signal_connect (gtk_builder_get_object (builder, "clear_all_button"), "clicked",
                      G_CALLBACK (clear_all_clicked), data);
    g_signal_connect (gtk_builder_get_object (builder, "hierarchy_cancel"), "clicked",
                      G_CALLBACK (hierarchy_cancel_clicked_cb), data);
    g_object_unref (builder);

    hierarchy_show_page (data, 0);
    gtk_window_present (data->dialog);
    return GTK_WIDGET (data->dialog);
}
GtkWidget*
gnc_ui_hierarchy_assistant(gboolean use_defaults)
{
    return gnc_create_hierarchy_assistant(use_defaults, nullptr);
}

GtkWidget*
gnc_ui_hierarchy_assistant_with_callback(gboolean use_defaults,
        GncHierarchyAssistantFinishedCallback when_finished)
{
    return gnc_create_hierarchy_assistant(use_defaults, when_finished);
}

static void
after_assistant(void)
{
    qof_book_mark_session_dirty(gnc_get_current_book());
    gnc_ui_file_access_for_save_as (gnc_ui_get_main_window (nullptr));
}

static void
gnc_ui_hierarchy_assistant_hook (void)
{
    if (gnc_prefs_get_bool(GNC_PREFS_GROUP, GNC_PREF_SHOW_ON_NEW_FILE))
    {
        gnc_ui_hierarchy_assistant_with_callback(TRUE, after_assistant);
    }
}

void
gnc_ui_hierarchy_assistant_initialize (void)
{
    gnc_hook_add_dangler(HOOK_NEW_BOOK,
                         (GFunc)gnc_ui_hierarchy_assistant_hook,
                         nullptr, nullptr);
}
