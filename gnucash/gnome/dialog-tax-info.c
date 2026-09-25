/********************************************************************\
 * dialog-tax-info.c -- tax information dialog                      *
 * Copyright (C) 2001 Gnumatic, Inc.                                *
 * Author: Dave Peticolas <dave@krondo.com>                         *
 *                                                                  *
 *                                                                  *
 * updated by  J. Alex Aycinena, July 2009                          *
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
#include <libguile.h>
#include "guile-mappings.h"
#include "gnc-guile-utils.h"

#include "Account.h"
#include "gnc-ui-util.h"
#include "dialog-utils.h"
#include "gnc-gtk-utils.h"
#include "gnc-locale-tax.h"
#include "gnc-prefs.h"
#include "gnc-tree-view-account.h"
#include "gnc-component-manager.h"
#include "gnc-session.h"
#include "qof.h"
#include "gnc-ui.h"

#define DIALOG_TAX_INFO_CM_CLASS "dialog-tax-info"
#define GNC_PREFS_GROUP    "dialogs.tax-info"
#define GNC_PREF_PANED_POS "paned-position"

enum
{
    INCOME,
    EXPENSE,
    ASSET,
    LIAB_EQ,
    N_CATEGORIES
};

static struct
{
    SCM payer_name_source;
    SCM form;
    SCM description;
    SCM help;
    SCM line_data;
    SCM last_year;
    SCM copy;

    SCM codes;

    SCM tax_entity_type;
    SCM tax_entity_desc;

    SCM tax_entity_types;
} getters;

typedef struct
{
    char *type_code;
    char *type;
    char *description;
    char *combo_box_entry;
} TaxTypeInfo;

typedef struct
{
    char *code;
    char *payer_name_source;
    char *form;
    char *description;
    char *help;
    gboolean copy;
} TXFInfo;

typedef struct
{
    GtkWidget * dialog;

    GtkWidget * entity_name_display;
    GtkWidget * entity_name_entry;
    GtkWidget * entity_type_display;
    GtkWidget * entity_type_combo;
    GtkWidget * tax_identity_edit_button;

    GtkWidget * acct_info;
    GtkWidget * income_radio;
    GtkWidget * expense_radio;
    GtkWidget * asset_radio;
    GtkWidget * liab_eq_radio;
    GtkWidget * account_treeview;
    GtkWidget * select_button;
    GtkWidget * num_acct_label;
    GtkWidget * apply_button;

    GtkWidget * txf_info;
    GtkWidget * tax_related_button;
    GtkWidget * txf_vbox;
    GtkWidget * txf_category_view;
    GListStore *txf_model;
    GtkSingleSelection *txf_selection;
    GtkWidget * txf_help_text;
    GtkWidget * help_scroll;
    GtkWidget * payer_vbox;
    GtkWidget * pns_vbox;
    GtkWidget * current_account_button;
    GtkWidget * parent_account_button;
    GtkWidget * copy_vbox;
    GtkWidget * copy_spin_button;

    GList * entity_type_infos;
    GList * income_txf_infos;
    GList * expense_txf_infos;
    GList * asset_txf_infos;
    GList * liab_eq_txf_infos;

    const gchar * tax_name;
    const gchar * tax_type;
    const gchar * tax_type_combo_text;
    const gchar * default_tax_type;

    QofBook *this_book;

    gboolean changed;
    gboolean tax_type_changed;

    GNCAccountType account_type;
} TaxInfoDialog;
typedef struct
{
    TaxInfoDialog *dialog;
    GtkWindow *window;
    GtkEntry *name_entry;
    GtkDropDown *type_dropdown;
} TaxIdentityEdit;

static GObject *
txf_row_new (TXFInfo *info)
{
    GObject *row = g_object_new (G_TYPE_OBJECT, NULL);
    g_object_set_data_full (row, "txf-form", g_strdup (info->form), g_free);
    g_object_set_data_full (row, "txf-description", g_strdup (info->description), g_free);
    g_object_set_data (row, "txf-info", info);
    return row;
}

static void
txf_factory_setup (GtkSignalListItemFactory *factory, GtkListItem *item,
                   gpointer user_data)
{
    (void)factory;
    (void)user_data;
    gtk_list_item_set_child (item, gtk_label_new (NULL));
}

static void
txf_factory_bind (GtkSignalListItemFactory *factory, GtkListItem *item,
                  gpointer user_data)
{
    GObject *row = gtk_list_item_get_item (item);
    (void)factory;
    gtk_label_set_text (GTK_LABEL (gtk_list_item_get_child (item)),
                        g_object_get_data (row, user_data));
}

static GtkColumnViewColumn *
txf_column_new (const char *title, const char *key)
{
    GtkListItemFactory *factory = gtk_signal_list_item_factory_new ();
    GtkColumnViewColumn *column;
    g_signal_connect (factory, "setup", G_CALLBACK (txf_factory_setup), NULL);
    g_signal_connect_data (factory, "bind", G_CALLBACK (txf_factory_bind),
                           (gpointer)key, NULL, 0);
    column = gtk_column_view_column_new (title, factory);
    gtk_column_view_column_set_expand (column, TRUE);
    return column;
}

static TXFInfo *
txf_selected_info (TaxInfoDialog *ti_dialog)
{
    guint position = gtk_single_selection_get_selected (ti_dialog->txf_selection);
    GObject *row;
    TXFInfo *info;

    if (position == GTK_INVALID_LIST_POSITION)
        return NULL;
    row = g_list_model_get_item (G_LIST_MODEL (ti_dialog->txf_selection), position);
    info = g_object_get_data (row, "txf-info");
    g_object_unref (row);
    return info;
}

static void
initialize_getters (void)
{
    gnc_locale_tax_init();

    getters.payer_name_source = scm_c_eval_string ("gnc:txf-get-payer-name-source");
    getters.form              = scm_c_eval_string ("gnc:txf-get-form");
    getters.description       = scm_c_eval_string ("gnc:txf-get-description");
    getters.help              = scm_c_eval_string ("gnc:txf-get-help");
    getters.line_data         = scm_c_eval_string ("gnc:txf-get-line-data");
    getters.last_year         = scm_c_eval_string ("gnc:txf-get-last-year");
    getters.copy              = scm_c_eval_string ("gnc:txf-get-multiple");

    getters.codes             = scm_c_eval_string ("gnc:txf-get-codes");

    getters.tax_entity_type   = scm_c_eval_string ("gnc:txf-get-tax-entity-type");
    getters.tax_entity_desc   = scm_c_eval_string
                                ("gnc:txf-get-tax-entity-type-description");

    getters.tax_entity_types = scm_c_eval_string
                               ("gnc:txf-get-tax-entity-type-codes");
}

static void
destroy_tax_type_info (gpointer data)
{
    TaxTypeInfo *tax_type = data;

    g_free (tax_type->type_code);
    tax_type->type_code = NULL;

    g_free (tax_type->type);
    tax_type->type = NULL;

    g_free (tax_type->description);
    tax_type->description = NULL;

    g_free (tax_type->combo_box_entry);
    tax_type->combo_box_entry = NULL;

    g_free (tax_type);
}

static inline void
destroy_tax_type_infos (GList *types)
{
    g_list_free_full (types, destroy_tax_type_info);
}

static void
destroy_txf_info (gpointer data)
{
    TXFInfo *txf_info = data;

    g_free (txf_info->code);
    txf_info->code = NULL;

    g_free (txf_info->payer_name_source);
    txf_info->payer_name_source = NULL;

    g_free (txf_info->form);
    txf_info->form = NULL;

    g_free (txf_info->description);
    txf_info->description = NULL;

    g_free (txf_info->help);
    txf_info->help = NULL;

    g_free (txf_info);
}

static inline void
destroy_txf_infos (GList *infos)
{
    g_list_free_full (infos, destroy_txf_info);
}

static void
gnc_tax_info_set_changed (TaxInfoDialog *ti_dialog, gboolean changed)
{
    ti_dialog->changed = changed;
    gtk_widget_set_sensitive (ti_dialog->apply_button, changed);
}

static GList *
load_txf_info (gint acct_category, TaxInfoDialog *ti_dialog)
{
    GList *infos = NULL;
    SCM tax_entity_type;
    SCM category;
    SCM codes;

    if (ti_dialog->tax_type == NULL ||
            (g_strcmp0 (ti_dialog->tax_type, "") == 0))
    {
        destroy_txf_infos (infos);
        return NULL;
    }
    else
    {
        tax_entity_type = scm_from_utf8_string (ti_dialog->tax_type);
    }

    switch (acct_category)
    {
    case INCOME:
        category = scm_c_eval_string ("txf-income-categories");
        break;
    case EXPENSE:
        category = scm_c_eval_string ("txf-expense-categories");
        break;
    case ASSET:
        category = scm_c_eval_string ("txf-asset-categories");
        break;
    case LIAB_EQ:
        category = scm_c_eval_string ("txf-liab-eq-categories");
        break;
    default:
        destroy_txf_infos (infos);
        return NULL;
    }

    if (category == SCM_UNDEFINED)
    {
        destroy_txf_infos (infos);
        return NULL;
    }

    codes = scm_call_2 (getters.codes, category, tax_entity_type);
    if (!scm_is_list (codes))
    {
        destroy_txf_infos (infos);
        return NULL;
    }

    while (!scm_is_null (codes))
    {
        TXFInfo *txf_info;
        SCM code_scm;
        const gchar *last_yr = _("Last Valid Year: ");
        const gchar *form_line = _("Form Line Data: ");
        /* Translators: Tax Code */
        const gchar *code_line_word = _("Code");
        const gchar *code_line_colon = ": ";
        const gchar *prefix = "N";
        gchar *str = NULL;
        gchar *num_code = NULL;
        gchar *form_line_data = NULL;
        gchar *help_text = NULL;
        SCM scm;
        gint year;
        gboolean cpy;

        code_scm  = SCM_CAR (codes);
        codes     = SCM_CDR (codes);

        scm = scm_call_3 (getters.payer_name_source, category, code_scm,
                          tax_entity_type);
        if (scm_is_symbol(scm))
            str = gnc_scm_symbol_to_locale_string (scm);
        else
            str = g_strdup ("");
        if (g_strcmp0 (str, "not-impl") == 0)
        {
            g_free (str);
            continue;
        }

        txf_info = g_new0 (TXFInfo, 1);

        if (g_strcmp0 (str, "none") == 0)
            txf_info->payer_name_source = NULL;
        else
            txf_info->payer_name_source = g_strdup (str);
        g_free (str);

        if (scm_is_symbol(code_scm))
            str = gnc_scm_symbol_to_locale_string (code_scm);
        else
            str = g_strdup ("");
        txf_info->code = g_strdup (str);
        if (g_str_has_prefix (str, prefix))
        {
            const gchar *num_code_tmp;
            num_code_tmp = g_strdup (str);
            num_code_tmp++; /* to lose the leading N */
            num_code = g_strdup (num_code_tmp);
            num_code_tmp--;
            g_free ((gpointer *) num_code_tmp);
        }
        else
            num_code = g_strdup (str);
        g_free (str);

        scm = scm_call_3 (getters.form, category, code_scm, tax_entity_type);
        if (scm_is_string(scm))
            txf_info->form = gnc_scm_to_utf8_string(scm);
        else
            txf_info->form = g_strdup ("");

        scm = scm_call_3 (getters.description, category, code_scm, tax_entity_type);
        if (scm_is_string(scm))
            txf_info->description = gnc_scm_to_utf8_string(scm);
        else
            txf_info->description = g_strdup ("");

        scm = scm_call_2 (getters.help, category, code_scm);
        if (scm_is_string(scm))
            help_text = gnc_scm_to_utf8_string(scm);
        else
            help_text = g_strdup ("");

        scm = scm_call_3 (getters.last_year, category, code_scm, tax_entity_type);
        year = scm_is_bool (scm) ? 0 : scm_to_int(scm);
        scm = scm_call_3 (getters.line_data, category, code_scm, tax_entity_type);
        if (scm_is_list (scm))
        {
            const gchar *now = _("now");
            gchar *until;

            until = (gchar *) now;
            form_line_data = g_strconcat ("\n", "\n", form_line, NULL);
            while (!scm_is_null (scm))
            {
                SCM year_scm;
                gint line_year;
                gchar *line = NULL;
                gchar *temp = NULL;
                gchar *temp2 = NULL;

                year_scm  = SCM_CAR (scm);
                scm       = SCM_CDR (scm);

                line_year = scm_is_bool (SCM_CAR (year_scm)) ? 0 :
                            scm_to_int (SCM_CAR (year_scm));
                if (scm_is_string((SCM_CAR (SCM_CDR (year_scm)))))
                    line = gnc_scm_to_utf8_string((SCM_CAR (SCM_CDR
                                                     (year_scm))));
                else
                    line = g_strdup ("");
                temp2 = g_strdup_printf ("%d", line_year);
                temp = g_strconcat (form_line_data, "\n", temp2, " - ",
                                    ((year != 0) && (until == now))
                                        ? g_strdup_printf("%d", year)
                                        : until,
                                    "   ", line, NULL);
                if (until != now)
                    g_free (until);
                until = g_strdup_printf ("%d", (line_year - 1));
                g_free (form_line_data);
                form_line_data = g_strdup (temp);
                g_free (line);
                g_free (temp);
                g_free (temp2);
            }
            if (g_strcmp0 (until, now) != 0)
                g_free (until);
        }
        if (year != 0)
        {
            gchar *temp = g_strdup_printf("%d", year);
            if (form_line_data != NULL)
                txf_info->help = g_strconcat (last_yr, temp, "\n", "\n",
                                              help_text, "\n", "\n",
                                              code_line_word,
                                              code_line_colon, num_code,
                                              form_line_data, NULL);
            else
                txf_info->help = g_strconcat (last_yr, temp, "\n", "\n",
                                              help_text, "\n", "\n",
                                              code_line_word,
                                              code_line_colon, num_code, NULL);
            g_free (temp);
        }
        else
        {
            if (form_line_data != NULL)
                txf_info->help = g_strconcat (help_text, "\n", "\n",
                                              code_line_word,
                                              code_line_colon, num_code,
                                              form_line_data, NULL);
            else
                txf_info->help = g_strconcat (help_text, "\n", "\n",
                                              code_line_word,
                                              code_line_colon, num_code, NULL);
        }

        g_free (num_code);
        g_free (help_text);
        g_free (form_line_data);

        scm = scm_call_3 (getters.copy, category, code_scm, tax_entity_type);
        cpy = scm_is_bool (scm) ? (scm_is_false (scm) ? FALSE : TRUE) : FALSE;
        txf_info->copy = cpy;

        infos = g_list_prepend (infos, txf_info);
    }
    return g_list_reverse (infos);
}

static GList *
tax_infos (TaxInfoDialog *ti_dialog)
{
    return
        (ti_dialog->account_type == ACCT_TYPE_INCOME)
        ? ti_dialog->income_txf_infos :
        ((ti_dialog->account_type == ACCT_TYPE_EXPENSE)
         ? ti_dialog->expense_txf_infos :
         (((ti_dialog->account_type == ACCT_TYPE_ASSET)
           ? ti_dialog->asset_txf_infos :
           ti_dialog->liab_eq_txf_infos)));
}

static void
load_tax_entity_type_list (TaxInfoDialog *ti_dialog)
{
    GList *types = NULL;
    SCM tax_types;

    ti_dialog->tax_type_combo_text = NULL;
    tax_types = scm_call_0 (getters.tax_entity_types);
    if (!scm_is_list (tax_types))
    {
        destroy_tax_type_infos (types);
        return;
    }

    while (!scm_is_null (tax_types))
    {
        TaxTypeInfo *tax_type_info;
        SCM type_scm;
        SCM scm;

        type_scm  = SCM_CAR (tax_types);
        tax_types = SCM_CDR (tax_types);

        ti_dialog->default_tax_type = NULL;

        tax_type_info = g_new0 (TaxTypeInfo, 1);

        if (scm_is_symbol(type_scm))
            tax_type_info->type_code = gnc_scm_symbol_to_locale_string (type_scm);
        else
            tax_type_info->type_code = g_strdup ("");

        scm = scm_call_1 (getters.tax_entity_type, type_scm);
        if (scm_is_string(scm))
            tax_type_info->type = gnc_scm_to_utf8_string(scm);
        else
            tax_type_info->type = g_strdup ("");

        scm = scm_call_1 (getters.tax_entity_desc, type_scm);
        if (scm_is_string(scm))
            tax_type_info->description = gnc_scm_to_utf8_string(scm);
        else
            tax_type_info->description = g_strdup ("");

        tax_type_info->combo_box_entry = g_strconcat(tax_type_info->type,
                                         " - ",
                                         tax_type_info->description, NULL);
        /* save combo text for current tax type code */
        if (g_strcmp0 (ti_dialog->tax_type, tax_type_info->type_code) == 0)
            ti_dialog->tax_type_combo_text = tax_type_info->combo_box_entry;
        /* the last will be default */
        ti_dialog->default_tax_type = tax_type_info->combo_box_entry;

        types = g_list_prepend (types, tax_type_info);
    }
    ti_dialog->entity_type_infos = g_list_reverse (types);
}

static void
load_category_list (TaxInfoDialog *ti_dialog)
{
    GList *codes;

    g_list_store_remove_all (ti_dialog->txf_model);
    for (codes = tax_infos (ti_dialog); codes; codes = codes->next)
    {
        GObject *row = txf_row_new (codes->data);
        g_list_store_append (ti_dialog->txf_model, row);
        g_object_unref (row);
    }
}
static void
clear_gui (TaxInfoDialog *ti_dialog)
{
    gtk_check_button_set_active
        (GTK_CHECK_BUTTON (ti_dialog->tax_related_button), FALSE);
    gtk_selection_model_unselect_all (GTK_SELECTION_MODEL (ti_dialog->txf_selection));
    gtk_check_button_set_active
        (GTK_CHECK_BUTTON (ti_dialog->current_account_button), TRUE);
    gtk_spin_button_set_value
        (GTK_SPIN_BUTTON (ti_dialog->copy_spin_button), 1);
}
static gboolean
gnc_tax_info_dialog_account_filter_func (Account *account,
        gpointer data)
{
    TaxInfoDialog *dialog = data;
    GNCAccountType fund_acct_type = xaccAccountTypeGetFundamental (xaccAccountGetType (account));
    gboolean included = FALSE;

    if ((dialog->account_type == ACCT_TYPE_INCOME) ||
        (dialog->account_type == ACCT_TYPE_EXPENSE))
        included = (xaccAccountGetType (account) == dialog->account_type);
    else if (dialog->account_type == ACCT_TYPE_ASSET)
        included = (ACCT_TYPE_ASSET == fund_acct_type);
    else if (dialog->account_type == ACCT_TYPE_LIABILITY)
        included = ((ACCT_TYPE_LIABILITY == fund_acct_type) ||
                    (ACCT_TYPE_EQUITY == fund_acct_type));
    else
        included = FALSE;
    return included;
}

static TXFInfo *
txf_infos_find_code (GList *infos, const char *code)
{
    for (; infos; infos = infos->next)
    {
        TXFInfo *info = infos->data;

        if (g_strcmp0 (code, info->code) == 0)
            return info;
    }

    return NULL;
}

static void
account_to_gui (TaxInfoDialog *ti_dialog, Account *account)
{
    gboolean tax_related;
    const char *str;
    TXFInfo *info;
    GList *infos;
    gint index = 0;

    if (!account)
    {
        clear_gui (ti_dialog);
        return;
    }
    tax_related = xaccAccountGetTaxRelated (account);
    gtk_check_button_set_active
        (GTK_CHECK_BUTTON (ti_dialog->tax_related_button), tax_related);
    infos = tax_infos (ti_dialog);
    str = xaccAccountGetTaxUSCode (account);
    info = txf_infos_find_code (infos, str);
    if (info)
        index = g_list_index (infos, info);
    if (index < 0)
        index = 0;
    gtk_selection_model_select_item (GTK_SELECTION_MODEL (ti_dialog->txf_selection),
                                     index, TRUE);
    gtk_column_view_scroll_to (GTK_COLUMN_VIEW (ti_dialog->txf_category_view), index, NULL,
                               GTK_LIST_SCROLL_FOCUS, NULL);

    str = xaccAccountGetTaxUSPayerNameSource (account);
    gtk_check_button_set_active (GTK_CHECK_BUTTON
        (g_strcmp0 (str, "parent") == 0 ? ti_dialog->parent_account_button :
                                           ti_dialog->current_account_button), TRUE);
    gtk_spin_button_set_value (GTK_SPIN_BUTTON (ti_dialog->copy_spin_button),
                               (gdouble) xaccAccountGetTaxUSCopyNumber (account));
}
static void
gui_to_accounts (TaxInfoDialog *ti_dialog)
{
    gboolean tax_related;
    const char *code;
    const char *pns;
    GList *accounts;
    GList *node;
    TXFInfo *info;
    gint64 copy_number;

    tax_related = gtk_check_button_get_active
        (GTK_CHECK_BUTTON (ti_dialog->tax_related_button));
    info = txf_selected_info (ti_dialog);
    if (!info)
        return;
    code = tax_related ? info->code : NULL;
    if (tax_related && info->payer_name_source)
    {
        gboolean current = gtk_check_button_get_active
            (GTK_CHECK_BUTTON (ti_dialog->current_account_button));
        pns = current ? "current" : "parent";
    }
    else
        pns = NULL;
    copy_number = tax_related && info->copy ? gtk_spin_button_get_value_as_int
        (GTK_SPIN_BUTTON (ti_dialog->copy_spin_button)) : 0;

    accounts = gnc_tree_view_account_get_selected_accounts
        (GNC_TREE_VIEW_ACCOUNT (ti_dialog->account_treeview));
    for (node = accounts; node; node = node->next)
    {
        Account *account = node->data;
        xaccAccountBeginEdit (account);
        xaccAccountSetTaxRelated (account, tax_related);
        xaccAccountSetTaxUSPayerNameSource (account, pns);
        xaccAccountSetTaxUSCopyNumber (account, copy_number);
        xaccAccountSetTaxUSCode (account, code);
        xaccAccountCommitEdit (account);
    }
    g_list_free (accounts);
}
static void
identity_edit_destroyed (GtkWidget *widget, TaxIdentityEdit *edit)
{
    (void)widget;
    if (edit->dialog)
    {
        edit->dialog->entity_name_entry = NULL;
        edit->dialog->entity_type_combo = NULL;
    }
    g_free (edit);
}

static void
window_destroy_cb (GtkWidget *object, gpointer data)
{
    TaxInfoDialog *ti_dialog = data;
    (void)object;
    gnc_unregister_gui_component_by_data (DIALOG_TAX_INFO_CM_CLASS, ti_dialog);
    g_clear_object (&ti_dialog->txf_selection);
    g_clear_object (&ti_dialog->txf_model);
    destroy_tax_type_infos (ti_dialog->entity_type_infos);
    destroy_txf_infos (ti_dialog->income_txf_infos);
    destroy_txf_infos (ti_dialog->expense_txf_infos);
    destroy_txf_infos (ti_dialog->asset_txf_infos);
    destroy_txf_infos (ti_dialog->liab_eq_txf_infos);
    g_free (ti_dialog);
}

static void cursor_changed_cb (GtkWidget *widget, gpointer data)
{
    TaxInfoDialog *ti_dialog = data;
    GncTreeViewAccount *account_tree;
    Account *account;
    gint num_children;

    account_tree = GNC_TREE_VIEW_ACCOUNT (ti_dialog->account_treeview);
    account = gnc_tree_view_account_get_cursor_account (account_tree);
    if (!account)
    {
        gtk_widget_set_sensitive(ti_dialog->select_button, FALSE);
        return;
    }

    num_children = gnc_tree_view_account_count_children(account_tree, account);
    gtk_widget_set_sensitive(ti_dialog->select_button, num_children > 0);
}

static void
select_subaccounts_clicked (GtkWidget *widget, gpointer data)
{
    TaxInfoDialog *ti_dialog = data;
    GncTreeViewAccount *account_tree;
    Account *account;

    account_tree = GNC_TREE_VIEW_ACCOUNT (ti_dialog->account_treeview);
    account = gnc_tree_view_account_get_cursor_account (account_tree);
    if (!account)
        return;

    gnc_tree_view_account_select_subaccounts (account_tree, account);

    gtk_widget_grab_focus (ti_dialog->account_treeview);
}

static void
gnc_tax_info_dialog_response (TaxInfoDialog *ti_dialog, gint response)
{
    if (ti_dialog->changed && (response == GTK_RESPONSE_APPLY || response == GTK_RESPONSE_OK))
        gui_to_accounts (ti_dialog);
    if (response != GTK_RESPONSE_APPLY)
        gnc_close_gui_component_by_data (DIALOG_TAX_INFO_CM_CLASS, ti_dialog);
}

static void
tax_info_apply_clicked (GtkButton *button, TaxInfoDialog *ti_dialog)
{
    (void)button;
    gnc_tax_info_dialog_response (ti_dialog, GTK_RESPONSE_APPLY);
}

static void
tax_info_ok_clicked (GtkButton *button, TaxInfoDialog *ti_dialog)
{
    (void)button;
    gnc_tax_info_dialog_response (ti_dialog, GTK_RESPONSE_OK);
}

static void
tax_info_cancel_clicked (GtkButton *button, TaxInfoDialog *ti_dialog)
{
    (void)button;
    gnc_tax_info_dialog_response (ti_dialog, GTK_RESPONSE_CANCEL);
}

static gboolean
tax_info_close_request (GtkWindow *window, TaxInfoDialog *ti_dialog)
{
    (void)window;
    gnc_tax_info_dialog_response (ti_dialog, GTK_RESPONSE_CANCEL);
    return TRUE;
}
static void
tax_info_show_acct_type_accounts (TaxInfoDialog *ti_dialog)
{
    GncTreeViewAccount *tree;
    AccountViewInfo info;
    GNCAccountType type;

    tree = GNC_TREE_VIEW_ACCOUNT (ti_dialog->account_treeview);

    gnc_tree_view_account_get_view_info (tree, &info);

    for (type = 0; type < NUM_ACCOUNT_TYPES; type++) /* from Account.h */
    {
        if (ti_dialog->account_type == ACCT_TYPE_EXPENSE)
            info.include_type[type] = (type == ACCT_TYPE_EXPENSE);
        else if (ti_dialog->account_type == ACCT_TYPE_INCOME)
            info.include_type[type] = (type == ACCT_TYPE_INCOME);
        else if (ti_dialog->account_type == ACCT_TYPE_ASSET)
            info.include_type[type] = ((type == ACCT_TYPE_BANK)      ||
                                       (type == ACCT_TYPE_CASH)      ||
                                       (type == ACCT_TYPE_ASSET)     ||
                                       (type == ACCT_TYPE_STOCK)     ||
                                       (type == ACCT_TYPE_MUTUAL)    ||
                                       (type == ACCT_TYPE_RECEIVABLE));
        else if (ti_dialog->account_type == ACCT_TYPE_LIABILITY)
            info.include_type[type] = ((type == ACCT_TYPE_CREDIT)    ||
                                       (type == ACCT_TYPE_LIABILITY) ||
                                       (type == ACCT_TYPE_EQUITY)    ||
                                       (type == ACCT_TYPE_PAYABLE));
        else
            info.include_type[type] = FALSE;
    }

    info.show_hidden = TRUE;
    gnc_tree_view_account_set_view_info (tree, &info);

    load_category_list (ti_dialog);
    cursor_changed_cb(GTK_WIDGET(tree), ti_dialog);
}

static int
gnc_tax_info_update_accounts (TaxInfoDialog *ti_dialog)
{
    GncTreeViewAccount *tree;
    GtkWidget *label;
    GList *accounts;
    int num_accounts;
    char *string;

    tree = GNC_TREE_VIEW_ACCOUNT (ti_dialog->account_treeview);
    accounts = gnc_tree_view_account_get_selected_accounts (tree);
    num_accounts = g_list_length (accounts);
    g_list_free (accounts);

    label = ti_dialog->num_acct_label;
    string = g_strdup_printf (_("Accounts Selected: %d"), num_accounts);
    gtk_label_set_text (GTK_LABEL (label), string);
    g_free (string);

    gtk_widget_set_sensitive (ti_dialog->txf_info, num_accounts > 0);

    return num_accounts;
}

static void
gnc_tax_info_set_acct (TaxInfoDialog *ti_dialog, Account *account)
{
    if (account == NULL)
        return;

    ti_dialog->account_type = xaccAccountTypeGetFundamental (xaccAccountGetType (account));

    if (ti_dialog->account_type == ACCT_TYPE_INCOME)
        gtk_check_button_set_active (GTK_CHECK_BUTTON(ti_dialog->income_radio), TRUE);
    else if (ti_dialog->account_type == ACCT_TYPE_EXPENSE)
        gtk_check_button_set_active (GTK_CHECK_BUTTON(ti_dialog->expense_radio), TRUE);
    else if (ti_dialog->account_type == ACCT_TYPE_ASSET)
        gtk_check_button_set_active (GTK_CHECK_BUTTON(ti_dialog->asset_radio), TRUE);
    else if ((ti_dialog->account_type == ACCT_TYPE_LIABILITY) ||
             (ti_dialog->account_type == ACCT_TYPE_EQUITY))
        gtk_check_button_set_active (GTK_CHECK_BUTTON(ti_dialog->liab_eq_radio), TRUE);
    else
        return;

    gnc_tree_view_account_set_selected_account (GNC_TREE_VIEW_ACCOUNT(ti_dialog->account_treeview),
                                                account);
}

static void
gnc_tax_info_acct_type_cb (GtkWidget *w, gpointer data)
{
    TaxInfoDialog *ti_dialog = data;
    const gchar *button_name;

    if (gtk_check_button_get_active (GTK_CHECK_BUTTON (w)))
    {
        button_name = gtk_buildable_get_buildable_id(GTK_BUILDABLE(w));
        if (g_strcmp0 (button_name, "income_radio") == 0)
            ti_dialog->account_type = ACCT_TYPE_INCOME;
        else if (g_strcmp0 (button_name, "expense_radio") == 0)
            ti_dialog->account_type = ACCT_TYPE_EXPENSE;
        else if (g_strcmp0 (button_name, "asset_radio") == 0)
            ti_dialog->account_type = ACCT_TYPE_ASSET;
        else if (g_strcmp0 (button_name, "liab_eq_radio") == 0)
            ti_dialog->account_type = ACCT_TYPE_LIABILITY;
        else
            return;
        tax_info_show_acct_type_accounts (ti_dialog);
        gnc_tree_view_account_refilter
        (GNC_TREE_VIEW_ACCOUNT (ti_dialog->account_treeview));
        gnc_tax_info_update_accounts (ti_dialog);
        clear_gui (ti_dialog);
    }
    else
        return;
}

static void
gnc_tax_info_account_changed_cb (GtkSelectionModel *selection, guint position,
                                 guint n_items, gpointer data)
{
    TaxInfoDialog *ti_dialog = data;
    GncTreeViewAccount *view;
    GList *accounts;
    int num_accounts;


    num_accounts = gnc_tax_info_update_accounts (ti_dialog);
    cursor_changed_cb (GTK_WIDGET (ti_dialog->account_treeview), ti_dialog);
    switch (num_accounts)
    {
    case 0:
        clear_gui (ti_dialog);
        gnc_tax_info_set_changed (ti_dialog, FALSE);
        return;

    case 1:
        /* Get the account. This view is set for multiple selection, so we
           can only get a list of accounts. */
        view = GNC_TREE_VIEW_ACCOUNT(ti_dialog->account_treeview);
        accounts = gnc_tree_view_account_get_selected_accounts (view);
        if (accounts == NULL)
        {
            clear_gui (ti_dialog);
            gnc_tax_info_set_changed (ti_dialog, FALSE);
            return;
        }
        account_to_gui (ti_dialog, accounts->data);
        g_list_free (accounts);

        gnc_tax_info_set_changed (ti_dialog, FALSE);
        break;

    default:
        gnc_tax_info_set_changed (ti_dialog, TRUE);
        return;
    }
}

static void
txf_code_select_row_cb (GtkSelectionModel *selection, guint position,
                        guint n_items, gpointer user_data)
{
    TaxInfoDialog *ti_dialog = user_data;
    TXFInfo *txf_info;
    GtkAdjustment *adj;
    GtkWidget *vbox;
    GtkTextBuffer *tb;
    const char *text;

    (void)selection;
    (void)position;
    (void)n_items;
    txf_info = txf_selected_info (ti_dialog);
    if (!txf_info)
        return;
    tb = gtk_text_view_get_buffer (GTK_TEXT_VIEW (ti_dialog->txf_help_text));
    text = txf_info->help ? txf_info->help : "";
    gtk_text_buffer_set_text (tb, text, -1);
    adj = gtk_scrolled_window_get_vadjustment (GTK_SCROLLED_WINDOW (ti_dialog->help_scroll));
    gtk_adjustment_set_value (adj, 0.0);

    vbox = ti_dialog->payer_vbox;
    if (txf_info->payer_name_source)
    {
        gboolean current = g_strcmp0 ("current", txf_info->payer_name_source) == 0;
        gtk_widget_set_sensitive (vbox, TRUE);
        gtk_check_button_set_active (GTK_CHECK_BUTTON
            (current ? ti_dialog->current_account_button : ti_dialog->parent_account_button), TRUE);
    }
    else
    {
        gtk_widget_set_sensitive (vbox, FALSE);
        gtk_check_button_set_active (GTK_CHECK_BUTTON (ti_dialog->current_account_button), TRUE);
    }
    gtk_widget_set_sensitive (ti_dialog->copy_vbox, txf_info->copy);
    gnc_tax_info_set_changed (ti_dialog, TRUE);
}
static void
set_focus_sensitivity (TaxInfoDialog *ti_dialog)
{
    if ((ti_dialog->tax_type == NULL) ||
            (g_strcmp0 (ti_dialog->tax_type, "Other") == 0) ||
            (g_strcmp0 (ti_dialog->tax_type, "") == 0))
    {
        gtk_widget_grab_focus (ti_dialog->tax_identity_edit_button);
        gtk_widget_set_sensitive (ti_dialog->acct_info, FALSE);
        gtk_widget_set_sensitive (ti_dialog->txf_info, FALSE);
        gtk_widget_set_visible (GTK_WIDGET(ti_dialog->txf_help_text), FALSE); /* textview doesn't go insensitive!? */
    }
    else if (ti_dialog->tax_type_changed)
    {
        gtk_widget_set_sensitive (ti_dialog->acct_info, TRUE);
        gtk_widget_set_sensitive (ti_dialog->txf_info, TRUE);
        gtk_widget_grab_focus (ti_dialog->account_treeview);
    }
    else
    {
        gtk_widget_set_sensitive (ti_dialog->acct_info, TRUE);
        gtk_widget_grab_focus (ti_dialog->account_treeview);
    }
    if (ti_dialog->asset_txf_infos == NULL)
        gtk_widget_set_visible (GTK_WIDGET(ti_dialog->asset_radio), FALSE);
    else
        gtk_widget_set_visible (GTK_WIDGET(ti_dialog->asset_radio), TRUE);

    if (ti_dialog->liab_eq_txf_infos == NULL)
        gtk_widget_set_visible (GTK_WIDGET(ti_dialog->liab_eq_radio), FALSE);
    else
        gtk_widget_set_visible (GTK_WIDGET(ti_dialog->liab_eq_radio), TRUE);
}

static void
identity_edit_apply (TaxIdentityEdit *edit)
{
    TaxInfoDialog *ti_dialog = edit->dialog;
    const gchar *entry_name = gtk_editable_get_text (GTK_EDITABLE (edit->name_entry));
    const gchar *entry_type = NULL;
    gboolean tax_name_changed = FALSE;
    guint active_item = gtk_drop_down_get_selected (edit->type_dropdown);
    TaxTypeInfo *selected_type = active_item == GTK_INVALID_LIST_POSITION ? NULL :
        g_list_nth_data (ti_dialog->entity_type_infos, active_item);

    if (selected_type)
    {
        entry_type = selected_type->type_code;
        if (g_strcmp0 (ti_dialog->tax_type, entry_type) != 0)
        {
            if (!((g_strcmp0 (ti_dialog->tax_type, "") == 0) &&
                  (g_strcmp0 (entry_type, "Other") == 0)))
            {
                ti_dialog->tax_type_changed = TRUE;
                ti_dialog->tax_type = entry_type;
                gtk_label_set_text (GTK_LABEL (ti_dialog->entity_type_display),
                                    selected_type->combo_box_entry);
                destroy_txf_infos (ti_dialog->income_txf_infos);
                destroy_txf_infos (ti_dialog->expense_txf_infos);
                destroy_txf_infos (ti_dialog->asset_txf_infos);
                destroy_txf_infos (ti_dialog->liab_eq_txf_infos);
                ti_dialog->income_txf_infos = load_txf_info (INCOME, ti_dialog);
                ti_dialog->expense_txf_infos = load_txf_info (EXPENSE, ti_dialog);
                ti_dialog->asset_txf_infos = load_txf_info (ASSET, ti_dialog);
                ti_dialog->liab_eq_txf_infos = load_txf_info (LIAB_EQ, ti_dialog);
                gtk_check_button_set_active (GTK_CHECK_BUTTON (ti_dialog->expense_radio), TRUE);
                tax_info_show_acct_type_accounts (ti_dialog);
                gnc_tree_view_account_refilter (GNC_TREE_VIEW_ACCOUNT (ti_dialog->account_treeview));
                gnc_tax_info_update_accounts (ti_dialog);
                clear_gui (ti_dialog);
            }
            else
                ti_dialog->tax_type_changed = FALSE;
        }
        else
            ti_dialog->tax_type_changed = FALSE;
    }

    if (g_strcmp0 (ti_dialog->tax_name, entry_name) != 0 &&
        !((ti_dialog->tax_name == NULL) && (g_strcmp0 (entry_name, "") == 0)))
    {
        tax_name_changed = TRUE;
        ti_dialog->tax_name = g_strdup (entry_name);
        gtk_label_set_text (GTK_LABEL (ti_dialog->entity_name_display), entry_name);
    }
    if (tax_name_changed || ti_dialog->tax_type_changed)
        gnc_set_current_book_tax_name_type (tax_name_changed, entry_name,
                                            ti_dialog->tax_type_changed, entry_type);
    set_focus_sensitivity (ti_dialog);
    ti_dialog->tax_type_changed = FALSE;
}

static void
identity_edit_apply_clicked (GtkButton *button, TaxIdentityEdit *edit)
{
    (void)button;
    identity_edit_apply (edit);
    gtk_window_destroy (edit->window);
}

static void
identity_edit_cancel_clicked (GtkButton *button, TaxIdentityEdit *edit)
{
    (void)button;
    gtk_window_destroy (edit->window);
}

static gboolean
identity_edit_close_request (GtkWindow *window, TaxIdentityEdit *edit)
{
    (void)edit;
    gtk_window_destroy (window);
    return TRUE;
}

static void
identity_edit_clicked_cb (GtkButton *button, gpointer user_data)
{
    TaxInfoDialog *ti_dialog = user_data;
    TaxIdentityEdit *edit;
    GtkWidget *content;
    GtkWidget *grid;
    GtkWidget *label;
    GtkWidget *buttons;
    GtkWidget *cancel;
    GtkWidget *apply;
    GtkStringList *types;
    GPtrArray *entries;
    GList *node;
    guint current_item = GTK_INVALID_LIST_POSITION;
    guint item = 0;

    (void)button;
    edit = g_new0 (TaxIdentityEdit, 1);
    edit->dialog = ti_dialog;
    edit->window = GTK_WINDOW (gtk_window_new ());
    gnc_window_bind_to_application (edit->window);
    gtk_window_set_title (edit->window, _("Income Tax Identity"));
    gtk_window_set_modal (edit->window, TRUE);
    gtk_window_set_transient_for (edit->window, GTK_WINDOW (ti_dialog->dialog));
    gtk_window_set_resizable (edit->window, FALSE);
    g_signal_connect_object (ti_dialog->dialog, "destroy", G_CALLBACK (gtk_window_destroy),
                             edit->window, G_CONNECT_SWAPPED);

    content = gtk_box_new (GTK_ORIENTATION_VERTICAL, 12);
    gtk_widget_set_margin_start (content, 18);
    gtk_widget_set_margin_end (content, 18);
    gtk_widget_set_margin_top (content, 18);
    gtk_widget_set_margin_bottom (content, 18);
    gtk_window_set_child (edit->window, content);
    grid = gtk_grid_new ();
    gtk_grid_set_column_spacing (GTK_GRID (grid), 12);
    gtk_grid_set_row_spacing (GTK_GRID (grid), 8);
    gtk_box_append (GTK_BOX (content), grid);

    label = gtk_label_new_with_mnemonic (_("_Name"));
    gtk_widget_set_halign (label, GTK_ALIGN_END);
    gtk_grid_attach (GTK_GRID (grid), label, 0, 0, 1, 1);
    edit->name_entry = GTK_ENTRY (gtk_entry_new ());
    gtk_editable_set_text (GTK_EDITABLE (edit->name_entry), ti_dialog->tax_name ? ti_dialog->tax_name : "");
    gtk_grid_attach (GTK_GRID (grid), GTK_WIDGET (edit->name_entry), 1, 0, 1, 1);
    gtk_label_set_mnemonic_widget (GTK_LABEL (label), GTK_WIDGET (edit->name_entry));

    label = gtk_label_new_with_mnemonic (_("_Type"));
    gtk_widget_set_halign (label, GTK_ALIGN_END);
    gtk_grid_attach (GTK_GRID (grid), label, 0, 1, 1, 1);
    entries = g_ptr_array_new ();
    for (node = ti_dialog->entity_type_infos; node; node = node->next, item++)
    {
        TaxTypeInfo *type = node->data;
        g_ptr_array_add (entries, type->combo_box_entry);
        if (g_strcmp0 (ti_dialog->tax_type, type->type_code) == 0)
            current_item = item;
    }
    g_ptr_array_add (entries, NULL);
    types = gtk_string_list_new ((const char * const *)entries->pdata);
    g_ptr_array_free (entries, TRUE);
    edit->type_dropdown = gnc_gtk_drop_down_new (G_LIST_MODEL (types), NULL);
    gtk_drop_down_set_selected (edit->type_dropdown, current_item);
    gtk_grid_attach (GTK_GRID (grid), GTK_WIDGET (edit->type_dropdown), 1, 1, 1, 1);
    gtk_label_set_mnemonic_widget (GTK_LABEL (label), GTK_WIDGET (edit->type_dropdown));

    label = gtk_label_new (_("CAUTION: If you set TXF categories, and later change 'Type', you will need to manually reset those categories one at a time"));
    gtk_label_set_max_width_chars (GTK_LABEL (label), 50);
    gtk_label_set_wrap (GTK_LABEL (label), TRUE);
    gtk_grid_attach (GTK_GRID (grid), label, 0, 2, 2, 1);
    buttons = gtk_box_new (GTK_ORIENTATION_HORIZONTAL, 6);
    gtk_widget_set_halign (buttons, GTK_ALIGN_END);
    cancel = gtk_button_new_with_mnemonic (_("_Cancel"));
    apply = gtk_button_new_with_mnemonic (_("_Apply"));
    gtk_box_append (GTK_BOX (buttons), cancel);
    gtk_box_append (GTK_BOX (buttons), apply);
    gtk_box_append (GTK_BOX (content), buttons);
    gtk_window_set_default_widget (edit->window, apply);
    ti_dialog->entity_name_entry = GTK_WIDGET (edit->name_entry);
    ti_dialog->entity_type_combo = GTK_WIDGET (edit->type_dropdown);
    g_signal_connect (apply, "clicked", G_CALLBACK (identity_edit_apply_clicked), edit);
    g_signal_connect (cancel, "clicked", G_CALLBACK (identity_edit_cancel_clicked), edit);
    g_signal_connect (edit->window, "close-request", G_CALLBACK (identity_edit_close_request), edit);
    g_signal_connect (edit->window, "destroy", G_CALLBACK (identity_edit_destroyed), edit);
    gtk_widget_grab_focus (GTK_WIDGET (edit->name_entry));
    gtk_window_present (edit->window);
}
static void
tax_related_toggled_cb (GtkCheckButton *togglebutton,
                        gpointer user_data)
{
    TaxInfoDialog *ti_dialog = user_data;
    GtkWidget *vbox;
    GtkWidget *hbox;
    gboolean on;

    on = gtk_check_button_get_active (togglebutton);

    vbox = ti_dialog->txf_vbox;
    hbox = ti_dialog->pns_vbox;
    gtk_widget_set_sensitive (vbox, on);

    gtk_widget_set_sensitive (hbox, on);

    if (on == FALSE)
        gtk_widget_set_visible (GTK_WIDGET(ti_dialog->txf_help_text), FALSE); /* textview doesn't go insensitive!? */
    else
        gtk_widget_set_visible (GTK_WIDGET(ti_dialog->txf_help_text), TRUE);

    gnc_tax_info_set_changed (ti_dialog, TRUE);
}

static void
current_account_toggled_cb (GtkCheckButton *togglebutton,
                            gpointer user_data)
{
    TaxInfoDialog *ti_dialog = user_data;

    gnc_tax_info_set_changed (ti_dialog, TRUE);
}

static void
copy_number_value_changed_cb (GtkSpinButton *spinbutton,
                              gpointer user_data)
{
    TaxInfoDialog *ti_dialog = user_data;

    gnc_tax_info_set_changed (ti_dialog, TRUE);
}

static void
gnc_tax_info_dialog_create (GtkWidget * parent, TaxInfoDialog *ti_dialog)
{
    GtkWidget *dialog;
    GtkBuilder  *builder;
GtkWidget *account_tree;
GtkSelectionModel *account_selection;
    GtkWidget *label;

    builder = gtk_builder_new();
    gnc_builder_add_from_file (builder, "dialog-tax-info.glade", "copy_spin_adj");
    gnc_builder_add_from_file (builder, "dialog-tax-info.glade", "tax_information_dialog");

    dialog = GTK_WIDGET(gtk_builder_get_object (builder, "tax_information_dialog"));
    ti_dialog->dialog = dialog;

    // Set the name for this dialog so it can be easily manipulated with css
    gtk_widget_set_name (GTK_WIDGET(dialog), "gnc-id-tax-information");
    gnc_widget_style_context_add_class (GTK_WIDGET(dialog), "gnc-class-taxes");

    initialize_getters ();

    g_signal_connect (dialog, "destroy", G_CALLBACK (window_destroy_cb), ti_dialog);
    g_signal_connect (dialog, "close-request", G_CALLBACK (tax_info_close_request), ti_dialog);
    g_signal_connect (gtk_builder_get_object (builder, "apply_button"), "clicked",
                      G_CALLBACK (tax_info_apply_clicked), ti_dialog);
    g_signal_connect (gtk_builder_get_object (builder, "ok_button"), "clicked",
                      G_CALLBACK (tax_info_ok_clicked), ti_dialog);
    g_signal_connect (gtk_builder_get_object (builder, "cancel_button"), "clicked",
                      G_CALLBACK (tax_info_cancel_clicked), ti_dialog);

    /* parent */
    if (parent != NULL)
        gtk_window_set_transient_for (GTK_WINDOW (dialog), GTK_WINDOW (parent));
    gtk_window_set_default_widget (GTK_WINDOW (dialog),
                                   GTK_WIDGET (gtk_builder_get_object (builder, "ok_button")));

    /* tax identity */
    {
        GtkWidget *label;
        GtkWidget *edit_button;

        ti_dialog->this_book = gnc_get_current_book();
        ti_dialog->tax_name = gnc_get_current_book_tax_name();
        ti_dialog->tax_type = gnc_get_current_book_tax_type();

        label = GTK_WIDGET(gtk_builder_get_object (builder, "entity_name"));
        ti_dialog->entity_name_display = label;
        gtk_label_set_text (GTK_LABEL (label), ti_dialog->tax_name);
        ti_dialog->entity_name_entry = NULL;

        load_tax_entity_type_list (ti_dialog); /* initialize tax_type_combo_text */

        label = GTK_WIDGET(gtk_builder_get_object (builder, "entity_type"));
        ti_dialog->entity_type_display = label;
        if (ti_dialog->tax_type != NULL)
            gtk_label_set_text (GTK_LABEL (label), ti_dialog->tax_type_combo_text);
        ti_dialog->entity_type_combo = NULL;

        edit_button = GTK_WIDGET(gtk_builder_get_object (builder, "identity_edit_button"));
        ti_dialog->tax_identity_edit_button = edit_button;
        g_signal_connect (G_OBJECT (edit_button), "clicked",
                          G_CALLBACK (identity_edit_clicked_cb), ti_dialog);
        ti_dialog->tax_type_changed = FALSE;
    }

    ti_dialog->income_txf_infos = load_txf_info (INCOME, ti_dialog);
    ti_dialog->expense_txf_infos = load_txf_info (EXPENSE, ti_dialog);
    ti_dialog->asset_txf_infos = load_txf_info (ASSET, ti_dialog);
    ti_dialog->liab_eq_txf_infos = load_txf_info (LIAB_EQ, ti_dialog);

    /* tax information */
    {
        GtkWidget *button;
        GtkWidget *text;

        ti_dialog->txf_info = GTK_WIDGET(gtk_builder_get_object (builder, "tax_info_vbox"));
        button = GTK_WIDGET(gtk_builder_get_object (builder, "tax_related_button"));
        ti_dialog->tax_related_button = button;

        g_signal_connect (G_OBJECT (button), "toggled",
                          G_CALLBACK  (tax_related_toggled_cb), ti_dialog);

        text = GTK_WIDGET(gtk_builder_get_object (builder, "txf_help_text"));
        gtk_text_view_set_wrap_mode(GTK_TEXT_VIEW(text), GTK_WRAP_WORD);
        ti_dialog->txf_help_text = text;

        ti_dialog->txf_category_view = GTK_WIDGET
            (gtk_builder_get_object (builder, "txf_category_view"));
        ti_dialog->txf_model = g_list_store_new (G_TYPE_OBJECT);
        ti_dialog->txf_selection = GTK_SINGLE_SELECTION (gtk_single_selection_new
            (G_LIST_MODEL (g_object_ref (ti_dialog->txf_model))));
        gtk_single_selection_set_autoselect (ti_dialog->txf_selection, FALSE);
        gtk_column_view_set_model (GTK_COLUMN_VIEW (ti_dialog->txf_category_view),
                                   GTK_SELECTION_MODEL (ti_dialog->txf_selection));
        GtkColumnViewColumn *column = txf_column_new (_("Form"), "txf-form");
        gtk_column_view_append_column (GTK_COLUMN_VIEW (ti_dialog->txf_category_view), column);
        g_object_unref (column);
        column = txf_column_new (_("Description"), "txf-description");
        gtk_column_view_append_column (GTK_COLUMN_VIEW (ti_dialog->txf_category_view), column);
        g_object_unref (column);
        g_signal_connect (ti_dialog->txf_selection, "selection-changed",
                          G_CALLBACK (txf_code_select_row_cb), ti_dialog);
        ti_dialog->apply_button = GTK_WIDGET(gtk_builder_get_object (builder, "apply_button"));

        button = GTK_WIDGET(gtk_builder_get_object (builder, "current_account_button"));
        ti_dialog->current_account_button = button;

        button = GTK_WIDGET(gtk_builder_get_object (builder, "parent_account_button"));
        ti_dialog->parent_account_button = button;

        ti_dialog->help_scroll = GTK_WIDGET(gtk_builder_get_object (builder, "help_scroll"));
        ti_dialog->payer_vbox = GTK_WIDGET(gtk_builder_get_object (builder, "payer_name_source_vbox"));
        ti_dialog->copy_vbox = GTK_WIDGET(gtk_builder_get_object (builder, "copy_number_vbox"));
        ti_dialog->txf_vbox = GTK_WIDGET(gtk_builder_get_object (builder, "txf_categories_vbox"));
        ti_dialog->pns_vbox = GTK_WIDGET(gtk_builder_get_object (builder, "pns_copy_hbox"));

        g_signal_connect (G_OBJECT (button), "toggled",
                          G_CALLBACK  (current_account_toggled_cb),
                          ti_dialog);

        button = GTK_WIDGET(gtk_builder_get_object (builder, "copy_spin_button"));
        ti_dialog->copy_spin_button = button;

        g_signal_connect (G_OBJECT (button), "value-changed",
                          G_CALLBACK  (copy_number_value_changed_cb),
                          ti_dialog);
    }

    /* account tree */
    {
        GtkWidget *income_radio, *expense_radio, *asset_radio,
                  *liab_eq_radio, *box;

        ti_dialog->acct_info = GTK_WIDGET(gtk_builder_get_object (builder, "acct_info_vbox"));
        ti_dialog->num_acct_label = GTK_WIDGET(gtk_builder_get_object (builder, "num_accounts_label"));

        account_tree = gnc_tree_view_account_new (FALSE);
        gnc_tree_view_account_set_filter (GNC_TREE_VIEW_ACCOUNT (account_tree),
                                          gnc_tax_info_dialog_account_filter_func,
                                          ti_dialog, NULL);
        gnc_tree_view_account_set_selection_mode (
            GNC_TREE_VIEW_ACCOUNT (account_tree), GTK_SELECTION_MULTIPLE);
        ti_dialog->account_treeview = account_tree;
        account_selection = gnc_tree_view_account_get_selection_model (
            GNC_TREE_VIEW_ACCOUNT (account_tree));
        g_signal_connect (account_selection, "selection-changed",
                          G_CALLBACK (gnc_tax_info_account_changed_cb), ti_dialog);

        gtk_widget_set_visible (GTK_WIDGET(ti_dialog->account_treeview), TRUE);
        box = GTK_WIDGET(gtk_builder_get_object (builder, "account_scroll"));
        gtk_scrolled_window_set_child (GTK_SCROLLED_WINDOW (box),
                                       GTK_WIDGET (ti_dialog->account_treeview));

        label = GTK_WIDGET(gtk_builder_get_object (builder, "accounts_label"));
        gtk_label_set_mnemonic_widget (GTK_LABEL (label), account_tree);

        income_radio = GTK_WIDGET(gtk_builder_get_object (builder, "income_radio"));
        ti_dialog->income_radio = income_radio;
        expense_radio = GTK_WIDGET(gtk_builder_get_object (builder, "expense_radio"));
        ti_dialog->expense_radio = expense_radio;
        asset_radio = GTK_WIDGET(gtk_builder_get_object (builder, "asset_radio"));
        ti_dialog->asset_radio = asset_radio;
        liab_eq_radio = GTK_WIDGET(gtk_builder_get_object (builder, "liab_eq_radio"));
        ti_dialog->liab_eq_radio = liab_eq_radio;
        ti_dialog->account_type = ACCT_TYPE_EXPENSE;
        gtk_check_button_set_active(GTK_CHECK_BUTTON(expense_radio), TRUE);

        g_signal_connect (G_OBJECT (income_radio), "toggled",
                          G_CALLBACK  (gnc_tax_info_acct_type_cb),
                          ti_dialog);
        g_signal_connect (G_OBJECT (expense_radio), "toggled",
                          G_CALLBACK  (gnc_tax_info_acct_type_cb),
                          ti_dialog);
        g_signal_connect (G_OBJECT (asset_radio), "toggled",
                          G_CALLBACK  (gnc_tax_info_acct_type_cb),
                          ti_dialog);
        g_signal_connect (G_OBJECT (liab_eq_radio), "toggled",
                          G_CALLBACK  (gnc_tax_info_acct_type_cb),
                          ti_dialog);
    }

    /* select subaccounts button */
    {
        GtkWidget *button;

        button = GTK_WIDGET(gtk_builder_get_object (builder, "select_subaccounts_button"));
        ti_dialog->select_button = button;

        g_signal_connect (G_OBJECT (button), "clicked",
                          G_CALLBACK  (select_subaccounts_clicked),
                          ti_dialog);
    }

    tax_info_show_acct_type_accounts (ti_dialog);
    gnc_tax_info_update_accounts (ti_dialog);
    clear_gui (ti_dialog);
    gnc_tax_info_set_changed (ti_dialog, FALSE);

    gnc_restore_window_size(GNC_PREFS_GROUP,
                            GTK_WINDOW(ti_dialog->dialog), GTK_WINDOW (parent));


    if (gnc_prefs_get_bool(GNC_PREFS_GROUP_GENERAL, GNC_PREF_SAVE_GEOMETRY))
    {
        GObject *object = gtk_builder_get_object (builder, "paned");
        gnc_prefs_bind (GNC_PREFS_GROUP, GNC_PREF_PANED_POS, NULL, object, "position");
    }
    g_object_unref (builder);
}

static void
close_handler (gpointer user_data)
{
    TaxInfoDialog *ti_dialog = user_data;

    gnc_save_window_size(GNC_PREFS_GROUP, GTK_WINDOW(ti_dialog->dialog));
    gtk_window_destroy (GTK_WINDOW (ti_dialog->dialog));
}

static void
refresh_handler (GHashTable *changes, gpointer user_data)
{
    TaxInfoDialog *ti_dialog = user_data;

    gnc_tax_info_update_accounts (ti_dialog);
}

/********************************************************************\
 * gnc_tax_info_dialog                                              *
 *   opens up a window to set account tax information               *
 *                                                                  *
 * Args:   parent  - the parent of the window to be created         *
 * Return: nothing                                                  *
\********************************************************************/
void
gnc_tax_info_dialog (GtkWidget * parent, Account * account)
{
    TaxInfoDialog *ti_dialog;
    gint component_id;

    ti_dialog = g_new0 (TaxInfoDialog, 1);

    gnc_tax_info_dialog_create (parent, ti_dialog);

    if (account)
        gnc_tax_info_set_acct (ti_dialog, account);

    component_id = gnc_register_gui_component (DIALOG_TAX_INFO_CM_CLASS,
                   refresh_handler, close_handler,
                   ti_dialog);
    gnc_gui_component_set_session (component_id, gnc_get_current_session ());

    gnc_gui_component_watch_entity_type (component_id,
                                         GNC_ID_ACCOUNT,
                                         QOF_EVENT_MODIFY | QOF_EVENT_DESTROY);

    set_focus_sensitivity (ti_dialog);

    gtk_window_present (GTK_WINDOW (ti_dialog->dialog));
}
