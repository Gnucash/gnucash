/*
 * Copyright (C) 2003,2005,2006 David Hampton <hampton@employees.org> *
 * Author: Copyright (C) 2003,2005,2006 David Hampton <hampton@employees.org>
 *                                                                    *
 * This program is free software; you can redistribute it and/or      *
 * modify it under the terms of the GNU General Public License as     *
 * published by the Free Software Foundation; either version 2 of     *
 * the License, or (at your option) any later version.                *
 *                                                                    *
 * This program is distributed in the hope that it will be useful,    *
 * but WITHOUT ANY WARRANTY; without even the implied warranty of     *
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the      *
 * GNU General Public License for more details.                       *
 *                                                                    *
 * You should have received a copy of the GNU General Public License  *
 * along with this program; if not, contact:                          *
 *                                                                    *
 * Free Software Foundation           Voice:  +1-617-542-5942         *
 * 51 Franklin Street, Fifth Floor    Fax:    +1-617-542-2652         *
 * Boston, MA  02110-1301,  USA       gnu@gnu.org                     *
 *                                                                    *
\**********************************************************************/

/* gnc-tree-view-account.h -- GTK4 account hierarchy view. */
#ifndef __GNC_TREE_VIEW_ACCOUNT_H
#define __GNC_TREE_VIEW_ACCOUNT_H

#include <gtk/gtk.h>
#include "gnc-tree-model-account.h"
#include "gnc-ui-util.h"
#include "gnc-plugin-page.h"

G_BEGIN_DECLS

#define GNC_TYPE_TREE_VIEW_ACCOUNT (gnc_tree_view_account_get_type ())
G_DECLARE_FINAL_TYPE (GncTreeViewAccount, gnc_tree_view_account, GNC,
                      TREE_VIEW_ACCOUNT, GtkBox)
#define GNC_TREE_VIEW_ACCOUNT_NAME "GncTreeViewAccount"

typedef struct AccountViewInfo_s AccountViewInfo;
struct AccountViewInfo_s
{
    gboolean include_type[NUM_ACCOUNT_TYPES];
    gboolean show_hidden;
};

typedef struct
{
    GtkWidget *dialog;
    GListModel *type_model;
    gboolean updating_type_selection;
    GncTreeViewAccount *tree_view;
    GHashTable *filter_override;
    guint32 visible_types;
    guint32 original_visible_types;
    gboolean show_hidden;
    gboolean original_show_hidden;
    gboolean show_zero_total;
    gboolean original_show_zero_total;
    gboolean show_unused;
    gboolean original_show_unused;
} AccountFilterDialog;

void account_filter_dialog_create (AccountFilterDialog *fd, GncPluginPage *page);
gboolean gnc_plugin_page_account_tree_filter_accounts (Account *account,
                                                         gpointer user_data);
void gppat_filter_show_hidden_toggled_cb (GtkCheckButton *togglebutton,
                                          AccountFilterDialog *fd);
void gppat_filter_show_zero_toggled_cb (GtkCheckButton *togglebutton,
                                        AccountFilterDialog *fd);
void gppat_filter_show_unused_toggled_cb (GtkCheckButton *togglebutton,
                                          AccountFilterDialog *fd);
void gppat_filter_clear_all_cb (GtkWidget *button, AccountFilterDialog *fd);
void gppat_filter_select_all_cb (GtkWidget *button, AccountFilterDialog *fd);
void gppat_filter_select_default_cb (GtkWidget *button, AccountFilterDialog *fd);
GtkWidget *gnc_tree_view_account_new_with_root (Account *root,
                                                  gboolean show_root);
GtkWidget *gnc_tree_view_account_new (gboolean show_root);
GtkColumnView *gnc_tree_view_account_get_column_view (GncTreeViewAccount *view);
GtkSelectionModel *gnc_tree_view_account_get_selection_model (GncTreeViewAccount *view);
void gnc_tree_view_account_set_selection_mode (GncTreeViewAccount *view,
                                                GtkSelectionMode mode);
void gnc_tree_view_account_set_headers_visible (GncTreeViewAccount *view,
                                                  gboolean visible);
GtkColumnViewColumn *gnc_tree_view_account_find_column (GncTreeViewAccount *view,
                                                          const gchar *name);
void gnc_tree_view_account_set_column_visible (GncTreeViewAccount *view,
                                                 const gchar *name,
                                                 gboolean visible);
gboolean gnc_tree_view_account_get_column_visible (GncTreeViewAccount *view,
                                                     const gchar *name);
void gnc_tree_view_account_set_state_section (GncTreeViewAccount *view,
                                               const gchar *section);
const gchar *gnc_tree_view_account_get_state_section (GncTreeViewAccount *view);

void gnc_tree_view_account_save (GncTreeViewAccount *view, AccountFilterDialog *fd,
                                  GKeyFile *key_file, const gchar *group_name);
void gnc_tree_view_account_restore (GncTreeViewAccount *view, AccountFilterDialog *fd,
                                     GKeyFile *key_file, const gchar *group_name);
void gnc_tree_view_account_save_filter (GncTreeViewAccount *view,
                                         AccountFilterDialog *fd,
                                         GKeyFile *key_file,
                                         const gchar *group_name);
void gnc_tree_view_account_restore_filter (GncTreeViewAccount *view,
                                            AccountFilterDialog *fd,
                                            GKeyFile *key_file,
                                            const gchar *group_name);

void gnc_tree_view_account_get_view_info (GncTreeViewAccount *view,
                                           AccountViewInfo *avi);
void gnc_tree_view_account_set_view_info (GncTreeViewAccount *view,
                                           AccountViewInfo *avi);
typedef gboolean (*gnc_tree_view_account_filter_func) (Account *account,
                                                         gpointer data);
void gnc_tree_view_account_set_filter (GncTreeViewAccount *view,
                                        gnc_tree_view_account_filter_func func,
                                        gpointer data, GDestroyNotify destroy);
gboolean gnc_tree_view_account_filter_by_view_info (Account *account,
                                                      gpointer data);
void gnc_tree_view_account_refilter (GncTreeViewAccount *view);
typedef void (*gnc_tree_view_account_edited_func) (Account *account, gpointer column, const gchar *text);
typedef gboolean (*gnc_tree_view_account_selection_filter_func) (Account *account,
                                                                  gpointer user_data);
void gnc_tree_view_account_set_selection_filter (GncTreeViewAccount *view,
                                                  gnc_tree_view_account_selection_filter_func filter,
                                                  gpointer user_data,
                                                  GDestroyNotify destroy);

gint gnc_tree_view_account_count_children (GncTreeViewAccount *view,
                                            Account *account);
void gnc_tree_view_account_clear_model_cache (GncTreeViewAccount *view);
void gnc_tree_view_account_rebind_columns (GncTreeViewAccount *view);
Account *gnc_tree_view_account_get_account_at (GncTreeViewAccount *view,
                                                guint position);
Account *gnc_tree_view_account_get_cursor_account (GncTreeViewAccount *view);
Account *gnc_tree_view_account_get_selected_account (GncTreeViewAccount *view);
void gnc_tree_view_account_set_selected_account (GncTreeViewAccount *view,
                                                   Account *account);
GList *gnc_tree_view_account_get_selected_accounts (GncTreeViewAccount *view);
void gnc_tree_view_account_set_selected_accounts (GncTreeViewAccount *view,
                                                    GList *accounts,
                                                    gboolean show_last);
void gnc_tree_view_account_select_subaccounts (GncTreeViewAccount *view,
                                                 Account *account);
void gnc_tree_view_account_expand_to_account (GncTreeViewAccount *view,
                                               Account *account);
void gnc_tree_view_account_collapse_all (GncTreeViewAccount *view);
void gnc_tree_view_account_toggle_expand (GncTreeViewAccount *view, Account *account);
void gnc_tree_view_account_set_code_edited (GncTreeViewAccount *view,
                                             gnc_tree_view_account_edited_func edited_cb);
void gnc_tree_view_account_set_description_edited (GncTreeViewAccount *view,
                                                    gnc_tree_view_account_edited_func edited_cb);
void gnc_tree_view_account_set_notes_edited (GncTreeViewAccount *view,
                                              gnc_tree_view_account_edited_func edited_cb);
void gnc_tree_view_account_set_editing_started_cb (GncTreeViewAccount *view,
                                                     GFunc callback,
                                                     gpointer user_data);
void gnc_tree_view_account_set_editing_finished_cb (GncTreeViewAccount *view,
                                                      GFunc callback,
                                                      gpointer user_data);
void gnc_tree_view_account_name_edited_cb (Account *account, gpointer column,
                                           const gchar *new_name);
void gnc_tree_view_account_code_edited_cb (Account *account, gpointer column,
                                           const gchar *new_code);
void gnc_tree_view_account_description_edited_cb (Account *account,
                                                  gpointer column,
                                                  const gchar *new_desc);
void gnc_tree_view_account_notes_edited_cb (Account *account, gpointer column,
                                            const gchar *new_notes);

G_END_DECLS
#endif
