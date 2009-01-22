/********************************************************************\
 * gnc-tree-view-account.h -- GtkTreeView implementation to display *
 *                            accounts in a GtkTreeView.            *
 * Copyright (C) 2003,2005,2006 David Hampton <hampton@employees.org> *
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
 *                                                                  *
\********************************************************************/

/** @addtogroup GUI
    @{ */
/** @addtogroup GuiTreeModel
 * @{ */
/** @file gnc-tree-view-account.h
    @brief GtkTreeView implementation for gnucash account tree.
    @author Copyright (C) 2003,2005,2006 David Hampton <hampton@employees.org>
*/

#ifndef __GNC_TREE_VIEW_ACCOUNT_H
#define __GNC_TREE_VIEW_ACCOUNT_H

#include <gtk/gtktreemodel.h>
#include <gtk/gtktreeview.h>
#include "gnc-tree-view.h"

#include "gnc-ui-util.h"
#include "gnc-plugin-page.h"

G_BEGIN_DECLS

/* type macros */
#define GNC_TYPE_TREE_VIEW_ACCOUNT            (gnc_tree_view_account_get_type ())
#define GNC_TREE_VIEW_ACCOUNT(obj)            (G_TYPE_CHECK_INSTANCE_CAST ((obj), GNC_TYPE_TREE_VIEW_ACCOUNT, GncTreeViewAccount))
#define GNC_TREE_VIEW_ACCOUNT_CLASS(klass)    (G_TYPE_CHECK_CLASS_CAST ((klass), GNC_TYPE_TREE_VIEW_ACCOUNT, GncTreeViewAccountClass))
#define GNC_IS_TREE_VIEW_ACCOUNT(obj)         (G_TYPE_CHECK_INSTANCE_TYPE ((obj), GNC_TYPE_TREE_VIEW_ACCOUNT))
#define GNC_IS_TREE_VIEW_ACCOUNT_CLASS(klass) (G_TYPE_CHECK_CLASS_TYPE ((klass), GNC_TYPE_TREE_VIEW_ACCOUNT))
#define GNC_TREE_VIEW_ACCOUNT_GET_CLASS(obj)  (G_TYPE_INSTANCE_GET_CLASS ((obj), GNC_TYPE_TREE_VIEW_ACCOUNT, GncTreeViewAccountClass))
#define GNC_TREE_VIEW_ACCOUNT_NAME            "GncTreeViewAccount"

/* typedefs & structures */
typedef struct AccountViewInfo_s     AccountViewInfo;


struct AccountViewInfo_s
{
  gboolean include_type[NUM_ACCOUNT_TYPES];
};


typedef struct {
	GncTreeView gnc_tree_view;
	int stamp;
} GncTreeViewAccount;

typedef struct {
	GncTreeViewClass gnc_tree_view;
} GncTreeViewAccountClass;

typedef	struct {
    GtkWidget    *dialog;
    GtkTreeModel *model;
    GncTreeViewAccount  *tree_view;
    guint32      visible_types;
    guint32      original_visible_types;
    gboolean     show_hidden;
    gboolean     original_show_hidden;
    gboolean     show_zero_total;
    gboolean     original_show_zero_total;
} AccountFilterDialog;

void account_filter_dialog_create(AccountFilterDialog *fd, 
                                  GncPluginPage *page);

gboolean gnc_plugin_page_account_tree_filter_accounts (Account *account, 
                                                       gpointer user_data);

/* "Filter By" dialog callbacks */
void gppat_filter_show_hidden_toggled_cb (GtkToggleButton *togglebutton, 
					  AccountFilterDialog *fd);
void gppat_filter_show_zero_toggled_cb (GtkToggleButton *togglebutton, 
                                        AccountFilterDialog *fd);
void gppat_filter_clear_all_cb (GtkWidget *button, AccountFilterDialog *fd);
void gppat_filter_select_all_cb (GtkWidget *button, AccountFilterDialog *fd);
void gppat_filter_select_default_cb (GtkWidget *button, 
                                     AccountFilterDialog *fd);
void gppat_filter_response_cb (GtkWidget *dialog, gint response, 
                               AccountFilterDialog *fd);

/* Saving/Restoring */
void gnc_tree_view_account_save(GncTreeViewAccount *tree_view, 
                                AccountFilterDialog *fd, 
                                GKeyFile *key_file, const gchar *group_name);
void gnc_tree_view_account_restore(GncTreeViewAccount *view, 
                                   AccountFilterDialog *fd, 
                                   GKeyFile *key_file, 
                                   const gchar *group_name);


/* Get the GType for an GncTreeViewAccount object. */
GType gnc_tree_view_account_get_type (void);


/** @name Account Tree View Constructors 
 @{ */

/** Create a new account tree view.  This view may or may not show a
 *  pseudo top-level account.  The gnucash engine does not have a
 *  single top level account (it has a list of top level accounts),
 *  but this code provides one so that it can be used with all parts
 *  of the gnucash gui.
 *
 *  @param root The account to use as the first level of the created tree.
 *
 *  @param show_root Show the pseudo top-level account in this view.
 *
 *  @return A pointer to a new account tree view.
 */
GtkTreeView *gnc_tree_view_account_new_with_root (Account *root, 
						  gboolean show_root);

/** Create a new account tree view.  This view may or may not show a
 *  pseudo top-level account.  The gnucash engine does not have a
 *  single top level account (it has a list of top level accounts),
 *  but this code provides one so that it can be used with all parts
 *  of the gnucash gui.  The first level of accounts in the created
 *  tree will be the top level of accounts in the current book.
 *
 *  @param show_root Show the pseudo top-level account in this view.
 *
 *  @return A pointer to a new account tree view.
 */
GtkTreeView *gnc_tree_view_account_new (gboolean show_root);

/** @} */


/** @name Account Tree View Configuration 
 @{ */

typedef gchar * (*GncTreeViewAccountColumnSource) (Account *account, 
                                                   GtkTreeViewColumn *col,
                                                   GtkCellRenderer *cell);

typedef void (*GncTreeViewAccountColumnTextEdited) (Account *account, 
                                                    GtkTreeViewColumn *col, 
                                                    const gchar *new_text);


/** Add a new custom column to the set of columns in an account tree
 *  view.  This column will be visible as soon as it is added and will
 *  query the provided functions to determine what data to display.
 *  The TreeView will own the resulting TreeViewColumn, but caller may
 *  set any additional properties they wish.
 *
 *  @param view A pointer to an account tree view.
 *
 *  @param column_title The title for this new column.
 *
 *  @param source_cb A callback function that is expected to provide
 *  the data to be displayed.
 *
 *  @param edited_cb A callback function that will be called if the
 *  user edits the displayed data.
 */
GtkTreeViewColumn * gnc_tree_view_account_add_custom_column(
    GncTreeViewAccount *view, const gchar *column_title,
    GncTreeViewAccountColumnSource source_cb, 
    GncTreeViewAccountColumnTextEdited edited_cb);

void gnc_tree_view_account_set_name_edited(GncTreeViewAccount *view,
                                           GncTreeViewAccountColumnTextEdited edited_cb);
void gnc_tree_view_account_name_edited_cb(Account *account, GtkTreeViewColumn *col, const gchar *new_name);

void gnc_tree_view_account_set_code_edited(GncTreeViewAccount *view,
                                           GncTreeViewAccountColumnTextEdited edited_cb);
void gnc_tree_view_account_code_edited_cb(Account *account, GtkTreeViewColumn *col, const gchar *new_code);

void gnc_tree_view_account_set_description_edited(GncTreeViewAccount *view,
                                                  GncTreeViewAccountColumnTextEdited edited_cb);
void gnc_tree_view_account_description_edited_cb(Account *account, GtkTreeViewColumn *col, const gchar *new_desc);

void gnc_tree_view_account_set_notes_edited(GncTreeViewAccount *view,
                                            GncTreeViewAccountColumnTextEdited edited_cb);
void gnc_tree_view_account_notes_edited_cb(Account *account, GtkTreeViewColumn *col, const gchar *new_notes);

/** Add a new column to the set of columns in an account tree view.
 *  This column will be visible as soon as it is added and will
 *  display the contents of the specified KVP slot.
 *
 *  @param view A pointer to an account tree view.
 *
 *  @param column_title The title for this new column.
 *
 *  @param kvp_key The lookup key to use for looking up data in the
 *  account KVP structures. The value associated with this key is what
 *  will be displayed in the column.
 */
GtkTreeViewColumn * 
gnc_tree_view_account_add_kvp_column (GncTreeViewAccount *view,
                                      const gchar *column_title,
                                      const gchar *kvp_key);

/** @} */


/** @name Account Tree View Filtering 
 @{ */

/** Given pointers to an account tree and old style filter block, this
 *  function will copy the current configuration of the account tree
 *  widget into the data block.  This may be used in conjunction with
 *  the gnc_tree_view_account_set_view_info function to modify the
 *  filters on an existing account tree.
 *
 *  @param account_view A pointer to an account tree view.
 *
 *  @param avi A pointer to an old style filter block to fill in.
 */
void gnc_tree_view_account_get_view_info (GncTreeViewAccount *account_view,
                                          AccountViewInfo *avi);

/** Given pointers to an account tree and old style filter block, this
 *  function will applies the settings specified to the current
 *  configuration of the account tree widget.  This may be used in
 *  conjunction with the gnc_tree_view_account_get_view_info function
 *  to modify the filters on an existing account tree.
 *
 *  @param account_view A pointer to an account tree view.
 *
 *  @param avi A pointer to an old style filter block to apply to the
 *  view.
 */
void gnc_tree_view_account_set_view_info (GncTreeViewAccount *account_view,
                                          AccountViewInfo *avi);


/** This is the description of a filter function used by the account tree. 
 *
 *  @param account The account to be tested.
 *
 *  @param data The data provided when the filter function was added.
 *
 *  @return TRUE if the account should be displayed.
 */
typedef gboolean (*gnc_tree_view_account_filter_func)(Account *account, gpointer data);


/** This function attaches a filter function to the given account
 *  tree.  This function will be called for each account that the view
 *  thinks should possibly show.  The filter may perform any actions
 *  necessary on the account to decide whether it should be shown or
 *  not.  (I.E. Check type, placeholder status, etc.)  If the filter
 *  returns TRUE then the account wil be displayed.
 *
 *  @param account_view A pointer to an account tree view.
 *
 *  @param func A filtration function that is called on individual
 *  elements in the tree.  If this function returns TRUE, the account
 *  will be displayed.
 *
 *  @param data A data block passed into each instance of the function.
 *
 *  @param destroy A function to destroy the data block.  This
 *  function will be called when the filter is destroyed.  may be
 *  NULL.
 */
void gnc_tree_view_account_set_filter (GncTreeViewAccount *account_view, 
				       gnc_tree_view_account_filter_func func,
				       gpointer data,
				       GtkDestroyNotify destroy);

/*  This is a convenient filter function for use with
 *  gnc_tree_view_account_set_filter() and the functions in
 *  gnc-tree-model-account-types.h.  If you have some view that is
 *  backed by the "account types" tree model, you can get a guint32
 *  from that view's tree selection.  Then, you can use that account
 *  type selection as a filter for the account tree view.  For
 *  example:
 *
 *  guint sel_mask;
 *
 *  sel_mask = gnc_tree_model_account_types_get_selection(view);
 *
 *  gnc_tree_view_account_set_filter(account_view, 
 *    gnc_tree_view_account_filter_by_type_selection, 
 *    GUINT_TO_POINTER(sel_mask), NULL);
 * 
 */
gboolean gnc_tree_view_account_filter_by_type_selection(
    Account* acct, gpointer data);


/** This function forces the account tree filter to be evaluated.  It
 *  may be necessary to call this function if the initial state of the
 *  view is incorrect.  This appears to only be necessary if the
 *  filter affects one of the top level accounts in gnucash.
 *
 *  @note This calls a function in gtk that is annotated in the
 *  sources as being slow.  You have been warned.
 *
 *  @param view A pointer to an account tree view.
 */
void gnc_tree_view_account_refilter (GncTreeViewAccount *view);
/** @} */


/** @name Account Tree View Get/Set Functions 
 @{ */

/** This function determines if an account in the account tree view
 *  has any visible children.
 *
 *  @param view A pointer to an account tree view.
 *
 *  @param account A pointer to the account to check.
 *
 *  @return The number of children of the specified account. Returns 0
 *  on error.
 */
gint gnc_tree_view_account_count_children (GncTreeViewAccount *view,
					   Account *account);



/** This function returns the account associated with the specified
 *  path.  This function is useful in selection callbacks on an
 *  account tree widget.
 *
 *  @param view A pointer to an account tree view.
 *
 *  @param path A path specifying a node in the account tree.
 *
 *  @return The account associated with this path.
 */
Account * gnc_tree_view_account_get_account_from_path (GncTreeViewAccount *view,
						       GtkTreePath *path);


/** This function returns the account associated with the specified
 *  iter.  This function is useful in selection callbacks on an
 *  account tree widget.
 *
 *  @param model The model provided to the callback function.
 *
 *  @param iter The iter provided to the callback function.
 *
 *  @return The account associated with this iter.
 */
Account * gnc_tree_view_account_get_account_from_iter (GtkTreeModel *model,
						       GtkTreeIter  *iter);


/** This function returns the account in the account tree view at the
 *  current location of the cursor. (The outline frame. Usually is
 *  selected and therefore filled in, but not always.)
 *
 *  @param view A pointer to an account tree view.
 *
 *  @return The account at the cursor.
 */
Account * gnc_tree_view_account_get_cursor_account (GncTreeViewAccount *view);


/** This function returns the account associated with the selected
 *  item in the account tree view.
 *
 *  @note It only makes sense to call this function when the account
 *  tree is set to select a single item.  There is a different
 *  function to use when the tree supports multiple selections.
 *
 *  @param view A pointer to an account tree view.
 *
 *  @return The selected account, or NULL if no account was selected.
 */
Account * gnc_tree_view_account_get_selected_account (GncTreeViewAccount *view);


/** This function selects an account in the account tree view.  All
 *  other accounts will be unselected.  In addition, this function
 *  collapses the entitre tree and then expands only the path to the
 *  selected account, making the item easy to find.  In general, this
 *  routine only need be called when initially putting up a window
 *  containing an account tree view widget.
 *
 *  @note It only makes sense to call this function when the account
 *  tree is set to select a single item.  There is a different
 *  function to use when the tree supports multiple selections.
 *
 *  @param view A pointer to an account tree view.
 *
 *  @param account A pointer to the account to select.
 */
void gnc_tree_view_account_set_selected_account (GncTreeViewAccount *view,
						 Account *account);


/** This function returns a list of the accounts associated with the
 *  selected items in the account tree view.
 *
 *  @note It only makes sense to call this function when the account
 *  tree is set to select multiple items.  There is a different
 *  function to use when the tree supports single selection.
 *
 *  @param view A pointer to an account tree view.
 *
 *  @return A list of accounts, or NULL if no account was selected.
 */
GList * gnc_tree_view_account_get_selected_accounts (GncTreeViewAccount *view);


/** This function selects a set of accounts in the account tree view.
 *  All other accounts will be unselected.  In addition, this function
 *  collapses the entitre tree and then expands only the path to the
 *  selected accounts, making them easy to find.  In general, this
 *  routine only need be called when initially putting up a window
 *  containing an account tree view widget.
 *
 *  @note It only makes sense to call this function when the account
 *  tree is set to select a single item.  There is a different
 *  function to use when the tree supports multiple selections.
 *
 *  @note It is the responsibility of the caller to free the returned
 *  list.
 *
 *  @param view A pointer to an account tree view.
 *
 *  @param account_list A list of accounts to select.
 *
 *  @param show_last Force the window to scroll to the last account
 *  selected.
 */
void gnc_tree_view_account_set_selected_accounts (GncTreeViewAccount *view,
						  GList *account_list,
						  gboolean show_last);


/** This function selects all sub-accounts of an account in the
 *  account tree view.  All other accounts will be unselected.
 *
 *  @note It only makes sense to call this function when the account
 *  tree is set to select multiple items.  There is a different
 *  function to use when the tree supports multiple selections.
 *
 *  @param view A pointer to an account tree view.
 *
 *  @param account A pointer to the account whose children should be
 *  selected.
 */
void gnc_tree_view_account_select_subaccounts (GncTreeViewAccount *view,
					       Account *account);

/** This function forces the account tree expand whatever levels are
 *  necessary to make the specified account visible.
 *
 *  @param view A pointer to an account tree view.
 *
 *  @param account A pointer to the account to show.
 */
void gnc_tree_view_account_expand_to_account (GncTreeViewAccount *view, Account *account);

/** @} */

/** @} */
/** @} */

G_END_DECLS

#endif /* __GNC_TREE_VIEW_ACCOUNT_H */
