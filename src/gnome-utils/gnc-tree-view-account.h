/********************************************************************\
 * gnc-tree-view-account.h -- GtkTreeView implementation to display *
 *                            accounts in a GtkTreeView.            *
 * Copyright (C) 2003 David Hampton <hampton@employees.org>         *
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
 * 59 Temple Place - Suite 330        Fax:    +1-617-542-2652       *
 * Boston, MA  02111-1307,  USA       gnu@gnu.org                   *
 *                                                                  *
\********************************************************************/

/** @addtogroup Engine
    @{ */
/** @file gnc-tree-view-account.h
    @brief GtkTreeView implementation for gnucash account tree.
    @author David Hampton <hampton@employees.org>
*/

#ifndef __GNC_TREE_VIEW_ACCOUNT_H
#define __GNC_TREE_VIEW_ACCOUNT_H

#include <gtk/gtktreemodel.h>
#include <gtk/gtktreeview.h>

#include "Group.h"
#include "eggtreemodelfilter.h"
#include "gnc-ui-util.h"

G_BEGIN_DECLS

/* type macros */
#define GNC_TYPE_TREE_VIEW_ACCOUNT            (gnc_tree_view_account_get_type ())
#define GNC_TREE_VIEW_ACCOUNT(obj)            (G_TYPE_CHECK_INSTANCE_CAST ((obj), GNC_TYPE_TREE_VIEW_ACCOUNT, GncTreeViewAccount))
#define GNC_TREE_VIEW_ACCOUNT_CLASS(klass)    (G_TYPE_CHECK_CLASS_CAST ((klass), GNC_TYPE_TREE_VIEW_ACCOUNT, GncTreeViewAccountClass))
#define GNC_IS_TREE_VIEW_ACCOUNT(obj)         (G_TYPE_CHECK_INSTANCE_TYPE ((obj), GNC_TYPE_TREE_VIEW_ACCOUNT))
#define GNC_IS_TREE_VIEW_ACCOUNT_CLASS(klass) (G_TYPE_CHECK_CLASS_TYPE ((klass), GNC_TYPE_TREE_VIEW_ACCOUNT))
#define GNC_TREE_VIEW_ACCOUNT_GET_CLASS(obj)  (G_TYPE_INSTANCE_GET_CLASS ((obj), GNC_TYPE_TREE_VIEW_ACCOUNT, GncTreeViewAccountClass))

/* typedefs & structures */
typedef struct GncTreeViewAccountPrivate GncTreeViewAccountPrivate;
typedef struct AccountViewInfo_s     AccountViewInfo;


struct AccountViewInfo_s
{
  gboolean include_type[NUM_ACCOUNT_TYPES];

  gboolean show_field[NUM_ACCOUNT_FIELDS];
};


typedef struct {
	GtkTreeView parent;

	GncTreeViewAccountPrivate *priv;

	int stamp;
} GncTreeViewAccount;

typedef struct {
	GtkTreeViewClass parent;
} GncTreeViewAccountClass;



/* Standard g_object type */
GType         gnc_tree_view_account_get_type              (void);


/** @name Account Tree View Constructors */
/** @{ */

/** Create a new account tree view.  This view may or may not show a
 *  pseudo top-level account.  The gnucash engine does not have a
 *  single top level account (it has a list of top level accounts),
 *  but this code provides one so that it can be used with all parts
 *  of the gnucash gui.
 *
 *  @param show_root Show the pseudo top-level account in this view.
 *
 *  @return A pointer to a new account tree view.
 */
GtkTreeView * gnc_tree_view_account_new_with_group (AccountGroup *group, gboolean show_root);
GtkTreeView  *gnc_tree_view_account_new                   (gboolean show_root);
/** @} */


/** @name Account Tree View Configuration */
/** @{ */

const char *gnc_tree_view_account_get_field_name (AccountFieldCode field);

/** Configure (by name) the set of visible columns in an account tree
 *  view.  By default, only the account name column is show.  The
 *  avalible list of columns can be found in the file
 *  gnc-tree-view-account.c
 *
 *  @param account_view A pointer to an account tree view.
 *
 *  @param column_names A list of column names to make visible.
 */
void          gnc_tree_view_account_configure_columns     (GncTreeViewAccount *account_view,
							   GSList *column_names);


/** Add a new column to the set of columns in an account tree view.
 *  This column will display the contents of a specified KVP slot.
 *
 *  @param view A pointer to an account tree view.
 *
 *  @param column_title The title for this new column.
 *
 *  @param kvp_key The lookup key to use for looking up data in the
 *  account KVP structures. The value associated with this key is what
 *  will be displayed in the column.
 */
void          gnc_tree_view_account_add_kvp_column (GncTreeViewAccount *view,
						    const gchar *column_title,
						    const gchar *kvp_key);
/** @} */


/** @name Account Tree View Filtering */
/** @{ */

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
void          gnc_tree_view_account_get_view_info         (GncTreeViewAccount *account_view,
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
 *  view..
 */
void          gnc_tree_view_account_set_view_info         (GncTreeViewAccount *account_view,
							   AccountViewInfo *avi);

/** Given a pointer to an old style filter block, initialize it to the
 *  default values for an account tree.  The defaults are to show all
 *  types of accounts, and show only the account name column.
 *
 *  @param avi A pointer to an old style filter block.
 */
void          gnc_tree_view_account_init_view_info        (AccountViewInfo *avi);


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
void          gnc_tree_view_account_set_filter            (GncTreeViewAccount *account_view, 
							   EggTreeModelFilterVisibleFunc  func,
							   gpointer                       data,
							   GtkDestroyNotify               destroy);


/** This function forces the account tree filter to be evaluated.  It
 *  may be necessary to call this function if the initial state of the
 *  view is incorrect.  This appears to only be necessary if the
 *  filter affects one of the top level accounts in gnucash.
 *
 *  @note This calls a function in libegg that is annotated as being
 *  slow.  You have been warned.
 *
 *  @param account_view A pointer to an account tree view.
 */
void          gnc_tree_view_account_refilter              (GncTreeViewAccount *view);
/** @} */


/** @name Account Tree View Get/Set Functions */
/** @{ */

/** This function returns the account associated with the top level
 *  pseudo-account.  The gnucash engine does not have a single top
 *  level account (it has a list of top level accounts), but this code
 *  provides one so that it can be used with all parts of the gnucash
 *  gui.
 *
 *  @note It only makes sense to call this function when the account
 *  tree is created such that the "top level account" is visible.  At
 *  the time this was written, only the "New/Edit Account" dialog does
 *  that.
 *
 *  @param account_view A pointer to an account tree view.
 *
 *  @return The top-level pseudo-account.
 */
Account     * gnc_tree_view_account_get_top_level         (GncTreeViewAccount *view);


Account     * gnc_tree_view_account_get_account_from_path (GncTreeViewAccount *view,
							   GtkTreePath *path);

/** This function returns the account associated with the selected
 *  item in the account tree view.
 *
 *  @note It only makes sense to call this function when the account
 *  tree is set to select a single item.  There is a different
 *  function to use when the tree supports multiple selections.
 *
 *  @param account_view A pointer to an account tree view.
 *
 *  @return The selected account, or NULL if no account was selected.
 */
Account     * gnc_tree_view_account_get_selected_account  (GncTreeViewAccount *view);


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
 *  @param account_view A pointer to an account tree view.
 *
 *  @param account A pointer to the account to select.
 */
void          gnc_tree_view_account_set_selected_account  (GncTreeViewAccount *view,
							   Account *account);


/** This function returns a list of the accounts associated with the
 *  selected items in the account tree view.
 *
 *  @note It only makes sense to call this function when the account
 *  tree is set to select multiple items.  There is a different
 *  function to use when the tree supports single selection.
 *
 *  @param account_view A pointer to an account tree view.
 *
 *  @return A list of accounts, or NULL if no account was selected.
 */
GList       * gnc_tree_view_account_get_selected_accounts (GncTreeViewAccount *view);


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
 *  @param account_view A pointer to an account tree view.
 *
 *  @param account A pointer to the account to select.
 *
 *  @param show_last Force the window to scroll to the last account
 *  selected.
 */
void          gnc_tree_view_account_set_selected_accounts (GncTreeViewAccount *view,
							   GList *account_list,
							   gboolean show_last);
/** @} */


G_END_DECLS

#endif /* __GNC_TREE_VIEW_ACCOUNT_H */
