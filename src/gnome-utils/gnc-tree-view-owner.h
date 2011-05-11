/********************************************************************\
 * gnc-tree-view-owner.h -- GtkTreeView implementation to display   *
 *                            owners in a GtkTreeView.              *
 * Copyright (C) 2011 Geert Janssens <geert@kobaltwit.be>           *
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
/** @file gnc-tree-view-owner.h
    @brief GtkTreeView implementation for gnucash owner tree.
    @author Copyright (C) 2003,2005,2006 David Hampton <hampton@employees.org>
*/

#ifndef __GNC_TREE_VIEW_OWNER_H
#define __GNC_TREE_VIEW_OWNER_H

#include <gtk/gtk.h>
#include "gncOwner.h"
#include "gnc-tree-view.h"

#include "gnc-ui-util.h"
#include "gnc-plugin-page.h"

G_BEGIN_DECLS

/* type macros */
#define GNC_TYPE_TREE_VIEW_OWNER            (gnc_tree_view_owner_get_type ())
#define GNC_TREE_VIEW_OWNER(obj)            (G_TYPE_CHECK_INSTANCE_CAST ((obj), GNC_TYPE_TREE_VIEW_OWNER, GncTreeViewOwner))
#define GNC_TREE_VIEW_OWNER_CLASS(klass)    (G_TYPE_CHECK_CLASS_CAST ((klass), GNC_TYPE_TREE_VIEW_OWNER, GncTreeViewOwnerClass))
#define GNC_IS_TREE_VIEW_OWNER(obj)         (G_TYPE_CHECK_INSTANCE_TYPE ((obj), GNC_TYPE_TREE_VIEW_OWNER))
#define GNC_IS_TREE_VIEW_OWNER_CLASS(klass) (G_TYPE_CHECK_CLASS_TYPE ((klass), GNC_TYPE_TREE_VIEW_OWNER))
#define GNC_TREE_VIEW_OWNER_GET_CLASS(obj)  (G_TYPE_INSTANCE_GET_CLASS ((obj), GNC_TYPE_TREE_VIEW_OWNER, GncTreeViewOwnerClass))
#define GNC_TREE_VIEW_OWNER_NAME            "GncTreeViewOwner"

/* typedefs & structures */
typedef struct OwnerViewInfo_s     OwnerViewInfo;


struct OwnerViewInfo_s
{
    gboolean show_inactive;
};


typedef struct
{
    GncTreeView gnc_tree_view;
    int stamp;
} GncTreeViewOwner;

typedef struct
{
    GncTreeViewClass gnc_tree_view;
} GncTreeViewOwnerClass;

typedef struct
{
    GtkWidget    *dialog;
    GncTreeViewOwner  *tree_view;
    gboolean     show_inactive;
    gboolean     original_show_inactive;
    gboolean     show_zero_total;
    gboolean     original_show_zero_total;
} OwnerFilterDialog;

void owner_filter_dialog_create(OwnerFilterDialog *fd,
                                  GncPluginPage *page);

gboolean gnc_plugin_page_owner_tree_filter_owners (GncOwner *owner,
        gpointer user_data);

/* "Filter By" dialog callbacks */
void gppot_filter_show_inactive_toggled_cb (GtkToggleButton *togglebutton,
        OwnerFilterDialog *fd);
void gppot_filter_show_zero_toggled_cb (GtkToggleButton *togglebutton,
                                        OwnerFilterDialog *fd);
void gppot_filter_response_cb (GtkWidget *dialog, gint response,
                               OwnerFilterDialog *fd);

/* Saving/Restoring */
void gnc_tree_view_owner_save(GncTreeViewOwner *tree_view,
                                OwnerFilterDialog *fd,
                                GKeyFile *key_file, const gchar *group_name);
void gnc_tree_view_owner_restore(GncTreeViewOwner *view,
                                   OwnerFilterDialog *fd,
                                   GKeyFile *key_file,
                                   const gchar *group_name,
                                   GncOwnerType owner_type);


/* Get the GType for an GncTreeViewOwner object. */
GType gnc_tree_view_owner_get_type (void);


/** @name Owner Tree View Constructor
 @{ */

/** Create a new owner tree view for one type of owners.
 *
 *  @param owner_type The type of owners to use in the view.
 *
 *  @return A pointer to a new owner tree view.
 */
GtkTreeView *gnc_tree_view_owner_new (GncOwnerType owner_type);

/** @} */


/** @name Owner Tree View Configuration
 @{ */

typedef gchar * (*GncTreeViewOwnerColumnSource) (GncOwner *owner,
        GtkTreeViewColumn *col,
        GtkCellRenderer *cell);

typedef void (*GncTreeViewOwnerColumnTextEdited) (GncOwner *owner,
        GtkTreeViewColumn *col,
        const gchar *new_text);


/** Add a new custom column to the set of columns in an owner tree
 *  view.  This column will be visible as soon as it is added and will
 *  query the provided functions to determine what data to display.
 *  The TreeView will own the resulting TreeViewColumn, but caller may
 *  set any additional properties they wish.
 *
 *  @param view A pointer to an owner tree view.
 *
 *  @param column_title The title for this new column.
 *
 *  @param source_cb A callback function that is expected to provide
 *  the data to be displayed.
 *
 *  @param edited_cb A callback function that will be called if the
 *  user edits the displayed data.
 */
GtkTreeViewColumn * gnc_tree_view_owner_add_custom_column(
    GncTreeViewOwner *view, const gchar *column_title,
    GncTreeViewOwnerColumnSource source_cb,
    GncTreeViewOwnerColumnTextEdited edited_cb);

void gnc_tree_view_owner_set_name_edited(GncTreeViewOwner *view,
        GncTreeViewOwnerColumnTextEdited edited_cb);
void gnc_tree_view_owner_name_edited_cb(GncOwner *owner, GtkTreeViewColumn *col, const gchar *new_name);

/** Add a new column to the set of columns in an owner tree view.
 *  This column will be visible as soon as it is added and will
 *  display the contents of the specified KVP slot.
 *
 *  @param view A pointer to an owner tree view.
 *
 *  @param column_title The title for this new column.
 *
 *  @param kvp_key The lookup key to use for looking up data in the
 *  owner KVP structures. The value associated with this key is what
 *  will be displayed in the column.
 */
GtkTreeViewColumn *
gnc_tree_view_owner_add_kvp_column (GncTreeViewOwner *view,
                                      const gchar *column_title,
                                      const gchar *kvp_key);

/** @} */


/** @name Owner Tree View Filtering
 @{ */

/** Given pointers to an owner tree and old style filter block, this
 *  function will copy the current configuration of the owner tree
 *  widget into the data block.  This may be used in conjunction with
 *  the gnc_tree_view_owner_set_view_info function to modify the
 *  filters on an existing owner tree.
 *
 *  @param owner_view A pointer to an owner tree view.
 *
 *  @param avi A pointer to an old style filter block to fill in.
 */
void gnc_tree_view_owner_get_view_info (GncTreeViewOwner *owner_view,
        OwnerViewInfo *avi);

/** Given pointers to an owner tree and old style filter block, this
 *  function will applies the settings specified to the current
 *  configuration of the owner tree widget.  This may be used in
 *  conjunction with the gnc_tree_view_owner_get_view_info function
 *  to modify the filters on an existing owner tree.
 *
 *  @param owner_view A pointer to an owner tree view.
 *
 *  @param avi A pointer to an old style filter block to apply to the
 *  view.
 */
void gnc_tree_view_owner_set_view_info (GncTreeViewOwner *owner_view,
        OwnerViewInfo *avi);


/** This is the description of a filter function used by the owner tree.
 *
 *  @param owner The owner to be tested.
 *
 *  @param data The data provided when the filter function was added.
 *
 *  @return TRUE if the owner should be displayed.
 */
typedef gboolean (*gnc_tree_view_owner_filter_func)(GncOwner *owner, gpointer data);


/** This function attaches a filter function to the given owner
 *  tree.  This function will be called for each owner that the view
 *  thinks should possibly show.  The filter may perform any actions
 *  necessary on the owner to decide whether it should be shown or
 *  not.  (I.E. Check type, placeholder status, etc.)  If the filter
 *  returns TRUE then the owner will be displayed.
 *
 *  @param owner_view A pointer to an owner tree view.
 *
 *  @param func A filtration function that is called on individual
 *  elements in the tree.  If this function returns TRUE, the owner
 *  will be displayed.
 *
 *  @param data A data block passed into each instance of the function.
 *
 *  @param destroy A function to destroy the data block.  This
 *  function will be called when the filter is destroyed.  may be
 *  NULL.
 */
void gnc_tree_view_owner_set_filter (GncTreeViewOwner *owner_view,
                                       gnc_tree_view_owner_filter_func func,
                                       gpointer data,
                                       GtkFunction destroy);


/** This function forces the owner tree filter to be evaluated.  It
 *  may be necessary to call this function if the initial state of the
 *  view is incorrect.  This appears to only be necessary if the
 *  filter affects one of the top level owners in gnucash.
 *
 *  @note This calls a function in gtk that is annotated in the
 *  sources as being slow.  You have been warned.
 *
 *  @param view A pointer to an owner tree view.
 */
void gnc_tree_view_owner_refilter (GncTreeViewOwner *view);
/** @} */


/** @name Owner Tree View Get/Set Functions
 @{ */

/** This function returns the owner associated with the specified
 *  path.  This function is useful in selection callbacks on an
 *  owner tree widget.
 *
 *  @param view A pointer to an owner tree view.
 *
 *  @param path A path specifying a node in the owner tree.
 *
 *  @return The owner associated with this path.
 */
GncOwner * gnc_tree_view_owner_get_owner_from_path (GncTreeViewOwner *view,
        GtkTreePath *path);


/** This function returns the owner associated with the specified
 *  iter.  This function is useful in selection callbacks on an
 *  owner tree widget.
 *
 *  @param model The model provided to the callback function.
 *
 *  @param iter The iter provided to the callback function.
 *
 *  @return The owner associated with this iter.
 */
GncOwner * gnc_tree_view_owner_get_owner_from_iter (GtkTreeModel *model,
        GtkTreeIter  *iter);


/** This function returns the owner in the owner tree view at the
 *  current location of the cursor. (The outline frame. Usually is
 *  selected and therefore filled in, but not always.)
 *
 *  @param view A pointer to an owner tree view.
 *
 *  @return The owner at the cursor.
 */
GncOwner * gnc_tree_view_owner_get_cursor_owner (GncTreeViewOwner *view);


/** This function returns the owner associated with the selected
 *  item in the owner tree view.
 *
 *  @note It only makes sense to call this function when the owner
 *  tree is set to select a single item.  There is a different
 *  function to use when the tree supports multiple selections.
 *
 *  @param view A pointer to an owner tree view.
 *
 *  @return The selected owner, or NULL if no owner was selected.
 */
GncOwner * gnc_tree_view_owner_get_selected_owner (GncTreeViewOwner *view);


/** This function selects an owner in the owner tree view.  All
 *  other owners will be unselected.  In addition, this function
 *  collapses the entitre tree and then expands only the path to the
 *  selected owner, making the item easy to find.  In general, this
 *  routine only need be called when initially putting up a window
 *  containing an owner tree view widget.
 *
 *  @note It only makes sense to call this function when the owner
 *  tree is set to select a single item.  There is a different
 *  function to use when the tree supports multiple selections.
 *
 *  @param view A pointer to an owner tree view.
 *
 *  @param owner A pointer to the owner to select.
 */
void gnc_tree_view_owner_set_selected_owner (GncTreeViewOwner *view,
        GncOwner *owner);


/** This function returns a list of the owners associated with the
 *  selected items in the owner tree view.
 *
 *  @note It only makes sense to call this function when the owner
 *  tree is set to select multiple items.  There is a different
 *  function to use when the tree supports single selection.
 *
 *  @param view A pointer to an owner tree view.
 *
 *  @return A list of owners, or NULL if no owner was selected.
 */
GList * gnc_tree_view_owner_get_selected_owners (GncTreeViewOwner *view);


/** This function selects a set of owners in the owner tree view.
 *  All other owners will be unselected.  In addition, this function
 *  collapses the entitre tree and then expands only the path to the
 *  selected owners, making them easy to find.  In general, this
 *  routine only need be called when initially putting up a window
 *  containing an owner tree view widget.
 *
 *  @note It only makes sense to call this function when the owner
 *  tree is set to select a single item.  There is a different
 *  function to use when the tree supports multiple selections.
 *
 *  @note It is the responsibility of the caller to free the returned
 *  list.
 *
 *  @param view A pointer to an owner tree view.
 *
 *  @param owner_list A list of owners to select.
 *
 *  @param show_last Force the window to scroll to the last owner
 *  selected.
 */
void gnc_tree_view_owner_set_selected_owners (GncTreeViewOwner *view,
        GList *owner_list,
        gboolean show_last);

G_END_DECLS

#endif /* __GNC_TREE_VIEW_OWNER_H */
