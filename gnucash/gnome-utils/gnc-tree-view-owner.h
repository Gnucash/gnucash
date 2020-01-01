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

#define GNC_OWNER_TREE_NAME_COL                 "name"
#define GNC_OWNER_TREE_TYPE_COL                 "type"
#define GNC_OWNER_TREE_ID_COL                   "owner-id"
#define GNC_OWNER_TREE_CURRENCY_COL             "currency"
#define GNC_OWNER_TREE_ADDRESS_NAME_COL         "address-name"
#define GNC_OWNER_TREE_ADDRESS_1_COL            "address-1"
#define GNC_OWNER_TREE_ADDRESS_2_COL            "address-2"
#define GNC_OWNER_TREE_ADDRESS_3_COL            "address-3"
#define GNC_OWNER_TREE_ADDRESS_4_COL            "address-4"
#define GNC_OWNER_TREE_PHONE_COL                "phone"
#define GNC_OWNER_TREE_FAX_COL                  "fax"
#define GNC_OWNER_TREE_EMAIL_COL                "email"
#define GNC_OWNER_TREE_BALANCE_COL              "balance"
#define GNC_OWNER_TREE_BALANCE_REPORT_COL       "balance-report"
#define GNC_OWNER_TREE_BALANCE_PERIOD_COL       "balance-period"
#define GNC_OWNER_TREE_NOTES_COL                "notes"
#define GNC_OWNER_TREE_ACTIVE_COL               "active"


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

/** @} */


/** @name Owner Tree View Filtering
 @{ */

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
                                     GSourceFunc destroy);


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
 *  collapses the entire tree and then expands only the path to the
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


G_END_DECLS

#endif /* __GNC_TREE_VIEW_OWNER_H */
