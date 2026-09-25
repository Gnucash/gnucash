/********************************************************************\
 * gnucash-item-list.h -- A scrollable GTK4 list box                     *
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

#ifndef GNUCASH_ITEM_LIST_H
#define GNUCASH_ITEM_LIST_H

#include <gtk/gtk.h>

/** @ingroup Register
 * @addtogroup Gnome
 * @{
 */
/** @file gnucash-item-list.h
 * @brief Public declarations for the GTK4 GncItemList class.
 */
#define GNC_TYPE_ITEM_LIST     (gnc_item_list_get_type ())
#define GNC_ITEM_LIST(o)       (G_TYPE_CHECK_INSTANCE_CAST ((o), GNC_TYPE_ITEM_LIST, GncItemList))
#define GNC_ITEM_LIST_CLASS(k) (G_TYPE_CHECK_CLASS_CAST ((k), GNC_TYPE_ITEM_LIST, GncItemListClass))
#define IS_GNC_ITEM_LIST(o)    (G_TYPE_CHECK_INSTANCE_TYPE ((o), GNC_TYPE_ITEM_LIST))

typedef struct
{
    GtkBox box;

    GtkListView *list_view;
    GtkScrolledWindow *scrollwin;
    GListStore *list_store;  /* Permanent list items. */
    GListStore *temp_store;  /* Typeahead result items. */
    GtkSortListModel *sorted_model;
    GtkSingleSelection *selection;
    GtkCustomSorter *sorter;
    gint cell_height;
} GncItemList;

typedef struct
{
    GtkBoxClass parent_class;

    void (*select_item) (GncItemList *item_list,
                         char        *item_string);

    void (*change_item) (GncItemList *item_list,
                         char        *item_string);

    void (*activate_item) (GncItemList *item_list,
                           char        *item_string);

    gboolean (*key_pressed) (GncItemList     *item_list,
                             guint            keyval,
                             guint            keycode,
                             GdkModifierType  state);
} GncItemListClass;

GType gnc_item_list_get_type (void);

/* The store owns generic row objects populated with the helpers below. */
GListStore *gnc_item_list_store_new (void);
void gnc_item_list_store_clear (GListStore *store);
void gnc_item_list_store_append (GListStore *store, const char *text,
                                 const char *markup, gint weight,
                                 gint found_location);

GtkWidget *gnc_item_list_new (GListStore *shared_store);

gint gnc_item_list_num_entries (GncItemList *item_list);
gint gnc_item_list_get_popup_height (GncItemList *item_list);

void gnc_item_list_clear (GncItemList *item_list);
void gnc_item_list_append (GncItemList *item_list, const char *string);
void gnc_item_list_set_sort_column (GncItemList *item_list, gint column_id);
gboolean gnc_item_in_list (GncItemList *item_list, const char *string);
void gnc_item_list_select (GncItemList *item_list, const char *string);
void gnc_item_list_select_at (GncItemList *item_list, guint position);
void gnc_item_list_show_selected (GncItemList *item_list);
char *gnc_item_list_get_selection (GncItemList *item_list);
gint gnc_item_list_get_selected_found_location (GncItemList *item_list);
GtkWidget *gnc_item_list_get_view (GncItemList *item_list);

int gnc_item_list_autosize (GncItemList *item_list);
void gnc_item_list_set_temp_store (GncItemList *item_list, GListStore *store);
gboolean gnc_item_list_using_temp (GncItemList *item_list);

/** @} */
#endif /* GNUCASH_ITEM_LIST_H */
