/********************************************************************\
 * gnucash-item-list.h -- A scrollable list box                     *
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


#define GNC_TYPE_ITEM_LIST     (gnc_item_list_get_type ())
#define GNC_ITEM_LIST(o)       (G_TYPE_CHECK_INSTANCE_CAST((o), GNC_TYPE_ITEM_LIST, GncItemList))
#define GNC_ITEM_LIST_CLASS(k) (G_TYPE_CHECK_CLASS_CAST ((k), GNC_TYPE_ITEM_LIST, GncItemListClass))
#define IS_GNC_ITEM_LIST(o)    (G_TYPE_CHECK_INSTANCE_TYPE((o), GNC_TYPE_ITEM_LIST))

typedef struct 
{
        GnomeCanvasWidget canvas_widget;

        GtkTreeView *tree_view;
	GtkListStore *list_store; /* Contains the list items */
        GtkWidget *frame;         /* frame around everything */
} GncItemList;

typedef struct
{
        GnomeCanvasWidgetClass parent_class;

	void (*select_item) (GncItemList *item_list,
			     char        *item_string);

        void (*change_item) (GncItemList *item_list,
                             char        *item_string);

        void (*activate_item) (GncItemList *item_list,
                               char        *item_string);

	void (*key_press_event) (GncItemList *item_list,
				 GdkEventKey *event);

} GncItemListClass;


GType gnc_item_list_get_type (void);

GnomeCanvasItem *gnc_item_list_new (GnomeCanvasGroup *parent, GtkListStore *shared_store);

gint gnc_item_list_num_entries (GncItemList *item_list);

void gnc_item_list_clear (GncItemList *item_list);

void gnc_item_list_append (GncItemList *item_list, const char *string);

void gnc_item_list_set_sort_enabled(GncItemList *item_list, gboolean enabled);

gboolean gnc_item_in_list (GncItemList *item_list, const char *string);

void gnc_item_list_select (GncItemList *item_list, const char *string);

void gnc_item_list_show_selected (GncItemList *item_list);

int gnc_item_list_autosize (GncItemList *item_list);



#endif /* GNUCASH_ITEM_LIST_H */


/*
  Local Variables:
  c-basic-offset: 8
  End:
*/
