/********************************************************************\
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

#ifndef GNUCASH_ITEM_LIST_H
#define GNUCASH_ITEM_LIST_H


#include "gnucash-sheet.h"
#include "gnucash-color.h"
#include "gnucash-grid.h"
#include "gnucash-cursor.h"


#define GNC_ITEM_LIST(obj) (GTK_CHECK_CAST((obj), gnc_item_list_get_type (), GNCItemList))
#define GNC_ITEM_LIST_CLASS(k) (GTK_CHECK_CLASS_CAST ((k), gnc_item_list_get_type (), GNCItemListClass))
#define IS_GNC_ITEM_LIST(o) (GTK_CHECK_TYPE((o), gnc_item_list_get_type ()))


typedef struct 
{
        GnomeCanvasWidget canvas_widget;

        GtkCList *clist; /* Contains the list items */
} GNCItemList;


GtkType gnc_item_list_get_type (void);

GnomeCanvasItem *gnc_item_list_new (GnomeCanvasGroup *parent);

void gnc_item_list_clear(GNCItemList *item_list);

void gnc_item_list_append(GNCItemList *item_list, char *string);

void gnc_item_list_select(GNCItemList *item_list, const char *string);

void gnc_item_list_show_selected(GNCItemList *item_list);

void gnc_item_list_sort(GNCItemList *item_list);

void gnc_item_list_autosize(GNCItemList *item_list);


typedef struct
{
        GnomeCanvasWidgetClass parent_class;

	void (*select_item) (GNCItemList *item_list,
			     char        *item_string);

        void (*change_item) (GNCItemList *item_list,
                             char        *item_string);

        void (*activate_item) (GNCItemList *item_list,
                               char        *item_string);

	void (*key_press_event) (GNCItemList *item_list,
				 GdkEventKey *event);

} GNCItemListClass;


#endif /* GNUCASH_ITEM_LIST_H */


/*
  Local Variables:
  c-basic-offset: 8
  End:
*/
