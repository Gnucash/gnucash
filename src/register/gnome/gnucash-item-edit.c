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
 * along with this program; if not, write to the Free Software      *
 * Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.        *
\********************************************************************/

/*
 *  An editor for the gnucash sheet.
 *  Cut and pasted from the gnumeric item-edit.c file.
 */


#include <config.h>

#include <gnome.h>
#include "gnucash-item-edit.h"
#include "gnucash-style.h"


static GnomeCanvasItemClass *item_edit_parent_class;

typedef struct _TextDrawInfo TextDrawInfo;
struct _TextDrawInfo
{
        char *text_1;
        char *text_2;

        int len_1;
        int len_2;

        GdkFont *font;

        GdkRectangle bg_rect;
        GdkRectangle text_rect;

        GdkColor *fg_color;
        GdkColor *bg_color;

        int text_x1;
        int text_x2;
        int text_y;

        int cursor_x;
        int cursor_y1;
        int cursor_y2;
};


/* The arguments we take */
enum {
        ARG_0,
        ARG_SHEET,     /* The Sheet argument      */
        ARG_GTK_ENTRY, /* The GtkEntry argument   */
        ARG_IS_COMBO,  /* Should this be a combo? */
};


static void item_edit_show_combo_toggle (ItemEdit *item_edit,
					 gint x, gint y,
					 gint width, gint height,
					 GtkAnchorType anchor);

/*
 * Returns the coordinates for the editor bounding box
 */
void
item_edit_get_pixel_coords (ItemEdit *item_edit, int *x, int *y,
			    int *w, int *h)
{
        GnucashSheet *sheet = item_edit->sheet;
        int xd, yd;

        gnome_canvas_get_scroll_offsets (GNOME_CANVAS(sheet), &xd, &yd);

        xd += gnucash_sheet_col_get_distance(sheet, sheet->left_block,
					     item_edit->virt_col)
                + sheet->left_block_offset;
        yd += gnucash_sheet_row_get_distance (sheet, sheet->top_block,
					      item_edit->virt_row)
                + sheet->top_block_offset;

        gnucash_sheet_style_get_cell_pixel_rel_coords (item_edit->style,
						       item_edit->cell_row,
						       item_edit->cell_col,
						       x, y, w, h);

        *x += xd;
        *y += yd;
}


static void
item_edit_draw_info(ItemEdit *item_edit, int x, int y, TextDrawInfo *info)
{
        GtkJustification align;
        SheetBlockStyle *style;
        GtkEditable *editable;
        int text_len, total_len, first_part_len;
        int xd, yd, wd, hd, dx, dy;
        int toggle_space;
        int xoffset;

        style = item_edit->style;
        info->font = style->fonts[item_edit->cell_row][item_edit->cell_col];

        info->bg_color = style->active_bg_color[item_edit->cell_row]
                                               [item_edit->cell_col];
        info->fg_color = &gn_black;

        info->text_1 = gtk_entry_get_text (GTK_ENTRY (item_edit->editor));
        text_len = strlen (info->text_1);

        editable = GTK_EDITABLE (item_edit->editor);
        info->len_1 = gtk_editable_get_position(editable);
        info->len_2 = text_len - info->len_1;

        info->text_2 = info->text_1 + info->len_1;

        total_len = gdk_text_measure (info->font, info->text_1, text_len);
        first_part_len = gdk_text_width (info->font, info->text_1,
                                         info->len_1);

        item_edit_get_pixel_coords (item_edit, &xd, &yd, &wd, &hd);

        dx = xd - x;
        dy = yd - y;

        info->bg_rect.x = dx + CELL_HPADDING;
        info->bg_rect.y = dy + CELL_VPADDING;
        info->bg_rect.width = wd - (2 * CELL_HPADDING);
        info->bg_rect.height = hd - (2 * CELL_VPADDING - info->font->descent);

        align = item_edit->style->alignments[item_edit->cell_row]
                                            [item_edit->cell_col];

        toggle_space = (item_edit->is_combo) ?
                item_edit->combo_toggle.toggle_offset : 0;

        info->text_rect.x = dx;
        info->text_rect.y = dy + CELL_VPADDING;
        info->text_rect.width = wd - toggle_space;
        info->text_rect.height = hd - (2*CELL_VPADDING - info->font->descent);

        switch (align) {
                case GTK_JUSTIFY_RIGHT:
                        xoffset = info->text_rect.width -
                                  (2*CELL_HPADDING + total_len);
                        if (xoffset > 0)
                                break;
                default:
                case GTK_JUSTIFY_LEFT:
                case GTK_JUSTIFY_FILL:
                case GTK_JUSTIFY_CENTER:
                        xoffset = MIN (CELL_HPADDING,
                                       info->text_rect.width -
                                       (2*CELL_HPADDING + first_part_len));
                        break;
        }

        info->text_x1 = dx + xoffset;
        info->text_x2 = info->text_x1 + first_part_len;
        info->text_y = dy + hd - CELL_VPADDING;

        info->cursor_x = info->text_x2;
        info->cursor_y1 = dy + CELL_VPADDING;
        info->cursor_y2 = info->text_y + info->font->descent;
}

static void
item_edit_draw (GnomeCanvasItem *item, GdkDrawable *drawable,
                int x, int y, int width, int height)
{
        ItemEdit *item_edit = ITEM_EDIT (item);
        TextDrawInfo info;

        /* be sure we're valid */
        if (item_edit->virt_row < 0 || item_edit->virt_col < 0)
                return;

        /* Get the measurements for drawing */
        item_edit_draw_info(item_edit, x, y, &info);

        /* Draw the background */
        gdk_gc_set_foreground(item_edit->gc, info.bg_color);
        gdk_draw_rectangle (drawable, item_edit->gc, TRUE,
                            info.bg_rect.x, info.bg_rect.y,
                            info.bg_rect.width, info.bg_rect.height);

        /* Draw the foreground text and cursor */
        gdk_gc_set_foreground (item_edit->gc, info.fg_color);

        gdk_gc_set_clip_rectangle (item_edit->gc, &info.text_rect);

        gdk_draw_text (drawable, info.font, item_edit->gc,
                       info.text_x1, info.text_y,
                       info.text_1, info.len_1);

        gdk_draw_line (drawable, item_edit->gc,
                       info.cursor_x, info.cursor_y1,
                       info.cursor_x, info.cursor_y2);

        gdk_draw_text (drawable, info.font, item_edit->gc,
                       info.text_x2, info.text_y,
                       info.text_2, info.len_2);

        gdk_gc_set_clip_rectangle (item_edit->gc, NULL);
}


static double
item_edit_point (GnomeCanvasItem *item, double c_x, double c_y, int cx, int cy,
                 GnomeCanvasItem **actual_item)
{
        int x, y, w, h;
 
        item_edit_get_pixel_coords (ITEM_EDIT (item), &x, &y, &w, &h);

        *actual_item = NULL;
        if ((cx < x) || (cy < y) || (cx > x+w) || (cy > y+w))
                return 10000.0;
 
        *actual_item = item;
        return 0.0;
}


static int
item_edit_event (GnomeCanvasItem *item, GdkEvent *event)
{
        return 0;
}


static void
item_edit_update (GnomeCanvasItem *item, double *affine, ArtSVP *clip_path,
		  int flags)
{
        ItemEdit *item_edit = ITEM_EDIT (item);
        GnucashSheet *sheet = item_edit->sheet;
	GtkAnchorType list_anchor;
        gint x, y, w, h;
	gint toggle_x, toggle_y, toggle_width, toggle_height;
	gint list_x, list_y, list_height, view_height;
	gint y_offset;

        if (GNOME_CANVAS_ITEM_CLASS (item_edit_parent_class)->update)
                (*GNOME_CANVAS_ITEM_CLASS(item_edit_parent_class)->update)
			(item, affine, clip_path, flags);

        view_height = GTK_WIDGET(sheet)->allocation.height;
        gnome_canvas_get_scroll_offsets(GNOME_CANVAS(sheet), NULL, &y_offset);

        item_edit_get_pixel_coords (item_edit, &x, &y, &w, &h);

        item->x1 = x;
        item->y1 = y;
        item->x2 = x + w;
        item->y2 = y + h;

	if (!item_edit->is_combo)
		return;

	toggle_height = h - 10;
	toggle_width = toggle_height;
	toggle_x = x + w - (toggle_width + 3);
	toggle_y = y + 5;

        item_edit->combo_toggle.toggle_offset = toggle_width + 3;

	item_edit_show_combo_toggle(item_edit, toggle_x, toggle_y,
				    toggle_width, toggle_height,
				    GTK_ANCHOR_NW);

	if (!item_edit->show_list)
		return;

	list_x = x;
	list_y = y + h;
	list_height = h * 6;

	if ((list_y - y_offset) + list_height > view_height) {
		list_y = y;
		list_anchor = GTK_ANCHOR_SW;
	}
	else
		list_anchor = GTK_ANCHOR_NW;

	item_edit_show_list(item_edit, list_x, list_y,
			    list_height, list_anchor);
}


static void
item_edit_realize (GnomeCanvasItem *item)
{
        GnomeCanvas *canvas = item->canvas;
        GdkWindow *window;
        ItemEdit *item_edit;

        if (GNOME_CANVAS_ITEM_CLASS (item_edit_parent_class)->realize)
                (*GNOME_CANVAS_ITEM_CLASS
		 (item_edit_parent_class)->realize)(item);

        item_edit = ITEM_EDIT (item);
        window = GTK_WIDGET (canvas)->window;

        item_edit->gc = gdk_gc_new (window);
}


/*
 * Instance initialization
 */
static void
item_edit_init (ItemEdit *item_edit)
{
        GnomeCanvasItem *item = GNOME_CANVAS_ITEM (item_edit);

        item->x1 = 0;
        item->y1 = 0;
        item->x2 = 1;
        item->y2 = 1;

        /* Set invalid values so that we know when we have been fully
	   initialized */
        item_edit->sheet = NULL;
	item_edit->parent = NULL;
	item_edit->editor = NULL;

	item_edit->combo_toggle.combo_button = NULL;
	item_edit->combo_toggle.combo_button_item = NULL;
        item_edit->combo_toggle.toggle_offset = 0;
	item_edit->combo_toggle.arrow = NULL;
	item_edit->combo_toggle.signals_connected = FALSE;

	item_edit->item_list = NULL;

	item_edit->gc = NULL;
	item_edit->style = NULL;

        item_edit->virt_col = -1;
        item_edit->virt_row = -1;
        item_edit->cell_col = -1;
        item_edit->cell_row = -1;
}


static void
queue_sync (ItemEdit *item_edit)
{
        GnomeCanvas *canvas = GNOME_CANVAS_ITEM (item_edit)->canvas;
        int x, y, w, h;

        item_edit_get_pixel_coords (item_edit, &x, &y, &w, &h);
 
        gnome_canvas_request_redraw (canvas, x, y, x+w, y+h);
}


static void
entry_changed (GtkEntry *entry, void *data)
{
        queue_sync(ITEM_EDIT (data));
}


static void
item_edit_destroy (GtkObject *object)
{
        ItemEdit *item_edit = ITEM_EDIT (object);

        gtk_signal_disconnect (GTK_OBJECT (item_edit->editor),
			       item_edit->signal);

        gtk_signal_disconnect (GTK_OBJECT (item_edit->editor),
			       item_edit->signal2);

        gdk_gc_destroy (item_edit->gc);

        if (GTK_OBJECT_CLASS (item_edit_parent_class)->destroy)
                (*GTK_OBJECT_CLASS (item_edit_parent_class)->destroy)(object);
}


gboolean
item_edit_set_cursor_pos (ItemEdit *item_edit, int x, int y,
                          gboolean changed_cells)
{
        GtkEditable *editable;
        TextDrawInfo info;
        gint pos;
        gint pos_x;
        gint o_x, o_y;
        gint virt_row, virt_col, cell_row, cell_col;
        char *text;

        g_return_val_if_fail (IS_ITEM_EDIT(item_edit), FALSE);

        gnucash_grid_find_cell_origin_by_pixel
                (GNUCASH_GRID(item_edit->sheet->grid),
                 x, y,
                 &virt_row, &virt_col,
                 &cell_row, &cell_col,
                 &o_x, &o_y);

        if ( (virt_row != item_edit->virt_row) ||
             (virt_col != item_edit->virt_col) ||
             (cell_row != item_edit->cell_row) ||
             (cell_col != item_edit->cell_col) )
                return FALSE;

        editable = GTK_EDITABLE (item_edit->editor);

        if (changed_cells) {
                GtkJustification align;

                align = item_edit->style->alignments[item_edit->cell_row]
                                                    [item_edit->cell_col];

                if (align == GTK_JUSTIFY_RIGHT)
                        gtk_editable_set_position(editable, -1);
                else
                        gtk_editable_set_position(editable, 0);
        }

        item_edit_draw_info(item_edit, o_x, o_y, &info);

        if (info.text_1 == NULL)
                return FALSE;

        pos = strlen(info.text_1);
        if (pos == 0)
                return FALSE;

        text = info.text_1 + (pos - 1);

        while (text >= info.text_1) {
                pos_x = o_x + info.text_x1 +
                        gdk_text_width (info.font, info.text_1, pos);

                if (pos_x <= x + (gdk_char_width(info.font, *text) / 2))
                        break;

                pos--;
                text--;
        }

        gtk_editable_set_position (editable, pos);
        queue_sync (item_edit);

        return TRUE;
}


static int
entry_event (GtkEntry *entry, GdkEvent *event, ItemEdit *item_edit)
{
        switch (event->type) {
		case GDK_KEY_PRESS:
		case GDK_KEY_RELEASE:
		case GDK_BUTTON_PRESS:
			queue_sync (item_edit);
		default:
			break;
        }

        return FALSE;
}


static void
item_edit_set_editor (ItemEdit *item_edit, void *data)
{
        item_edit->editor = GTK_WIDGET (data);

        item_edit->signal = gtk_signal_connect
		(GTK_OBJECT (item_edit->editor), "changed",
		 GTK_SIGNAL_FUNC(entry_changed), item_edit);

        item_edit->signal2 = gtk_signal_connect_after
		(GTK_OBJECT (item_edit->editor), "event",
		 GTK_SIGNAL_FUNC(entry_event), item_edit);
}


void
item_edit_configure (ItemEdit *item_edit)
{
        GnucashSheet *sheet = item_edit->sheet;
        GnucashItemCursor *cursor;

        cursor = GNUCASH_ITEM_CURSOR
		(GNUCASH_CURSOR(sheet->cursor)->cursor[GNUCASH_CURSOR_BLOCK]);

        item_edit->virt_row = cursor->row;
        item_edit->virt_col = cursor->col;

        item_edit->style = gnucash_sheet_get_style (item_edit->sheet,
                                                    item_edit->virt_row,
						    item_edit->virt_col);

        cursor = GNUCASH_ITEM_CURSOR
		(GNUCASH_CURSOR(sheet->cursor)->cursor[GNUCASH_CURSOR_CELL]);

        item_edit->cell_row = cursor->row;
        item_edit->cell_col = cursor->col;

        item_edit_update (GNOME_CANVAS_ITEM(item_edit), NULL, NULL, 0);
}


static void
item_edit_show_combo_toggle (ItemEdit *item_edit, gint x, gint y,
			     gint width, gint height, GtkAnchorType anchor)
{
	g_return_if_fail(IS_ITEM_EDIT(item_edit));

	gnome_canvas_item_raise_to_top
		(item_edit->combo_toggle.combo_button_item);

	gnome_canvas_item_set(item_edit->combo_toggle.combo_button_item,
			      "x", (gdouble) x,
			      "y", (gdouble) y,
			      "width", (gdouble) width,
			      "height", (gdouble) height,
			      "anchor", anchor,
			      NULL);
}


static void
item_edit_hide_combo_toggle (ItemEdit *item_edit)
{
	g_return_if_fail(IS_ITEM_EDIT(item_edit));

	/* safely out of the way */
	gnome_canvas_item_set(item_edit->combo_toggle.combo_button_item,
			      "x", -10000.0, NULL);
}


static gboolean
key_press_combo_cb (GtkWidget *widget, GdkEventKey *event, gpointer data)
{
	ItemEdit *item_edit = ITEM_EDIT(data);

	gtk_signal_emit_stop_by_name(GTK_OBJECT(widget), "key_press_event");

	gtk_widget_event(GTK_WIDGET(item_edit->sheet), (GdkEvent *) event);

	return TRUE;
}


static void
item_edit_combo_toggled (GtkToggleButton *button, gpointer data)
{
	ItemEdit *item_edit = ITEM_EDIT(data);
	GtkArrowType arrow_type;
	GtkShadowType shadow_type;

	item_edit->show_list = gtk_toggle_button_get_active(button);

	if (!item_edit->show_list) {
		item_edit_hide_list(item_edit);
		arrow_type = GTK_ARROW_DOWN;
		shadow_type = GTK_SHADOW_IN;
	}
	else {
		arrow_type = GTK_ARROW_UP;
		shadow_type = GTK_SHADOW_OUT;
	}

	gtk_arrow_set(item_edit->combo_toggle.arrow, arrow_type, shadow_type);

	item_edit_configure (item_edit);
}


static void
connect_combo_signals (ItemEdit *item_edit)
{
	gint signal;

	g_return_if_fail(IS_ITEM_EDIT(item_edit));

	if (item_edit->combo_toggle.signals_connected)
		return;

	signal = gtk_signal_connect
		(GTK_OBJECT(item_edit->combo_toggle.combo_button),
		 "toggled", GTK_SIGNAL_FUNC(item_edit_combo_toggled),
		 (gpointer) item_edit);
	item_edit->combo_toggle.toggle_signal = signal;

	signal = gtk_signal_connect
		(GTK_OBJECT(item_edit->combo_toggle.combo_button),
		 "key_press_event", GTK_SIGNAL_FUNC(key_press_combo_cb),
		 (gpointer) item_edit);
	item_edit->combo_toggle.key_press_signal = signal;

	item_edit->combo_toggle.signals_connected = TRUE;
}


static void
disconnect_combo_signals (ItemEdit *item_edit)
{
	g_return_if_fail(IS_ITEM_EDIT(item_edit));

	if (!item_edit->combo_toggle.signals_connected)
		return;

	gtk_signal_disconnect(GTK_OBJECT(item_edit->combo_toggle.combo_button),
			      item_edit->combo_toggle.toggle_signal);

	gtk_signal_disconnect(GTK_OBJECT(item_edit->combo_toggle.combo_button),
			      item_edit->combo_toggle.key_press_signal);

	item_edit->combo_toggle.signals_connected = FALSE;
}


static void
item_edit_set_arg (GtkObject *object, GtkArg *arg, guint arg_id)
{
        GnomeCanvasItem *item;
        ItemEdit *item_edit;

        item = GNOME_CANVAS_ITEM (object);
        item_edit = ITEM_EDIT (object);

        switch (arg_id) {
        case ARG_SHEET:
                item_edit->sheet = GNUCASH_SHEET(GTK_VALUE_POINTER (*arg));
                break;
        case ARG_GTK_ENTRY:
                item_edit_set_editor (item_edit, GTK_VALUE_POINTER (*arg));
                break;
        case ARG_IS_COMBO:
                item_edit->is_combo = GTK_VALUE_BOOL (*arg);

		if (!item_edit->is_combo) {
			item_edit->show_list = FALSE;

			disconnect_combo_signals(item_edit);

			gtk_arrow_set(item_edit->combo_toggle.arrow,
				      GTK_ARROW_DOWN, GTK_SHADOW_IN);

			gtk_toggle_button_set_active
				(item_edit->combo_toggle.combo_button, FALSE);

			item_edit_hide_list(item_edit);
			item_edit_hide_combo_toggle(item_edit);
		}
		else
			connect_combo_signals(item_edit);

                item_edit_configure (item_edit);
		break;
        default:
                break;
        }
}


/*
 * ItemEdit class initialization
 */
static void
item_edit_class_init (ItemEditClass *item_edit_class)
{
        GtkObjectClass  *object_class;
        GnomeCanvasItemClass *item_class;

        item_edit_parent_class = gtk_type_class (gnome_canvas_item_get_type());
 
        object_class = (GtkObjectClass *) item_edit_class;
        item_class = (GnomeCanvasItemClass *) item_edit_class;

        gtk_object_add_arg_type ("ItemEdit::sheet", GTK_TYPE_POINTER,
                                 GTK_ARG_WRITABLE, ARG_SHEET);
        gtk_object_add_arg_type ("ItemEdit::GtkEntry", GTK_TYPE_POINTER,
                                 GTK_ARG_WRITABLE, ARG_GTK_ENTRY);
        gtk_object_add_arg_type ("ItemEdit::is_combo", GTK_TYPE_BOOL,
                                 GTK_ARG_WRITABLE, ARG_IS_COMBO);

        object_class->set_arg = item_edit_set_arg;
        object_class->destroy = item_edit_destroy;
 
        /* GnomeCanvasItem method overrides */
        item_class->update      = item_edit_update;
        item_class->draw        = item_edit_draw;
        item_class->point       = item_edit_point;
        item_class->event       = item_edit_event;
        item_class->realize     = item_edit_realize;
}

GtkType
item_edit_get_type (void)
{
        static GtkType item_edit_type = 0;

        if (!item_edit_type) {
                GtkTypeInfo item_edit_info = {
                        "ItemEdit",
                        sizeof (ItemEdit),
                        sizeof (ItemEditClass),
                        (GtkClassInitFunc) item_edit_class_init,
                        (GtkObjectInitFunc) item_edit_init,
                        NULL, /* reserved_1 */
                        NULL, /* reserved_2 */
                        (GtkClassInitFunc) NULL
                };

                item_edit_type =
			gtk_type_unique(gnome_canvas_item_get_type (),
					&item_edit_info);
        }

        return item_edit_type;
}


static void
create_combo_toggle(GnomeCanvasGroup *parent, ComboToggle *ct)
{
	GtkWidget *button, *arrow;

	arrow = gtk_arrow_new(GTK_ARROW_DOWN, GTK_SHADOW_IN);
	gtk_misc_set_alignment(GTK_MISC(arrow), 0.5, 0.5);
	ct->arrow = GTK_ARROW(arrow);

	button = gtk_toggle_button_new();
	ct->combo_button = GTK_TOGGLE_BUTTON(button);
	gtk_container_add(GTK_CONTAINER(button), arrow);

	ct->combo_button_item =
		gnome_canvas_item_new(parent, gnome_canvas_widget_get_type(),
				      "widget", button,
				      "size_pixels", TRUE,
				      NULL);
}


GnomeCanvasItem *
item_edit_new (GnomeCanvasGroup *parent, GnucashSheet *sheet, GtkWidget *entry)
{
        GnomeCanvasItem *item;
        ItemEdit *item_edit;

        item = gnome_canvas_item_new (parent,
				      item_edit_get_type (),
				      "ItemEdit::sheet", sheet,
				      "ItemEdit::GtkEntry", sheet->entry,
				      NULL);

        item_edit = ITEM_EDIT(item);

	item_edit->parent = parent;

	create_combo_toggle(parent, &item_edit->combo_toggle);

        return item;
}


GNCItemList *
item_edit_new_list (ItemEdit *item_edit)
{
	g_return_val_if_fail(IS_ITEM_EDIT(item_edit), NULL);

	return GNC_ITEM_LIST(gnc_item_list_new(item_edit->parent));
}


void
item_edit_show_list (ItemEdit *item_edit, gint x, gint y,
		     gint height, GtkAnchorType anchor)
{
	g_return_if_fail(IS_ITEM_EDIT(item_edit));

	if (item_edit->item_list == NULL)
		return;

	gnome_canvas_item_set(GNOME_CANVAS_ITEM(item_edit->item_list),
			      "x", (gdouble) x,
			      "y", (gdouble) y,
			      "height", (gdouble) height,
			      "anchor", anchor,
			      NULL);
}


void
item_edit_hide_list (ItemEdit *item_edit)
{
	g_return_if_fail(IS_ITEM_EDIT(item_edit));

	if (item_edit->item_list == NULL)
		return;

	/* safely out of the way */
	gnome_canvas_item_set(GNOME_CANVAS_ITEM(item_edit->item_list),
			      "x", -10000.0, NULL);
}


void
item_edit_set_list (ItemEdit *item_edit, GNCItemList *item_list)
{
	g_return_if_fail(IS_ITEM_EDIT(item_edit));
	g_return_if_fail((item_list == NULL) || IS_GNC_ITEM_LIST(item_list));

	item_edit_hide_list(item_edit);
	item_edit->item_list = item_list;
        item_edit_update (GNOME_CANVAS_ITEM(item_edit), NULL, NULL, 0);
}

/*
  Local Variables:
  c-basic-offset: 8
  End:
*/
