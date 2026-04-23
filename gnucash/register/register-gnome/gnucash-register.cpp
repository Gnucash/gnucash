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
 * 51 Franklin Street, Fifth Floor    Fax:    +1-617-542-2652       *
 * Boston, MA  02110-1301,  USA       gnu@gnu.org                   *
 *                                                                  *
\********************************************************************/

/*
 * The Gnucash Register widget
 *
 *  Based heavily on the Gnumeric Sheet widget.
 *
 * Authors:
 *     Heath Martin <martinh@pegasus.cc.ucf.edu>
 *     Dave Peticolas <dave@krondo.com>
 */

#include <config.h>
#include <glib.h>
#include <glib/gprintf.h>
#include <gdk/gdkkeysyms.h>

#include "gnucash-register.h"
#include "gnucash-sheet.h"
#include "gnucash-sheetP.hpp"

#include "gnucash-cursor.hpp"
#include "gnucash-style.h"
#include "gnucash-header.hpp"
#include "gnucash-item-edit.hpp"
#include "split-register.h"
#include "gnc-engine.h"         // For debugging, e.g. ENTER(), LEAVE()
#include "gnc-prefs.h"
#include "gnc-state.h"

#include "combocell.h"
#include "completioncell.h"
#include "datecell.h"
#include "formulacell-gnome.hpp"
#include "pricecell-gnome.hpp"
#include "quickfillcell-gnome.hpp"
#include "table-gnome.hpp"


/* Register signals */
enum
{
    ACTIVATE_CURSOR,
    REDRAW_ALL,
    REDRAW_HELP,
    SHOW_POPUP_MENU,
    LAST_SIGNAL
};


/** Static Globals *****************************************************/

/* This static indicates the debugging module that this .o belongs to. */
static QofLogModule log_module = GNC_MOD_REGISTER;
static unsigned int register_signals[LAST_SIGNAL];


struct _GnucashRegister
{
    GtkGrid table;

    GtkWidget *hscrollbar;
    GtkWidget *sheet;
    bool  hscrollbar_visible;
};


struct _GnucashRegisterClass
{
    GtkGridClass parent_class;

    void (*activate_cursor) (GnucashRegister *reg);
    void (*redraw_all)      (GnucashRegister *reg);
    void (*redraw_help)     (GnucashRegister *reg);
    void (*show_popup_menu) (GnucashRegister *reg);
};

/** Implementation *****************************************************/

G_DEFINE_TYPE (GnucashRegister, gnucash_register, GTK_TYPE_GRID)

void
gnucash_register_add_cell_types (void) noexcept
{
    gnc_register_add_cell_type (COMBO_CELL_TYPE_NAME, gnc_combo_cell_new);
    gnc_register_add_cell_type (COMPLETION_CELL_TYPE_NAME, gnc_completion_cell_new);
    gnc_register_add_cell_type (DATE_CELL_TYPE_NAME, gnc_date_cell_new);
    gnc_register_add_cell_type (PRICE_CELL_TYPE_NAME,
                                gnc_price_cell_gnome_new);
    gnc_register_add_cell_type (QUICKFILL_CELL_TYPE_NAME,
                                gnc_quickfill_cell_gnome_new);
    gnc_register_add_cell_type( FORMULA_CELL_TYPE_NAME,
                                gnc_formula_cell_gnome_new );
    gnc_table_gnome_init ();
}

gboolean
gnucash_register_has_selection (GnucashRegister *reg) noexcept
{
    g_return_val_if_fail((reg != nullptr), FALSE);
    g_return_val_if_fail(GNUCASH_IS_REGISTER(reg), FALSE);

    GnucashSheet *sheet = GNUCASH_SHEET(reg->sheet);
    GncItemEdit *item_edit = GNC_ITEM_EDIT(sheet->item_editor);

    return gnc_item_edit_get_has_selection(item_edit);
}

void
gnucash_register_cut_clipboard (GnucashRegister *reg) noexcept
{
    g_return_if_fail(reg != nullptr);
    g_return_if_fail(GNUCASH_IS_REGISTER(reg));

    GnucashSheet *sheet = GNUCASH_SHEET(reg->sheet);
    GncItemEdit *item_edit = GNC_ITEM_EDIT(sheet->item_editor);

    gnc_item_edit_cut_clipboard(item_edit);
}

void
gnucash_register_copy_clipboard (GnucashRegister *reg) noexcept
{
    g_return_if_fail(reg != nullptr);
    g_return_if_fail(GNUCASH_IS_REGISTER(reg));

    GnucashSheet *sheet = GNUCASH_SHEET(reg->sheet);
    GncItemEdit *item_edit = GNC_ITEM_EDIT(sheet->item_editor);

    gnc_item_edit_copy_clipboard(item_edit);
}

void
gnucash_register_paste_clipboard (GnucashRegister *reg) noexcept
{
    g_return_if_fail(reg != nullptr);
    g_return_if_fail(GNUCASH_IS_REGISTER(reg));

    GnucashSheet *sheet = GNUCASH_SHEET(reg->sheet);
    GncItemEdit *item_edit = GNC_ITEM_EDIT(sheet->item_editor);

    gnc_item_edit_paste_clipboard (item_edit);
}

void
gnucash_register_refresh_from_prefs (GnucashRegister *reg) noexcept
{
    g_return_if_fail(reg != nullptr);
    g_return_if_fail(GNUCASH_IS_REGISTER(reg));

    GnucashSheet *sheet = GNUCASH_SHEET(reg->sheet);
    gnucash_sheet_refresh_from_prefs(sheet);
    gnc_header_request_redraw (GNC_HEADER(sheet->header_item));
}

void
gnucash_register_reset_sheet_layout (GnucashRegister *reg) noexcept
{
    g_return_if_fail (reg != nullptr);

    GnucashSheet *sheet = GNUCASH_SHEET(reg->sheet);

    g_return_if_fail (sheet != nullptr);
    g_return_if_fail (GNUCASH_IS_SHEET (sheet));

    int current_width = sheet->window_width - 1;

    GNCHeaderWidths widths = gnc_header_widths_new ();
    gnucash_sheet_set_header_widths (sheet, widths);

    gnucash_sheet_styles_set_dimensions (sheet, current_width);

    gnucash_sheet_compile_styles (sheet);
    gnucash_sheet_table_load (sheet, TRUE);
    gnucash_sheet_cursor_set_from_table (sheet, TRUE);
    gnucash_sheet_redraw_all (sheet);
    gnc_header_widths_destroy (widths);
}

void
gnucash_register_goto_virt_cell (GnucashRegister *reg,
                                 VirtualCellLocation vcell_loc) noexcept
{
    VirtualLocation virt_loc{};

    g_return_if_fail(reg != nullptr);
    g_return_if_fail(GNUCASH_IS_REGISTER(reg));

    GnucashSheet *sheet = GNUCASH_SHEET(reg->sheet);

    virt_loc.vcell_loc = vcell_loc;
    virt_loc.phys_row_offset = 0;
    virt_loc.phys_col_offset = 0;

    gnucash_sheet_goto_virt_loc(sheet, virt_loc);
}

void
gnucash_register_goto_virt_loc (GnucashRegister *reg,
                                VirtualLocation virt_loc) noexcept
{
    g_return_if_fail(reg != nullptr);
    g_return_if_fail(GNUCASH_IS_REGISTER(reg));

    GnucashSheet *sheet = GNUCASH_SHEET(reg->sheet);

    gnucash_sheet_goto_virt_loc(sheet, virt_loc);
}

void
gnucash_register_goto_next_virt_row (GnucashRegister *reg) noexcept
{
    VirtualLocation virt_loc{};

    g_return_if_fail (reg != nullptr);
    g_return_if_fail (GNUCASH_IS_REGISTER(reg));

    GnucashSheet *sheet = GNUCASH_SHEET(reg->sheet);

    gnucash_cursor_get_virt (GNUCASH_CURSOR(sheet->cursor), &virt_loc);

    /* Move down one physical row at a time until we
     * reach the next visible virtual cell. */
    int start_virt_row = virt_loc.vcell_loc.virt_row;
    do
    {
        if (!gnc_table_move_vertical_position (sheet->table, &virt_loc, 1))
            return;
    }
    while (start_virt_row == virt_loc.vcell_loc.virt_row);

    if (virt_loc.vcell_loc.virt_row >= sheet->num_virt_rows)
        return;

    virt_loc.phys_row_offset = 0;
    virt_loc.phys_col_offset = 0;

    gnucash_sheet_goto_virt_loc (sheet, virt_loc);
}

void
gnucash_register_goto_next_matching_row (GnucashRegister *reg,
        VirtualLocationMatchFunc match,
        gpointer user_data) noexcept
{
    VirtualLocation virt_loc{};

    g_return_if_fail (reg != nullptr);
    g_return_if_fail (GNUCASH_IS_REGISTER(reg));
    g_return_if_fail (match != nullptr);

    GnucashSheet *sheet = GNUCASH_SHEET(reg->sheet);

    gnucash_cursor_get_virt (GNUCASH_CURSOR(sheet->cursor), &virt_loc);

    do
    {
        if (!gnc_table_move_vertical_position (sheet->table,
                                               &virt_loc, 1))
            return;

        if (virt_loc.vcell_loc.virt_row >= sheet->num_virt_rows)
            return;

        SheetBlockStyle *style = gnucash_sheet_get_style (sheet, virt_loc.vcell_loc);
        if (!style || !style->cursor)
            return;
    }
    while (!match (virt_loc, user_data));

    virt_loc.phys_row_offset = 0;
    virt_loc.phys_col_offset = 0;

    gnucash_sheet_goto_virt_loc (sheet, virt_loc);
}

static gboolean
gnucash_register_sheet_resize (GnucashRegister *reg)
{
    // Sometimes the space left by the horizontal scrollbar does
    // not get filled on load, this makes sure it does
    if (!reg->hscrollbar_visible)
        gtk_widget_queue_resize (GTK_WIDGET (reg->sheet));

    return FALSE;
}

static void
gnucash_register_update_hadjustment (GtkAdjustment *adj,
                                     GnucashRegister *reg)
{
    g_return_if_fail (reg != nullptr);
    g_return_if_fail (GNUCASH_IS_REGISTER(reg));

    if (gtk_adjustment_get_upper (adj) - gtk_adjustment_get_lower (adj)
        > gtk_adjustment_get_page_size (adj))
    {
        if (!reg->hscrollbar_visible)
        {
            gtk_widget_show(reg->hscrollbar);
            reg->hscrollbar_visible = true;
        }
    }
    else
    {
        if (reg->hscrollbar_visible)
        {
            gtk_widget_hide(reg->hscrollbar);
            reg->hscrollbar_visible = false;
            // When sheet first loaded and the scrollbar is hidden, the space left
            // is not always automatically taken up by the sheet so queue a resize
            // when all is idle
            g_idle_add (reinterpret_cast<GSourceFunc>(gnucash_register_sheet_resize), reg);
        }
    }
}

/*************************************************************/


static void
gnucash_register_class_init (GnucashRegisterClass *klass)
{
    GObjectClass *gobject_class = G_OBJECT_CLASS (klass);

    gtk_widget_class_set_css_name (GTK_WIDGET_CLASS(klass), "gnc-id-register");

    register_signals[ACTIVATE_CURSOR] =
        g_signal_new("activate_cursor",
                     G_TYPE_FROM_CLASS(gobject_class),
                     G_SIGNAL_RUN_LAST,
                     G_STRUCT_OFFSET(GnucashRegisterClass,
                                     activate_cursor),
                     nullptr, nullptr,
                     g_cclosure_marshal_VOID__VOID,
                     G_TYPE_NONE, 0);

    register_signals[REDRAW_ALL] =
        g_signal_new("redraw_all",
                     G_TYPE_FROM_CLASS(gobject_class),
                     G_SIGNAL_RUN_LAST,
                     G_STRUCT_OFFSET(GnucashRegisterClass,
                                     redraw_all),
                     nullptr, nullptr,
                     g_cclosure_marshal_VOID__VOID,
                     G_TYPE_NONE, 0);

    register_signals[REDRAW_HELP] =
        g_signal_new("redraw_help",
                     G_TYPE_FROM_CLASS(gobject_class),
                     G_SIGNAL_RUN_LAST,
                     G_STRUCT_OFFSET(GnucashRegisterClass,
                                     redraw_help),
                     nullptr, nullptr,
                     g_cclosure_marshal_VOID__VOID,
                     G_TYPE_NONE, 0);

    register_signals[SHOW_POPUP_MENU] =
        g_signal_new("show_popup_menu",
                     G_TYPE_FROM_CLASS(gobject_class),
                     G_SIGNAL_RUN_LAST,
                     G_STRUCT_OFFSET(GnucashRegisterClass,
                                     show_popup_menu),
                     nullptr, nullptr,
                     g_cclosure_marshal_VOID__VOID,
                     G_TYPE_NONE, 0);

    klass->activate_cursor = nullptr;
    klass->redraw_all = nullptr;
    klass->redraw_help = nullptr;
    klass->show_popup_menu = nullptr;
}


static void
gnucash_register_init (GnucashRegister *g_reg)
{
    GtkGrid *table = GTK_GRID(g_reg);

    gtk_widget_set_can_focus (GTK_WIDGET(table), FALSE);
    gtk_widget_set_can_default (GTK_WIDGET(table), FALSE);

    gtk_grid_set_row_homogeneous (GTK_GRID(table), FALSE);
    gtk_grid_set_column_homogeneous (GTK_GRID(table), FALSE);
}

void
gnucash_register_attach_popup (GnucashRegister *reg,
                               GtkWidget *popup,
                               gpointer data) noexcept
{
    g_return_if_fail (GNUCASH_IS_REGISTER(reg));
    g_return_if_fail (reg->sheet != nullptr);
    if (popup)
        g_return_if_fail (GTK_IS_WIDGET(popup));

    gnucash_sheet_set_popup (GNUCASH_SHEET (reg->sheet), popup, data);
}


/* Um, this function checks that data is not null but never uses it.
 *  Weird.  Also, since this function only works with a GnucashRegister
 *  widget, maybe some of it should be moved to gnucash-sheet.c. */
/* Adding to previous note:  Since data doesn't appear do anything and to
 *  align the function with save_state() I've removed the check for
 *  nullptr and changed two calls in dialog_order.c and dialog_invoice.c
 *  to pass nullptr as second parameter. */

static void
gnucash_register_configure (GnucashSheet *sheet, const gchar * state_section)
{
    GKeyFile *state_file = gnc_state_get_current();

    // Stuff for per-register settings load.
    g_return_if_fail (sheet != nullptr);
    g_return_if_fail (GNUCASH_IS_SHEET (sheet));

    PINFO("state_section=%s",state_section);

    ENTER("sheet=%p, data=%p", sheet, "");

    Table *table = sheet->table;
    gnc_table_init_gui (table);
    table->ui_data = sheet;

    g_object_ref (sheet);

    /* config the cell-block styles */

    GNCHeaderWidths widths = gnc_header_widths_new ();

    if (state_section && gnc_prefs_get_bool(GNC_PREFS_GROUP_GENERAL, GNC_PREF_SAVE_GEOMETRY))
    {
        GList *node = gnc_table_layout_get_cells (table->layout);
        for (; node; node = node->next)
        {
            auto cell = static_cast<BasicCell *>(node->data);

            if (cell->expandable)
                continue;

            /* Remember whether the column is visible */
            char *key = g_strdup_printf("%s_width", cell->cell_name);
            unsigned int value = g_key_file_get_integer (state_file, state_section, key, nullptr);
            if (value != 0)
                gnc_header_widths_set_width (widths, cell->cell_name, value);
            g_free(key);
        }
    }

    gnucash_sheet_create_styles (sheet);

    gnucash_sheet_set_header_widths (sheet, widths);

    gnucash_sheet_compile_styles (sheet);

    gnucash_sheet_table_load (sheet, TRUE);
    gnucash_sheet_cursor_set_from_table (sheet, TRUE);
    gnucash_sheet_redraw_all (sheet);

    gnc_header_widths_destroy (widths);

    LEAVE(" ");
}


static GtkWidget *
gnucash_register_create_widget (Table *table)
{
    auto reg = static_cast<GnucashRegister *>(
        g_object_new (GNUCASH_TYPE_REGISTER, nullptr)
    );
    GtkWidget *widget = GTK_WIDGET(reg);

    GtkWidget *sheet = gnucash_sheet_new (table);
    reg->sheet = sheet;
    GNUCASH_SHEET(sheet)->reg = widget;

    GtkWidget *header = gnc_header_new (GNUCASH_SHEET(sheet));

    gtk_grid_attach (GTK_GRID(widget), header, 0, 0, 1, 1);
    gtk_widget_set_hexpand (header, TRUE);
    gtk_widget_set_halign (header, GTK_ALIGN_FILL);
    gtk_widget_set_vexpand (header, FALSE);
    gtk_widget_set_valign (header, GTK_ALIGN_FILL);
    g_object_set (header, "margin", 0, nullptr);
    gtk_widget_show (header);

    gtk_grid_attach (GTK_GRID(widget), sheet, 0, 1, 1, 1);
    gtk_widget_set_hexpand (sheet, TRUE);
    gtk_widget_set_halign (sheet, GTK_ALIGN_FILL);
    gtk_widget_set_vexpand (sheet, TRUE);
    gtk_widget_set_valign (sheet, GTK_ALIGN_FILL);
    g_object_set (sheet, "margin", 0, nullptr);
    gtk_widget_show (sheet);

    GtkWidget *scrollbar = gtk_scrollbar_new (GTK_ORIENTATION_VERTICAL, GNUCASH_SHEET(sheet)->vadj);
    gtk_grid_attach (GTK_GRID(widget), GTK_WIDGET(scrollbar), 1, 0, 1, 2);
    gtk_widget_set_hexpand (GTK_WIDGET(scrollbar), FALSE);
    gtk_widget_set_halign (GTK_WIDGET(scrollbar), GTK_ALIGN_FILL);
    gtk_widget_set_vexpand (GTK_WIDGET(scrollbar), TRUE);
    gtk_widget_set_valign (GTK_WIDGET(scrollbar), GTK_ALIGN_FILL);
    g_object_set (GTK_WIDGET(scrollbar), "margin", 0, nullptr);
    gtk_widget_show (scrollbar);
    GNUCASH_SHEET(sheet)->vscrollbar = scrollbar;

    scrollbar = gtk_scrollbar_new (GTK_ORIENTATION_HORIZONTAL, GNUCASH_SHEET(sheet)->hadj);
    gtk_grid_attach (GTK_GRID(widget), GTK_WIDGET(scrollbar), 0, 2, 1, 1);
    gtk_widget_set_hexpand (GTK_WIDGET(scrollbar), TRUE);
    gtk_widget_set_halign (GTK_WIDGET(scrollbar), GTK_ALIGN_FILL);
    gtk_widget_set_vexpand (GTK_WIDGET(scrollbar), FALSE);
    gtk_widget_set_valign (GTK_WIDGET(scrollbar), GTK_ALIGN_FILL);
    g_object_set (GTK_WIDGET(scrollbar), "margin", 0, nullptr);
    reg->hscrollbar = scrollbar;
    gtk_widget_show (reg->hscrollbar);
    reg->hscrollbar_visible = true;
    GNUCASH_SHEET(sheet)->hscrollbar = scrollbar;

    g_signal_connect (GNUCASH_SHEET(sheet)->hadj, "changed",
                      G_CALLBACK (gnucash_register_update_hadjustment), reg);

    return widget;
}


GtkWidget *
gnucash_register_new (Table *table, const gchar *state_section) noexcept
{
    GtkWidget *widget = gnucash_register_create_widget(table);
    GnucashRegister *reg = GNUCASH_REGISTER(widget);

    gnucash_register_configure (GNUCASH_SHEET(reg->sheet), state_section);

    return widget;
}


void gnucash_register_set_moved_cb (GnucashRegister *reg,
                                    GFunc cb, gpointer cb_data) noexcept
{
    if (!reg || !reg->sheet)
        return;
    GnucashSheet *sheet = GNUCASH_SHEET(reg->sheet);
    sheet->moved_cb = cb;
    sheet->moved_cb_data = cb_data;
}


GnucashSheet *gnucash_register_get_sheet (GnucashRegister *reg) noexcept
{
    g_return_val_if_fail (reg != nullptr, nullptr);
    g_return_val_if_fail (GNUCASH_IS_REGISTER(reg), nullptr);

    return GNUCASH_SHEET(reg->sheet);
}


void
gnucash_register_set_open_doclink_cb (GnucashRegister *reg,
                                      GFunc cb, gpointer cb_data) noexcept
{
    if (!reg || !reg->sheet)
        return;
    GnucashSheet *sheet = GNUCASH_SHEET(reg->sheet);
    sheet->open_doclink_cb = cb;
    sheet->open_doclink_cb_data = cb_data;
}


