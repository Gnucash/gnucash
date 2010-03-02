/********************************************************************\
 * table-model.h -- 2D grid table object model                      *
 * Copyright (c) 2001 Free Software Foundation                      *
 * Author: Dave Peticolas <dave@krondo.com>                         *
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

#ifndef TABLE_MODEL_H
#define TABLE_MODEL_H

#include <glib.h>

#include "basiccell.h"
#include "register-common.h"

typedef enum
{
    XACC_CELL_ALLOW_NONE       = 0,
    XACC_CELL_ALLOW_INPUT      = 1 << 0,
    XACC_CELL_ALLOW_SHADOW     = 1 << 1,
    XACC_CELL_ALLOW_ALL        = XACC_CELL_ALLOW_INPUT | XACC_CELL_ALLOW_SHADOW,
    XACC_CELL_ALLOW_EXACT_ONLY = 1 << 2,
    XACC_CELL_ALLOW_ENTER	     = 1 << 3,
    XACC_CELL_ALLOW_READ_ONLY  = XACC_CELL_ALLOW_SHADOW | XACC_CELL_ALLOW_ENTER
} CellIOFlags;

typedef enum
{
    CELL_BORDER_LINE_NONE,
    CELL_BORDER_LINE_LIGHT,
    CELL_BORDER_LINE_NORMAL,
    CELL_BORDER_LINE_HEAVY,
    CELL_BORDER_LINE_HIGHLIGHT
} PhysicalCellBorderLineStyle;

typedef struct
{
    PhysicalCellBorderLineStyle top;
    PhysicalCellBorderLineStyle bottom;
    PhysicalCellBorderLineStyle left;
    PhysicalCellBorderLineStyle right;
} PhysicalCellBorders;

typedef const char * (*TableGetEntryHandler) (VirtualLocation virt_loc,
        gboolean translate,
        gboolean *conditionally_changed,
        gpointer user_data);

typedef const char * (*TableGetLabelHandler) (VirtualLocation virt_loc,
        gpointer user_data);

typedef char * (*TableGetHelpHandler) (VirtualLocation virt_loc,
                                       gpointer user_data);

typedef CellIOFlags (*TableGetCellIOFlagsHandler) (VirtualLocation virt_loc,
        gpointer user_data);

typedef guint32 (*TableGetFGColorHandler) (VirtualLocation virt_loc,
        gpointer user_data);

typedef guint32 (*TableGetBGColorHandler) (VirtualLocation virt_loc,
        gboolean *hatching,
        gpointer user_data);

typedef void (*TableGetCellBorderHandler) (VirtualLocation virt_loc,
        PhysicalCellBorders *borders,
        gpointer user_data);

typedef gboolean (*TableConfirmHandler) (VirtualLocation virt_loc,
        gpointer user_data);

typedef void (*TableSaveCellHandler) (BasicCell * cell,
                                      gpointer save_data,
                                      gpointer user_data);

typedef void (*TableSaveHandler) (gpointer save_data,
                                  gpointer user_data);

typedef gpointer (*VirtCellDataAllocator)   (void);
typedef void     (*VirtCellDataDeallocator) (gpointer cell_data);
typedef void     (*VirtCellDataCopy)        (gpointer to, gconstpointer from);

typedef struct
{
    GHashTable *entry_handlers;
    GHashTable *label_handlers;
    GHashTable *help_handlers;
    GHashTable *io_flags_handlers;
    GHashTable *fg_color_handlers;
    GHashTable *bg_color_handlers;
    GHashTable *cell_border_handlers;
    GHashTable *confirm_handlers;

    GHashTable *save_handlers;
    TableSaveHandler pre_save_handler;
    TableSaveHandler post_save_handler;

    gpointer handler_user_data;

    /* If true, denotes that this table is read-only
     * and edits should not be allowed. */
    gboolean read_only;

    /* If positive, denotes a row that marks a boundary that should
     * be visually distinguished. */
    int dividing_row;

    VirtCellDataAllocator cell_data_allocator;
    VirtCellDataDeallocator cell_data_deallocator;
    VirtCellDataCopy cell_data_copy;
} TableModel;


TableModel * gnc_table_model_new (void);
void         gnc_table_model_destroy (TableModel *model);

void         gnc_table_model_set_read_only (TableModel *model,
        gboolean read_only);
gboolean     gnc_table_model_read_only (TableModel *model);

void gnc_table_model_set_entry_handler
(TableModel *model,
 TableGetEntryHandler entry_handler,
 const char * cell_name);
void gnc_table_model_set_default_entry_handler
(TableModel *model,
 TableGetEntryHandler entry_handler);
TableGetEntryHandler gnc_table_model_get_entry_handler
(TableModel *model,
 const char * cell_name);

void gnc_table_model_set_label_handler
(TableModel *model,
 TableGetLabelHandler label_handler,
 const char * cell_name);
void gnc_table_model_set_default_label_handler
(TableModel *model,
 TableGetLabelHandler label_handler);
TableGetLabelHandler gnc_table_model_get_label_handler
(TableModel *model,
 const char * cell_name);

void gnc_table_model_set_help_handler
(TableModel *model,
 TableGetHelpHandler help_handler,
 const char * cell_name);
void gnc_table_model_set_default_help_handler
(TableModel *model,
 TableGetHelpHandler help_handler);
TableGetHelpHandler gnc_table_model_get_help_handler
(TableModel *model,
 const char * cell_name);

void gnc_table_model_set_io_flags_handler
(TableModel *model,
 TableGetCellIOFlagsHandler io_flags_handler,
 const char * cell_name);
void gnc_table_model_set_default_io_flags_handler
(TableModel *model,
 TableGetCellIOFlagsHandler io_flags_handler);
TableGetCellIOFlagsHandler gnc_table_model_get_io_flags_handler
(TableModel *model,
 const char * cell_name);

void gnc_table_model_set_fg_color_handler
(TableModel *model,
 TableGetFGColorHandler io_flags_handler,
 const char * cell_name);
void gnc_table_model_set_default_fg_color_handler
(TableModel *model,
 TableGetFGColorHandler io_flags_handler);
TableGetFGColorHandler gnc_table_model_get_fg_color_handler
(TableModel *model,
 const char * cell_name);

void gnc_table_model_set_bg_color_handler
(TableModel *model,
 TableGetBGColorHandler io_flags_handler,
 const char * cell_name);
void gnc_table_model_set_default_bg_color_handler
(TableModel *model,
 TableGetBGColorHandler io_flags_handler);
TableGetBGColorHandler gnc_table_model_get_bg_color_handler
(TableModel *model,
 const char * cell_name);

void gnc_table_model_set_cell_border_handler
(TableModel *model,
 TableGetCellBorderHandler io_flags_handler,
 const char * cell_name);
void gnc_table_model_set_default_cell_border_handler
(TableModel *model,
 TableGetCellBorderHandler io_flags_handler);
TableGetCellBorderHandler gnc_table_model_get_cell_border_handler
(TableModel *model,
 const char * cell_name);

void gnc_table_model_set_confirm_handler
(TableModel *model,
 TableConfirmHandler io_flags_handler,
 const char * cell_name);
void gnc_table_model_set_default_confirm_handler
(TableModel *model,
 TableConfirmHandler io_flags_handler);
TableConfirmHandler gnc_table_model_get_confirm_handler
(TableModel *model,
 const char * cell_name);

void gnc_table_model_set_save_handler
(TableModel *model,
 TableSaveCellHandler save_handler,
 const char * cell_name);
void gnc_table_model_set_pre_save_handler
(TableModel *model,
 TableSaveHandler save_handler);
void gnc_table_model_set_post_save_handler
(TableModel *model,
 TableSaveHandler save_handler);
TableSaveCellHandler gnc_table_model_get_save_handler
(TableModel *model,
 const char * cell_name);
TableSaveHandler gnc_table_model_get_pre_save_handler
(TableModel *model);
TableSaveHandler gnc_table_model_get_post_save_handler
(TableModel *model);

#endif
