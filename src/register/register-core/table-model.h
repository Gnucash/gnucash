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
 * 59 Temple Place - Suite 330        Fax:    +1-617-542-2652       *
 * Boston, MA  02111-1307,  USA       gnu@gnu.org                   *
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
  XACC_CELL_ALLOW_EXACT_ONLY = 1 << 2
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

typedef CellIOFlags (*TableGetCellIOFlags) (VirtualLocation virt_loc,
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

typedef gpointer (*VirtCellDataAllocator)   (void);
typedef void     (*VirtCellDataDeallocator) (gpointer cell_data);
typedef void     (*VirtCellDataCopy)        (gpointer to, gconstpointer from);

typedef struct
{
  TableGetEntryHandler entry_handler;
  TableGetLabelHandler label_handler;
  TableGetCellIOFlags io_flag_handler;
  TableGetFGColorHandler fg_color_handler;
  TableGetBGColorHandler bg_color_handler;
  TableGetCellBorderHandler cell_border_handler;
  TableConfirmHandler confirm_handler;

  gpointer handler_user_data;

  /* If positive, denotes a row that marks a boundary that should
   * be visually distinguished. */
  int dividing_row;

  VirtCellDataAllocator cell_data_allocator;
  VirtCellDataDeallocator cell_data_deallocator;
  VirtCellDataCopy cell_data_copy;
} TableModel;


TableModel * gnc_table_model_new (void);
void         gnc_table_model_destroy (TableModel *model);

#endif
