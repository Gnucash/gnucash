/*
 * gnc-commodity-edit.c --  Commodity editor widget
 *
 * Copyright (C) 1997, 1998, 1999, 2000 Free Software Foundation
 * All rights reserved.
 *
 * Gnucash is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * as published by the Free Software Foundation; either version 2 of the
 * License, or (at your option) any later version.
 *
 * Gnucash is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Library General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, contact:
 *
 * Free Software Foundation           Voice:  +1-617-542-5942
 * 51 Franklin Street, Fifth Floor    Fax:    +1-617-542-2652
 * Boston, MA  02110-1301,  USA       gnu@gnu.org
 *
 */
/*
  @NOTATION@
 */

/*
 * Commodity editor widget
 *
 * Authors: Dave Peticolas <dave@krondo.com>
 * 	    Derek Atkins <warlord@MIT.EDU>
 */

#include <config.h>

#include <gtk/gtk.h>

#include "dialog-commodity.h"
#include "gnc-commodity-edit.h"

typedef struct
{
    GNCGeneralSelectAsyncResultCB completed;
    gpointer user_data;
} CommoditySelectCompletion;

const char * gnc_commodity_edit_get_string (gpointer ptr)
{
    gnc_commodity * comm = (gnc_commodity *)ptr;
    return gnc_commodity_get_printname(comm);
}

static void
gnc_commodity_edit_select_completed (QofBook *book, gnc_commodity *commodity,
                                     gpointer user_data)
{
    CommoditySelectCompletion *completion = user_data;
    completion->completed (book ? commodity : NULL, completion->user_data);
    g_free (completion);
}

void
gnc_commodity_edit_new_select_async (
    gpointer arg, gpointer ptr, GtkWidget *toplevel,
    GNCGeneralSelectAsyncResultCB completed, gpointer user_data)
{
    dialog_commodity_mode mode = arg ? *(dialog_commodity_mode *)arg : DIAG_COMM_ALL;
    CommoditySelectCompletion *completion;

    g_return_if_fail (completed != NULL);
    completion = g_new (CommoditySelectCompletion, 1);
    completion->completed = completed;
    completion->user_data = user_data;
    gnc_ui_select_commodity_async_full (
        GNC_COMMODITY (ptr), toplevel, mode, NULL, NULL, NULL, NULL,
        gnc_commodity_edit_select_completed, completion);
}
