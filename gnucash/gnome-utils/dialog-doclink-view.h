/********************************************************************\
 * dialog-doclink-view.h -- Document link dialog Columnview         *
 * Copyright (C) 2024 Robert Fewell                                 *
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
\********************************************************************/

#ifndef DIALOG_DOCLINK_VIEW_H
#define DIALOG_DOCLINK_VIEW_H

#ifdef __cplusplus
extern "C" {
#endif

enum
{
    PROP_DOCLINK_0,
    PROP_DOCLINK_ITEM_DATE,
    PROP_DOCLINK_ITEM_TIME64,     // used for sorting date_item
    PROP_DOCLINK_INVOICE_ID,
    PROP_DOCLINK_DESCRIPTION,
    PROP_DOCLINK_DISPLAY_URI,
    PROP_DOCLINK_AVAILABLE,
    PROP_DOCLINK_ITEM_POINTER,
    PROP_DOCLINK_URI,
    PROP_DOCLINK_URI_RELATIVE,    // used just for sorting relative_pix
    PROP_DOCLINK_URI_RELATIVE_PIX,

    N_PROPS_DOCLINK
};

#define DOCLINKVIEW_TYPE_ITEM (doclink_view_item_get_type ())
G_DECLARE_FINAL_TYPE (DoclinkViewItem, doclink_view_item, DOCLINKVIEW, ITEM, GObject)

typedef struct _DoclinkViewItem DoclinkViewItem;

struct _DoclinkViewItem {
    GObject   parent_instance;
    gchar    *item_date;
    gint64    item_time64;      // used just for sorting date column
    gchar    *invoice_id;
    gchar    *description;
    gchar    *display_uri;
    gchar    *available;
    gpointer  item_pointer;
    gchar    *uri;
    gboolean  uri_relative;     // used just for sorting relative_pix
    gchar    *uri_relative_pix;
};

GtkWidget * gnc_doclink_create_column_view (GtkWidget *sw, GListModel *model);

#ifdef __cplusplus
}
#endif

#endif
