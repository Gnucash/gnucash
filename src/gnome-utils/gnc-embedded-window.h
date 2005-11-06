/* 
 * gnc-embedded-window.h -- GtkWindow which represents an
 *	emvedded GnuCash window.
 *
 * Copyright (C) 2003 Jan Arne Petersen <jpetersen@uni-bonn.de>
 * Copyright (C) 2003 David Hampton <hampton@employees.org>
 *
 * This program is free software; you can redistribute it and/or
 * modify it under the terms of the GNU General Public License as
 * published by the Free Software Foundation; either version 2 of
 * the License, or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, contact:
 *
 * Free Software Foundation           Voice:  +1-617-542-5942
 * 59 Temple Place - Suite 330        Fax:    +1-617-542-2652
 * Boston, MA  02111-1307,  USA       gnu@gnu.org
 */

/** @addtogroup Windows
    @{ */
/** @addtogroup GncEmbeddedWindow Embedded Window Functions
    @{ */
/** @file gnc-embedded-window.h
    @brief Functions that are supported by all types of windows.
    @author Copyright (C) 2003 Jan Arne Petersen
    @author Copyright (C) 2003 David Hampton <hampton@employees.org>
*/

#ifndef __GNC_EMBEDDED_WINDOW_H
#define __GNC_EMBEDDED_WINDOW_H

#include <gtk/gtkwindow.h>

#include "gnc-plugin-page.h"

G_BEGIN_DECLS

#define PLUGIN_PAGE_LABEL "plugin-page"

/* type macros */
#define GNC_TYPE_EMBEDDED_WINDOW            (gnc_embedded_window_get_type ())
#define GNC_EMBEDDED_WINDOW(obj)            (G_TYPE_CHECK_INSTANCE_CAST ((obj), GNC_TYPE_EMBEDDED_WINDOW, GncEmbeddedWindow))
#define GNC_EMBEDDED_WINDOW_CLASS(klass)    (G_TYPE_CHECK_CLASS_CAST ((klass), GNC_TYPE_EMBEDDED_WINDOW, GncEmbeddedWindowClass))
#define GNC_IS_EMBEDDED_WINDOW(obj)         (G_TYPE_CHECK_INSTANCE_TYPE ((obj), GNC_TYPE_EMBEDDED_WINDOW))
#define GNC_IS_EMBEDDED_WINDOW_CLASS(klass) (G_TYPE_CHECK_CLASS_TYPE ((klass), GNC_TYPE_EMBEDDED_WINDOW))
#define GNC_EMBEDDED_WINDOW_GET_CLASS(obj)  (G_TYPE_INSTANCE_GET_CLASS ((obj), GNC_TYPE_EMBEDDED_WINDOW, GncEmbeddedWindowClass))

/* typedefs & structures */
typedef struct {
	GtkVBox vbox;
	GtkUIManager   	 *ui_merge;
} GncEmbeddedWindow;

typedef struct {
	GtkVBoxClass vbox;
} GncEmbeddedWindowClass;

/* function prototypes */
GType               gnc_embedded_window_get_type (void);

GncEmbeddedWindow * gnc_embedded_window_new      (const gchar *action_group_name,
						  GtkActionEntry *action_entries,
						  gint n_action_entries,
						  const gchar *ui_filename,
						  GtkWidget *enclosing_win,
						  gboolean add_accelerators,
						  gpointer user_data);

void            gnc_embedded_window_open_page    (GncEmbeddedWindow *window,
						  GncPluginPage *page);
void            gnc_embedded_window_close_page	 (GncEmbeddedWindow *window,
						  GncPluginPage *page);
GncPluginPage  *gnc_embedded_window_get_page     (GncEmbeddedWindow *window);

G_END_DECLS

#endif /* __GNC_EMBEDDED_WINDOW_H */

/** @} */
/** @} */
