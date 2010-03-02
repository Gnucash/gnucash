/*
 * gnc-window.h -- structure which represents a GnuCash window.
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
 * 51 Franklin Street, Fifth Floor    Fax:    +1-617-542-2652
 * Boston, MA  02110-1301,  USA       gnu@gnu.org
 */

/** @addtogroup Windows
    @{ */
/** @addtogroup GncWindow Common Window Functions
    @{ */
/** @file gnc-window.h
    @brief Functions that are supported by all types of windows.
    @author Copyright (C) 2003 Jan Arne Petersen
    @author Copyright (C) 2003 David Hampton <hampton@employees.org>

    GnuCash has two types of "windows" that can show Plugin Pages.
    The first is called a "Main Window" and is implemented on top of a
    GtkWindow.  The second is called an "Embedded Window" and is
    implemented on top of a GtkVBox.  The functions in this file will
    work with either type of window.
*/

#ifndef __GNC_WINDOW_H
#define __GNC_WINDOW_H

#include <gtk/gtkwindow.h>
#include "gnc-plugin-page.h"

G_BEGIN_DECLS

/* type macros */
#define GNC_TYPE_WINDOW            (gnc_window_get_type ())
#define GNC_WINDOW(o)            (G_TYPE_CHECK_INSTANCE_CAST ((o), GNC_TYPE_WINDOW, GncWindow))
#define GNC_IS_WINDOW(o)         (G_TYPE_CHECK_INSTANCE_TYPE ((o), GNC_TYPE_WINDOW))
#define GNC_WINDOW_GET_IFACE(o)  (G_TYPE_INSTANCE_GET_INTERFACE ((o), GNC_TYPE_WINDOW, GncWindowIface))

/* typedefs & structures */
typedef struct GncWindow GncWindow; /* dummy typedef */

typedef struct
{
    GTypeInterface parent;

    /* Virtual Table */
    GtkWindow * (* get_gtk_window) (GncWindow *window);
    GtkWidget * (* get_statusbar) (GncWindow *window);
    GtkWidget * (* get_progressbar) (GncWindow *window);
    void (* ui_set_sensitive) (GncWindow *window, gboolean sensitive);
} GncWindowIface;

/* function prototypes */
GType          gnc_window_get_type (void);

GtkWindow     *gnc_window_get_gtk_window (GncWindow *window);

void           gnc_window_update_status (GncWindow *window, GncPluginPage *page);
void           gnc_window_set_status (GncWindow *window, GncPluginPage *page, const gchar *message);

void           gnc_window_set_progressbar_window (GncWindow *window);
GncWindow     *gnc_window_get_progressbar_window (void);
void           gnc_window_show_progress (const char *message, double percentage);

G_END_DECLS

#endif /* __GNC_WINDOW_H */

/** @} */
/** @} */
