/* 
 * gnc-main-window.h -- GtkWindow which represents the
 *	GnuCash main window.
 *
 * Copyright (C) 2003 Jan Arne Petersen
 * Author: Jan Arne Petersen <jpetersen@uni-bonn.de>
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

#ifndef __GNC_MAIN_WINDOW_H
#define __GNC_MAIN_WINDOW_H

#include <gtk/gtkwindow.h>

#include "egg-menu-merge.h"

#include "gnc-plugin-page.h"

G_BEGIN_DECLS

/* type macros */
#define GNC_TYPE_MAIN_WINDOW            (gnc_main_window_get_type ())
#define GNC_MAIN_WINDOW(obj)            (G_TYPE_CHECK_INSTANCE_CAST ((obj), GNC_TYPE_MAIN_WINDOW, GncMainWindow))
#define GNC_MAIN_WINDOW_CLASS(klass)    (G_TYPE_CHECK_CLASS_CAST ((klass), GNC_TYPE_MAIN_WINDOW, GncMainWindowClass))
#define GNC_IS_MAIN_WINDOW(obj)         (G_TYPE_CHECK_INSTANCE_TYPE ((obj), GNC_TYPE_MAIN_WINDOW))
#define GNC_IS_MAIN_WINDOW_CLASS(klass) (G_TYPE_CHECK_CLASS_TYPE ((klass), GNC_TYPE_MAIN_WINDOW))
#define GNC_MAIN_WINDOW_GET_CLASS(obj)  (G_TYPE_INSTANCE_GET_CLASS ((obj), GNC_TYPE_MAIN_WINDOW, GncMainWindowClass))

/* typedefs & structures */
typedef struct GncMainWindowPrivate GncMainWindowPrivate;

typedef struct {
	GtkWindow parent;

	EggMenuMerge *ui_merge;

	GncMainWindowPrivate *priv;
} GncMainWindow;

typedef struct {
	GtkWindowClass parent;
} GncMainWindowClass;

typedef struct {
	GncMainWindow *window;
	gpointer data;
} GncMainWindowActionData;

/* function prototypes */
GType           gnc_main_window_get_type          (void);

GncMainWindow  *gnc_main_window_new               (void);

void            gnc_main_window_open_page	  (GncMainWindow *window,
						   GncPluginPage *page);
void            gnc_main_window_close_page	  (GncMainWindow *window,
						   GncPluginPage *page);
GncPluginPage  *gnc_main_window_get_current_page  (GncMainWindow *window);

void            gnc_main_window_merge_actions     (GncMainWindow *window,
						   const gchar *group_name,
						   EggActionEntry *entries,
						   guint n_entries,
						   const gchar *ui_file,
						   gpointer user_data);
void            gnc_main_window_unmerge_actions   (GncMainWindow *window,
						   const gchar *group_name);
EggActionGroup *gnc_main_window_get_action_group  (GncMainWindow *window,
                                                   const gchar *group_name);

G_END_DECLS

#endif /* __GNC_MAIN_WINDOW_H */
