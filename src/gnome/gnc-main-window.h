/* 
 * gnc-main-window.h -- GtkWindow which represents the GnuCash main window.
 * Copyright (C) 2003 Jan Arne Petersen
 * Author: Jan Arne Petersen <jpetersen@uni-bonn.de>
 */

#ifndef __GNC_MAIN_WINDOW_H
#define __GNC_MAIN_WINDOW_H

#include <gtk/gtkwindow.h>

#include "egg-menu-merge.h"

#include "gnc-plugin.h"
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

/* function prototypes */
GType          gnc_main_window_get_type          (void);

GncMainWindow *gnc_main_window_new               (void);

void           gnc_main_window_open_page	 (GncMainWindow *window,
						  GncPluginPage *page);
void           gnc_main_window_close_page	 (GncMainWindow *window,
						  GncPluginPage *page);
GncPluginPage *gnc_main_window_get_current_page  (GncMainWindow *window);

G_END_DECLS

#endif /* __GNC_MAIN_WINDOW_H */
