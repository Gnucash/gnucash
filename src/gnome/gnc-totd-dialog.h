/*
 * gnc-totd-dialog.h : dialog to display a "tip of the day"
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

#ifndef DIALOG_TOTD_H
#define DIALOG_TOTD_H

#include <gtk/gtkdialog.h>

G_BEGIN_DECLS

/* type macros */
#define GNC_TYPE_TOTD_DIALOG            (gnc_totd_dialog_get_type ())
#define GNC_TOTD_DIALOG(obj)            (G_TYPE_CHECK_INSTANCE_CAST ((obj), GNC_TYPE_TOTD_DIALOG, GncTotdDialog))
#define GNC_TOTD_DIALOG_CLASS(klass)    (G_TYPE_CHECK_CLASS_CAST ((klass), GNC_TYPE_TOTD_DIALOG, GncTotdDialogClass))
#define GNC_IS_TOTD_DIALOG(obj)         (G_TYPE_CHECK_INSTANCE_TYPE ((obj), GNC_TYPE_TOTD_DIALOG))
#define GNC_IS_TOTD_DIALOG_CLASS(klass) (G_TYPE_CHECK_CLASS_TYPE ((klass), GNC_TYPE_TOTD_DIALOG))
#define GNC_TOTD_DIALOG_GET_CLASS(obj)  (G_TYPE_INSTANCE_GET_CLASS ((obj), GNC_TYPE_TOTD_DIALOG, GncTotdDialogClass))

/* typedefs & structures */
typedef struct GncTotdDialogPrivate GncTotdDialogPrivate;

typedef struct {
	GtkDialog parent;

	GncTotdDialogPrivate *priv;
} GncTotdDialog;

typedef struct {
	GtkDialogClass parent;

	/* Signals */
	void (* close) (GncTotdDialog *dialog);
} GncTotdDialogClass;

/* function prototypes */
GType      gnc_totd_dialog_get_type       (void);

GtkWidget *gnc_totd_dialog_new            (GtkWindow *parent);

void       gnc_totd_dialog_create_and_run (void);

G_END_DECLS

#endif 
