/*
 * gnc-totd-dialog.h : dialog to display a "tip of the day"
 * Copyright (C) 2003 Jan Arne Petersen
 * Author: Jan Arne Petersen <jpetersen@uni-bonn.de>
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
