/* vim: set sw=8: -*- Mode: C; tab-width: 8; indent-tabs-mode: t; c-basic-offset: 8 -*- */
/*
 * gtk-combo-box.h - a customizable combobox
 * Copyright 2000, 2001, Ximian, Inc.
 *
 * Authors:
 *   Miguel de Icaza <miguel@ximian.com>
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public
 * License, version 2, as published by the Free Software Foundation.
 *
 * This library is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Library General Public License for more details.
 *
 * You should have received a copy of the GNU Library General Public
 * License along with this library; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA  02110-1301
 * USA.
 */

#ifndef _GO_COMBO_BOX_H_
#define _GO_COMBO_BOX_H_

#include <gtk/gtkhbox.h>
#include <gtk/gtktooltips.h>

G_BEGIN_DECLS

#define GO_COMBO_BOX_TYPE	(go_combo_box_get_type())
#define GO_COMBO_BOX(o)		G_TYPE_CHECK_INSTANCE_CAST ((o), GO_COMBO_BOX_TYPE, GOComboBox)
#define IS_GO_COMBO_BOX(o)	G_TYPE_CHECK_INSTANCE_TYPE ((o), GO_COMBO_BOX_TYPE)
#define GO_COMBO_BOX_CLASS(k)	G_TYPE_CHECK_CLASS_CAST ((k), GO_COMBO_BOX_TYPE, GOComboBoxClass)

typedef struct _GOComboBox	  GOComboBox;
typedef struct _GOComboBoxPrivate GOComboBoxPrivate;
typedef struct _GOComboBoxClass   GOComboBoxClass;

struct _GOComboBox {
	GtkHBox hbox;
	GOComboBoxPrivate *priv;
};

struct _GOComboBoxClass {
	GtkHBoxClass	base;

	/* virtual */
	void  (*set_title) (GOComboBox *cbox, char const *title);

	/* invoked when the popup has been hidden, if the signal
	 * returns TRUE, it means it should be killed */
	gboolean   (*pop_down_done)   (GOComboBox *cbox, GtkWidget *);
};

/* public */
GType	    go_combo_box_get_type     (void);
void	    go_combo_box_set_tooltip  (GOComboBox *combo, GtkTooltips *tips,
				       char const *text, char const *priv_text);
void	    go_combo_box_set_relief   (GOComboBox *combo, GtkReliefStyle relief);
void	    go_combo_box_set_title    (GOComboBox *combo, char const *title);
char const *go_combo_box_get_title    (GOComboBox *combo);
void	    go_combo_box_set_tearable (GOComboBox *combo, gboolean tearable);

/* protected */
void go_combo_box_construct	(GOComboBox *combo,
				 GtkWidget  *display_widget,
				 GtkWidget  *popdown_container,
				 GtkWidget  *popdown_focus);
void go_combo_box_get_pos	(GOComboBox *combo, int *x, int *y);
void go_combo_box_popup_hide	(GOComboBox *combo);
void go_combo_box_popup_display	(GOComboBox *combo);
void go_combo_box_set_display	(GOComboBox *combo,
				 GtkWidget *display_widget);
gboolean _go_combo_is_updating (GOComboBox const *combo);

G_END_DECLS

#endif /* _GO_COMBO_BOX_H_ */
