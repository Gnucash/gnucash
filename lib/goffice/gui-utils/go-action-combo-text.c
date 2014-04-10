/* vim: set sw=8: -*- Mode: C; tab-width: 8; indent-tabs-mode: t; c-basic-offset: 8 -*- */
/*
 * go-action-combo-text .c: A custom GtkAction to handle lists in menus/toolbars
 *
 * Copyright (C) 2003-2004 Jody Goldberg (jody@gnome.org)
 *
 * This program is free software; you can redistribute it and/or
 * modify it under the terms of version 2 of the GNU General Public
 * License as published by the Free Software Foundation.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307
 * USA
 */
#include <goffice/goffice-config.h>
#include "go-action-combo-text.h"
#include "go-combo-box.h"
#include "go-combo-text.h"
//#include <src/gui-util.h>
#include <gui-util.h>

#include <gtk/gtkaction.h>
#include <gtk/gtktoolitem.h>
#include <gsf/gsf-impl-utils.h>
#include <glib/gi18n.h>

typedef struct {
	GtkToolItem	 base;
	GoComboText	*combo; /* container has a ref, not us */
} GOToolComboText;
typedef GtkToolItemClass GOToolComboTextClass;

#define GO_TOOL_COMBO_TEXT_TYPE		(go_tool_combo_text_get_type ())
#define GO_TOOL_COMBO_TEXT(o)		(G_TYPE_CHECK_INSTANCE_CAST (o, GO_TOOL_COMBO_TEXT_TYPE, GOToolComboText))
#define IS_GO_TOOL_COMBO_TEXT(o)	(G_TYPE_CHECK_INSTANCE_TYPE (o, GO_TOOL_COMBO_TEXT_TYPE))

static GType go_tool_combo_text_get_type (void);
#if 0
static void
go_tool_combo_text_finalize (GObject *obj)
{
	/* Call parent->finalize (obj).  */
}
static gboolean
go_tool_combo_text_create_menu_proxy (GtkToolItem *tool_item)
{
}
static gboolean
go_tool_combo_text_set_tooltip (GtkToolItem *tool_item, GtkTooltips *tooltips,
				char const *tip_text,
				char const *tip_private)
{
}
#endif
static void
go_tool_combo_text_class_init (GtkToolItemClass *tool_item_klass)
{
#if 0
	gobject_klass->finalize		   = go_tool_combo_stack_finalize;
	tool_item_klass->create_menu_proxy = go_tool_combo_stack_create_menu_proxy;
	tool_item_klass->set_tooltip	   = go_tool_combo_stack_set_tooltip;
#endif
}

static GSF_CLASS (GOToolComboText, go_tool_combo_text,
	   go_tool_combo_text_class_init, NULL,
	   GTK_TYPE_TOOL_ITEM)

/*****************************************************************************/

struct _GOActionComboText {
	GtkAction	base;
	GSList		*elements;
	char const 	*largest_elem;
	char *entry_val;
};
typedef struct {
	GtkActionClass	base;
} GOActionComboTextClass;

static GObjectClass *combo_text_parent;

static void
set_entry_val (GOActionComboText *taction, char const *text)
{
	if (taction->entry_val != text) {
		g_free (taction->entry_val);
		taction->entry_val = g_strdup (text);
	}
}

#if 0
static void
go_action_combo_text_connect_proxy (GtkAction *action, GtkWidget *proxy)
{
}

static void
go_action_combo_disconnect_proxy (GtkAction *action,
				  GtkWidget *proxy)
{
}
#endif

static gboolean
cb_entry_changed (GoComboText *ct, char const *text, GOActionComboText *taction)
{
	set_entry_val (taction, text);
	gtk_action_activate (GTK_ACTION (taction));
	return TRUE;
}

static GtkWidget *
go_action_combo_create_tool_item (GtkAction *act)
{
	GOActionComboText *taction = GO_ACTION_COMBO_TEXT (act);
	GOToolComboText *tool = g_object_new (GO_TOOL_COMBO_TEXT_TYPE, NULL);
	GSList *ptr;
	int tmp, w = -1;

	tool->combo = (GoComboText *)go_combo_text_new (NULL);
	if (taction->largest_elem != NULL)
		w = gnm_measure_string (
			gtk_widget_get_pango_context (GTK_WIDGET (tool->combo)),
			go_combo_text_get_entry (tool->combo)->style->font_desc, 
			taction->largest_elem);
	for (ptr = taction->elements; ptr != NULL ; ptr = ptr->next) {
		go_combo_text_add_item	(tool->combo, ptr->data);
		if (taction->largest_elem == NULL) {
			tmp = gnm_measure_string (
				gtk_widget_get_pango_context (GTK_WIDGET (tool->combo)),
				go_combo_text_get_entry (tool->combo)->style->font_desc, 
				ptr->data);
			if (w < tmp)
				w = tmp;
		}
	}
				
	go_combo_box_set_title (GO_COMBO_BOX (tool->combo),
		_(gtk_action_get_name (act)));
	gtk_widget_set_size_request (
		go_combo_text_get_entry (tool->combo), w, -1);
	g_object_set (G_OBJECT (tool), "visible_vertical", FALSE, NULL);

	go_combo_box_set_relief (GO_COMBO_BOX (tool->combo), GTK_RELIEF_NONE);
	go_combo_box_set_tearable (GO_COMBO_BOX (tool->combo), TRUE);
	gtk_container_add (GTK_CONTAINER (tool), GTK_WIDGET (tool->combo));
	gtk_widget_show (GTK_WIDGET (tool->combo));
	gtk_widget_show (GTK_WIDGET (tool));
	g_signal_connect (tool->combo,
		"entry_changed",
		G_CALLBACK (cb_entry_changed), taction);
	return GTK_WIDGET (tool);
}

static void
go_action_combo_text_finalize (GObject *obj)
{
	combo_text_parent->finalize (obj);
}
static void
go_action_combo_text_class_init (GtkActionClass *gtk_act_klass)
{
	GObjectClass *gobject_klass = (GObjectClass *)gtk_act_klass;

	combo_text_parent = g_type_class_peek_parent (gobject_klass);
	gobject_klass->finalize		= go_action_combo_text_finalize;

	gtk_act_klass->create_tool_item = go_action_combo_create_tool_item;
#if 0
	gtk_act_klass->create_menu_item = Use the default
	gtk_act_klass->connect_proxy	= go_action_combo_stack_connect_proxy;
	gtk_act_klass->disconnect_proxy = go_action_combo_stack_disconnect_proxy;
#endif
}

GSF_CLASS (GOActionComboText, go_action_combo_text,
	   go_action_combo_text_class_init, NULL,
	   GTK_TYPE_ACTION)

void
go_action_combo_text_add_item (GOActionComboText *taction, char const *item)
{
	taction->elements = g_slist_append (taction->elements, g_strdup (item));
}

void
go_action_combo_text_set_width (GOActionComboText *taction, char const *largest_elem)
{
	taction->largest_elem = largest_elem;
}

char const *
go_action_combo_text_get_entry (GOActionComboText const *a)
{
	return a->entry_val;
}

void
go_action_combo_text_set_entry (GOActionComboText *taction, char const *text,
				GOActionComboTextSearchDir dir)
{
	GSList *ptr = gtk_action_get_proxies (GTK_ACTION (taction));

	set_entry_val (taction, text);
	for ( ; ptr != NULL ; ptr = ptr->next)
		if (IS_GO_TOOL_COMBO_TEXT (ptr->data))
			go_combo_text_set_text (GO_TOOL_COMBO_TEXT (ptr->data)->combo, text, dir);
}
