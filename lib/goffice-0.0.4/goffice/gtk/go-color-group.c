/* vim: set sw=8: -*- Mode: C; tab-width: 8; indent-tabs-mode: t; c-basic-offset: 8 -*- */
/*
 * color-group.c - Utility to keep a shared memory of custom colors
 *                 between arbitrary widgets.
 * Copyright 2000, Michael Levy
 * Copyright 2001, Almer S. Tigelaar
 * Copyright 2004, Jody Goldberg
 *
 * Authors:
 * 	Michael Levy (mlevy@genoscope.cns.fr)
 * Revised and polished by:
 *   Almer S. Tigelaar <almer@gnome.org>
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

#include <goffice/goffice-config.h>
#include "go-color-group.h"
#include <gsf/gsf-impl-utils.h>
#include <string.h>

typedef struct {
	GObjectClass base;

	void (*history_changed) (GOColorGroup *group);
} GOColorGroupClass;

enum {
        HISTORY_CHANGED,
        LAST_SIGNAL
};

static GObjectClass *go_color_group_parent_class;
static guint	     go_color_group_signals [LAST_SIGNAL] = { 0 };
static GHashTable   *go_color_groups = NULL;

static void
go_color_group_finalize (GObject *obj)
{
	GOColorGroup *cg = GO_COLOR_GROUP (obj);

	/* make this name available */
	if (cg->name) {
		g_hash_table_remove (go_color_groups, cg);
		g_free (cg->name);
		cg->name = NULL;
	}

	(go_color_group_parent_class->finalize) (obj);
}

static void
go_color_group_class_init (GOColorGroupClass *klass)
{
	GObjectClass *object_class;

	object_class = (GObjectClass*) klass;

	object_class->finalize = &go_color_group_finalize;
	go_color_group_parent_class = g_type_class_peek (G_TYPE_OBJECT);
	go_color_group_signals [HISTORY_CHANGED] =
		g_signal_new ("history-changed",
			GO_COLOR_GROUP_TYPE,
			G_SIGNAL_RUN_LAST,
			G_STRUCT_OFFSET (GOColorGroupClass, history_changed),
			NULL, NULL,
			g_cclosure_marshal_VOID__VOID,
			G_TYPE_NONE, 0);
}

static void
go_color_group_init (GOColorGroup *cg)
{
	int i;

	cg->name = NULL;
	cg->context = NULL;
	for (i = 0 ; i < GO_COLOR_GROUP_HISTORY_SIZE ; i++)
		cg->history[i] = RGBA_BLACK;
}

GSF_CLASS (GOColorGroup, go_color_group,
	   go_color_group_class_init, go_color_group_init,
	   G_TYPE_OBJECT)

/**
 * go_color_group_find :
 * @name :
 * @context :
 *
 * Look up the name/context specific color-group.  Return NULL if it is not found.
 * No reference is added if it is found.
 */
GOColorGroup *
go_color_group_find (char const *name, gpointer context)
{
	GOColorGroup tmp_key;

	if (go_color_groups == NULL)
		return NULL;

	g_return_val_if_fail(name != NULL, NULL);

	tmp_key.name = (char *)name;
	tmp_key.context = context;
	return (GOColorGroup *) g_hash_table_lookup (go_color_groups, &tmp_key);
	}

static guint
cg_hash (GOColorGroup const *key)
{
	return g_str_hash (key->name);
}

static gint
cg_equal (GOColorGroup const *a, GOColorGroup const *b)
{
	if (a == b)
		return TRUE;
	if (a->context != b->context)
		return FALSE;
	return g_str_equal (a->name, b->name);
}

/**
 * go_color_group_fetch :
 * @name :
 * @context :
 *
 * if name is NULL or a name not currently in use by another group
 * then a new group is created and returned. If name was NULL
 * then the new group is given a unique name prefixed by "__cg_autogen_name__"
 * (thereby insuring namespace separation).
 * If name was already used by a group then the reference count is
 * incremented and a pointer to the group is returned.
 */
GOColorGroup *
go_color_group_fetch (const gchar *name, gpointer context)
{
	GOColorGroup *cg;
	gchar *new_name;

	if (go_color_groups == NULL)
		go_color_groups = g_hash_table_new (
			(GHashFunc) cg_hash, (GEqualFunc) cg_equal);

	if (name == NULL) {
		static gint count = 0;

		while (1) {
			new_name = g_strdup_printf("color_group_number_%i", count++);
			if (go_color_group_find (new_name, context) == NULL)
				break;
			g_free (new_name);
		}
	} else {
		new_name = g_strdup (name);
		cg = go_color_group_find (new_name, context);
	if (cg != NULL) {
		g_free (new_name);
		g_object_ref (G_OBJECT (cg));
		return cg;
	}
	}

	cg = g_object_new (go_color_group_get_type (), NULL);

	g_return_val_if_fail(cg != NULL, NULL);

	cg->name = new_name;
	cg->context = context;

	/* lastly register this name */
	g_hash_table_insert (go_color_groups, cg, cg);

	return cg;
}

/**
 * go_color_group_add_color :
 * @cg : #GOColorGroup
 * @c : the color
 *
 * Potentially slide the history to add the new colour.  If it was already in
 * the history reorder.
 **/
void
go_color_group_add_color (GOColorGroup *cg, GOColor c)
{
	unsigned i;
	g_return_if_fail (IS_GO_COLOR_GROUP (cg));

	for (i = GO_COLOR_GROUP_HISTORY_SIZE ; i-- > 0 ;)
		if (cg->history[i] == c)
			break;
	for ( ; i < GO_COLOR_GROUP_HISTORY_SIZE-1 ; i++)
		cg->history [i] = cg->history [i+1];
	cg->history [GO_COLOR_GROUP_HISTORY_SIZE-1] = c;
	g_signal_emit (G_OBJECT (cg),
		go_color_group_signals [HISTORY_CHANGED], 0);
}
