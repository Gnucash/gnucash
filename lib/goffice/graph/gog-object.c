/* vim: set sw=8: -*- Mode: C; tab-width: 8; indent-tabs-mode: t; c-basic-offset: 8 -*- */
/*
 * gog-object.c :
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
#include <goffice/graph/gog-object.h>
#include <goffice/graph/gog-graph-impl.h> /* for gog_graph_request_update */
#include <goffice/graph/gog-data-set.h>
#include <goffice/graph/go-data.h>

#include <gsf/gsf-impl-utils.h>
#include <glib/gi18n.h>
#include <string.h>
#include <stdlib.h>

enum {
	CHILD_ADDED,
	CHILD_REMOVED,
	CHILD_NAME_CHANGED,
	CHILDREN_REORDERED,
	NAME_CHANGED,
	CHANGED,
	LAST_SIGNAL
};
static gulong gog_object_signals [LAST_SIGNAL] = { 0, };

static GObjectClass *parent_klass;
static void
gog_object_finalize (GObject *gobj)
{
	GogObject *obj = GOG_OBJECT (gobj);

	g_free (obj->user_name); obj->user_name = NULL;
	g_free (obj->id); obj->id = NULL;

	g_slist_foreach (obj->children, (GFunc) g_object_unref, NULL);
	g_slist_free (obj->children);
	obj->children = NULL;

	(parent_klass->finalize) (gobj);
}

static void
gog_object_parent_changed (GogObject *child, gboolean was_set)
{
	GSList *ptr = child->children;
	for (; ptr != NULL ; ptr = ptr->next) {
		GogObjectClass *klass = GOG_OBJECT_GET_CLASS (ptr->data);
		(*klass->parent_changed) (ptr->data, was_set);
	}

	if (IS_GOG_DATASET (child))
		gog_dataset_parent_changed (GOG_DATASET (child), was_set);
}

static void
gog_object_class_init (GObjectClass *klass)
{
	GogObjectClass *gog_klass = (GogObjectClass *)klass;
	parent_klass = g_type_class_peek_parent (klass);
	klass->finalize = gog_object_finalize;
	gog_klass->parent_changed = gog_object_parent_changed;

	gog_object_signals [CHILD_ADDED] = g_signal_new ("child-added",
		G_TYPE_FROM_CLASS (klass),
		G_SIGNAL_RUN_LAST,
		G_STRUCT_OFFSET (GogObjectClass, child_added),
		NULL, NULL,
		g_cclosure_marshal_VOID__OBJECT,
		G_TYPE_NONE,	1, G_TYPE_OBJECT);
	gog_object_signals [CHILD_REMOVED] = g_signal_new ("child-removed",
		G_TYPE_FROM_CLASS (klass),
		G_SIGNAL_RUN_LAST,
		G_STRUCT_OFFSET (GogObjectClass, child_removed),
		NULL, NULL,
		g_cclosure_marshal_VOID__OBJECT,
		G_TYPE_NONE,	1, G_TYPE_OBJECT);
	gog_object_signals [CHILD_NAME_CHANGED] = g_signal_new ("child-name-changed",
		G_TYPE_FROM_CLASS (gog_klass),
		G_SIGNAL_RUN_LAST,
		G_STRUCT_OFFSET (GogObjectClass, child_name_changed),
		NULL, NULL,
		g_cclosure_marshal_VOID__OBJECT,
		G_TYPE_NONE,	1, G_TYPE_OBJECT);
	gog_object_signals [CHILDREN_REORDERED] = g_signal_new ("children-reordered",
		G_TYPE_FROM_CLASS (klass),
		G_SIGNAL_RUN_LAST,
		G_STRUCT_OFFSET (GogObjectClass, children_reordered),
		NULL, NULL,
		g_cclosure_marshal_VOID__VOID,
		G_TYPE_NONE, 0);
	gog_object_signals [NAME_CHANGED] = g_signal_new ("name-changed",
		G_TYPE_FROM_CLASS (klass),
		G_SIGNAL_RUN_LAST,
		G_STRUCT_OFFSET (GogObjectClass, name_changed),
		NULL, NULL,
		g_cclosure_marshal_VOID__VOID,
		G_TYPE_NONE, 0);
	gog_object_signals [CHANGED] = g_signal_new ("changed",
		G_TYPE_FROM_CLASS (klass),
		G_SIGNAL_RUN_LAST,
		G_STRUCT_OFFSET (GogObjectClass, changed),
		NULL, NULL,
		g_cclosure_marshal_VOID__BOOLEAN,
		G_TYPE_NONE, 1, G_TYPE_BOOLEAN);
}

static void
gog_object_init (GogObject *obj)
{
	obj->children = NULL;
	obj->user_name = NULL;
	obj->id = NULL;
	obj->needs_update = FALSE;
	obj->being_updated = FALSE;
}

GSF_CLASS (GogObject, gog_object,
	   gog_object_class_init, gog_object_init,
	   G_TYPE_OBJECT)

static char *
gog_object_generate_name (GogObject *obj)
{
	GogObjectClass *klass = GOG_OBJECT_GET_CLASS (obj);
	GogObject *tmp;
	char const *type_name;
	unsigned name_len, i, max_index = 0;
	GSList *ptr;

	g_return_val_if_fail (klass != NULL, NULL);
	g_return_val_if_fail (obj->role != NULL, NULL);

	switch (obj->role->naming_conv) {
	default :
	case GOG_OBJECT_NAME_MANUALLY :
		g_warning ("Role %s should not be autogenerating names",
			   obj->role->id);

	case GOG_OBJECT_NAME_BY_ROLE :
		g_return_val_if_fail (obj->role != NULL, NULL);
		type_name = _(obj->role->id);
		break;

	case GOG_OBJECT_NAME_BY_TYPE :
		g_return_val_if_fail (klass->type_name != NULL, NULL);
		type_name = _((*klass->type_name) (obj));
		break;
	}

	g_return_val_if_fail (type_name != NULL, NULL);
	name_len = strlen (type_name);

	for (ptr = obj->parent->children; ptr != NULL ; ptr = ptr->next) {
		tmp = GOG_OBJECT (ptr->data);
		if (tmp->id != NULL &&
		    0 == strncmp (type_name, tmp->id, name_len)) {
			i = strtol (tmp->id + name_len, NULL, 10);
			if (max_index < i)
				max_index = i;
		}
	}
	return g_strdup_printf ("%s%d", type_name, max_index + 1);
}

/**
 * gog_object_dup :
 * @src : #GogObject
 * @new_parent : #GogObject the parent tree for the object (can be NULL)
 *
 * Create a deep copy of @obj using @new_parent as its parent.
 **/
GogObject *
gog_object_dup (GogObject const *src, GogObject *new_parent)
{
	gint	     n, last;
	GParamSpec **props;
	GogObject   *dst = NULL;
	GSList      *ptr;
	GValue	     val = { 0 };

	if (src == NULL)
		return NULL;

	g_return_val_if_fail (GOG_OBJECT (src) != NULL, NULL);

	if (src->role == NULL || src->explicitly_typed_role)
		dst = g_object_new (G_OBJECT_TYPE (src), NULL);
	if (new_parent)
		dst = gog_object_add_by_role (new_parent, src->role, dst);

	dst->position = src->position;
	/* properties */
	props = g_object_class_list_properties (G_OBJECT_GET_CLASS (src), &n);
	while (n-- > 0)
		if (props[n]->flags & GOG_PARAM_PERSISTENT) {
			g_value_init (&val, props[n]->value_type);
			g_object_get_property (G_OBJECT (src), props[n]->name, &val);
			g_object_set_property (G_OBJECT (dst), props[n]->name, &val);
			g_value_unset (&val);
		}
	g_free (props);

	if (IS_GOG_DATASET (src)) {	/* convenience to save data */
		GogDataset const *src_set = GOG_DATASET (src);
		GogDataset *dst_set = GOG_DATASET (dst);

		gog_dataset_dims (src_set, &n, &last);
		for ( ; n <= last ; n++)
			gog_dataset_set_dim (dst_set, n,
				go_data_dup (gog_dataset_get_dim (src_set, n)),
				NULL);
	}

	for (ptr = src->children; ptr != NULL ; ptr = ptr->next)
		/* children added directly to new parent, no need to use the
		 * function result */
		gog_object_dup (ptr->data, dst);

	return dst;
}

/**
 * gog_object_get_parent :
 * @obj : a #GogObject
 *
 * Returns @obj's parent, potentially NULL if it has not been added to a
 * heirarchy yet.  does not change ref-count in any way.
 **/
GogObject *
gog_object_get_parent (GogObject const *obj)
{
	g_return_val_if_fail (GOG_OBJECT (obj) != NULL, NULL);
	return obj->parent;
}

/**
 * gog_object_get_parent_typed :
 * @obj : a #GogObject
 * @type : a #GType
 *
 * Returns @obj's parent of type @type, potentially NULL if it has not been
 * added to a heirarchy yet or none of the parents are of type @type.
 **/
GogObject *
gog_object_get_parent_typed (GogObject const *obj, GType t)
{
	g_return_val_if_fail (GOG_OBJECT (obj) != NULL, NULL);

	for (; obj != NULL ; obj = obj->parent)
		if (G_TYPE_CHECK_INSTANCE_TYPE (obj, t))
			return GOG_OBJECT (obj); /* const cast */
	return NULL;
}

/**
 * gog_object_get_graph :
 * @obj : const * #GogObject
 *
 * Returns the parent graph.
 **/
GogGraph *
gog_object_get_graph (GogObject const *obj)
{
	g_return_val_if_fail (GOG_OBJECT (obj) != NULL, NULL);

	for (; obj != NULL ; obj = obj->parent)
		if (IS_GOG_GRAPH (obj))
			return GOG_GRAPH (obj);
	return NULL;
}

GogTheme *
gog_object_get_theme (GogObject const *obj)
{
	GogGraph *graph = gog_object_get_graph (obj);

	return (graph != NULL) ? gog_graph_get_theme (graph) : NULL;
}

/**
 * gog_object_get_name :
 * @obj : a #GogObject
 *
 * No need to free the result
 **/
char const *
gog_object_get_name (GogObject const *obj)
{
	g_return_val_if_fail (GOG_OBJECT (obj) != NULL, NULL);
	return (obj->user_name != NULL && *obj->user_name != '\0') ? obj->user_name : obj->id;
}

/**
 * gog_object_set_name :
 * @obj : #GogObject
 * @name :
 * @err : #GError
 *
 * Assign the new name and signals that it has changed.
 * NOTE : it _absorbs_ @name rather than copying it, and generates a new name
 * if @name == NULL
 **/
void
gog_object_set_name (GogObject *obj, char *name, GError **err)
{
	GogObject *tmp;

	g_return_if_fail (GOG_OBJECT (obj) != NULL);

	if (obj->user_name == name)
		return;
	g_free (obj->user_name);
	obj->user_name = name;

	g_signal_emit (G_OBJECT (obj),
		gog_object_signals [NAME_CHANGED], 0);

	for (tmp = obj; tmp != NULL ; tmp = tmp->parent)
		g_signal_emit (G_OBJECT (tmp),
			gog_object_signals [CHILD_NAME_CHANGED], 0, obj);
}

/**
 * gog_object_get_children :
 * @obj : a #GogObject
 * @filter : an optional #GogObjectRole to use as a filter
 *
 * The list needs to be Freed
 **/
GSList *
gog_object_get_children (GogObject const *obj, GogObjectRole const *filter)
{
	GSList *ptr, *res = NULL;

	g_return_val_if_fail (GOG_OBJECT (obj) != NULL, NULL);

	if (filter == NULL)
		return g_slist_copy (obj->children);

	for (ptr = obj->children ; ptr != NULL ; ptr = ptr->next)
		if (GOG_OBJECT (ptr->data)->role == filter)
			res = g_slist_prepend (res, ptr->data);
	return g_slist_reverse (res);
}

/**
 * gog_object_get_child_by_role :
 * @obj : a #GogObject
 * @role : a #GogObjectRole to use as a filter
 *
 * A convenience routine to handle a unique child
 * Returns NULL and spews an error if there is more than one.
 **/
GogObject *
gog_object_get_child_by_role (GogObject const *obj, GogObjectRole const *role)
{
	GogObject *res = NULL;
	GSList *children = gog_object_get_children (obj, role);

	if (children != NULL && children->next == NULL)
		res = children->data;
	g_slist_free (children);
	return res;
}

/**
 * gog_object_is_deletable :
 * @obj : a #GogObject
 *
 * Can the specified @obj be deleted ?
 **/
gboolean
gog_object_is_deletable (GogObject const *obj)
{
	g_return_val_if_fail (GOG_OBJECT (obj) != NULL, FALSE);

	if (IS_GOG_GRAPH (obj))
		return FALSE;

	return obj->role == NULL || obj->role->can_remove == NULL ||
		(obj->role->can_remove) (obj);
}

struct possible_add_closure {
	GSList *res;
	GogObject const *parent;
};

static void
cb_collect_possible_additions (char const *name, GogObjectRole const *role,
			       struct possible_add_closure *data)
{
	if (role->can_add == NULL || (role->can_add) (data->parent))
		data->res = g_slist_prepend (data->res, (gpointer)role);
}

static int
gog_object_position_cmp (GogObjectPosition pos)
{
	if (pos & GOG_POSITION_COMPASS)
		return 0;
	if (pos == GOG_POSITION_SPECIAL)
		return 2;
	return 1; /* GOG_POSITION_MANUAL */
}

static int
gog_role_cmp (GogObjectRole const *a, GogObjectRole const *b)
{
	int index_a = gog_object_position_cmp (a->allowable_positions);
	int index_b = gog_object_position_cmp (b->allowable_positions);

	/* intentionally reverse to put SPECIAL at the top */
	if (index_a < index_b)
		return 1;
	else if (index_a > index_b)
		return -1;
	return b->priority - a->priority;
}

static int
gog_role_cmp_full (GogObjectRole const *a, GogObjectRole const *b)
{
	int res = gog_role_cmp (a, b);
	if (res != 0)
		return res;
	return g_utf8_collate (a->id, b->id);
}

/**
 * gog_object_possible_additions :
 * @parent : a #GogObject
 *
 * returns a list of GogObjectRoles that could be added
 *
 * The resulting list needs to be freed
 **/
GSList *
gog_object_possible_additions (GogObject const *parent)
{
	GogObjectClass *klass = GOG_OBJECT_GET_CLASS (parent);
	g_return_val_if_fail (klass != NULL, NULL);

	if (klass->roles != NULL) {
		struct possible_add_closure data;
		data.res = NULL;
		data.parent = parent;

		g_hash_table_foreach (klass->roles,
			(GHFunc) cb_collect_possible_additions, &data);

		return g_slist_sort (data.res, (GCompareFunc) gog_role_cmp_full);
	}

	return NULL;
}

/**
 * gog_object_can_reorder :
 * @obj : #GogObject
 * @inc_ok : possibly NULL pointer.
 * @dec_ok : possibly NULL pointer.
 *
 * If @obj can move forward or backward in its parents child list
 **/
void
gog_object_can_reorder (GogObject const *obj, gboolean *inc_ok, gboolean *dec_ok)
{
	GogObject const *parent;
	GSList *ptr;

	g_return_if_fail (GOG_OBJECT (obj) != NULL);

	if (inc_ok != NULL)
		*inc_ok = FALSE;
	if (dec_ok != NULL)
		*dec_ok = FALSE;

	if (obj->parent == NULL || gog_object_get_graph (obj) == NULL)
		return;
	parent = obj->parent;
	ptr = parent->children;

	g_return_if_fail (ptr != NULL);

	/* find a pointer to the previous sibling */
	if (ptr->data != obj) {
		while (ptr->next != NULL && ptr->next->data != obj)
			ptr = ptr->next;

		g_return_if_fail (ptr->next != NULL);

		if (inc_ok != NULL &&
		    !gog_role_cmp (((GogObject *)ptr->data)->role, obj->role))
			*inc_ok = TRUE;

		ptr = ptr->next;
	}

	/* ptr now points at @obj */
	if (dec_ok != NULL && ptr->next != NULL &&
	    !gog_role_cmp (obj->role, ((GogObject *)ptr->next->data)->role))
		*dec_ok = TRUE;
}

/**
 * gog_object_reorder :
 * @obj : #GogObject
 * @inc :
 * @goto_max :
 *
 * Returns the object just before @obj in the new ordering.
 **/
GogObject *
gog_object_reorder (GogObject const *obj, gboolean inc, gboolean goto_max)
{
	GogObject *parent, *obj_follows;
	GSList **ptr, *tmp;

	g_return_val_if_fail (GOG_OBJECT (obj) != NULL, NULL);

	if (obj->parent == NULL || gog_object_get_graph (obj) == NULL)
		return NULL;
	parent = obj->parent;

	if (inc)
		parent->children = g_slist_reverse (parent->children);

	for (ptr = &parent->children; *ptr != NULL && (*ptr)->data != obj ;)
		ptr = &(*ptr)->next;

	g_return_val_if_fail (*ptr != NULL, NULL);
	g_return_val_if_fail ((*ptr)->next != NULL, NULL);

	tmp = *ptr;
	*ptr = tmp->next;
	ptr = &(*ptr)->next;

	while (goto_max && *ptr != NULL &&
	       !gog_role_cmp (obj->role, ((GogObject *)((*ptr)->data))->role))
		ptr = &(*ptr)->next;

	tmp->next = *ptr;
	*ptr = tmp;

	if (inc)
		parent->children = g_slist_reverse (parent->children);

	if (parent->children->data != obj) {
		for (tmp = parent->children ; tmp->next->data != obj ; )
			tmp = tmp->next;
		obj_follows = tmp->data;
	} else
		obj_follows = NULL;

	/* Pass the sibling that precedes obj, or NULL if is the head */
	g_signal_emit (G_OBJECT (parent),
		gog_object_signals [CHILDREN_REORDERED], 0);
	gog_object_emit_changed (parent, TRUE);

	return obj_follows;
}

/**
 * gog_object_get_editor :
 * @obj   : #GogObject
 * @dalloc : #GogDataAllocator
 * @cc     : #GnmCmdContext
 *
 **/
gpointer
gog_object_get_editor (GogObject *obj, GogDataAllocator *dalloc,
		       GnmCmdContext *cc)
{
	GogObjectClass *klass = GOG_OBJECT_GET_CLASS (obj);
	g_return_val_if_fail (klass != NULL, NULL);
	if (klass->editor) {
		/* If there are pending updates do them before creating the editor
		 * to avoid expensive widget changes later */
		gog_graph_force_update (gog_object_get_graph (obj));
		return (*klass->editor) (obj, dalloc, cc);
	}
	return NULL;
}

/**
 * gog_object_new_view :
 * @obj : a #GogObject
 * @data :
 **/
GogView *
gog_object_new_view (GogObject const *obj, GogView *parent)
{
	GogObjectClass *klass = GOG_OBJECT_GET_CLASS (obj);

	g_return_val_if_fail (klass != NULL, NULL);

	if (klass->view_type != 0)
		/* set model before parent */
		return g_object_new (klass->view_type,
			"model", obj,
			"parent", parent,
			NULL);

	return NULL;
}

void
gog_object_update (GogObject *obj)
{
	GogObjectClass *klass = GOG_OBJECT_GET_CLASS (obj);
	GSList *ptr;

	g_return_if_fail (klass != NULL);

	ptr = obj->children; /* depth first */
	for (; ptr != NULL ; ptr = ptr->next)
		gog_object_update (ptr->data);

	if (obj->needs_update) {
		obj->needs_update = FALSE;
		obj->being_updated = TRUE;
		gog_debug (0, g_warning ("updating %s (%p)", G_OBJECT_TYPE_NAME (obj), obj););
		if (klass->update != NULL)
			(*klass->update) (obj);
		obj->being_updated = FALSE;
	}
}

gboolean
gog_object_request_update (GogObject *obj)
{
	GogGraph *graph;
	g_return_val_if_fail (GOG_OBJECT (obj), FALSE);
	g_return_val_if_fail (!obj->being_updated, FALSE);

	if (obj->needs_update)
		return FALSE;

	graph = gog_object_get_graph (obj);
	if (graph == NULL) /* we are not linked into a graph yet */
		return FALSE;

	gog_graph_request_update (graph);
	obj->needs_update = TRUE;

	return TRUE;
}

void
gog_object_emit_changed (GogObject *obj, gboolean resize)
{
	GogObjectClass *gog_klass;
	
	g_return_if_fail (GOG_OBJECT (obj));

	gog_klass = GOG_OBJECT_GET_CLASS (obj);

	if (gog_klass->use_parent_as_proxy) {
		obj = obj->parent;
		if (obj != NULL) {
			g_return_if_fail (IS_GOG_OBJECT (obj));
			gog_object_emit_changed (obj, resize);
		}
		return;
	}
	g_signal_emit (G_OBJECT (obj),
		gog_object_signals [CHANGED], 0, resize);
}

/******************************************************************************/

/**
 * gog_object_clear_parent :
 * @obj : #GogObject
 *
 * Does _not_ unref the child, which in effect adds a ref by freeing up the ref
 * previously associated with the parent.
 **/
gboolean
gog_object_clear_parent (GogObject *obj)
{
	GogObjectClass *klass = GOG_OBJECT_GET_CLASS (obj);
	GogObject *parent;

	g_return_val_if_fail (GOG_OBJECT (obj), FALSE);
	g_return_val_if_fail (obj->parent != NULL, FALSE);
	g_return_val_if_fail (gog_object_is_deletable (obj), FALSE);

	parent = obj->parent;
	g_signal_emit (G_OBJECT (parent),
		gog_object_signals [CHILD_REMOVED], 0, obj);
	(*klass->parent_changed) (obj, FALSE);

	if (obj->role != NULL && obj->role->pre_remove != NULL)
		(obj->role->pre_remove) (parent, obj);

	parent->children = g_slist_remove (parent->children, obj);
	obj->parent = NULL;

	if (obj->role != NULL && obj->role->post_remove != NULL)
		(obj->role->post_remove) (parent, obj);

	obj->role = NULL;

	return TRUE;
}

/**
 * gog_object_set_parent :
 * @child  : #GogObject.
 * @parent : #GogObject.
 * @id : optionally %NULL.
 * @role : a static string that can be sent to @parent::add
 *
 * Absorbs a ref to @child
 **/
gboolean
gog_object_set_parent (GogObject *child, GogObject *parent,
		       GogObjectRole const *role, char *id)
{
	GogObjectClass *klass = GOG_OBJECT_GET_CLASS (child);
	GSList **step;

	g_return_val_if_fail (GOG_OBJECT (child), FALSE);
	g_return_val_if_fail (child->parent == NULL, FALSE);
	g_return_val_if_fail (role != NULL, FALSE);

	child->parent	= parent;
	child->role	= role;
	child->position = role->default_position;

	/* Insert sorted based on hokey little ordering */
	step = &parent->children;
	while (*step != NULL &&
	       gog_role_cmp_full (GOG_OBJECT ((*step)->data)->role, role) >= 0)
		step = &((*step)->next);
	*step = g_slist_prepend (*step, child);

	g_free (child->id);
	g_free (child->user_name);
	child->id = (id != NULL) ? id : gog_object_generate_name (child);
	if (child->id == NULL) child->id = g_strdup ("BROKEN");

	if (role->post_add != NULL)
		(role->post_add) (parent, child);
	(*klass->parent_changed) (child, TRUE);

	g_signal_emit (G_OBJECT (parent),
		gog_object_signals [CHILD_ADDED], 0, child);

	return TRUE;
}

GogObject *
gog_object_add_by_role (GogObject *parent, GogObjectRole const *role, GogObject *child)
{
	GType is_a;
	gboolean const explicitly_typed_role = (child != NULL);

	g_return_val_if_fail (role != NULL, NULL);
	g_return_val_if_fail (GOG_OBJECT (parent) != NULL, NULL);

	is_a = g_type_from_name (role->is_a_typename);

	g_return_val_if_fail (is_a != 0, NULL);

	if (child == NULL)
		child = (role->allocate)
			? (role->allocate) (parent)
			: g_object_new (is_a, NULL);

	g_return_val_if_fail (G_TYPE_CHECK_INSTANCE_TYPE (child, is_a), NULL);
	child->explicitly_typed_role = explicitly_typed_role;
	if (gog_object_set_parent (child, parent, role, NULL))
		return child;
	g_object_unref (child);
	return NULL;
}

/**
 * gog_object_add_by_name :
 * @parent : #GogObject
 * @role :
 * @child : optionally null #GogObject
 *
 * Returns a newly created child of @parent in @role.  If @child is provided,
 * it is assumed to be an unaffiliated object that will be assigned in @role.
 * On failure return NULL.
 **/
GogObject *
gog_object_add_by_name (GogObject *parent,
			char const *role, GogObject *child)
{
	return gog_object_add_by_role (parent,
		gog_object_find_role_by_name (parent, role), child);
}

GogObjectPosition
gog_object_get_pos (GogObject const *obj)
{
	g_return_val_if_fail (GOG_OBJECT (obj) != NULL, GOG_POSITION_SPECIAL);
	return obj->position;
}

/**
 * gog_object_set_pos :
 * @obj : #GogObject
 * @pos : #GogObjectPosition
 *
 * Attempts to set the position of @obj to @pos.
 * Returns TRUE the new position is permitted.
 **/
gboolean
gog_object_set_pos (GogObject *obj, GogObjectPosition pos)
{
	g_return_val_if_fail (GOG_OBJECT (obj) != NULL, FALSE);
	g_return_val_if_fail (obj->role != NULL, FALSE);

	if (obj->position == pos)
		return TRUE;

	if ((obj->role->allowable_positions & pos) !=
	    (pos & (GOG_POSITION_COMPASS|GOG_POSITION_ANY_MANUAL)))
		return FALSE;
	obj->position = pos;
	gog_object_emit_changed (obj, TRUE);
	return TRUE;
}

GogObjectRole const *
gog_object_find_role_by_name (GogObject const *obj, char const *role)
{
	GogObjectClass *klass = GOG_OBJECT_GET_CLASS (obj);

	g_return_val_if_fail (klass != NULL, NULL);

	return g_hash_table_lookup (klass->roles, role);
}

void
gog_object_register_roles (GogObjectClass *klass,
			   GogObjectRole const *roles, unsigned n_roles)
{
	unsigned i;
	if (klass->roles == NULL)
		klass->roles = g_hash_table_new (g_str_hash, g_str_equal);

	for (i = 0 ; i < n_roles ; i++) {
		g_return_if_fail (g_hash_table_lookup (klass->roles,
			(gpointer )roles[i].id) == NULL);
		g_hash_table_replace (klass->roles,
			(gpointer )roles[i].id, (gpointer) (roles + i));
	}
}
