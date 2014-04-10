/* vim: set sw=8: -*- Mode: C; tab-width: 8; indent-tabs-mode: t; c-basic-offset: 8 -*- */
/*
 * gog-object.h : 
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
 * Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA  02110-1301
 * USA
 */
#ifndef GOG_OBJECT_H
#define GOG_OBJECT_H

#include <goffice/graph/goffice-graph.h>
#include <goffice/app/goffice-app.h>
#include <glib-object.h>
#include <libart_lgpl/art_rect.h>

G_BEGIN_DECLS

typedef struct {
	char const	*label;		/* label for notebook page */
	gpointer 	 widget;	/* GtkWidget* */	
} GogEditorPage;

typedef struct {
	unsigned	*store_page;	/* pointer to a place for storing last edited page */
	GSList		*pages;		/* GogEditorPage */
} GogEditor;

GogEditor	*gog_editor_new 		 (void);
void		 gog_editor_add_page 		 (GogEditor *editor, gpointer widget, char const *label);
void 		 gog_editor_set_store_page  	 (GogEditor *editor, unsigned *store_page);
gpointer 	 gog_editor_get_notebook 	 (GogEditor *editor);
void 		 gog_editor_free 		 (GogEditor *editor);

typedef enum {
	GOG_OBJECT_NAME_BY_ROLE	 = 1,
	GOG_OBJECT_NAME_BY_TYPE  = 2,
	GOG_OBJECT_NAME_MANUALLY = 3
} GogObjectNamingConv;

struct _GogObjectRole {
	char const *id;	/* for persistence */
	char const *is_a_typename;
	unsigned    priority;

	guint32		  	allowable_positions;
	GogObjectPosition 	default_position;
	GogObjectNamingConv	naming_conv;

	gboolean   (*can_add)	  (GogObject const *parent);
	gboolean   (*can_remove)  (GogObject const *child);
	GogObject *(*allocate)    (GogObject *parent);
	void	   (*post_add)    (GogObject *parent, GogObject *child);
	void       (*pre_remove)  (GogObject *parent, GogObject *child);
	void       (*post_remove) (GogObject *parent, GogObject *child);

	union { /* allow people to tack some useful tidbits on the end */
		int		i;
		float		f;
		gpointer	p;
	} user;
};

struct _GogObject {
	GObject		 base;

	unsigned	 id;
	char		*user_name;	/* user assigned, NULL will fall back to system generated */
	char		*auto_name;	/* system generated, in current locale */
	
	GogObjectRole const *role;

	GogObject	*parent;
	GSList		*children;

	GogObjectPosition  position;
	GogViewAllocation  manual_position;

	unsigned needs_update : 1;
	unsigned being_updated : 1;
	unsigned explicitly_typed_role : 1; /* did we create it automaticly */
};

typedef struct {
	GObjectClass	base;

	GHashTable *roles;
	GType	    view_type;

	unsigned use_parent_as_proxy : 1; /* when we change, pretend it was our parent */
	unsigned can_manual_size : 1; 
	unsigned roles_allocated : 1;

	/* Virtuals */
	void	     (*update)		(GogObject *obj);
	void	     (*parent_changed)	(GogObject *obj, gboolean was_set);
	char const  *(*type_name)	(GogObject const *obj);
	void	     (*populate_editor)	(GogObject *obj, 
					 GogEditor *editor,
					 GogDataAllocator *dalloc, 
					 GOCmdContext *cc);

	/* signals */
	void (*changed)		(GogObject *obj, gboolean size);
	void (*name_changed)	(GogObject *obj);
	void (*possible_additions_changed) (GogObject const *obj);
	void (*child_added)	   (GogObject *parent, GogObject *child);
	void (*child_removed)	   (GogObject *parent, GogObject *child);
	void (*child_name_changed) (GogObject const *obj, GogObject const *child);
	void (*children_reordered) (GogObject *obj);
} GogObjectClass;

#define GOG_OBJECT_TYPE		(gog_object_get_type ())
#define GOG_OBJECT(o)		(G_TYPE_CHECK_INSTANCE_CAST ((o), GOG_OBJECT_TYPE, GogObject))
#define IS_GOG_OBJECT(o)	(G_TYPE_CHECK_INSTANCE_TYPE ((o), GOG_OBJECT_TYPE))
#define GOG_OBJECT_CLASS(k)	(G_TYPE_CHECK_CLASS_CAST ((k), GOG_OBJECT_TYPE, GogObjectClass))
#define IS_GOG_OBJECT_CLASS(k)	(G_TYPE_CHECK_CLASS_TYPE ((k), GOG_OBJECT_TYPE))
#define GOG_OBJECT_GET_CLASS(o)	(G_TYPE_INSTANCE_GET_CLASS ((o), GOG_OBJECT_TYPE, GogObjectClass))

#define GOG_PARAM_PERSISTENT	(1 << (G_PARAM_USER_SHIFT+0))
#define GOG_PARAM_FORCE_SAVE	(1 << (G_PARAM_USER_SHIFT+1))	/* even if the value == default */

GType gog_object_get_type (void);

typedef void (*GogDataDuplicator) (GogDataset const *src, GogDataset *dst);

GogObject   *gog_object_dup		 (GogObject const *obj, GogObject *new_parent, GogDataDuplicator dup);
GogObject   *gog_object_get_parent	 (GogObject const *obj);
GogObject   *gog_object_get_parent_typed (GogObject const *obj, GType t);
GogGraph    *gog_object_get_graph	 (GogObject const *obj);
GogTheme    *gog_object_get_theme	 (GogObject const *obj);
unsigned     gog_object_get_id		 (GogObject const *obj);
char const  *gog_object_get_name	 (GogObject const *obj);
void	     gog_object_set_name	 (GogObject *obj, char *name, GError **err);
GSList      *gog_object_get_children	 (GogObject const *obj, GogObjectRole const *filter);
GogObject   *gog_object_get_child_by_role(GogObject const *obj, GogObjectRole const *role);
gpointer     gog_object_get_editor	 (GogObject *obj,
					  GogDataAllocator *dalloc, GOCmdContext *cc);
GogView	  *gog_object_new_view	 	 (GogObject const *obj, GogView *view);
gboolean   gog_object_is_deletable	 (GogObject const *obj);
GSList    *gog_object_possible_additions (GogObject const *obj);
GogObject *gog_object_add_by_role	 (GogObject *parent,
					  GogObjectRole const *role, GogObject *child);
GogObject   *gog_object_add_by_name	 (GogObject *parent,
					  char const *role, GogObject *child);
void		  gog_object_can_reorder (GogObject const *obj,
					  gboolean *inc_ok, gboolean *dec_ok);
GogObject	 *gog_object_reorder	 (GogObject const *obj,
					  gboolean inc, gboolean goto_max);

GogObjectPosition gog_object_get_position_flags	   (GogObject const *obj, GogObjectPosition mask);
gboolean	  gog_object_set_position_flags	   (GogObject *obj, GogObjectPosition p, GogObjectPosition mask);
void	          gog_object_get_manual_position   (GogObject *obj, GogViewAllocation *pos);
void 		  gog_object_set_manual_position   (GogObject *obj, GogViewAllocation const *pos);

GogViewAllocation gog_object_get_manual_allocation (GogObject *gobj, 
						    GogViewAllocation const *parent_allocation, 
						    GogViewRequisition const *requisition);

GogObjectRole const *gog_object_find_role_by_name (GogObject const *obj,
						   char const *role);

/* protected */
void	 gog_object_update		(GogObject *obj);
gboolean gog_object_request_update	(GogObject *obj);
void 	 gog_object_emit_changed	(GogObject *obj, gboolean size);
gboolean gog_object_clear_parent	(GogObject *obj);
gboolean gog_object_set_parent	 	(GogObject *child, GogObject *parent,
					 GogObjectRole const *role, unsigned id);
void 	 gog_object_register_roles	(GogObjectClass *klass,
					 GogObjectRole const *roles, unsigned n_roles);

G_END_DECLS

#endif /* GOG_OBJECT_H */
