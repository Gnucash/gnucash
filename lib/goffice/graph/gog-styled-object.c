/* vim: set sw=8: -*- Mode: C; tab-width: 8; indent-tabs-mode: t; c-basic-offset: 8 -*- */
/*
 * gog-styled-object.c : A base class for objects that have associated styles
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
#include <goffice/graph/gog-styled-object.h>
#include <goffice/graph/gog-style.h>
#include <goffice/graph/gog-theme.h>
#include <goffice/graph/gog-graph.h>

#include <glib/gi18n.h>
#include <gsf/gsf-impl-utils.h>

enum {
	STYLED_OBJECT_PROP_0,
	STYLED_OBJECT_PROP_STYLE
};

enum {
	STYLE_CHANGED,
	LAST_SIGNAL
};
static gulong gog_styled_object_signals [LAST_SIGNAL] = { 0, };
static GObjectClass *parent_klass;

void
gog_styled_object_style_changed (GogStyledObject *obj)
{
	g_signal_emit (G_OBJECT (obj),
		gog_styled_object_signals [STYLE_CHANGED], 0, obj->style);
}

static void
gog_styled_object_set_property (GObject *obj, guint param_id,
				GValue const *value, GParamSpec *pspec)
{
	GogStyledObject *gso = GOG_STYLED_OBJECT (obj);
	gboolean resize = FALSE;

	switch (param_id) {

	case STYLED_OBJECT_PROP_STYLE :
		resize = gog_styled_object_set_style (gso, 
	      		g_value_get_object (value));
		break;

	default: G_OBJECT_WARN_INVALID_PROPERTY_ID (obj, param_id, pspec);
		 return; /* NOTE : RETURN */
	}
	gog_object_emit_changed (GOG_OBJECT (obj), resize);
}

static void
gog_styled_object_get_property (GObject *obj, guint param_id,
				GValue *value, GParamSpec *pspec)
{
	GogStyledObject *gso = GOG_STYLED_OBJECT (obj);

	switch (param_id) {
	case STYLED_OBJECT_PROP_STYLE :
		g_value_set_object (value, gso->style);
		break;

	default: G_OBJECT_WARN_INVALID_PROPERTY_ID (obj, param_id, pspec);
		 break;
	}
}

static void
gog_styled_object_finalize (GObject *obj)
{
	GogStyledObject *gso = GOG_STYLED_OBJECT (obj);

	if (gso->style != NULL) {
		g_object_unref (gso->style);
		gso->style = NULL;
	}

	(*parent_klass->finalize) (obj);
}

static gpointer
styled_object_editor (GogObject *gobj, GogDataAllocator *dalloc,
		      GnmCmdContext *cc)
{
	return gog_styled_object_editor (GOG_STYLED_OBJECT (gobj), cc, NULL);
}

static void
gog_styled_object_parent_changed (GogObject *obj, gboolean was_set)
{
	GogObjectClass *gog_object_klass = GOG_OBJECT_CLASS (parent_klass);
	if (was_set) {
		GogStyledObject *gso = GOG_STYLED_OBJECT (obj);
		gog_theme_fillin_style (gog_object_get_theme (GOG_OBJECT (gso)),
			gso->style, GOG_OBJECT (gso), 0, TRUE);
		gog_styled_object_apply_theme (gso, gso->style);
	}
	gog_object_klass->parent_changed (obj, was_set);
}

static void
gog_styled_object_init_style (GogStyledObject *gso, GogStyle *style)
{
	style->interesting_fields = GOG_STYLE_OUTLINE | GOG_STYLE_FILL; /* default */
	gog_theme_fillin_style (gog_object_get_theme (GOG_OBJECT (gso)),
		style, GOG_OBJECT (gso), 0, FALSE);
}

static void
gog_styled_object_class_init (GogObjectClass *gog_klass)
{
	GObjectClass *gobject_klass = (GObjectClass *) gog_klass;
	GogStyledObjectClass *style_klass = (GogStyledObjectClass *) gog_klass;

	gog_styled_object_signals [STYLE_CHANGED] = g_signal_new ("style-changed",
		G_TYPE_FROM_CLASS (gog_klass),
		G_SIGNAL_RUN_LAST,
		G_STRUCT_OFFSET (GogStyledObjectClass, style_changed),
		NULL, NULL,
		g_cclosure_marshal_VOID__OBJECT,
		G_TYPE_NONE,
		1, G_TYPE_OBJECT);

	parent_klass = g_type_class_peek_parent (gog_klass);
	gobject_klass->set_property = gog_styled_object_set_property;
	gobject_klass->get_property = gog_styled_object_get_property;
	gobject_klass->finalize	    = gog_styled_object_finalize;
	gog_klass->editor    	    = styled_object_editor;
	gog_klass->parent_changed   = gog_styled_object_parent_changed;
	style_klass->init_style	    = gog_styled_object_init_style;

	g_object_class_install_property (gobject_klass, STYLED_OBJECT_PROP_STYLE,
		g_param_spec_object ("style", "style",
			"GogStyle *",
			GOG_STYLE_TYPE, G_PARAM_READWRITE|GOG_PARAM_PERSISTENT));
}

static void
gog_styled_object_init (GogStyledObject *gso)
{
	gso->style = gog_style_new (); /* use the defaults */
}

GSF_CLASS (GogStyledObject, gog_styled_object,
	   gog_styled_object_class_init, gog_styled_object_init,
	   GOG_OBJECT_TYPE)

gboolean
gog_styled_object_set_style (GogStyledObject *gso,
			     GogStyle *style)
{
	gboolean resize;

	g_return_val_if_fail (GOG_STYLED_OBJECT (gso) != NULL, FALSE);

	if (gso->style == style)
		return FALSE;
	style = gog_style_dup (style);

	/* which fields are we interested in for this object */
	gog_styled_object_apply_theme (gso, style);
	gog_styled_object_style_changed (gso);
	resize = gog_style_is_different_size (gso->style, style);
	if (gso->style != NULL)
		g_object_unref (gso->style);
	gso->style = style;

	return resize;
}

/**
 * gog_styled_object_get_style :
 * @gso : #GogStyledObject
 *
 * Returns a pointer to @gso's style but does not reference it.
 **/
GogStyle *
gog_styled_object_get_style (GogStyledObject *gso)
{
	g_return_val_if_fail (GOG_STYLED_OBJECT (gso) != NULL, NULL);
	return gso->style;
}

/**
 * gog_styled_object_get_auto_style :
 * @gso : #GogStyledObject
 *
 * Returns a new style that is initialized with the auto values for @gso.
 * Caller is responsible for the result.
 **/
GogStyle *
gog_styled_object_get_auto_style (GogStyledObject *gso)
{
	GogStyle *res = gog_style_dup (gso->style);
	gog_style_force_auto (res);
	gog_styled_object_apply_theme (gso, res);
	return res;
}

void
gog_styled_object_apply_theme (GogStyledObject *gso, GogStyle *style)
{
	GogGraph const *graph = gog_object_get_graph (GOG_OBJECT (gso));
	if (graph != NULL) {
		GogStyledObjectClass *klass = GOG_STYLED_OBJECT_GET_CLASS (gso);
		(klass->init_style) (gso, style);
	}
}
