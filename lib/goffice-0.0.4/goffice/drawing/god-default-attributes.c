/* vim: set sw=8: -*- Mode: C; tab-width: 8; indent-tabs-mode: t; c-basic-offset: 8 -*- */
/*
 * god-default-attributes.c: MS Office Graphic Object support
 *
 * Copyright (C) 2000-2002
 *	Jody Goldberg (jody@gnome.org)
 *	Michael Meeks (mmeeks@gnu.org)
 *      Christopher James Lahey <clahey@ximian.com>
 *
 * This program is free software; you can redistribute it and/or
 * modify it under the terms of the GNU General Public License as
 * published by the Free Software Foundation; either version 2 of the
 * License, or (at your option) any later version.
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

#include <goffice/goffice-config.h>
#include <goffice/drawing/god-default-attributes.h>

#include <pango/pango-attributes.h>

#include <glib/gi18n.h>
#include <gsf/gsf-impl-utils.h>

static GObjectClass *parent_class;

struct GodDefaultAttributesPrivate_ {
	GPtrArray *paragraph_attributes;
	GPtrArray *pango_attributes;
};

GodDefaultAttributes *
god_default_attributes_new (void)
{
	GodDefaultAttributes *default_attributes;

	default_attributes = g_object_new (GOD_DEFAULT_ATTRIBUTES_TYPE, NULL);

	return default_attributes;
}

void
god_default_attributes_set_paragraph_attributes             (GodDefaultAttributes   *attributes,
							     GodParagraphAttributes *paragraph_attributes)
{
	god_default_attributes_set_paragraph_attributes_for_indent  (attributes,
								     0,
								     paragraph_attributes);
}

void
god_default_attributes_set_paragraph_attributes_for_indent  (GodDefaultAttributes   *attributes,
							     guint                     indent,
							     GodParagraphAttributes *paragraph_attributes)
{
	if (attributes->priv->paragraph_attributes == NULL)
		attributes->priv->paragraph_attributes = g_ptr_array_new();
	if (attributes->priv->paragraph_attributes->len <= indent)
		g_ptr_array_set_size (attributes->priv->paragraph_attributes, indent + 1);
	if (g_ptr_array_index (attributes->priv->paragraph_attributes, indent))
		g_object_unref (g_ptr_array_index (attributes->priv->paragraph_attributes, indent));
	g_ptr_array_index (attributes->priv->paragraph_attributes, indent) = paragraph_attributes;
	if (paragraph_attributes)
		g_object_ref (paragraph_attributes);
}

void
god_default_attributes_set_pango_attributes                 (GodDefaultAttributes   *attributes,
							     GList                  *pango_attributes)
{
	god_default_attributes_set_pango_attributes_for_indent  (attributes,
								 0,
								 pango_attributes);
}

static void
add_attributes (PangoAttribute *attribute, GList **new_attributes)
{
	PangoAttribute *new_attribute = pango_attribute_copy (attribute);
	new_attribute->start_index = 0;
	new_attribute->end_index = (guint) -1;
	*new_attributes = g_list_prepend (*new_attributes, pango_attribute_copy (attribute));
}

void
god_default_attributes_set_pango_attributes_for_indent      (GodDefaultAttributes  *attributes,
							     guint                  indent,
							     GList                 *pango_attributes)
{
	GList **pango_attributes_location;
	if (attributes->priv->pango_attributes == NULL)
		attributes->priv->pango_attributes = g_ptr_array_new();
	if (attributes->priv->pango_attributes->len <= indent)
		g_ptr_array_set_size (attributes->priv->pango_attributes, indent + 1);

	pango_attributes_location = (GList **) &g_ptr_array_index (attributes->priv->pango_attributes, indent);

	g_list_foreach (*pango_attributes_location, (GFunc) pango_attribute_destroy, NULL);
	g_list_free (*pango_attributes_location);
	*pango_attributes_location = NULL;
	g_list_foreach (pango_attributes, (GFunc) add_attributes, pango_attributes_location);
	*pango_attributes_location = g_list_reverse (*pango_attributes_location);
}

const GodParagraphAttributes *
god_default_attributes_get_paragraph_attributes             (GodDefaultAttributes   *attributes)
{
	return god_default_attributes_get_paragraph_attributes_for_indent  (attributes,
									    0);
}

const GodParagraphAttributes *
god_default_attributes_get_paragraph_attributes_for_indent  (GodDefaultAttributes   *attributes,
							     guint                   indent)
{
	if (attributes->priv->paragraph_attributes == NULL)
		return NULL;
	if (attributes->priv->paragraph_attributes->len <= indent)
		return NULL;
	return g_ptr_array_index (attributes->priv->paragraph_attributes, indent);
}

const GList *
god_default_attributes_get_pango_attributes                 (GodDefaultAttributes   *attributes)
{
	return god_default_attributes_get_pango_attributes_for_indent  (attributes,
									0);
}

const GList *
god_default_attributes_get_pango_attributes_for_indent      (GodDefaultAttributes   *attributes,
							     guint                     indent)
{
	if (attributes->priv->pango_attributes == NULL)
		return NULL;
	if (attributes->priv->pango_attributes->len <= indent)
		return NULL;
	return g_ptr_array_index (attributes->priv->pango_attributes, indent);
}

static void
god_default_attributes_init (GObject *object)
{
	GodDefaultAttributes *default_attributes = GOD_DEFAULT_ATTRIBUTES (object);
	default_attributes->priv = g_new0 (GodDefaultAttributesPrivate, 1);
	default_attributes->priv->paragraph_attributes = NULL;
	default_attributes->priv->pango_attributes = NULL;
}

static void
maybe_unref (gpointer data, gpointer user_data)
{
	if (data)
		g_object_unref (data);
}

static void
free_list (gpointer data, gpointer user_data)
{
	g_list_foreach (data, (GFunc) pango_attribute_destroy, NULL);
}

static void
god_default_attributes_finalize (GObject *object)
{
	GodDefaultAttributes *default_attributes = GOD_DEFAULT_ATTRIBUTES (object);

	g_ptr_array_foreach (default_attributes->priv->paragraph_attributes, maybe_unref, NULL);
	g_ptr_array_foreach (default_attributes->priv->pango_attributes, free_list, NULL);
	g_ptr_array_free (default_attributes->priv->paragraph_attributes, TRUE);
	g_ptr_array_free (default_attributes->priv->pango_attributes, TRUE);
	g_free (default_attributes->priv);
	default_attributes->priv = NULL;

	G_OBJECT_CLASS (parent_class)->finalize (object);
}

static void
god_default_attributes_class_init (GodDefaultAttributesClass *class)
{
	GObjectClass *object_class;

	object_class               = (GObjectClass *) class;

	parent_class               = g_type_class_peek_parent (class);

	object_class->finalize     = god_default_attributes_finalize;
}

GSF_CLASS (GodDefaultAttributes, god_default_attributes,
	   god_default_attributes_class_init, god_default_attributes_init,
	   G_TYPE_OBJECT)
