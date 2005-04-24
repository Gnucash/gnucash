/* vim: set sw=8: -*- Mode: C; tab-width: 8; indent-tabs-mode: t; c-basic-offset: 8 -*- */
/*
 * gog-object-xml.h : 
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
#ifndef GOG_OBJECT_XML_H
#define GOG_OBJECT_XML_H

#include <goffice/graph/goffice-graph.h>
#include <goffice/utils/goffice-utils.h>
#include <glib-object.h>
#include <libxml/tree.h>
#include <gsf/gsf-libxml.h>

G_BEGIN_DECLS

typedef struct _GogPersist GogPersist;

typedef struct {
	GTypeInterface base;

	gboolean (*dom_load) (GogPersist *gp, xmlNode *node);
	void     (*dom_save) (GogPersist const *gp, xmlNode *parent);
	void     (*sax_save) (GogPersist const *gp, GsfXMLOut *output);
} GogPersistClass;

#define GOG_PERSIST_TYPE	 (gog_persist_get_type ())
#define GOG_PERSIST(o)		 (G_TYPE_CHECK_INSTANCE_CAST ((o), GOG_PERSIST_TYPE, GogPersist))
#define IS_GOG_PERSIST(o)	 (G_TYPE_CHECK_INSTANCE_TYPE ((o), GOG_PERSIST_TYPE))
#define GOG_PERSIST_CLASS(k)	 (G_TYPE_CHECK_CLASS_CAST ((k), GOG_PERSIST_TYPE, GogPersistClass))
#define IS_GOG_PERSIST_CLASS(k)	 (G_TYPE_CHECK_CLASS_TYPE ((k), GOG_PERSIST_TYPE))
#define GOG_PERSIST_GET_CLASS(o) (G_TYPE_INSTANCE_GET_INTERFACE ((o), GOG_PERSIST_TYPE, GogPersistClass))

GType gog_persist_get_type (void);

gboolean gog_persist_dom_load (GogPersist *gp, xmlNode *node);
void     gog_persist_dom_save (GogPersist const *gp, xmlNode *parent);
void     gog_persist_sax_save (GogPersist const *gp, GsfXMLOut *output);

void	   gog_object_set_arg	   (char const *name, char const *val, GogObject *obj);
void	   gog_object_write_xml_sax(GogObject const *obj, GsfXMLOut *output);
xmlNode   *gog_object_write_xml	   (GogObject *obj, xmlDoc *doc);
GogObject *gog_object_new_from_xml (GogObject *parent, xmlNode *node);

void	   go_xml_out_add_color (GsfXMLOut *out, char const *id, GOColor c);

G_END_DECLS

#endif /* GOG_OBJECT_XML_H */
