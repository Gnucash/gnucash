/* vim: set sw=8: -*- Mode: C; tab-width: 8; indent-tabs-mode: t; c-basic-offset: 8 -*- */
/*
 * gog-object-xml.c :
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

#include <goffice/goffice-config.h>
#include <goffice/graph/gog-object-xml.h>
#include <goffice/graph/gog-object.h>
#include <goffice/graph/gog-plot.h>
#include <goffice/graph/gog-reg-curve.h>
#include <goffice/graph/gog-data-set.h>
#include <goffice/data/go-data.h>
#include <goffice/utils/go-color.h>

#include <glade/glade-build.h>	/* for the xml utils */
#include <string.h>
#include <stdlib.h>

GType
gog_persist_get_type (void)
{
	static GType gog_persist_type = 0;

	if (!gog_persist_type) {
		static GTypeInfo const gog_persist_info = {
			sizeof (GogPersistClass),	/* class_size */
			NULL,		/* base_init */
			NULL,		/* base_finalize */
		};

		gog_persist_type = g_type_register_static (G_TYPE_INTERFACE,
			"GogPersist", &gog_persist_info, 0);
	}

	return gog_persist_type;
}

gboolean
gog_persist_dom_load (GogPersist *gp, xmlNode *node)
{
	g_return_val_if_fail (IS_GOG_PERSIST (gp), FALSE);
	return GOG_PERSIST_GET_CLASS (gp)->dom_load (gp, node);
}

void
gog_persist_dom_save (GogPersist const *gp, xmlNode *parent)
{
	g_return_if_fail (IS_GOG_PERSIST (gp));
	GOG_PERSIST_GET_CLASS (gp)->dom_save (gp, parent);
}
void
gog_persist_sax_save (GogPersist const *gp, GsfXMLOut *output)
{
	g_return_if_fail (IS_GOG_PERSIST (gp));
	GOG_PERSIST_GET_CLASS (gp)->sax_save (gp, output);
}

static void
gog_object_set_arg_full (char const *name, char const *val, GogObject *obj, xmlNode *xml_node)
{
	GParamSpec *pspec = g_object_class_find_property (G_OBJECT_GET_CLASS (obj), name);
	GType prop_type;
	GValue res = { 0 };
	gboolean success = TRUE;

	if (pspec == NULL) {
		g_warning ("unknown property `%s' for class `%s'",
			   name, G_OBJECT_TYPE_NAME (obj));
		return;
	}

	prop_type = G_PARAM_SPEC_VALUE_TYPE (pspec);
	if (val == NULL &&
	    G_TYPE_FUNDAMENTAL (prop_type) != G_TYPE_BOOLEAN) {
		g_warning ("could not convert NULL to type `%s' for property `%s'",
			   g_type_name (prop_type), pspec->name);
		return;
	}

	g_value_init (&res, prop_type);
	switch (G_TYPE_FUNDAMENTAL (prop_type)) {
	case G_TYPE_CHAR:
		g_value_set_char (&res, val[0]);
		break;
	case G_TYPE_UCHAR:
		g_value_set_uchar (&res, (guchar)val[0]);
		break;
	case G_TYPE_BOOLEAN:
		g_value_set_boolean (&res, 
			val == NULL ||
			g_ascii_tolower (val[0]) == 't' ||
			g_ascii_tolower (val[0]) == 'y' ||
			strtol (val, NULL, 0));
		break;
	case G_TYPE_INT:
		g_value_set_int (&res, strtol (val, NULL, 0));
		break;
	case G_TYPE_UINT:
		g_value_set_uint (&res, strtoul (val, NULL, 0));
		break;
	case G_TYPE_LONG:
		g_value_set_long (&res, strtol (val, NULL, 0));
		break;
	case G_TYPE_ULONG:
		g_value_set_ulong (&res, strtoul (val, NULL, 0));
		break; 
	case G_TYPE_ENUM:
		g_value_set_enum (&res, glade_enum_from_string (prop_type, val));
		break;
	case G_TYPE_FLAGS:
		g_value_set_flags (&res, glade_flags_from_string (prop_type, val));
		break;
	case G_TYPE_FLOAT:
		g_value_set_float (&res, g_strtod (val, NULL));
		break;
	case G_TYPE_DOUBLE:
		g_value_set_double (&res, g_strtod (val, NULL));
		break;
	case G_TYPE_STRING:
		g_value_set_string (&res, val);
		break;

	case G_TYPE_OBJECT:
		if (g_type_is_a (prop_type, G_TYPE_OBJECT)) {
			xmlChar *type_name;
			GType    type = 0;
			GObject *val_obj;

			success = FALSE;
			type_name = xmlGetProp (xml_node, 
						(xmlChar const *) "type");
			if (type_name != NULL) {
				type = g_type_from_name (type_name);
			}
			xmlFree (type_name);
			if (type != 0) {
				val_obj = g_object_new (type, NULL);
				if (IS_GOG_PERSIST (val_obj) &&
				    gog_persist_dom_load (GOG_PERSIST (val_obj), xml_node)) {
					g_value_set_object (&res, val_obj);
					success = TRUE;
				}
				g_object_unref (val_obj);
			}
		}
		break;

	default:
		success = FALSE;
	}

	if (!success) {
		g_warning ("could not convert string to type `%s' for property `%s'",
			   g_type_name (prop_type), pspec->name);
	} else
		g_object_set_property (G_OBJECT (obj), name, &res);
	g_value_unset (&res);
}

void
gog_object_set_arg (char const *name, char const *val, GogObject *obj)
{
	gog_object_set_arg_full (name, val, obj, NULL);
}

static void
gog_object_write_property_sax (GogObject const *obj, GParamSpec *pspec, GsfXMLOut *output)
{
	GObject *val_obj;
	GType    prop_type = G_PARAM_SPEC_VALUE_TYPE (pspec);
	GValue	 value = { 0 };

	g_value_init (&value, prop_type);
	g_object_get_property  (G_OBJECT (obj), pspec->name, &value);

	/* No need to save default values */
	if (!(pspec->flags & GOG_PARAM_FORCE_SAVE) &&
	    g_param_value_defaults (pspec, &value)) {
		g_value_unset (&value);
		return;
	}

	switch (G_TYPE_FUNDAMENTAL (prop_type)) {
	case G_TYPE_CHAR:
	case G_TYPE_UCHAR:
	case G_TYPE_BOOLEAN:
	case G_TYPE_INT:
	case G_TYPE_UINT:
	case G_TYPE_LONG:
	case G_TYPE_ULONG:
	case G_TYPE_FLOAT:
	case G_TYPE_DOUBLE: {
		GValue str = { 0 };
		g_value_init (&str, G_TYPE_STRING);
		g_value_transform (&value, &str);
		gsf_xml_out_start_element (output, "property");
		gsf_xml_out_add_cstr_unchecked (output, "name", pspec->name);
		gsf_xml_out_add_cstr (output, NULL, g_value_get_string (&str));
		gsf_xml_out_end_element (output); /* </property> */
		g_value_unset (&str);
		break;
	}

	case G_TYPE_STRING: {
		char const *str = g_value_get_string (&value);
		if (str != NULL) {
			gsf_xml_out_start_element (output, "property");
			gsf_xml_out_add_cstr_unchecked (output, "name", pspec->name);
			gsf_xml_out_add_cstr (output, NULL, str);
			gsf_xml_out_end_element (output); /* </property> */
		}
		break;
	}

	case G_TYPE_OBJECT:
		val_obj = g_value_get_object (&value);
		if (val_obj != NULL) {
			if (IS_GOG_PERSIST (val_obj)) {
				gsf_xml_out_start_element (output, "property");
				gsf_xml_out_add_cstr_unchecked (output, "name", pspec->name);
				gog_persist_sax_save (GOG_PERSIST (val_obj), output);
				gsf_xml_out_end_element (output); /* </property> */
			} else
				g_warning ("How are we supposed to persist this ??");
		}
		break;

	default:
		;
	}
	g_value_unset (&value);
}

static void
gog_object_write_property (GogObject *obj, GParamSpec *pspec, xmlNode *parent)
{
	GObject *val_obj;
	GType    prop_type = G_PARAM_SPEC_VALUE_TYPE (pspec);
	gboolean success = TRUE;
	GValue	 value = { 0 };
	xmlNode *node;

	g_value_init (&value, prop_type);
	g_object_get_property  (G_OBJECT (obj), pspec->name, &value);

	/* No need to save default values */
	if (!(pspec->flags & GOG_PARAM_FORCE_SAVE) &&
	    g_param_value_defaults (pspec, &value)) {
		g_value_unset (&value);
		return;
	}

	node = xmlNewDocNode (parent->doc, NULL,
			      (xmlChar const *)"property", NULL);

	switch (G_TYPE_FUNDAMENTAL (prop_type)) {
	case G_TYPE_CHAR:
	case G_TYPE_UCHAR:
	case G_TYPE_BOOLEAN:
	case G_TYPE_INT:
	case G_TYPE_UINT:
	case G_TYPE_LONG:
	case G_TYPE_ULONG:
	case G_TYPE_FLOAT:
	case G_TYPE_DOUBLE: {
		GValue str = { 0 };
		g_value_init (&str, G_TYPE_STRING);
		g_value_transform (&value, &str);
		xmlNodeSetContent (node, g_value_get_string (&str));
		g_value_unset (&str);
		break;
	}

	case G_TYPE_STRING: {
		char const *str = g_value_get_string (&value);
		if (str != NULL)
			xmlNodeSetContent (node, str);
		else
			success = FALSE;
		break;
	}

	case G_TYPE_OBJECT:
		val_obj = g_value_get_object (&value);
		if (val_obj != NULL) {
			if (IS_GOG_PERSIST (val_obj)) {
				gog_persist_dom_save (GOG_PERSIST (val_obj), node);
			} else
				g_warning ("How are we supposed to persist this ??");
		} else
			success = FALSE;
		break;

	default:
		success = FALSE;
	}
	g_value_unset (&value);

	if (success) {
		xmlSetProp (node, (xmlChar const *) "name", pspec->name);
		xmlAddChild (parent, node);
	} else
		xmlFreeNode (node);
}

static void
gog_dataset_load (GogDataset *set, xmlNode *node)
{
	xmlNode *ptr;
	xmlChar *id, *val, *type;

	for (ptr = node->xmlChildrenNode ; ptr != NULL ; ptr = ptr->next) {
		if (xmlIsBlankNode (ptr) || ptr->name == NULL)
			continue;
		if (!strcmp (ptr->name, "data"))
			break;
	}
	if (ptr == NULL)
		return;
	for (ptr = ptr->xmlChildrenNode ; ptr != NULL ; ptr = ptr->next) {
		if (xmlIsBlankNode (ptr) || ptr->name == NULL)
			continue;
		if (!strcmp (ptr->name, "dimension")) {
			id   = xmlGetProp (ptr, (xmlChar const *) "id");
			type = xmlGetProp (ptr, (xmlChar const *) "type");
			val  = xmlNodeGetContent (ptr);
			if (id != NULL && type != NULL && val != NULL) {
				unsigned dim_id = strtoul (id, NULL, 0);
				GOData *dat = g_object_new (g_type_from_name (type), NULL);
				if (dat != NULL && go_data_from_str (dat, val))
					gog_dataset_set_dim (set, dim_id, dat, NULL);
			}

			if (id != NULL)	  xmlFree (id);
			if (type != NULL) xmlFree (type);
			if (val != NULL)  xmlFree (val);
		}
	}
}

static void
gog_dataset_sax_save (GogDataset const *set, GsfXMLOut *output)
{
	GOData  *dat;
	char    *tmp, buffer[10];
	int      i, last;

	gsf_xml_out_start_element (output, "data");
	gog_dataset_dims (set, &i, &last);
	for ( ; i <= last ; i++) {
		dat = gog_dataset_get_dim (set, i);
		if (dat == NULL)
			continue;

		gsf_xml_out_start_element (output, "dimension");
		g_snprintf (buffer, sizeof buffer, "%d", i);
		gsf_xml_out_add_cstr (output, "id", buffer);
		gsf_xml_out_add_cstr (output, "type", 
			G_OBJECT_TYPE_NAME (dat));
		tmp = go_data_as_str (dat);
		gsf_xml_out_add_cstr (output, NULL, tmp);
		g_free (tmp);
		gsf_xml_out_end_element (output); /* </dimension> */
	}
	gsf_xml_out_end_element (output); /* </data> */

}
static void
gog_dataset_dom_save (GogDataset *set, xmlNode *parent)
{
	xmlNode *node, *child;
	char    *tmp, buffer[10];
	GOData  *dat;
	int      i, last;

	node = xmlNewDocNode (parent->doc, NULL, (xmlChar const *)"data", NULL);
	gog_dataset_dims (set, &i, &last);
	for ( ; i <= last ; i++) {
		dat = gog_dataset_get_dim (set, i);
		if (dat == NULL)
			continue;

		tmp = go_data_as_str (dat);
		child = xmlNewChild (node, NULL,
			(xmlChar const *) ("dimension"), tmp);
		g_free (tmp);

		g_snprintf (buffer, sizeof buffer, "%d", i);
		xmlSetProp (child, (xmlChar const *) "id", buffer);
		xmlSetProp (child, (xmlChar const *) "type",
			G_OBJECT_TYPE_NAME (dat));
	}

	xmlAddChild (parent, node);
}

void
gog_object_write_xml_sax (GogObject const *obj, GsfXMLOut *output)
{
	gint	     n;
	GParamSpec **props;
	GSList	    *ptr;

	gsf_xml_out_start_element (output, "GogObject");

	/* Primary details */
	if (obj->role != NULL)
		gsf_xml_out_add_cstr (output, "role", obj->role->id);
	if (obj->explicitly_typed_role || obj->role == NULL)
		gsf_xml_out_add_cstr (output, "type", G_OBJECT_TYPE_NAME (obj));

	/* properties */
	props = g_object_class_list_properties (G_OBJECT_GET_CLASS (obj), &n);
	while (n-- > 0)
		if (props[n]->flags & GOG_PARAM_PERSISTENT)
			gog_object_write_property_sax (obj, props[n], output);

	g_free (props);

	if (IS_GOG_PERSIST (obj))	/* anything special for this class */
		gog_persist_sax_save (GOG_PERSIST (obj), output);
	if (IS_GOG_DATASET (obj))	/* convenience to save data */
		gog_dataset_sax_save (GOG_DATASET (obj), output);

	/* the children */
	for (ptr = obj->children; ptr != NULL ; ptr = ptr->next)
		gog_object_write_xml_sax (ptr->data, output);

	gsf_xml_out_end_element (output); /* </GogObject> */
}

xmlNode  *
gog_object_write_xml (GogObject *obj, xmlDoc *doc)
{
	gint	     n;
	GParamSpec **props;
	GSList	    *ptr;
	xmlNode	    *node = xmlNewDocNode (doc, NULL,
		(xmlChar const *)"GogObject", NULL);

	/* Primary details */
	if (obj->role != NULL)
		xmlSetProp (node, (xmlChar const *) "role", obj->role->id);
	if (obj->explicitly_typed_role || obj->role == NULL)
		xmlSetProp (node, (xmlChar const *) "type", G_OBJECT_TYPE_NAME (obj));

	/* properties */
	props = g_object_class_list_properties (G_OBJECT_GET_CLASS (obj), &n);
	while (n-- > 0)
		if (props[n]->flags & GOG_PARAM_PERSISTENT)
			gog_object_write_property (obj, props[n], node);

	g_free (props);

	if (IS_GOG_PERSIST (obj))	/* anything special for this class */
		gog_persist_dom_save (GOG_PERSIST (obj), node);
	if (IS_GOG_DATASET (obj))	/* convenience to save data */
		gog_dataset_dom_save (GOG_DATASET (obj), node);

	/* the children */
	for (ptr = obj->children; ptr != NULL ; ptr = ptr->next)
		xmlAddChild (node, gog_object_write_xml (ptr->data, doc));

	return node;
}

GogObject *
gog_object_new_from_xml (GogObject *parent, xmlNode *node)
{
	xmlChar   *role, *name, *val, *type_name;
	xmlNode   *ptr;
	GogObject *res = NULL;
	gboolean explicitly_typed_role = FALSE;

	type_name = xmlGetProp (node, (xmlChar const *) "type");
	if (type_name != NULL) {
		GType type = g_type_from_name (type_name);
		if (type == 0) {
			GogPlot *plot = gog_plot_new_by_name (type_name);
			if (plot)
				res = GOG_OBJECT (plot);
			else {
				GogRegCurve *curve = gog_reg_curve_new_by_name (type_name);
				res = GOG_OBJECT (curve);
			}
		} else
			res = g_object_new (type, NULL);
		xmlFree (type_name);
		explicitly_typed_role = TRUE;
		g_return_val_if_fail (res != NULL, NULL);
	}
	role = xmlGetProp (node, (xmlChar const *) "role");
	if (role == NULL) {
		g_return_val_if_fail (parent == NULL, NULL);
	} else {
		res = gog_object_add_by_name (parent, role, res);
		xmlFree (role);
	}

	g_return_val_if_fail (res != NULL, NULL);

	res->explicitly_typed_role = explicitly_typed_role;

	if (IS_GOG_PERSIST (res))
		gog_persist_dom_load (GOG_PERSIST (res), node);
	if (IS_GOG_DATASET (res))	/* convenience to save data */
		gog_dataset_load (GOG_DATASET (res), node);

	for (ptr = node->xmlChildrenNode ; ptr != NULL ; ptr = ptr->next) {
		if (xmlIsBlankNode (ptr) || ptr->name == NULL)
			continue;
		if (!strcmp (ptr->name, "property")) {
			name = xmlGetProp (ptr, "name");
			if (name == NULL) {
				g_warning ("missing name for property entry");
				continue;
			}
			val = xmlNodeGetContent (ptr);
			gog_object_set_arg_full (name, val, res, ptr);
			xmlFree (val);
			xmlFree (name);
		} else if (!strcmp (ptr->name, "GogObject"))
			gog_object_new_from_xml (res, ptr);
	}
	return res;
}

void
go_xml_out_add_color (GsfXMLOut *output, char const *id, GOColor c)
{
	char *str = go_color_as_str (c);
	gsf_xml_out_add_cstr_unchecked (output, id, str);
	g_free (str);
}
